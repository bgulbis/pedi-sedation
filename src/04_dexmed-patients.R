library(tidyverse)
library(lubridate)
library(edwr)
library(openxlsx)

dir_raw <- "data/raw/dexmed"
tz <- "US/Central"
picu <- "HC PICU"
id_col <- "millennium.id"

dirr::gzip_files(dir_raw)

# run MBO query
#   * Patients - by Medication (Generic) - Location
#       - Facility (Curr): HC Childrens
#       - Nurse Unit (Med): HC PICU
#       - Medication (Generic): dexmedetomidine;clonidine
#       - Admit Date: 7/1/2016 - 4/30/2018

raw_pts <- read_data(dir_raw, "patients", FALSE) %>%
    as.patients()

mbo_pts <- concat_encounters(raw_pts$millennium.id, 500)

# run MBO queries
#   * Medications - Inpatient - Prompt
#       - Medication (Generic): dexmedetomidine;clonidine

raw_meds <- read_data(dir_raw, "meds-inpt", FALSE) %>%
    as.meds_inpt()

meds_dexm <- raw_meds %>%
    filter(
        med == "dexmedetomidine",
        !is.null(event.tag),
        med.location == picu
    ) 

meds_clon <- raw_meds %>%
    filter(
        med == "clonidine",
        med.location == picu
    ) 

pts_include <- meds_dexm %>%
    semi_join(meds_clon, by = id_col) %>%
    distinct(millennium.id)

mbo_id <- concat_encounters(pts_include$millennium.id) 

dc <- raw_pts %>%
    semi_join(pts_include, by = id_col) %>%
    filter(!is.na(discharge.datetime)) %>%
    select(millennium.id, discharge.datetime) 

# run MBO queries
#   * Clinical Events - Measures
#   * Demographics - Pedi
#   * Identifiers - by Millennium Encounter id
#   * Location History
#   * Medications - Inpatient - All
#   * Vent Times

demog <- read_data(dir_raw, "demog", FALSE) %>%
    as.demographics(
        extras = list("age.days" = "Age- Days (At Admit)")
    ) %>%
    semi_join(dc, by = id_col)
    
id <- read_data(dir_raw, "ident", FALSE) %>%
    as.id() %>%
    semi_join(dc, by = id_col)

meds <- read_data(dir_raw, "meds-all-inpt", FALSE) %>%
    as.meds_inpt() %>%
    semi_join(dc, by = id_col)

data_vent <- read_data(dir_raw, "vent-times", FALSE) %>%
    as.vent_times() %>%
    tidy_data(dc) %>%
    filter(vent.duration > 0) %>%
    semi_join(dc, by = id_col)

weights <- read_data(dir_raw, "events-measures", FALSE) %>%
    as.events(order_var = FALSE) %>%
    filter(event == "weight") %>%
    mutate_at("event.result", as.numeric) %>%
    semi_join(dc, by = id_col)

# sedatives --------------------------------------

tmp_dexmed <- meds_dexm %>%
    semi_join(dc, by = id_col) %>%
    calc_runtime() %>%
    summarize_data() 

tmp_weight <- weights %>%
    left_join(
        tmp_dexmed[c(id_col, "start.datetime")],
        by = id_col
    ) %>%
    arrange(millennium.id, event.datetime) %>%
    group_by(millennium.id) %>%
    summarize(
        weight.first = first(event.result),
        weight.last = last(event.result)
    )

tmp_clonidine <- meds_clon %>%
    semi_join(dc, by = id_col) %>%
    left_join(
        tmp_dexmed[c(id_col, "start.datetime")],
        by = id_col
    ) %>%
    filter(
        med.datetime >= start.datetime,
        !route %in% c("TOP", "IV", "EPIDURAL")
    ) %>%
    mutate_at(
        "med.dose",
        funs(
            if_else(
                med.dose.units == "mg", 
                . * 1000,
                .
            )
        )
    ) 

data_clonidine <- tmp_clonidine %>%
    calc_runtime(cont = FALSE) %>%
    summarize_data(cont = FALSE)

# data_clonidine_daily <- tmp_clonidine %>%
#     mutate(med.day = floor_date(med.datetime, unit = "days")) %>%
#     calc_runtime(med.day, cont = FALSE) %>%
#     summarize_data(med.day, cont = FALSE)

data_clonidine_wt <- tmp_clonidine %>%
    left_join(tmp_weight, by = id_col) %>%
    mutate(med.day = floor_date(med.datetime, unit = "days")) %>%
    mutate_at("med.dose", funs(. / weight.first)) %>%
    calc_runtime(med.day, cont = FALSE) %>%
    summarize_data(med.day, cont = FALSE) %>%
    group_by(millennium.id) %>%
    summarize(
        first.dose = first(cum.dose),
        max.dose = max(cum.dose)
    ) %>%
    left_join(
        data_clonidine[c(id_col, "first.datetime", "last.datetime")],
        by = id_col
    ) %>%
    left_join(
        tmp_dexmed[c(id_col, "stop.datetime")],
        by = id_col
    ) %>%
    mutate(clon.24h = first.datetime <= stop.datetime + hours(24)) %>%
    group_by(
        millennium.id,
        first.dose, 
        max.dose, 
        first.datetime, 
        last.datetime
    ) %>%
    summarize_at("clon.24h", sum, na.rm = TRUE) %>%
    mutate_at("clon.24h", funs(. == 1))

data_dexmed <- tmp_dexmed %>%
    semi_join(data_clonidine, by = id_col) 

data_dexmed_daily <- meds_dexm %>%
    semi_join(data_clonidine, by = id_col) %>%
    arrange(millennium.id, med.datetime) %>%
    mutate(med.day = floor_date(med.datetime, unit = "days")) %>%
    calc_runtime(med.day) %>%
    summarize_data(med.day) %>%
    filter(cum.dose > 0)

# get order.id for lorazepam, methadone - only want scheduled meds

meds_alt <- meds %>%
    filter(
        med %in% c("lorazepam", "methadone"),
        med.location == picu
    ) %>%
    mutate(orig.order.id = order.parent.id) %>%
    mutate_at("orig.order.id", na_if, y = 0L) %>%
    mutate_at("orig.order.id", funs(coalesce(., order.id)))

mbo_order <- concat_encounters(meds_alt$orig.order.id)

# run MBO query
#   * Orders Meds - Details - by Order Id

orders <- read_data(dir_raw, "orders-details", FALSE) %>%
    as.order_detail(extras = list("order.product" = "Mnemonic (Product)"))

data_meds_other <- meds_alt %>%
    semi_join(data_clonidine, by = id_col) %>%
    left_join(
        orders[c(id_col, "order.id", "prn")], 
        by = c(id_col, "orig.order.id" = "order.id")
    ) %>%
    filter(prn == "Scheduled") %>%
    left_join(
        data_dexmed[c(id_col, "start.datetime")],
        by = id_col
    ) %>%
    filter(med.datetime > start.datetime) %>%
    arrange(millennium.id, med.datetime) %>%
    distinct(millennium.id, med, .keep_all = TRUE) %>%
    mutate(
        time.dexmed = difftime(
            med.datetime,
            start.datetime,
            units = "days"
        )
    ) %>%
    select(
        millennium.id,
        med,
        med.datetime,
        med.dose,
        med.dose.units,
        time.dexmed
    ) 

# data sets --------------------------------------------

data_demographics <- id %>%
    semi_join(data_clonidine, by = id_col) %>%
    left_join(demog, by = id_col) %>%
    left_join(tmp_weight, by = id_col) %>%
    select(-race, -(disposition:visit.type))

data_locations <- read_data(dir_raw, "locations", FALSE) %>%
    as.locations() %>%
    tidy_data() %>%
    filter(location == picu) %>%
    semi_join(data_clonidine, by = id_col)

dirr::save_rds("data/tidy/dexmed", "data_")

ls(.GlobalEnv, pattern = "data_") %>%
    walk(
        ~write.xlsx(
            get(.x), 
            paste0("data/external/dexmed/", .x, ".xlsx")
        )
    )
