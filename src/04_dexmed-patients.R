library(tidyverse)
library(lubridate)
library(edwr)

dir_raw <- "data/raw/dexmed"
tz <- "US/Central"
picu <- "HC PICU"

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
    semi_join(meds_clon, by = "millennium.id") %>%
    distinct(millennium.id)

mbo_id <- concat_encounters(pts_include$millennium.id) 

dc <- raw_pts %>%
    semi_join(pts_include, by = "millennium.id") %>%
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
    semi_join(dc, by = "millennium.id")
    
id <- read_data(dir_raw, "ident", FALSE) %>%
    as.id() %>%
    semi_join(dc, by = "millennium.id")

locations <- read_data(dir_raw, "locations", FALSE) %>%
    as.locations() %>%
    tidy_data() %>%
    filter(location == picu) %>%
    semi_join(dc, by = "millennium.id")

meds <- read_data(dir_raw, "meds-all-inpt", FALSE) %>%
    as.meds_inpt() %>%
    semi_join(dc, by = "millennium.id")

data_vent <- read_data(dir_raw, "vent-times", FALSE) %>%
    as.vent_times() %>%
    tidy_data(dc) %>%
    filter(vent.duration > 0) %>%
    semi_join(dc, by = "millennium.id")

weights <- read_data(dir_raw, "events-measures", FALSE) %>%
    as.events(order_var = FALSE) %>%
    filter(event == "weight") %>%
    mutate_at("event.result", as.numeric) %>%
    semi_join(dc, by = "millennium.id")

# dexmedetomidine --------------------------------------

dexmed <- meds_dexm %>%
    semi_join(dc, by = "millennium.id") %>%
    calc_runtime() %>%
    summarize_data()

dexmed_daily <- meds_dexm %>%
    semi_join(dc, by = "millennium.id") %>%
    arrange(millennium.id, med.datetime) %>%
    mutate(med.day = floor_date(med.datetime, unit = "days")) %>%
    calc_runtime(med.day) %>%
    summarize_data(med.day) %>%
    filter(cum.dose > 0)

clon <- meds_clon %>%
    semi_join(dc, by = "millennium.id") %>%
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

clon_summary <- clon %>%
    calc_runtime(cont = FALSE) %>%
    summarize_data(cont = FALSE)
    
clon_first <- clon %>%
    arrange(millennium.id, med.datetime) %>%
    distinct(millennium.id, .keep_all = TRUE) %>%
    select(
        millennium.id,
        clon.start = med.datetime,
        clon.dose = med.dose,
        clon.route = route
    )

dexmed_clon <- dexmed %>%
    left_join(clon_first, by = "millennium.id") %>%
    mutate(
        clon.wean = clon.start <= stop.datetime + hours(24)
    )

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
    left_join(
        orders[c("millennium.id", "order.id", "prn")], 
        by = c("millennium.id", "orig.order.id" = "order.id")
    ) %>%
    filter(prn == "Scheduled") %>%
    left_join(
        dexmed[c("millennium.id", "start.datetime")],
        by = "millennium.id"
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
