library(tidyverse)
library(lubridate)
# library(edwr)
library(openxlsx)

dir_data <- "data/tidy/dexmed-only"
tz_locale <- locale(tz = "US/Central")

dirr::gzip_files(dir_data)

get_data <- function(path, pattern, col_types = NULL) {
    f <- list.files(path, pattern, full.names = TRUE)
    
    n <- f %>% 
        purrr::map_int(~ nrow(data.table::fread(.x, select = 1L))) 
    
    f[n > 0] %>%
        purrr::map_df(
            readr::read_csv,
            locale = tz_locale,
            col_types = col_types
        ) %>%
        rename_all(stringr::str_to_lower)
}

df_demog <- get_data(dir_data, "demographics") %>%
    select(-race)

df_dexmed <- get_data(dir_data, "dexmed_events")

df_meds <- get_data(dir_data, "meds")

df_weights <- get_data(dir_data, "weights")

df_locations <- get_data(dir_data, "locations")

df_vent_times <- get_data(dir_data, "vent_times") 

df_vent_events <- get_data(dir_data, "vent_events") 

tmp_vent <- df_vent_events %>%
    bind_rows(df_vent_times) %>%
    arrange(encounter_id, result_datetime) %>%
    group_by(encounter_id) %>%
    mutate(new_event = event != lag(event) | is.na(lag(event))) %>%
    mutate_at("new_event", cumsum) 
    



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

data_dexmed <- tmp_dexmed 

data_dexmed_daily <- meds_dexm %>%
    # semi_join(data_clonidine, by = id_col) %>%
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
    # semi_join(data_clonidine, by = id_col) %>%
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
    # semi_join(data_clonidine, by = id_col) %>%
    left_join(demog, by = id_col) %>%
    left_join(tmp_weight, by = id_col) %>%
    select(-race, -(disposition:visit.type))

data_locations <- read_data(dir_raw, "locations", FALSE) %>%
    as.locations() %>%
    tidy_data() %>%
    filter(location == picu) 
    # semi_join(data_clonidine, by = id_col)

dirr::save_rds("data/tidy/dexmed-only", "data_")

ls(.GlobalEnv, pattern = "data_") %>%
    walk(
        ~write.xlsx(
            get(.x), 
            paste0("data/external/dexmed-only/", .x, ".xlsx")
        )
    )

