library(tidyverse)
library(lubridate)
# library(edwr)
library(openxlsx)

dir_data <- "U:/pedi_sedation/data/tidy/dexmed-only"
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

calc_runtime <- function(x, ..., drip_off = 12, undocumented = 24, units = "hours") {
    group_var <- quos(...)
    
    id <- sym("encounter_id")
    med <- sym("medication")
    event_datetime <- sym("clinical_event_datetime")
    rate <- sym("rate")
    rate_unit <- sym("rate_unit")
    chg_n <- sym("chg_n")
    rate_chg <- sym("rate_chg")
    rate_calc <- sym("rate_calc")
    rate_duration <- sym("rate_duration")
    rate_start <- sym("rate_start")
    rate_stop <- sym("rate_stop")
    time_next <- sym("time_next")
    drip_start <- sym("drip_start")
    drip_stop <- sym("drip_stop")
    drip_n <- sym("drip_n")
    duration <- sym("duration")
    
    cont <- x %>%
        arrange(!!id, !!!group_var, !!med, !!event_datetime) %>%
        
        # determine if it's a valid rate documentation
        group_by(!!id, !!!group_var, !!med) %>%
        mutate(
            !!"rate_chg" := !is.na(!!rate_unit),
            !!"chg_n" := cumsum(!!rate_chg)
        ) %>%

        # mutate(rate = if_else(is.na(rate_unit), NA_real_, rate)) %>%
        # fill(rate, rate_unit) %>%
        # filter(!is.na(rate)) %>%
                
        # fill in missing rates
        group_by(!!id, !!!group_var, !!med, !!chg_n) %>%
        mutate(
            !!"rate_calc" := dplyr::if_else(
                is.na(!!rate_unit),
                dplyr::first(!!rate),
                !!rate
            )
        ) %>%
        
        # calculate time between rows and order of rate changes
        group_by(!!id, !!!group_var, !!med) %>%
        mutate(
            !!"time_next" := difftime(
                dplyr::lead(!!event_datetime),
                !!event_datetime,
                units = units
            ),
            !!"rate_chg" := is.na(dplyr::lag(!!rate_calc)) |
                rate_calc != dplyr::lag(!!rate_calc),
            !!"chg_n" := cumsum(!!rate_chg)
        ) %>%
        
        # calculate how long the drip was at each rate
        group_by(!!id, !!!group_var, !!med, !!chg_n) %>%
        summarize(
            !!"rate" := dplyr::first(!!rate_calc),
            !!"rate_start" := dplyr::first(!!event_datetime),
            !!"rate_stop" := dplyr::last(!!event_datetime),
            !!"rate_duration" := difftime(
                dplyr::last(!!event_datetime),
                dplyr::first(!!event_datetime),
                units = units
            ),
            !!"time_next" := dplyr::last(!!time_next)
        ) %>%
        
        # identify individual drips
        group_by(!!id, !!!group_var, !!med) %>%
        mutate(
            !!"duration" := dplyr::if_else(
                !!time_next < drip_off & !is.na(!!time_next),
                !!rate_duration + !!time_next,
                !!rate_duration
            ),
            !!"drip_stop" := is.na(!!time_next) | !!time_next > undocumented |
                (!!rate == 0 & !!duration > drip_off),
            !!"drip_start" := !!chg_n == 1 | dplyr::lag(!!drip_stop),
            !!"drip_n" := cumsum(!!drip_start)
        ) %>%
        dplyr::mutate_at("duration", as.numeric) %>%
        
        # calculate run time
        group_by(!!id, !!!group_var, !!med, !!drip_n) %>%
        mutate(
            !!"run_time" := difftime(
                !!rate_start,
                dplyr::first(!!rate_start),
                units = units
            )
        ) %>%
        
        # remove unnecessary columns
        select(
            -!!rate_duration,
            -!!time_next,
            -!!drip_start,
            -!!drip_stop,
            -!!chg_n
        )
    
    # update drip stop information if rate of last row isn't 0
    drip.end <- cont %>%
        filter(
            !!rate_stop == dplyr::last(!!rate_stop),
            !!rate > 0
        ) %>%
        
        # calculate the run time for the last drip row
        mutate(
            !!"run_time" := !!duration + !!sym("run_time"),
            !!"rate_start" := !!rate_stop,
            !!"duration" := 0
        ) %>%
        ungroup()
    
    # bind the rows with drip end data and arrange by date/time; need to ungroup
    # first for bind_rows to keep edwr class assigment
    cont %>%
        ungroup() %>%
        dplyr::bind_rows(drip.end) %>%
        arrange(!!id, !!!group_var, !!med, !!drip_n, !!rate_start) %>%
        distinct()
}

summarize_data <- function(x, ..., units = "hours") {
    # turn off scientific notation
    options(scipen = 999)
    
    id <- sym("encounter_id")
    group_var <- quos(...)
    
    grp_by <- quos(!!id, !!!group_var, !!sym("medication"), !!sym("drip_n"))
    rate <- sym("rate")
    run_time <- sym("run_time")
    rate_start <- sym("rate_start")
    
    # cont <- x %>%
    #     group_by(!!!grp_by) %>%
    #     filter(!!run_time > 0)
    
    # get last and min non-zero rate
    nz_rate <- x %>%
        group_by(!!!grp_by) %>%
        filter(!!rate > 0) %>%
        summarize(
            !!"last_rate" := dplyr::last(!!rate),
            !!"min_rate" := min(!!rate, na.rm = TRUE),
            !!"run_time" := sum(!!sym("duration"), na.rm = TRUE)
        )
    
    # get first and max rates and AUC
    x %>%
        group_by(!!!grp_by) %>%
        summarize(
            !!"start_datetime" := dplyr::first(!!rate_start),
            !!"stop_datetime" := dplyr::if_else(
                dplyr::last(!!rate) == 0,
                dplyr::last(!!rate_start),
                dplyr::last(!!sym("rate_stop"))
            ),
            !!"cum_dose" := sum(!!rlang::parse_expr("rate * duration"), na.rm = TRUE),
            !!"first_rate" := dplyr::first(!!rate),
            !!"max_rate" := max(!!rate, na.rm = TRUE),
            !!"auc" := MESS::auc(!!run_time, !!rate),
            !!"duration" := dplyr::last(!!run_time)
        ) %>%
        # join the last and min data, then calculate the time-weighted average
        # and interval
        inner_join(
            nz_rate,
            by = c(
                rlang::quo_text(id),
                purrr::map_chr(group_var, rlang::quo_text),
                "medication",
                "drip_n"
            )
        ) %>%
        group_by(!!!grp_by) %>%
        dplyr::mutate_at("duration", as.numeric) %>%
        mutate(!!"time_wt_avg" := !!rlang::parse_expr("auc / duration")) %>%
        ungroup()
}

# data sets --------------------------------------------

df_demog <- get_data(dir_data, "demographics") %>%
    select(-race)

df_dexmed <- get_data(dir_data, "rates")

df_meds <- get_data(dir_data, "meds")

df_weights <- get_data(dir_data, "weights")

df_locations <- get_data(dir_data, "locations") %>%
    arrange(encounter_id, begin_effective_datetime)

df_vent_times <- get_data(dir_data, "vent_times") 

df_vent_events <- get_data(dir_data, "vent_events") 

tmp_weights <- df_weights %>%
    filter(
        event == "Weight",
        result_unit == "kg"
    ) %>%
    arrange(encounter_id, clinical_event_datetime) %>%
    group_by(encounter_id) %>%
    summarize_at("result", list(first_weight = first, last_weight = last))



tmp_vent_last <- df_vent_times %>%
    filter(event == "Vent Stop Time") %>%
    arrange(encounter_id, result_datetime) %>%
    group_by(encounter_id) %>%
    summarize_at("result_datetime", max) %>%
    rename(last_vent_datetime = result_datetime)

data_vent <- df_vent_events %>%
    bind_rows(df_vent_times) %>%
    filter(event %in% c("Vent Start Time", "Extubation Event")) %>%
    mutate_at(
        "event",
        str_replace_all,
        pattern = c(
            "Vent Start Time" = "intubation",
            "Extubation Event" = "extubation"
        )
    ) %>%
    arrange(encounter_id, result_datetime) %>%
    group_by(encounter_id) %>%
    mutate(new_event = event != lag(event) | is.na(lag(event))) %>%
    mutate_at("new_event", cumsum) %>%
    distinct(encounter_id, new_event, .keep_all = TRUE) %>%
    filter(!(new_event == 1 & event == "extubation")) %>%
    group_by(encounter_id, event) %>%
    mutate(
        vent = TRUE,
        vent_n = cumsum(vent)
    ) %>%
    select(encounter_id, vent_n, event, result_datetime) %>%
    spread(event, result_datetime) %>%
    select(
        encounter_id,
        vent_n,
        intubate_datetime = intubation,
        extubate_datetime = extubation
    ) %>%
    left_join(tmp_vent_last, by = "encounter_id") %>%
    mutate_at("extubate_datetime", list(~coalesce(., last_vent_datetime))) %>%
    select(-last_vent_datetime) %>%
    group_by(encounter_id, vent_n) %>%
    mutate(
        vent_duration = difftime(
            extubate_datetime,
            intubate_datetime,
            units = "hours"
        )
    ) %>%
    filter(vent_duration > 0)


# sedatives --------------------------------------

data_dexmed <- df_dexmed %>%
    filter(!is.na(iv_event)) %>%
    calc_runtime() %>%
    summarize_data()

data_dexmed_daily <- df_dexmed %>%
    filter(!is.na(iv_event)) %>%
    mutate(med_day = floor_date(clinical_event_datetime, unit = "days")) %>%
    calc_runtime(med_day) %>%
    summarize_data(med_day) %>%
    filter(cum_dose > 0)


# lorazepam, methadone ---------------------------------

data_meds_other <- df_meds %>%
    left_join(
        data_dexmed[c("encounter_id", "start_datetime")],
        by = "encounter_id"
    ) %>%
    filter(clinical_event_datetime > start_datetime) %>%
    arrange(encounter_id, clinical_event_datetime) %>%
    distinct(encounter_id, medication, .keep_all = TRUE) %>%
    mutate(
        time_from_dexmed = difftime(
            clinical_event_datetime,
            start_datetime,
            units = "days"
        )
    ) %>%
    select(
        encounter_id,
        medication,
        med_datetime = clinical_event_datetime,
        dose,
        dose_unit,
        time_from_dexmed
    ) 

# data sets --------------------------------------------

data_demographics <- df_demog %>%
    left_join(tmp_weights, by = "encounter_id") 

data_locations <- df_locations %>%
    group_by(encounter_id) %>%
    mutate(
        chg_unit = (
            (nurse_unit != lag(nurse_unit) &
                 end_effective_datetime != lag(begin_effective_datetime)) |
                (is.na(lag(nurse_unit)))
        )
    ) %>%
    mutate_at("chg_unit", cumsum) %>%
    group_by(encounter_id, nurse_unit, chg_unit) %>%
    summarize(
        arrive_datetime = min(begin_effective_datetime),
        depart_datetime = max(end_effective_datetime)
    ) %>%
    arrange(encounter_id, chg_unit) %>%
    mutate(
        unit_los = difftime(
            depart_datetime,
            arrive_datetime,
            units = "days"
        )
    ) %>%
    filter(nurse_unit == "HC PICU")

# dirr::save_rds("data/tidy/dexmed-only", "data_")

ls(.GlobalEnv, pattern = "data_") %>%
    walk(
        ~write.xlsx(
            get(.x), 
            paste0("U:/pedi_sedation/final/tidy/dexmed-only/", .x, ".xlsx")
        )
    )

