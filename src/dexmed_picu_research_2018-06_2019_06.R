library(tidyverse)
library(lubridate)
library(mbohelpr)
library(openxlsx)

dir_data <- "U:/Data/pedi_sedation/tidy/dexmed_picu_2018-06_2019-06"
tz_locale <- locale(tz = "US/Central")

get_data <- function(f) {
    read_csv(
        str_c(dir_data, f, sep="/"),
        locale = tz_locale
    ) %>%
        rename_all(str_to_lower)
}

data_demographics <- get_data("dexmed_demographics.csv")

df_dexmed <- get_data("dexmed_rates.csv")

df_meds <- get_data("dexmed_meds.csv")

df_locations <- get_data("dexmed_locations.csv") 

df_vent_times <- get_data("dexmed_vent_times.csv") 

df_vent_events <- get_data("dexmed_vent_events.csv") 

# vent data --------------------------------------------

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


# sedatives --------------------------------------------

data_dexmed <- df_dexmed %>%
    filter(!is.na(iv_event)) %>%
    drip_runtime(.grp_var = vars(encounter_id)) %>%
    summarize_drips(.grp_var = vars(encounter_id))

data_dexmed_daily <- df_dexmed %>%
    filter(!is.na(iv_event)) %>%
    mutate(med_day = floor_date(med_datetime, unit = "days")) %>%
    drip_runtime(.grp_var = vars(encounter_id, med_day)) %>%
    summarize_drips(.grp_var = vars(encounter_id, med_day)) %>%
    filter(cum_dose > 0)

# lorazepam, methadone ---------------------------------

data_meds_other <- df_meds %>%
    left_join(
        data_dexmed[c("encounter_id", "start_datetime")],
        by = "encounter_id"
    ) %>%
    filter(med_datetime > start_datetime) %>%
    arrange(encounter_id, med_datetime) %>%
    distinct(encounter_id, medication, .keep_all = TRUE) %>%
    mutate(
        time_from_dexmed = difftime(
            med_datetime,
            start_datetime,
            units = "days"
        )
    ) %>%
    select(
        encounter_id,
        medication,
        med_datetime,
        dose,
        dose_unit,
        time_from_dexmed
    ) 

# nurse units ------------------------------------------

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

# export data ------------------------------------------

l = list(
    "demographics" = data_demographics,
    "dexmed" = data_dexmed,
    "dexmed_daily" = data_dexmed_daily,
    "locations" = data_locations,
    "meds_other" = data_meds_other,
    "vent" = data_vent
)

write.xlsx(l, "U:/Data/pedi_sedation/final/dexmed_picu_2018-06_2019-06.xlsx")
