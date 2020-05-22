library(tidyverse)
library(readxl)
library(lubridate)
library(mbohelpr)
library(openxlsx)

pts <- read_excel("U:/Data/pedi_sedation/external/case_control_patients.xlsx")
mbo_id <- edwr::concat_encounters(pts$encounter_id)
print(mbo_id)

meds <- read_csv(
    "U:/Data/pedi_sedation/tidy/case_control_meds.csv", 
    locale = locale(tz = "US/Central")
) %>%
    rename_all(str_to_lower) %>%
    mutate_at("dose_weight", ~coalesce(., weight))

# miss_wt <- filter(meds, is.na(dose_weight), !is.na(iv_event))

df_drips <- meds %>%
    filter(!is.na(iv_event)) %>%
    mutate_at(
        "rate", 
        ~if_else(
            str_detect(rate_unit, "kg"), 
            rate * dose_weight, 
            rate
        )
    ) %>%
    drip_runtime(.grp_var = vars(encntr_id)) %>%
    summarize_drips(.grp_var = vars(encntr_id)) %>%
    group_by(encntr_id, medication) %>%
    summarize(
        first_datetime = first(start_datetime),
        last_datetime = last(stop_datetime),
        cum_dose = sum(cum_dose, na.rm = TRUE)
    )

df_sched <- meds %>%
    filter(is.na(iv_event)) %>%
    mutate_at(
        "dose", 
        ~if_else(
            str_detect(dose_unit, "kg"), 
            dose * dose_weight, 
            dose
        )
    ) %>%
    calc_runtime(
        .grp_var = vars(encntr_id), 
        event = medication,
        event_datetime = med_datetime
    ) %>%
    summarize_data(
        .grp_var = vars(encntr_id), 
        event = medication,
        event_datetime = med_datetime,
        result = dose
    ) %>%
    select(
        encntr_id,
        medication,
        first_datetime,
        last_datetime,
        cum_dose = cum_sum,
        starting_dose = first_result
    )

data_meds <- df_sched %>%
    bind_rows(df_drips) %>%
    group_by(encntr_id, medication) %>%
    summarize(
        first_datetime = first(first_datetime),
        last_datetime = last(last_datetime),
        cum_dose = sum(cum_dose, na.rm = TRUE),
        starting_dose = first(starting_dose)
    ) %>%
    mutate_at(
        "starting_dose", 
        ~if_else(
            medication %in% c("FENTanyl", "midazolam", "HYDROmorphone"), 
            NA_real_, 
            .
        )
    )

write.xlsx(data_meds, "U:Data/pedi_sedation/final/case_control_data.xlsx")
