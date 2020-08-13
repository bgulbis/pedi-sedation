library(tidyverse)
library(readxl)
library(lubridate)
library(mbohelpr)
library(openxlsx)

pts <- read_excel("U:/Data/pedi_sedation/raw/sternotomy_opioid_patients.xlsx") %>%
    select(
        fin = `FIN#`,
        surgery_date = Surgery_Date
    ) %>%
    mutate_at("fin", as.character)

fin_id <- edwr::concat_encounters(pts$fin)
print(fin_id)


# data --------------------------------------------------------------------

surg <- read_excel("U:/Data/pedi_sedation/raw/sternotomy_surgeries.xlsx") %>%
    rename_all(str_to_lower) %>%
    mutate(surgery_date = floor_date(surgery_start_datetime, unit = "day")) %>%
    inner_join(pts, by = c("fin", "surgery_date")) %>%
    distinct(fin, surgery_date, .keep_all = TRUE) %>%
    mutate(
        postop_los = difftime(
            disch_datetime,
            surgery_stop_datetime, 
            units = "days"
        )
    ) %>%
    mutate_at("postop_los", as.numeric)

meds <- read_excel("U:/Data/pedi_sedation/raw/sternotomy_opioids.xlsx") %>%
    rename_all(str_to_lower)

df_24 <- meds %>%
    inner_join(surg, by = "fin") %>%
    mutate(
        time_start = difftime(
            med_datetime, 
            surgery_start_datetime,
            units = "hours"
        ),
        time_stop = difftime(
            med_datetime, 
            surgery_stop_datetime,
            units = "hours"
        )
    ) %>%
    filter(
        time_start > 0,
        time_stop <= 24
    )

df_24_int <- df_24 %>%
    filter(is.na(iv_event)) %>%
    group_by(fin, surgery_date, medication) %>%
    summarize_at("dose", sum, na.rm = TRUE)

df_24_cont <- df_24 %>%
    filter(!is.na(iv_event)) %>%
    drip_runtime(.grp_var = vars(fin, surgery_date), id = fin) %>%
    summarize_drips(.grp_var = vars(fin, surgery_date), id = fin) %>%
    select(fin, surgery_date, medication, dose = cum_dose)

df_24_total <- df_24_int %>%
    bind_rows(df_24_cont) %>%
    group_by(fin, surgery_date, medication) %>%
    summarize_at("dose", sum, na.rm = TRUE) %>%
    pivot_wider(names_from = "medication", values_from = "dose")


df_72 <- meds %>%
    inner_join(surg, by = "fin") %>%
    mutate(
        time_start = difftime(
            med_datetime, 
            surgery_start_datetime,
            units = "hours"
        ),
        time_stop = difftime(
            med_datetime, 
            surgery_stop_datetime,
            units = "hours"
        )
    ) %>%
    filter(
        time_start > 0,
        time_stop <= 72
    )

df_72_int <- df_72 %>%
    filter(is.na(iv_event)) %>%
    group_by(fin, surgery_date, medication) %>%
    summarize_at("dose", sum, na.rm = TRUE)

df_72_cont <- df_72 %>%
    filter(!is.na(iv_event)) %>%
    drip_runtime(.grp_var = vars(fin, surgery_date), id = fin) %>%
    summarize_drips(.grp_var = vars(fin, surgery_date), id = fin) %>%
    select(fin, surgery_date, medication, dose = cum_dose)

df_72_total <- df_72_int %>%
    bind_rows(df_72_cont) %>%
    group_by(fin, surgery_date, medication) %>%
    summarize_at("dose", sum, na.rm = TRUE) %>%
    pivot_wider(names_from = "medication", values_from = "dose")

df_postop <- meds %>%
    inner_join(surg, by = "fin") %>%
    filter(med_datetime > surgery_start_datetime)

df_postop_int <- df_postop %>%
    filter(is.na(iv_event)) %>%
    group_by(fin, surgery_date, medication) %>%
    summarize_at("dose", sum, na.rm = TRUE)

df_postop_cont <- df_postop %>%
    filter(!is.na(iv_event)) %>%
    drip_runtime(.grp_var = vars(fin, surgery_date), id = fin) %>%
    summarize_drips(.grp_var = vars(fin, surgery_date), id = fin) %>%
    select(fin, surgery_date, medication, dose = cum_dose)

df_postop_total <- df_postop_int %>%
    bind_rows(df_postop_cont) %>%
    group_by(fin, surgery_date, medication) %>%
    summarize_at("dose", sum, na.rm = TRUE) %>%
    pivot_wider(names_from = "medication", values_from = "dose")

df_24_export <- surg %>%
    select(fin, surgery_date, postop_los) %>%
    left_join(df_24_total, by = c("fin", "surgery_date"))

l <- list(
    "24h_data" = df_24_export, 
    "72h_data" = df_72_total,
    "total" = df_postop_total)

write.xlsx(l, "U:/Data/pedi_sedation/final/sternotomy_opioids_data.xlsx")
