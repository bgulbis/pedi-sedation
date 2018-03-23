library(tidyverse)
library(readxl)
library(lubridate)
library(edwr)

dir_raw <- "data/raw"

raw_pts <- read_excel(
    "data/raw/patient_list.xlsx", 
    skip = 1,
    col_names = c("patient.id", "fin", "name")
) %>%
    filter(!is.na(fin)) %>%
    mutate_at("fin", as.character)

mbo_fin <- concat_encounters(raw_pts$fin)

manual <- read_excel(
    "data/external/manual_data.xlsx",
    sheet = "CAPD scores", 
    range = "A2:L628",
    # skip = 1,
    col_names = c(
        "patient.id", 
        "fin",
        "name", 
        "screen.date", 
        "time.day",
        "wat",
        "comfort.b",
        "capd.score",
        "match",
        "nurse.assess",
        "valid.score",
        "pos"
    ),
    col_types = c(
        "numeric",
        "text",
        "text",
        "date",
        "numeric",
        "numeric",
        "numeric",
        "numeric",
        "numeric",
        "numeric",
        "numeric",
        "numeric"
    )
) %>%
    filter(!is.na(fin)) %>%
    mutate_at("fin", as.character) %>%
    mutate_at("match", funs(. == 1))

capd_pos <- manual %>%
    filter(capd.score >= 9) %>%
    distinct(fin, screen.date) 

# run MBO query
#   * Identifiers - by FIN

id_pts <- read_data(dir_raw, "id-fin", FALSE) %>%
    as.id()

mbo_id <- concat_encounters(id_pts$millennium.id)

# run MBO query
#   * Clinical Events - Measures
#   * Clincial Events - No Order Id - Prompt
#       - Clinical Event: COMFORT-B Scale Total
#   * Demographics - Pedi
#   * Location History
#   * Medications - Inpatient - All
#   * Orders
#       - Mnemonic (Primary Generic) FILTER ON: Physical Therapy Acute Evaluation & Trea;Physical Therapy Evaluation and Treatmen;Physical Therapy Evaluation & Treatments;SR Physical Therapy Referral
#   * Vent - Times
#   * Visit Data

# COMFORT-B Scale Total
# Physical Therapy Acute Evaluation & Treatment


demog <- read_data(dir_raw, "demographics", FALSE) %>%
    as.demographics(extras = list("age.days" = "Age- Days (At Admit)")) %>%
    mutate_at("age.days", as.numeric) %>%
    left_join(id_pts, by = "millennium.id")

# demog %>%
#     left_join(id_pts, by = "millennium.id") %>%
#     write.csv("data/external/demographics.csv", row.names = FALSE)

locations <- read_data(dir_raw, "locations", FALSE) %>%
    as.locations() %>%
    tidy_data()

measures <- read_data(dir_raw, "events-measures", FALSE) %>%
    as.events(order_var = FALSE)

visits <- read_data(dir_raw, "visits", FALSE) %>%
    as.visits() %>%
    select(-nurse.unit.admit) %>%
    distinct()

vent <- read_data(dir_raw, "vent-times", FALSE) %>%
    as.vent_times() %>%
    tidy_data(dc = visits)

meds_keep <- bind_rows(med_lookup("benzodiazepines"),
                       med_lookup("narcotic analgesics"),
                       med_class_lookup("propofol"),
                       med_class_lookup("dexmedetomidine"),
                       med_lookup("loop diuretics"),
                       med_lookup("neuromuscular blocking agents"),
                       med_class_lookup("melatonin"),
                       med_lookup("anticholinergic antiemetics"),
                       med_lookup("glucocorticoids"),
                       med_lookup("antipsychotics")) %>%
    distinct(med.name) %>%
    mutate_at("med.name", str_to_lower)

# eda -----------------------------------

pos_pts <- manual %>%
    filter(
        capd.score >= 9,
        match
    ) %>%
    distinct(fin) %>%
    mutate(group = "Positive")

neg_pts <- manual %>%
    anti_join(pos_pts, by = "fin") %>%
    distinct(fin) %>%
    mutate(group = "Negative")

data_patients <- raw_pts %>%
    left_join(
        bind_rows(pos_pts, neg_pts),
        by = "fin"
    ) %>%
    left_join(id_pts, by = "fin") 

# write.csv(
#     data_patients, 
#     "data/external/patient_groups.csv", 
#     row.names = FALSE
# )

valid <- manual %>%
    semi_join(pos_pts, by = "fin") %>%
    filter(valid.score >= 9) %>%
    distinct(fin)


# pos capd >= 9
# comfort.b scores pos vs neg, median, iqr
# time wt-avg comfort.b for 5 days prior to capd score, compare across groups
# meds 5 days prior to capd score; time wt-dose compare across groups
# picu only

data_screen <- data_patients %>%
    left_join(capd_pos, by = "fin") %>%
    select(millennium.id, group, screen.date)

meds <- read_data(dir_raw, "meds-inpt", FALSE) %>%
    as.meds_inpt() %>%
    semi_join(meds_keep, by = c("med" = "med.name")) %>%
    filter(med.location == "HC PICU") 

meds_capd_neg <- meds %>%
    left_join(data_screen, by = "millennium.id") %>%
    filter(
        group == "Negative",
        !is.na(event.tag)
    ) %>%
    calc_runtime() %>%
    summarize_data()

meds_capd_pos <- meds %>%
    left_join(data_screen, by = "millennium.id") %>%
    filter(
        group == "Positive",
        floor_date(med.datetime, "day") >= screen.date - days(5),
        floor_date(med.datetime, "day") < screen.date
    ) %>%
    calc_runtime() %>%
    summarize_data()

meds_capd <- meds_capd_pos %>%
    bind_rows(meds_capd_neg) %>%
    left_join(
        data_patients[c("millennium.id", "group")], 
        by = "millennium.id"
    ) 

meds_capd %>%
    ggplot(aes(x = group, y = time.wt.avg)) +
    geom_boxplot() +
    facet_wrap(~ med, scales = "free_y")

data_meds <- meds_capd %>%
    group_by(group, med) %>%
    summarize_at(
        c("time.wt.avg", "max.rate", "run.time"),
        funs(mean, sd, median, q25 = quantile(., 0.25), q75 = quantile(., 0.75)),
        na.rm = TRUE
    )

# meds_drips <- meds %>%
#     filter(!is.na(event.tag)) %>%
#     calc_runtime() %>%
#     summarize_data()

meds_int <- meds %>%
    filter(is.na(event.tag)) %>%
    calc_runtime(cont = FALSE) %>%
    summarize_data(cont = FALSE)

comf_b <- read_data(dir_raw, "comfort", FALSE) %>%
    as.events(order_var = FALSE) %>%
    filter(!is.na(event.result))

comf_b_neg <- comf_b %>%
    left_join(data_screen, by = "millennium.id") %>%
    filter(group == "Negative") %>%
    calc_runtime() %>%
    summarize_data()

comf_b_pos <- comf_b %>%
    left_join(data_screen, by = "millennium.id") %>%
    filter(
        group == "Positive",
        floor_date(event.datetime, "day") >= screen.date - days(5),
        floor_date(event.datetime, "day") < screen.date
    ) %>%
    calc_runtime() %>%
    summarize_data()

labs_comf_b <- comf_b_pos %>%
    bind_rows(comf_b_neg) %>%
    left_join(
        data_patients[c("millennium.id", "group")], 
        by = "millennium.id"
    ) 

data_comf_b <- labs_comf_b %>%
    group_by(group) %>%
    summarize_at(
        c("time.wt.avg", "max.result", "duration"),
        funs(mean, sd, median, q25 = quantile(., 0.25), q75 = quantile(., 0.75)),
        na.rm = TRUE
    )

labs_comf_b %>%
    ggplot(aes(x = group, y = time.wt.avg)) +
    geom_boxplot()
