library(tidyverse)
library(readxl)
library(edwr)

dir_raw <- "data/raw"

raw_pts <- read_excel("data/raw/patient_list.xlsx", 
                      skip = 1,
                      col_names = c("patient.id", "fin", "name")) %>%
    filter(!is.na(fin))

mbo_fin <- concat_encounters(raw_pts$fin)

# run MBO query
#   * Identifiers - by FIN

id_pts <- read_data(dir_raw, "id-fin", FALSE) %>%
    as.id()

mbo_id <- concat_encounters(id_pts$millennium.id)

# run MBO query
#   * Clinical Events - Measures
#   * Demographics - Pedi
#   * Location History
#   * Medications - Inpatient - All
#   * Vent - Times
#   * Visit Data

demog <- read_data(dir_raw, "demographics", FALSE) %>%
    as.demographics(extras = list("age.days" = "Age- Days (At Admit)")) %>%
    mutate_at("age.days", as.numeric)

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

meds <- read_data(dir_raw, "meds-inpt", FALSE) %>%
    as.meds_inpt() %>%
    semi_join(meds_keep, by = c("med" = "med.name"))

meds_drips <- meds %>%
    filter(!is.na(event.tag)) %>%
    calc_runtime() %>%
    summarize_data()

meds_int <- meds %>%
    filter(is.na(event.tag)) %>%
    calc_runtime(cont = FALSE) %>%
    summarize_data(cont = FALSE)
