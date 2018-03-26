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
