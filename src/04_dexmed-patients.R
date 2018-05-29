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

mbo_id <- concat_encounters(raw_pts$millennium.id, 1000)

# run MBO queries
#   * Medications - Inpatient - All

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
