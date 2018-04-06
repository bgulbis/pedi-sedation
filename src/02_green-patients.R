library(tidyverse)
library(readxl)
library(writexl)
library(edwr)

dir_raw <- "data/raw"

id_pts <- read_data(dir_raw, "id-fin", FALSE) %>%
    as.id()

green <- "data/external/patients_green.xlsx" %>%
    read_excel(
        col_names = c("patient.id", "fin", "name"), 
        col_types = c("numeric", "text", "text"),
        skip = 1
    ) %>%
    left_join(id_pts, by = "fin")

demog <- read_data(dir_raw, "demographics", FALSE) %>%
    as.demographics(extras = list("age.days" = "Age- Days (At Admit)")) %>%
    mutate_at("age.days", as.numeric) 

green %>%
    left_join(demog, by = "millennium.id") %>%
    select(-facility, -visit.type) %>%
    write_xlsx(path = "data/external/green_demographics.xlsx")
