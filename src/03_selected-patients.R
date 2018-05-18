library(tidyverse)
library(edwr)
library(readxl)
library(openxlsx)

dir_raw <- "data/raw"

select_pts <- read_excel(
    "data/external/2018-05-18_selected_patients.xlsx"
) %>%
    mutate_at(
        c("fin", "millennium.id"),
        as.character
    )

cmfrt_b <- read_data(dir_raw, "comfort-b", FALSE) %>%
    as.events(order_var = FALSE) %>%
    semi_join(select_pts, by = "millennium.id") %>%
    mutate_at("event.result", as.numeric) %>%
    arrange(millennium.id, event.datetime) %>%
    select(
        millennium.id:event.result,
        event.location
    )

visits <- read_data(dir_raw, "visits", FALSE) %>%
    as.visits() %>%
    semi_join(select_pts, by = "millennium.id")

vent <- read_data(dir_raw, "vent-times", FALSE) %>%
    as.vent_times() %>%
    tidy_data(dc = visits) %>%
    semi_join(select_pts, by = "millennium.id")

write.xlsx(cmfrt_b, "data/external/2018-05-18_comfort-b.xlsx")
write.xlsx(vent, "data/external/2018-05-18_vent.xlsx")
