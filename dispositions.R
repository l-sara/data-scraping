library(tidyverse)
library(plumber)
library(rvest)
library(lubridate)
library(stringr)
library(dplyr)
library(rappdirs)

#---------------------------------------------------------------------------------------------------

#* @get /dispositions_OFO
#* OFO Dispositions

function() {
  dispositons_url <- "https://www.cbp.gov/newsroom/stats/custody-and-transfer-statistics"
  
  data <- 
    session(dispositons_url) |> 
    html_element("table") |> 
    html_table() |>
    pivot_longer(-1) |>
    mutate(value = parse_number(value)) |>
    pivot_wider(names_from = "Disposition", values_from = "value") %>% rename(year = name) |>
    separate(year, c("Month", "Year"), sep = "-") |>
    mutate(Year_num = str_glue("20{Year}"),
           Month_num = set_names(1:12, month.abb)[Month],
           Date = str_glue("{Month_num}/1/{Year_num}"),
           # Date = make_date(year = Year_num, month = Month_num, day = 1),
           ID = parse_number(str_glue("{Year_num}{Month_num}"))) |>
    select(Date,`EXPEDITED REMOVAL-CREDIBLE FEAR (ERCF)1`, `NOTICE TO APPEAR (NTA)2`, `NOTICE TO APPEAR (NTA)-PERSON RELEASED`, 
           `NOTICE TO APPEAR (NTA)-PERSON DETAINED`,`VISA WAIVER PROGRAM (VWP)-REMOVAL-LIMITED REVIEW3`, 
           `VISA WAIVER PROGRAM (VWP)-REFUSAL LIMITED REVIEW3`, `STOWAWAY-LIMITED REVIEW3`, `Total Credible Fear Inadmissibles`, ID)
  
  old_data <- read_csv(file = "OFO Dispositions.csv") # contains data from 2022 that is no longer online
  
  bind_rows(old_data,
            anti_join(data, old_data, by = "ID")) |>
    write_csv(file = "OFO Dispositions.csv")
  
  data
  
}


#---------------------------------------------------------------------------------------------------

#* @get /dispositions_CBP
#* CBP Dispositions

function() {
  dispositons_url <- "https://www.cbp.gov/newsroom/stats/custody-and-transfer-statistics"
  
  data <- 
    session(dispositons_url) |> 
    html_element("table[summary='Southwest Border Apprehensions by Processing Disposition']") |>
    html_table() |>
    pivot_longer(-1) |>
    mutate(value = parse_number(value)) |>
    pivot_wider(names_from = "Processing Disposition", values_from = "value") %>% rename(year = name) |>
    separate(year, c("Month", "Year"), sep = "-") |>
    mutate(Year_num = str_glue("20{Year}"),
           Month_num = set_names(1:12, month.abb)[Month],
           Date = str_glue("{Month_num}/1/{Year_num}"),
           # Date = make_date(year = Year_num, month = Month_num, day = 1),
           ID = parse_number(str_glue("{Year_num}{Month_num}"))) |>
    mutate(Other = sum(Paroles1, `Notice to Report (NTR)`,`Reinstatement of Prior Order of Removal`, `Warrant of Arrest/Notice To Appear - (Detained)`,
                          MPP, `Other2`)) |>
    rename(`Notice To Appear/Own Recognizance` = `Notice To Appear/Own Recognizance (NTA-OR)`, `Expedited Removal` = `Expedited Removal (ER)`) |>
    select(Date, `Notice To Appear/Own Recognizance`, `Expedited Removal`, `Voluntary Return`, Other, ID)
  
  old_data <- read_csv(file = "CBP Dispositions.csv") # contains data from 2022 that is no longer online
  
  bind_rows(old_data,
            anti_join(data, old_data, by = "ID")) |>
    write_csv(file = "CBP Dispositions.csv")
  
  data
  
}
