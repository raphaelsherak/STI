library(tidyverse)
library(readr)
library(readxl)
> encounters <- read_excel("~/Documents/Current_Projects/STI/STI_JDAT_March1.xlsx", 
                           +     sheet = "ED_ENC_and_DEM", col_types = c("text", "text", "date", "numeric", "text", "text", "text", "date", "text", "numeric", 
                                                                         +         "numeric", "numeric", "text", "text", 
                                                                         +         "text", "text", "text", "numeric", 
                                                                         +         "numeric", "date", "text", "text", 
                                                                         +         "text", "text", "text"))

encounters <-
  read_excel(
    "~/Documents/Current_Projects/STI/STI_JDAT_March1.xlsx",
    sheet = "ED_ENC_and_DEM",
    col_types = c(
      "text",
      "text",
      "date",
      "numeric",
      "text",
      "text",
      "text",
      "date",
      "text",
      "numeric",
      "numeric",
      "numeric",
      "text",
      "text",
      "text",
      "text",
      "text",
      "numeric",
      "numeric",
      "date",
      "text",
      "text",
      "text",
      "text",
      "text"
    )
  )
print(colnames(encounters))
encounters %>% summarise(patients = n_distinct(PAT_MRN_ID), visits = n_distinct(PAT_ENC_CSN_ID), earliest = min(ED_ADMISSION_TIME), latest = max(ED_ADMISSION_TIME))
