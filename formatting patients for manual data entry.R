library(tidyverse)
library(readxl)
library(janitor)

dem_data <- read_excel("~/Documents/Current_Projects/STI/STI_JDAT_March1.xlsx", 
                     sheet = "ED_ENC_and_DEM", col_types = c("text", "text", "date", 
                                                             "numeric", "text", "text", "text", 
                                                             "date", "date", "numeric", "numeric", 
                                                             "numeric", "text", "text", "text", 
                                                             "text", "date", "numeric", "numeric", 
                                                             "date", "text", "text", "text", "text", 
                                                             "text"), guess_max = 30000)
dx_data <- read_excel("~/Documents/Current_Projects/STI/STI_JDAT_March1.xlsx", sheet = "STI_DX_IN_ED_ENC")

lab_tests_sheet <- read_excel("~/Documents/Current_Projects/STI/STI_JDAT_March1.xlsx", sheet = "Lab_tests")
abx_rx <- read_excel("~/Documents/Current_Projects/STI/STI_JDAT_March1.xlsx", sheet = "Anitbiotic_prescr")

a <- dem_data %>% 
  select(PAT_ID, PAT_MRN_ID,PATIENT_BIRTH_DATE, ED_ADMISSION_TIME, ED_DISPOSITION, PAT_ENC_CSN_ID )
discharged <- a %>% 
  filter(ED_DISPOSITION == "Discharge")

doxy_rx<- abx_rx %>% 
  select(PAT_ID, PAT_MRN_ID, PAT_ENC_CSN_ID, Order_Date, ORDERING_TIME, GENERIC_NAME, MedName, Med_ShortName) %>% 
  filter((str_detect(pattern = "doxy", string = Med_ShortName)))

cohort<-inner_join(doxy_rx, discharged, by= join_by("PAT_ENC_CSN_ID", "PAT_MRN_ID")) %>% mutate(CSN = as.character(PAT_ENC_CSN_ID)) %>% group_by(CSN) %>% mutate(number = row_number()) %>% ungroup() %>% mutate(date = as_datetime(ED_ADMISSION_TIME, tz = "UTC")) %>% mutate(date = as_date(date))

cohort_nodupes<- cohort %>% filter(number == 1) %>% arrange(desc(date)) %>% relocate(date, .after = PAT_MRN_ID)
cohort_mulitple_orders <- cohort %>% filter(number > 1)
#how many patients had mulitiple doxycycline orders?
cohort_mulitple_orders %>% filter(number == 2) %>% nrow() %>% print()
# 136 patients had multiple orders

# write_csv(cohort_nodupes, "sti.cohort.csv")
# write_excel_csv(cohort_nodupes, "sti.cohort.xl.csv")

#update Sept 9th 2024- importing data so far; fixing missing dates
amster.aug <- read_excel("sti.cohort.8.31.24.amster.xlsx") %>% rename("Filled_Within_Month"= "Column1") %>% mutate(Comments = NA) 
unfilled<- amster.aug %>% filter(is.na(Data_available))
unfilled_csn <- unfilled$PAT_ENC_CSN_ID %>% as.character()

cohort_nodupes_needs_data_entry <- cohort_nodupes %>% filter(CSN %in% unfilled_csn)
cohort_nodupes_filled <-cohort_nodupes %>% filter(!(CSN %in% unfilled_csn)) %>% nrow() %>% print()
# write_excel_csv(cohort_nodupes_needs_data_entry, "cohort.needs.data.sept.csv")
amster.oct <- read_excel("cohort.final.update.la.Oct.xlsx") %>% rename("PAT_ID" = "PAT_ID.x")
clms<-colnames(amster.aug)
amster.oct.r <- amster.oct %>% select(all_of(clms))
oct.pts <- amster.oct.r$PAT_ENC_CSN_ID
amster.aug.only <- amster.aug %>% filter(!(PAT_ENC_CSN_ID %in% oct.pts) & !(is.na(date))) %>% mutate(Order_Date = as.POSIXct(Order_Date), PATIENT_BIRTH_DATE = as_date(PATIENT_BIRTH_DATE))
final.cohort <- bind_rows(amster.aug.only,amster.oct.r)
# add dem data
final.cohort <- left_join(final.cohort, dem_data)
# add lab_test_data
chlamydia_lab <- lab_tests_sheet %>% select(PAT_ENC_CSN_ID, EXTERNAL_NAME, RESULT_TIME, RESULT_UNIT, ORD_VALUE, COMPONENT_COMMENT) %>% filter(str_detect(EXTERNAL_NAME, "Chlam"))
# plan to fix w/ pivot longer solution
final.cohort.ctlab <- left_join(final.cohort, chlamydia_lab, by = join_by(PAT_ENC_CSN_ID), relationship = "one-to-many") %>%  group_by(PAT_ENC_CSN_ID) %>% mutate(number = row_number()) %>% ungroup()
final.cohort.ctlab1 <- final.cohort.ctlab %>% filter(number == 1) %>% select(-number) %>% 
  mutate(Filled_Within_Month = case_when(
    is.na(Filled_Within_Month) ~ 0,
    Filled_Within_Month == 1 ~ 1,
    Filled_Within_Month == "x" ~ 1,
    .default = NA
  )
  )
second.lab <- final.cohort.ctlab %>% filter(number == 2) %>% select(PAT_ENC_CSN_ID, EXTERNAL_NAME, RESULT_TIME, ORD_VALUE, COMPONENT_COMMENT)
final.cohort.ctlab2 <- left_join(final.cohort.ctlab1, second.lab, by = join_by(PAT_ENC_CSN_ID), suffix = c(".1", ".2"))
third.lab <- final.cohort.ctlab %>% filter(number == 3) %>% select(PAT_ENC_CSN_ID, EXTERNAL_NAME, RESULT_TIME, ORD_VALUE, COMPONENT_COMMENT) %>% rename_with(~ paste0(.x, ".3") , -PAT_ENC_CSN_ID)
final.cohort.ctlab3 <- left_join(final.cohort.ctlab2, third.lab, by = join_by(PAT_ENC_CSN_ID)) 

cohort.analysis.1<- final.cohort.ctlab3 %>% 
  mutate(Filled_Rx = case_when(
    Data_available == 0 ~ NA,
    Filled_Rx == 1 ~ 1,
    Filled_Rx == 0 ~ 0, 
    .default = NA
  ), Any_Fill = case_when(
    Filled_Rx == 1 | Filled_Within_Month == 1 ~ 1,
    Filled_Rx == 0 & Filled_Within_Month == 0 ~ 0,
    Data_available == 0 ~ NA,
    .default = NA
  ),
  Ct_Tested = if_else(!is.na(ORD_VALUE.1), 1, 0),
  Ct_Pos = case_when(
    ORD_VALUE.1 == "Positive" | ORD_VALUE.2 == "Positive" | ORD_VALUE.3 == "Positive" ~ 1,
    ORD_VALUE.1 == "Negative" & (ORD_VALUE.2 == "Negative" | is.na(ORD_VALUE.2)) & (ORD_VALUE.3 == "Negative" | is.na(ORD_VALUE.3)) ~ 0,
    .default = NA
  ),
  ) %>% 
  mutate(Ct_Results_Categorical = case_when(
    is.na(ORD_VALUE.1) ~ "Not Performed",
    (ORD_VALUE.1 == "Equivocal" | ORD_VALUE.1 == "Invalid") & is.na(ORD_VALUE.2) ~ "Test Error",
    Ct_Pos == 1 ~ "Positive",
    Ct_Pos == 0 ~ "Negative"
  ))
HIV_labs <- lab_tests_sheet %>% filter(EXTERNAL_NAME %in% c("HIV 1/2 Antibody Screen", "HIV 1 and 2 Antibody/HIV-1 Antigen Screen")) %>% select(PAT_ENC_CSN_ID, PAT_MRN_ID, COMPONENT_NAME, EXTERNAL_NAME, ORD_VALUE, RESULT_TIME) %>% mutate(HIV_Result = case_when(
  ORD_VALUE == "Positive" ~ 1,
  ORD_VALUE == "Repeatedly Reactive" ~ 1,
  ORD_VALUE == "Negative" ~ 0,
  ORD_VALUE == "Non-Reactive" ~ 0
)) %>% select(PAT_ENC_CSN_ID, PAT_MRN_ID, HIV_Result) %>% unique() %>% mutate(HIV_Test = 1)
# upon review, 13 pts had 2 HIV tests done, sometimes days apart but all negative. 
# double.hiv.labs.csn <- HIV_labs %>% group_by(PAT_ENC_CSN_ID) %>% mutate(count = row_number()) %>% ungroup() %>% filter(count > 1) %>% select(PAT_ENC_CSN_ID) %>% as_vector()
# double.hiv.labs <- HIV_labs %>% filter(PAT_ENC_CSN_ID %in% double.hiv.labs.csn)
cohort.analysis.1 <- left_join(cohort.analysis.1, HIV_labs) %>% mutate(HIV_Test = if_else(is.na(HIV_Test), 0, 1))
urine_lab <-  lab_tests_sheet %>% select(PAT_ENC_CSN_ID, EXTERNAL_NAME, RESULT_TIME, ORD_VALUE, COMPONENT_COMMENT) %>% filter(str_detect(EXTERNAL_NAME, "Urine")) %>% 
  group_by(PAT_ENC_CSN_ID) %>% 
  mutate(UCx_Num = row_number()) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = PAT_ENC_CSN_ID,
    names_from = UCx_Num,
    values_from = c(ORD_VALUE, COMPONENT_COMMENT),
    names_prefix = "UCx_"
  )
cohort.analysis.1 <- left_join(cohort.analysis.1, urine_lab)

# still need: HSV, Syphylis

dx_data_wide <- dx_data %>% group_by(PAT_ENC_CSN_ID) %>%
  mutate(Diagnosis_Num = row_number()) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = PAT_ENC_CSN_ID,
    names_from = Diagnosis_Num,
    values_from = DX_NAME,
    names_prefix = "Diagnosis_"
  )

cohort.analysis.2 <- left_join(cohort.analysis.1, dx_data_wide) %>% remove_empty(c("rows", "cols"))

