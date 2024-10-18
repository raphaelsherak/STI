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
final.cohort.csn <- final.cohort$PAT_ENC_CSN_ID %>% as.character()
# add lab_test_data
chlamydia_lab <- lab_tests_sheet %>% select(PAT_ENC_CSN_ID, EXTERNAL_NAME, RESULT_TIME, ORD_VALUE, COMPONENT_COMMENT) %>% filter(str_detect(EXTERNAL_NAME, "Chlam")) %>% 
  rename(Ct_Test_Name = EXTERNAL_NAME) %>% 
  group_by(PAT_ENC_CSN_ID) %>% 
  mutate(Ct_Lab_Num = row_number()) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = c(PAT_ENC_CSN_ID, Ct_Test_Name),
    names_from = Ct_Lab_Num,
    values_from = c(ORD_VALUE, COMPONENT_COMMENT),
    names_glue = "Ct_Lab_{.value}_{Ct_Lab_Num}"
  )

invalid_equivocal_negative <- c("Invalid", "Equivocal", "Negative")
final.cohort.ctlab <- left_join(final.cohort, chlamydia_lab, by = join_by(PAT_ENC_CSN_ID), relationship = "one-to-many") %>%
  mutate(Filled_Within_Month = case_when(
    is.na(Filled_Within_Month) ~ 0,
    Filled_Within_Month == 1 ~ 1,
    Filled_Within_Month == "x" ~ 1,
    .default = NA
  )
  ) %>% 
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
  Ct_Tested = if_else(!is.na(Ct_Lab_ORD_VALUE_1), 1, 0),
  Ct_Pos = case_when(
    Ct_Lab_ORD_VALUE_1 == "Positive" | Ct_Lab_ORD_VALUE_2 == "Positive" | Ct_Lab_ORD_VALUE_3 == "Positive" ~ 1,
    Ct_Lab_ORD_VALUE_1 == "Negative" & (Ct_Lab_ORD_VALUE_2 == "Negative" | is.na(Ct_Lab_ORD_VALUE_2)) & (Ct_Lab_ORD_VALUE_3 == "Negative" | is.na(Ct_Lab_ORD_VALUE_3)) ~ 0,
    Ct_Lab_ORD_VALUE_1 %in% invalid_equivocal_negative & (Ct_Lab_ORD_VALUE_2 %in% invalid_equivocal_negative | is.na(Ct_Lab_ORD_VALUE_2)) ~ 0,
    .default = NA
  ),
  ) %>% 
  mutate(Ct_Results_Categorical = case_when(
    is.na(Ct_Lab_ORD_VALUE_1) ~ "Not Performed",
    (Ct_Lab_ORD_VALUE_1 == "Equivocal" | Ct_Lab_ORD_VALUE_1 == "Invalid") & is.na(Ct_Lab_ORD_VALUE_1) ~ "Test Error",
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
final.cohort.ctlab <- left_join(final.cohort.ctlab, HIV_labs) %>% mutate(HIV_Test = if_else(is.na(HIV_Test), 0, 1))
urine_lab <-  lab_tests_sheet %>% select(PAT_ENC_CSN_ID, EXTERNAL_NAME, RESULT_TIME, ORD_VALUE, COMPONENT_COMMENT) %>% filter(str_detect(EXTERNAL_NAME, "Urine")) %>% 
  filter(PAT_ENC_CSN_ID %in% final.cohort.csn) %>% 
  group_by(PAT_ENC_CSN_ID) %>% 
  mutate(UCx_Num = row_number()) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = PAT_ENC_CSN_ID,
    names_from = UCx_Num,
    values_from = c(ORD_VALUE, COMPONENT_COMMENT),
    names_glue = "UCx_{.value}_{UCx_Num}"
  )
final.cohort.ctlab.urinelab <- left_join(final.cohort.ctlab, urine_lab)
NG_TV_test_results <- read_excel("~/Documents/Current_Projects/STI/sti_jdat_population/2402241_gonorrhea_trichonomanas_vaginalis_test_results.xlsx")

NG_NAAT_Lab <- NG_TV_test_results %>% 
  filter(PAT_ENC_CSN_ID %in% final.cohort.csn) %>% 
  filter(str_detect(EXTERNAL_NAME, "Neisseria gonorrhoeae, DNA Probe")) %>% 
  group_by(PAT_ENC_CSN_ID) %>% 
  mutate(NG_NAAT_Num = row_number()) %>%
  filter(NG_NAAT_Num == 1) %>%  #5 patients had 3x NAAT tests done for some reason. all neg. will reduce tosimplify
  ungroup() %>%
  pivot_wider(
    id_cols = PAT_ENC_CSN_ID,
    names_from = NG_NAAT_Num,
    values_from = c(ORD_VALUE, COMPONENT_COMMENT),
    names_glue = "NG_NAAT_{.value}_{NG_NAAT_Num}"
  )
#only 1 patient had NG culture- was negative and had negative NAAT so will disregaurd. 
final.cohort.ctlab.urinelab.nglab <- left_join(final.cohort.ctlab.urinelab, NG_NAAT_Lab) %>% 
  mutate(NG_Tested = if_else(!is.na(NG_NAAT_ORD_VALUE_1), 1, 0),
NG_Pos = case_when(
  NG_NAAT_ORD_VALUE_1 == "Positive"  ~ 1,
  NG_NAAT_ORD_VALUE_1 == "Negative"  ~ 0,
  NG_NAAT_ORD_VALUE_1 %in% invalid_equivocal_negative ~ 0,
  is.na(NG_NAAT_ORD_VALUE_1) ~ NA,
  .default = NA
),
) %>% 
  mutate(NG_Results_Categorical = case_when(
    NG_Pos == 1 ~ "Positive",
    is.na(NG_NAAT_ORD_VALUE_1) ~ "Not Performed",
    (NG_NAAT_ORD_VALUE_1 == "Equivocal" | NG_NAAT_ORD_VALUE_1 == "Invalid") & is.na(NG_NAAT_ORD_VALUE_1) ~ "Test Error",
    NG_Pos == 0 ~ "Negative"
  ))


TV_Lab <- NG_TV_test_results %>% 
  filter(PAT_ENC_CSN_ID %in% final.cohort.csn) %>% 
  filter(str_detect(EXTERNAL_NAME, "Trichomonas vaginalis by NAAT")) %>% 
  group_by(PAT_ENC_CSN_ID) %>% 
  mutate(TV_NAAT_Num = row_number()) %>%
  filter(TV_NAAT_Num == 1) %>%  #26 patients had 2x TV NAAT tests done for some reason. all with same results as test 1, so will reduce to simplify
  ungroup() %>%
  pivot_wider(
    id_cols = PAT_ENC_CSN_ID,
    names_from = TV_NAAT_Num,
    values_from = c(ORD_VALUE),
    names_glue = "TV_NAAT_{.value}_{TV_NAAT_Num}"
  )

cohort.analysis.1 <- left_join(final.cohort.ctlab.urinelab.nglab, TV_Lab)  %>% 
  mutate(TV_Tested = if_else(!is.na(TV_NAAT_ORD_VALUE_1), 1, 0),
         TV_Pos = case_when(
           TV_NAAT_ORD_VALUE_1 == "Positive"  ~ 1,
           TV_NAAT_ORD_VALUE_1 == "Negative"  ~ 0,
           TV_NAAT_ORD_VALUE_1 %in% invalid_equivocal_negative ~ 0,
           is.na(TV_NAAT_ORD_VALUE_1) ~ NA,
           .default = NA
         ),
  ) %>% 
  mutate(TV_Results_Categorical = case_when(
    TV_Pos == 1 ~ "Positive",
    is.na(TV_NAAT_ORD_VALUE_1) ~ "Not Performed",
    (TV_NAAT_ORD_VALUE_1 == "Equivocal" | TV_NAAT_ORD_VALUE_1 == "Invalid") & is.na(TV_NAAT_ORD_VALUE_1) ~ "Test Error",
    TV_Pos == 0 ~ "Negative"
  ))



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
# still need: HSV, Syphylis

save(cohort.analysis.2, file = "STI_Cohort_Cleaned.RData")
