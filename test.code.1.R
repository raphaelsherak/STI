query <- encounters %>% select(PAT_MRN_ID, PAT_ENC_CSN_ID, ED_DISPOSITION, ARRIVAL_LOCATION)
discharged<- query %>% filter(ED_DISPOSITION == "Discharge")
query2<- STI_JDAT_March1 %>% select(PAT_MRN_ID,PAT_ENC_CSN_ID, Order_Date, Med_ShortName)
doxyrx<- query2 %>% filter(str_detect(pattern = "doxy", string = Med_ShortName))
