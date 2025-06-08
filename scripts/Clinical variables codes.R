#remove from environment
rm(df)
#remove all
rm(list = ls())

#load
included_studies <- read.csv(file.choose (), header = TRUE)
#to remove excluded studies
included_studies <- included_studies %>%
  filter(!is.na(samplesize_report) & samplesize_report !="")


#always remember to save, replace as appropriate
write.csv(clinical_variables_summarynum, "clinicalvariablesnumsummary190525.csv", row.names = FALSE)
write.csv(clinical_variables_summarycat, "clinicalvariablescatsummary190525.csv", row.names = FALSE)
write.csv(summary_table_clinnum, "summary_table_clinnum190525.csv", row.names = FALSE)
write.csv(summary_table_clincat, "summary_table_clincat190525.csv", row.names = FALSE)
write.csv(koos_studies, "koos_studies190525.csv", row.names = FALSE)
write.csv(womac_studies, "womac_studies190525.csv", row.names = FALSE)
write.csv(included_studies, "included_studies190525.csv", row.names = FALSE)

#select for WOMAC only
womac_studies <- included_studies %>%
  select (number,
          title,
          samplesize_total,
          womac_report,
          womac_painmean,
          womac_painsd,
          womac_stiffmean,
          womac_stiffsd,
          womac_funcmean,
          womac_funcsd,
          womac_totalonly) %>%
  filter(womac_report !="no")

#replace "nr" with NA
womac_studies<- womac_studies %>%
  mutate(across(where(is.character), ~ na_if(., "nr")))

#select for KOOS only
koos_studies <- included_studies %>%
  select (number,
          title,
          samplesize_total,
          koos_report,
          koos_painmean,
          koos_painsd,
          koos_sympmean,
          koos_sympsd,
          koos_adlmean,
          koos_adlsd,
          koos_sportmean,
          koos_sportsd,
          koos_qolmean,
          koos_qolsd) %>%
  filter(koos_report !="no")


#filter for variables to keep numerical

clinical_variables_summarynum <- included_studies %>%
  select(samplesize_total,
         sexgender_femalewoman,
         sexgender_maleman,
         age_mean,
         height_mean,
         weight_mean,
         bmi_mean,
         painvas100_mean)

#filter for variables to keep categorical
clinical_variables_summarycat <- included_studies %>%
  select(comorbidities_report,
         comorbidities_howmany,
         koos_report,
         womac_report)

#check class of variables
str(clinical_variables_summarycat)
str(clinical_variables_summarynum)

#convert all to numeric


#summarise numeric variables
summary_table_clinnum <- clinical_variables_summarynum %>%
  summarize_if(is.numeric, list(
    total = ~sum(., na.rm = TRUE),
    mean = ~mean(., na.rm = TRUE),
    sd = ~sd(., na.rm = TRUE)
  ))

#summarise categorical variables with frequency table
#mutate all as factors
clinical_variables_summarycat <-  clinical_variables_summarycat %>%
  mutate_all(as.factor)

summary_table_clincat <- clinical_variables_summarycat %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
  count(Variable, Value) %>%
  pivot_wider(names_from = Value, values_from = n, values_fill = list(n = 0))
