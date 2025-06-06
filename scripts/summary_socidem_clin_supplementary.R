#included studies summary

#To mutate and add gender summary
included_studies<- included_studies %>%
  mutate(sexgender_report = if_else(!is.na(sexgender_femalewoman) & sexgender_femalewoman != "", "yes", "no"))

included_studies_summary_socdem <- included_studies %>%
  select(authors,
         year, 
         title,
         samplesize_total,
         p_geog_report,
         r_ethnicityrace_report,
         r_prilangspoken_report,
         r_cob_report,
         o_employment_report,
         sexgender_report,
         r_religion_report,
         e_edu_report,
         se_income_report,
         sc_report,
         age_report_plus,
         plus_characteristics,
         plus_which
  )

#to get just first author
included_studies_summary_socdem <- included_studies_summary_socdem %>%
  mutate(author = case_when(
    str_detect(authors, "^[^,]+,[^;]+") ~ str_replace(authors, "^([^,]+),([^;]+);.*", "\\2. \\1"),
    TRUE ~ str_extract(authors, "^[^;]+")
  )) %>%
  mutate(author = str_replace_all(author, "\\.\\.", "\\."))  # <- this line fixes ".."

#save
write.csv(included_studies_summary_socdem, "included_studies_summary_socdem170425.csv", row.names = FALSE)

included_studies_summary_socdem <- included_studies_summary_socdem %>%
  select(author,
         year, 
         title,
         samplesize_total,
         p_geog_report,
         r_ethnicityrace_report,
         r_prilangspoken_report,
         r_cob_report,
         o_employment_report,
         sexgender_report,
         r_religion_report,
         e_edu_report,
         se_income_report,
         sc_report,
         age_report_plus,
         plus_characteristics,
         plus_which
  )

#save
write.csv(included_studies_summary_socdem, "included_studies_summary_socdem170425.csv", row.names = FALSE)

#for clinical summary
included_studies_summary_clin <- included_studies %>%
  select(authors,
         year, 
         title,
         samplesize_total,
         height_report,
         weight_report,
         comorbidities_report,
         painvas_report,
         womac_report,
         koos_report
  )

#to get just first author
included_studies_summary_clin <- included_studies_summary_clin %>%
  mutate(author = case_when(
    str_detect(authors, "^[^,]+,[^;]+") ~ str_replace(authors, "^([^,]+),([^;]+);.*", "\\2. \\1"),
    TRUE ~ str_extract(authors, "^[^;]+")
  )) %>%
  mutate(author = str_replace_all(author, "\\.\\.", "\\."))  # <- this line fixes ".."

included_studies_summary_clin <- included_studies_summary_clin %>%
  select(author,
         year, 
         title,
         samplesize_total,
         height_report,
         weight_report,
         comorbidities_report,
         painvas_report,
         womac_report,
         koos_report
  )

write.csv(included_studies_summary_clin, "included_studies_summary_clin170425.csv", row.names = FALSE)
