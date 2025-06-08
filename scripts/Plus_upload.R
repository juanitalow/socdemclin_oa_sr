#remove all
rm(list = ls())

#load
included_studies <- read.csv(file.choose (), header = TRUE)

#Plus - all features
plus <- included_studies %>%
  select(number,
         authors, 
         year,
         title,
         study_country,
         samplesize_total,
         age_report_plus,
         plus_characteristics,
         plus_which) %>%
  filter(plus_characteristics !="no")

#change to factors
plus <- plus %>%
  mutate_all(as.factor)

#summary of plus_which, smokingdrinking and smoking to be together in the same category smoking and/or drinking. 
summary(plus$plus_which)

#save
write.csv(plus, "plus220525.csv", row.names = FALSE)