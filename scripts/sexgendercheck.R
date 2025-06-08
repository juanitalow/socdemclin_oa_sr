#remove all
rm(list = ls())


#load
included_studies <- read.csv(file.choose (), header = TRUE)

#select variables
gendersex <- included_studies %>%
  select(number,
         authors, 
         year,
         title, 
         samplesize_total,
         sex_report, 
         gender_report, 
         g_gendersex_report,
         sexgender_femalewoman, 
         sexgender_maleman
         )



#check sum
sum(gendersex$sexgender_femalewoman, na.rm = TRUE)

sum(gendersex$sexgender_maleman, na.rm = TRUE)

#remove NAs where both sexgender are not reported
gendersex <- gendersex %>%
  filter(!(is.na(sexgender_femalewoman) & is.na(sexgender_maleman)))
  

#check classification
#change to factors with dplyr to visualise
gendersex$g_gendersex_report <- as.factor(gendersex$g_gendersex_report)
summary(gendersex$g_gendersex_report)


#always remember to save, replace as appropriate
write.csv(gendersex, "gendersexcheck210525.csv", row.names = FALSE)