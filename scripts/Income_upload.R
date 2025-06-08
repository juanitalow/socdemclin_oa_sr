#remove from environment
rm(df)
#remove all
rm(list = ls())

#load
included_studies <- read.csv(file.choose (), header = TRUE)

#always remember to save, replace as appropriate
write.csv(freq_table_income_test_ordered, "sliceanddice_Sincome220525.csv", row.names = FALSE)


#to remove excluded studies
included_studies <- included_studies %>%
  filter(!is.na(samplesize_report) & samplesize_report !="")

#to extract just income 
income_test <- included_studies %>%
  select(number,
         authors, 
         year,
         title,
         study_country,
         samplesize_total,
         se_income_report,
         se_income_categories,
         se_income_desc,
         se_income_prop)

#change to factors with dplyr to visualise
income_test <-  income_test %>%
  mutate_all(as.factor)

freq_table_income_test<- income_test %>%
  count(number,
        authors, 
        year,
        title,
        study_country,
        samplesize_total,
        se_income_report,
        se_income_categories,
        se_income_desc,
        se_income_prop)

#rearrange with n in second column
freq_table_income_test <- freq_table_income_test %>%
  select(number,
         authors, 
         year,
         title,
         study_country,
         se_income_report,
         n,
         samplesize_total,
         se_income_categories,
         se_income_desc,
         se_income_prop)

# Remove all instances of "or" from the 'e_incomedesc' column \\b is a word boundary so it makes sure that it is specific to just the word and not including "/", it's actually better to just substitute "/"
#freq_table_incometest<- freq_table_incometest %>%
# mutate(status = gsub("\\bor\\b", " ", e_incomedesc))

#OR THIS WORKS BETTER

freq_table_income_test <- freq_table_income_test %>%
  mutate(se_income_desc = gsub(" or ", " ", se_income_desc))


#how to check max categories
# Calculate the maximum number of categories
max_categories <- max(sapply(strsplit(freq_table_income_test$se_income_desc, " "), length))


#use dplyr and tidyr to split into separate categories
#create categories dynamically (should be cleaner)
column_names <- paste0("category", 1:max_categories)

# Print maximum number of categories and column names
print(paste("Maximum number of categories:", max_categories))
print(column_names)

# Split the 'status' column into the dynamically determined number of columns
freq_table_income_test <- freq_table_income_test %>%
  separate(se_income_desc, into = column_names, fill = "right")

#now do the same with the proportion column
#sub "v" to split values in _prop

freq_table_income_test <- freq_table_income_test %>%
  mutate(se_income_prop = gsub("v", " ", se_income_prop))


#how to check max categories
# Calculate the maximum number of categories
max_categories1 <- max(sapply(strsplit(freq_table_income_test$se_income_prop, " "), length))


#use dplyr and tidyr to split into separate categories
#create categories dynamically (should be cleaner)
column_namesprop <- paste0("prop", 1:max_categories)

# Print maximum number of categories and column names
print(paste("Maximum number of categories:", max_categories1))
print(column_namesprop)

# Split the 'status' column into the dynamically determined number of columns
freq_table_income_test <- freq_table_income_test %>%
  separate(se_income_prop, into = column_namesprop, fill = "right")

#REORDER TABLE DYNAMICALLY
# Generate a list of patterns from "1" to "6"
patterns <- as.character(1:6)

# Initialize an empty vector to store the ordered column names
ordered_cols <- c()

# Loop through each pattern and find matching columns
for (pattern in patterns) {
  matched_cols <- names(freq_table_income_test)[str_detect(names(freq_table_income_test), paste0(pattern, "$"))]
  ordered_cols <- c(ordered_cols, matched_cols)
}

# Reorder the dataframe based on the ordered columns
freq_table_income_test_ordered <- freq_table_income_test %>%
  select(number,
         authors, 
         year,
         title,
         study_country,
         se_income_report,
         n,
         samplesize_total,
         se_income_categories,
         all_of(ordered_cols))

##filter out the unreported ones##
freq_table_income_test_ordered <- freq_table_income_test_ordered %>%
  filter( se_income_report !="no")

#save
write.csv(freq_table_income_test_ordered, "sliceanddice_Sincome220525.csv", row.names = FALSE)