#remove from environment
rm(df)
#remove all
rm(list = ls())

#load
included_studies <- read.csv(file.choose (), header = TRUE)
edu_categorised <- read.csv(file.choose (), header = TRUE)

#always remember to save, replace as appropriate
write.csv(freq_table_education_test_ordered, "sliceanddice_education220525.csv", row.names = FALSE)
write.csv(edu_years, "edu_years220525.csv", row.names = FALSE)
write.csv(edu_categorised, "edu_cat220525.csv", row.names = FALSE)
write.csv(edu_reordered, "reordered_edu220525.csv", row.names = FALSE)
write.csv(edu_recategorised, "edu_recat220525.csv", row.names = FALSE)


#to remove excluded studies
included_studies <- included_studies %>%
  filter(!is.na(samplesize_report) & samplesize_report !="")

#to extract just education 
education_test <- included_studies %>%
  select(number,
         authors, 
         year,
         title,
         study_country,
         samplesize_total,
         e_edu_report,
         e_edu_categories,
         e_edu_desc,
         e_edu_prop,
         e_edu_mean)

#change to factors with dplyr to visualise
education_test <-  education_test %>%
  mutate_all(as.factor)

freq_table_education_test<- education_test %>%
  count(number,
        authors, 
        year,
        title,
        study_country,
        samplesize_total,
        e_edu_report,
        e_edu_categories,
        e_edu_desc,
        e_edu_prop,
        e_edu_mean)

#rearrange with n in second column
freq_table_education_test <- freq_table_education_test %>%
  select(number,
         authors, 
         year,
         title,
         study_country,
         e_edu_report,
         n,
         samplesize_total,
         e_edu_categories,
         e_edu_desc,
         e_edu_prop,
         e_edu_mean)

#to remove excluded studies i.e. "no"
freq_table_education_test <- freq_table_education_test %>%
  filter(!is.na(e_edu_report) & e_edu_report !="no")

#to mutate as character strings
freq_table_education_test <-  freq_table_education_test %>%
  mutate_all(as.character)

# Remove all instances of "or" from the 'e_education_desc' column \\b is a word boundary so it makes sure that it is specific to just the word and not including "/", it's actually better to just substitute "/"
#freq_table_education_test<- freq_table_education_test %>%
# mutate(status = gsub("\\bor\\b", " ", e_education_desc))

#OR THIS WORKS BETTER

freq_table_education_test <- freq_table_education_test %>%
  mutate(e_edu_desc = gsub(" or ", " ", e_edu_desc))


#how to check max categories
# Calculate the maximum number of categories
max_categories <- max(sapply(strsplit(freq_table_education_test$e_edu_desc, " "), length))


#use dplyr and tidyr to split into separate categories
#create categories dynamically (should be cleaner)
column_names <- paste0("category", 1:max_categories)

# Print maximum number of categories and column names
print(paste("Maximum number of categories:", max_categories))
print(column_names)

# Split the 'status' column into the dynamically determined number of columns
freq_table_education_test <- freq_table_education_test %>%
  separate(e_edu_desc, into = column_names, fill = "right")

#now do the same with the proportion column
#sub "v" to split values in _prop

freq_table_education_test <- freq_table_education_test %>%
  mutate(e_edu_prop = gsub("v", " ", e_edu_prop))


#how to check max categories
# Calculate the maximum number of categories
max_categories1 <- max(sapply(strsplit(freq_table_education_test$e_edu_prop, " "), length))


#use dplyr and tidyr to split into separate categories
#create categories dynamically (should be cleaner)
column_namesprop <- paste0("prop", 1:max_categories)

# Print maximum number of categories and column names
print(paste("Maximum number of categories:", max_categories1))
print(column_namesprop)

# Split the 'status' column into the dynamically determined number of columns
freq_table_education_test <- freq_table_education_test %>%
  separate(e_edu_prop, into = column_namesprop, fill = "right")

#REORDER TABLE DYNAMICALLY
# Generate a list of patterns from "1" to "5"
patterns <- as.character(1:9)

# Initialize an empty vector to store the ordered column names
ordered_cols <- c()

# Loop through each pattern and find matching columns
for (pattern in patterns) {
  matched_cols <- names(freq_table_education_test)[str_detect(names(freq_table_education_test), paste0(pattern, "$"))]
  ordered_cols <- c(ordered_cols, matched_cols)
}

# Reorder the dataframe based on the ordered columns
freq_table_education_test_ordered <- freq_table_education_test %>%
  select(number,
         authors, 
         year,
         title,
         study_country,
         e_edu_report,
         n,
         samplesize_total,
         e_edu_categories,
         all_of(ordered_cols),
         e_edu_mean)

#new dataframe to split num and cat
edu_categorised <- freq_table_education_test_ordered

#create years in a separate dataframe and remove from mainframe
edu_years <- edu_categorised %>%
  filter(e_edu_categories == "years")

#remove extra columns that are empty
edu_years <- edu_years %>%
  select(where(~ !all(is.na(.) | . == "")))

#clean edu_categorised
edu_categorised <- edu_categorised %>%
  filter(e_edu_categories !="years")

#remove extra columns that are empty
edu_categorised <- edu_categorised %>%
  select(where(~ !all(is.na(.) | . == "")))

#save
write.csv(freq_table_education_test_ordered, "sliceanddice_education220525.csv", row.names = FALSE)
write.csv(edu_years, "edu_years220525.csv", row.names = FALSE)
write.csv(edu_categorised, "edu_cat220525.csv", row.names = FALSE)

####clear everything and reload edu_categorised
rm(list = ls())
edu_categorised <- read.csv(file.choose (), header = TRUE)

#now decide how the values should be categorised, in the case of education, I want "tertiary, "non-tertiary"
#now create a set of conditions to replace data categories THE ORDER MATTERS
# Define the categorization function
edu_category_mapping1 <- function(category1) {
  case_when(
    is.na(category1) ~ NA_character_,  # Handle NA values
    
    
    str_detect(category1, "pri|secondarybelow|litera|7|11|none|secsch|secedu|3highsch|notbachelors|notcollegeeducated|noteducated|nothighschoolormore|lessthanhigh|lessthan6years|lessthan12|lessthanequal8years|notabovehighschool|noteducated|middletohighsch|below12thgrade|notcollegegrad|nocollege|form3andbelow|6to12years|elementarysch|nothighschoolandmore|highschoolgrad|notmorethanhighschool|elementary|middleschool|senior") ~ "non-tertiary",
    
    str_detect(category1, "bachelor|atleastsomegradschool|uni|nothighschoolless|someedubeyondhighschool|highschoolormore|highschoolandmore|more12years|highschandabove|abovesecondary|abovehighschool|morethanhighschool|morethan12|above12thgrade|voc|trade|masters|prof|tertiary|1to4y|collegeuni|associate|somecollege|lessthan4years|master|overhigh") ~ "tertiary",
    
    category1 %in% c("highschoolless", "12", "highschool", "highsch", "highschgrad","secondary") ~ "non-tertiary",
    
    str_detect(category1, "college|grad") ~ "tertiary",
    
    category1 %in% c("intermediate", "matriculation", "educated", "aboveform3", "morethan8") ~ "unclear",
    
    TRUE ~ category1  # Return the original value for any other status
  )
}

#now use mutate to apply across the columns that have been identified
edu_recategorised <- edu_categorised %>%
  mutate(across(c(category1,
                  category2,
                  category3,
                  category4,
                  category5,
                  category6,
                  category7,
                  category8,
                  category9), 
                edu_category_mapping1))

#change props back to numeric
edu_recategorised <-  edu_recategorised %>%
  mutate(across(contains("prop"),as.numeric))


#now to identify duplicates (+remove) and to reorder the dataframe i.e. category 1 = non-tertiary, category 2 = tertiary, category 3 = unclear
# Function to reorder and remove duplicates in a row
reorder_row <- function(category, prop) {
  reordered <- rep(NA, length(category))
  props <- rep(NA, length(prop))
  
  # Aggregate prop for "non-tertiary"
  if ("non-tertiary" %in% category) {
    reordered[1] <- "non-tertiary"
    props[1] <- sum(prop[category == "non-tertiary"], na.rm = TRUE)
  }
  
  # Aggregate prop for "tertiary"
  if ("tertiary" %in% category) {
    reordered[2] <- "tertiary"
    props[2] <- sum(prop[category == "tertiary"], na.rm = TRUE)
  }
  
  # Aggregate prop for "unclear"
  if ("unclear" %in% category) {
    reordered[3] <- "unclear"
    props[3] <- sum(prop[category == "unclear"], na.rm = TRUE)
  }
  
  
  # Handle remaining categories
  remaining_categories <- setdiff(category, c("non-tertiary", "tertiary", "unclear", NA))
  remaining_props <- prop[category %in% remaining_categories]
  
  if (length(remaining_categories) > 0) {
    reordered[3:(2 + length(remaining_categories))] <- remaining_categories
    props[3:(2 + length(remaining_categories))] <- remaining_props
  }
  
  return(list(reordered = reordered, props = props))
}

# Apply the reorder function to each row
edu_reordered <- edu_recategorised %>% 
  rowwise() %>%
  mutate(
    result = list(reorder_row(c(category1, category2, category3, category4, category5, category6, category7, category8, category9), 
                              c(prop1, prop2, prop3, prop4, prop5, prop6, prop7, prop8, prop9))),
    # Assign the reordered values back to the original columns
    category1 = result$reordered[1], prop1 = result$props[1],
    category2 = result$reordered[2], prop2 = result$props[2],
    category3 = result$reordered[3], prop3 = result$props[3],
    category4 = result$reordered[4], prop4 = result$props[4],
    category5 = result$reordered[5], prop5 = result$props[5],
    category6 = result$reordered[6], prop6 = result$props[6],
    category7 = result$reordered[7], prop7 = result$props[7],
    category8 = result$reordered[8], prop8 = result$props[8],
    category9 = result$reordered[9], prop9 = result$props[9]
  ) %>%
  select(-result) %>%
  ungroup()

# Make sure prop1..., and samplesize_total are numeric
edu_reordered <- edu_reordered %>%
  mutate(across(starts_with("prop"), ~ as.numeric(.)))

edu_reordered$samplesize_total <- as.numeric(edu_reordered$samplesize_total)

# Add counts based on props and sample size
edu_reordered<- edu_reordered %>%
  mutate(across(
    .cols = starts_with("prop"),
    .fns = ~ round((. / 100) * samplesize_total, 2),
    .names = "count{.col}"
  ))

# Reorder columns to interleave prop and count
edu_reordered <- edu_reordered %>%
  relocate(countprop1, .after = prop1) %>%
  relocate(countprop2, .after = prop2) %>%
  relocate(countprop3, .after = prop3) %>%
  relocate(countprop4, .after = prop4) %>%
  relocate(countprop5, .after = prop5) %>%
  relocate(countprop6, .after = prop6)

#remove extra columns that are empty
edu_reordered <- edu_reordered %>%
  select(where(~ !all(is.na(.) | . == "")))

##save
write.csv(edu_categorised, "edu_cat220525.csv", row.names = FALSE)
write.csv(edu_reordered, "reordered_edu220525.csv", row.names = FALSE)
write.csv(edu_recategorised, "edu_recat220525.csv", row.names = FALSE)

#kiv will need to append the studies to dataframe later in a supplementary