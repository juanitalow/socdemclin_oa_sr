#race and ethnicity
#remove from environment
rm(df)
#remove all
rm(list = ls())


#always remember to save, replace as appropriate
write.csv(freq_table_race_test_ordered, "sliceanddice_raceethnicity220525.csv", row.names = FALSE)

#load
included_studies <- read.csv(file.choose (), header = TRUE)

#to remove excluded studies
included_studies <- included_studies %>%
  filter(!is.na(samplesize_report) & samplesize_report !="")

#to extract just race/ethnicity
race_test <- included_studies %>%
  select(number,
         authors, 
         year,
         title,
         study_country,
         samplesize_total,
         r_ethnicityrace_report,
         r_ethnicityrace_lumped,
         r_ethnicityrace_class,
         r_ethnicityrace_categories,
         r_ethnicityrace_desc,
         r_ethnicityrace_prop)

#change to factors with dplyr to visualise
race_test <-  race_test %>%
  mutate_all(as.factor)

freq_table_race_test<- race_test %>%
  count(number,
        authors, 
        year,
        title,
        study_country,
        samplesize_total,
        r_ethnicityrace_report,
        r_ethnicityrace_lumped,
        r_ethnicityrace_class,
        r_ethnicityrace_categories,
        r_ethnicityrace_desc,
        r_ethnicityrace_prop)

#rearrange with n in second column
freq_table_race_test <- freq_table_race_test %>%
  select(number,
         authors, 
         year,
         title,
         study_country,
         r_ethnicityrace_report,
         n,
         samplesize_total,
         r_ethnicityrace_lumped,
         r_ethnicityrace_class,
         r_ethnicityrace_categories,
         r_ethnicityrace_desc,
         r_ethnicityrace_prop)

# Remove all instances of "or" from the 'e_incomedesc' column \\b is a word boundary so it makes sure that it is specific to just the word and not including "/", it's actually better to just substitute "/"
#freq_table_incometest<- freq_table_incometest %>%
# mutate(status = gsub("\\bor\\b", " ", e_incomedesc))

#OR THIS WORKS BETTER

freq_table_race_test <- freq_table_race_test %>%
  mutate(r_ethnicityrace_desc = gsub(" or ", " ", r_ethnicityrace_desc))


#how to check max categories
# Calculate the maximum number of categories
max_categories <- max(sapply(strsplit(freq_table_race_test$r_ethnicityrace_desc, " "), length))


#use dplyr and tidyr to split into separate categories
#create categories dynamically (should be cleaner)
column_names <- paste0("category", 1:max_categories)

# Print maximum number of categories and column names
print(paste("Maximum number of categories:", max_categories))
print(column_names)

# Split the 'status' column into the dynamically determined number of columns
freq_table_race_test <- freq_table_race_test %>%
  separate(r_ethnicityrace_desc, into = column_names, fill = "right")

#now do the same with the proportion column
#sub "v" to split values in _prop

freq_table_race_test <- freq_table_race_test %>%
  mutate(r_ethnicityrace_prop = gsub("v", " ", r_ethnicityrace_prop))


#how to check max categories
# Calculate the maximum number of categories
max_categories1 <- max(sapply(strsplit(freq_table_race_test$r_ethnicityrace_prop, " "), length))


#use dplyr and tidyr to split into separate categories
#create categories dynamically (should be cleaner)
column_namesprop <- paste0("prop", 1:max_categories)

# Print maximum number of categories and column names
print(paste("Maximum number of categories:", max_categories1))
print(column_namesprop)

# Split the 'status' column into the dynamically determined number of columns
freq_table_race_test <- freq_table_race_test %>%
  separate(r_ethnicityrace_prop, into = column_namesprop, fill = "right")

#REORDER TABLE DYNAMICALLY
# Generate a list of patterns from "1" to "6"
patterns <- as.character(1:6)

# Initialize an empty vector to store the ordered column names
ordered_cols <- c()

# Loop through each pattern and find matching columns
for (pattern in patterns) {
  matched_cols <- names(freq_table_race_test)[str_detect(names(freq_table_race_test), paste0(pattern, "$"))]
  ordered_cols <- c(ordered_cols, matched_cols)
}

# Reorder the dataframe based on the ordered columns
freq_table_race_test_ordered <- freq_table_race_test %>%
  select(number,
         authors, 
         year,
         title,
         study_country,
         r_ethnicityrace_report,
         n,
         samplesize_total,
         r_ethnicityrace_lumped,
         r_ethnicityrace_class,
         r_ethnicityrace_categories,
         all_of(ordered_cols))

#always remember to save, replace as appropriate
write.csv(freq_table_race_test_ordered, "sliceanddice_raceethnicity220525.csv", row.names = FALSE)

#clear workspace for cleaner environment
#remove all
rm(list = ls())
#open slice and diced and reordered csv
race_categorised <- read.csv(file.choose (), header = TRUE)

#create methds of classification in a separate dataframe and remove from mainframe
racethnicity_classification <- race_categorised %>%
  select(number,
         authors, 
         year,
         title,
         study_country,
         n,
         samplesize_total,
         r_ethnicityrace_lumped,
         r_ethnicityrace_class)

racethnicity_classification$r_ethnicityrace_lumped <- as.factor(racethnicity_classification$r_ethnicityrace_lumped)
racethnicity_classification$r_ethnicityrace_class <- as.factor(racethnicity_classification$r_ethnicityrace_class)

#THIS IS TO CHECK HOW THE RACE+/-ETHNICITY VARIABLES ARE CLASSIFIED
freq_table_raceethnicity_classification <- racethnicity_classification %>%
  count(r_ethnicityrace_lumped, r_ethnicityrace_class)

##SAVE##
write.csv(freq_table_raceethnicity_classification, "raceethnicity_classification_table220525.csv", row.names = FALSE)

#frequencies and prop for race/ethnicity
race_recategorised <- race_categorised %>%
  select(-r_ethnicityrace_class, 
         -r_ethnicityrace_lumped)

#to remove excluded studies i.e. "no"
race_recategorised <- race_recategorised %>%
  filter(!is.na(r_ethnicityrace_report) & r_ethnicityrace_report !="no")

#now decide how the values should be categorised, in the case of education, I want "white", "black", "asian or pacific islander", "native american or alaskan native", "latino", "non latino", unclear" 
#now create a set of conditions to replace data categories THE ORDER MATTERS
# Define the categorization function
race_category_mapping1 <- function(category1) {
  case_when(
    is.na(category1) ~ NA_character_,  # Handle NA values
    
    category1 %in% c("other", "morethan1race", "not", "minority", "notwhite", "nonwhite", "notcaucasian", "notafricanamerican") ~ "unclear",
    
    str_detect(category1, "nothispanic") ~ "not hispanic or latino",
    
    str_detect(category1, "hispanic|latino") ~ "hispanic or latino",
    
    str_detect(category1, "white|anglo|caucasian") ~ "white",
    
    str_detect(category1, "asia|chin") ~ "asian or pacific islander",
    
    str_detect(category1, "native") ~ "native american or alaskan native",
    
    str_detect(category1, "black|african") ~ "black or african american",
    
    TRUE ~ category1  # Return the original value for any other status
  )
}

#now use mutate to apply across the columns that have been identified
raceethnicity_recategorised <- race_recategorised %>%
  mutate(across(c(category1,
                  category2,
                  category3,
                  category4,
                  category5,
                  category6), 
                race_category_mapping1))

#change props back to numeric
raceethnicity_recategorised <-  raceethnicity_recategorised %>%
  mutate(across(contains("prop"),as.numeric))

#add columns as more categories than current dataframe
new_columns <- c("category7", "prop7", "category8", "prop8", "category9", "prop9", "category10", "prop10") 
raceethnicity_recategorised[new_columns] <- NA

#now to identify duplicates (+remove) and to reorder the dataframe i.e. category 1 = white, category 2 = black or african american, category 3 = asian or pacific islander etc...
# Function to reorder and remove duplicates in a row
reorder_row <- function(category, prop) {
  # Initialize empty vectors
  reordered <- rep(NA, 10)
  props <- rep(NA, 10)
  
  # Check and aggregate props based on categories
  if ("white" %in% category) {
    reordered[1] <- "white"
    props[1] <- sum(prop[category == "white"], na.rm = TRUE)
  }
  
  if ("black or african american" %in% category) {
    reordered[2] <- "black or african american"
    props[2] <- sum(prop[category == "black or african american"], na.rm = TRUE)
  }
  
  if ("asian or pacific islander" %in% category) {
    reordered[3] <- "asian or pacific islander"
    props[3] <- sum(prop[category == "asian or pacific islander"], na.rm = TRUE)
  }
  
  if ("native american or alaskan native" %in% category) {
    reordered[4] <- "native american or alaskan native"
    props[4] <- sum(prop[category == "native american or alaskan native"], na.rm = TRUE)
  }
  
  if ("hispanic or latino" %in% category) {
    reordered[5] <- "hispanic or latino"
    props[5] <- sum(prop[category == "hispanic or latino"], na.rm = TRUE)
  } 
  
  if ("not hispanic or latino" %in% category) {
    reordered[6] <- "not hispanic or latino"
    props[6] <- sum(prop[category == "not hispanic or latino"], na.rm = TRUE)
  } 
  
  if ("unclear" %in% category) {
    reordered[7] <- "unclear"
    props[7] <- sum(prop[category == "unclear"], na.rm = TRUE)
  } 
  
  if ("atsi" %in% category) {
    reordered[8] <- "atsi"
    props[8] <- sum(prop[category == "atsi"], na.rm = TRUE)
  }
  
  if ("notatsi" %in% category) {
    reordered[9] <- "notatsi"
    props[9] <- sum(prop[category == "notatsi"], na.rm = TRUE)
  }
  
  # Return the results as a list (always the same structure)
  return(list(reordered = reordered, props = props))
}


# Apply the reorder function to each row
raceethnicity_reordered <- raceethnicity_recategorised %>%
  rowwise() %>%
  mutate(
    # Ensure 'reorder_row' is applied to each row, and we unpack the result correctly
    result = list(reorder_row(c(category1, category2, category3, category4, category5, category6, category7, category8, category9, category10),
                              c(prop1, prop2, prop3, prop4, prop5, prop6, prop7, prop8, prop9, prop10))),
    
    # Assign the reordered categories and their corresponding proportions to the appropriate columns
    category1 = result$reordered[1], prop1 = result$props[1],
    category2 = result$reordered[2], prop2 = result$props[2],
    category3 = result$reordered[3], prop3 = result$props[3],
    category4 = result$reordered[4], prop4 = result$props[4],
    category5 = result$reordered[5], prop5 = result$props[5],
    category6 = result$reordered[6], prop6 = result$props[6],
    category7 = result$reordered[7], prop7 = result$props[7],
    category8 = result$reordered[8], prop8 = result$props[8],
    category9 = result$reordered[9], prop9 = result$props[9],
    category10 = result$reordered[10], prop10 = result$props[10]
  ) %>%
  select(-result) %>%
  ungroup()


#remove extra columns that are empty
raceethnicity_reordered <- raceethnicity_reordered %>%
  select(where(~ !all(is.na(.) | . == "")))

# Add counts based on props and sample size
raceethnicity_reordered <- raceethnicity_reordered %>%
  mutate(across(
    .cols = starts_with("prop"),
    .fns = ~ round((. / 100) * samplesize_total, 2),
    .names = "count{.col}"
  ))

# Reorder columns to interleave prop and count
raceethnicity_reordered <- raceethnicity_reordered %>%
  relocate(countprop1, .after = prop1) %>%
  relocate(countprop2, .after = prop2) %>%
  relocate(countprop3, .after = prop3) %>%
  relocate(countprop4, .after = prop4) %>%
  relocate(countprop5, .after = prop5) %>%
  relocate(countprop6, .after = prop6) %>%
  relocate(countprop7, .after = prop7) %>%
  relocate(countprop8, .after = prop8) %>%
  relocate(countprop9, .after = prop9)

#Select for race/ethnicity variables that explicitly report identifiable prop and counts
#assumption made here for "unclear" race/ethnicity variables when reporting in table: (total sample size of reporting studies)-(all the listed categories)

raceethnicity_reordered <- raceethnicity_reordered %>%
  select(-category6,
         -prop6,
         -countprop6,
         -category7,
         -prop7,
         -countprop7,
         -category9,
         -prop9,
         -countprop9)


#save
write.csv(racethnicity_classification, "raceethnicity_classification220525.csv", row.names = FALSE)
write.csv(freq_table_raceethnicity_classification, "raceethnicity_classification_table220525.csv", row.names = FALSE)
write.csv(raceethnicity_reordered, "raceethnicity_reordered220525.csv", row.names = FALSE)
