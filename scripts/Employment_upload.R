#remove from environment
rm(df)
#remove all
rm(list = ls())

#always remember to save, replace as appropriate
write.csv(freq_table_employment_test_ordered, "sliceanddice_employment210525.csv", row.names = FALSE)

#load
included_studies <- read.csv(file.choose (), header = TRUE)

#to remove excluded studies
included_studies <- included_studies %>%
  filter(!is.na(samplesize_report) & samplesize_report !="")

#to extract just employment 
employment_test <- included_studies %>%
  select(number,
         authors, 
         year,
         title,
         study_country,
         samplesize_total,
         o_employment_report,
         o_employment_categories,
         o_employment_desc,
         o_employment_prop)

#change to factors with dplyr to visualise
employment_test <-  employment_test %>%
  mutate_all(as.factor)

freq_table_employment_test<- employment_test %>%
  count(number,
        authors, 
        year,
        title,
        study_country,
        samplesize_total,
        o_employment_report,
        o_employment_categories,
        o_employment_desc,
        o_employment_prop)

#rearrange with n in second column
freq_table_employment_test <- freq_table_employment_test %>%
  select(number,
         authors, 
         year,
         title,
         study_country,
         samplesize_total,
         o_employment_report,
        n,
        o_employment_categories,
        o_employment_desc,
        o_employment_prop)

# Remove all instances of "or" from the 'o_employment_desc' column \\b is a word boundary so it makes sure that it is specific to just the word and not including "/", it's actually better to just substitute "/"
#freq_table_employment_test<- freq_table_employment_test %>%
 # mutate(status = gsub("\\bor\\b", " ", o_employment_desc))

#OR THIS WORKS BETTER

freq_table_employment_test <- freq_table_employment_test %>%
  mutate(o_employment_desc = gsub("/", "q", o_employment_desc))

freq_table_employment_test <- freq_table_employment_test %>%
  mutate(o_employment_desc = gsub(" or ", " ", o_employment_desc))


#how to check max categories
# Calculate the maximum number of categories
max_categories <- max(sapply(strsplit(freq_table_employment_test$o_employment_desc, " "), length))


#use dplyr and tidyr to split into separate categories
#create categories dynamically (should be cleaner)
column_names <- paste0("category", 1:max_categories)

# Print maximum number of categories and column names
print(paste("Maximum number of categories:", max_categories))
print(column_names)

# Split the 'status' column into the dynamically determined number of columns
freq_table_employment_test <- freq_table_employment_test %>%
  separate(o_employment_desc, into = column_names, fill = "right")

#now do the same with the proportion column
#sub "v" to split values in _prop

freq_table_employment_test <- freq_table_employment_test %>%
  mutate(o_employment_prop = gsub("v", " ", o_employment_prop))


#how to check max categories
# Calculate the maximum number of categories
max_categories1 <- max(sapply(strsplit(freq_table_employment_test$o_employment_prop, " "), length))


#use dplyr and tidyr to split into separate categories
#create categories dynamically (should be cleaner)
column_namesprop <- paste0("prop", 1:max_categories)

# Print maximum number of categories and column names
print(paste("Maximum number of categories:", max_categories1))
print(column_namesprop)

# Split the 'status' column into the dynamically determined number of columns
freq_table_employment_test <- freq_table_employment_test %>%
  separate(o_employment_prop, into = column_namesprop, fill = "right")

#REORDER TABLE DYNAMICALLY
# Generate a list of patterns from "1" to "5"
patterns <- as.character(1:5)

# Initialize an empty vector to store the ordered column names
ordered_cols <- c()

# Loop through each pattern and find matching columns
for (pattern in patterns) {
  matched_cols <- names(freq_table_employment_test)[str_detect(names(freq_table_employment_test), paste0(pattern, "$"))]
  ordered_cols <- c(ordered_cols, matched_cols)
}

# Reorder the dataframe based on the ordered columns
freq_table_employment_test_ordered <- freq_table_employment_test %>%
  select(number,
         authors, 
         year,
         title,
         study_country,
         samplesize_total,
         o_employment_report,
         n,
         o_employment_categories,
         all_of(ordered_cols))

#Save
write.csv(freq_table_employment_test_ordered, "sliceanddice_employment210525.csv", row.names = FALSE)

#clean and clear environment
#remove all
rm(list = ls())

#open slice and diced and reordered csv
employment_categorised <- read.csv(file.choose (), header = TRUE)

#now decide how the values should be categorised, in the case of employment, I want "employed", "unemployed", "retired" and "other"
#now create a set of conditions to replace data categories THE ORDER MATTERS
# Define the categorization function
employment_category_mapping <- function(category1) {
  case_when(
    is.na(category1) ~ NA_character_,  # Handle NA values
    str_detect(category1, "retired") ~ "retired",
    str_detect(category1, "notw|unem|no|cant|unable") ~ "unemployed",
    str_detect(category1, "current|worker|employed|working|yes|yee|part|prof") ~ "employed",
    TRUE ~ "other"  # Handles any other status not covered
  )
}

#now use mutate to apply across the columns that have been identified
employment_recategorised <- employment_categorised %>%
  mutate(across(c(category1,
                  category2,
                  category3,
                  category4,
                  category5), 
                employment_category_mapping))

#now to identify duplicates (+remove) and to reorder the dataframe i.e. category 1 = employed, category 2 = unemployed, category 3 = retired, category 4 = other
# Function to reorder and remove duplicates in a row
reorder_row <- function(category, prop) {
  reordered <- c(NA, NA, NA, NA)
  props <- c(NA, NA, NA, NA)
  
  # Check for each category and aggregate corresponding prop scores
  if ("employed" %in% category) {
    reordered[1] <- "employed"
    props[1] <- sum(prop[category == "employed"], na.rm = TRUE)  # Aggregate scores
  }
  if ("unemployed" %in% category) {
    reordered[2] <- "unemployed"
    props[2] <- sum(prop[category == "unemployed"], na.rm = TRUE)  # Aggregate scores
  }
  if ("retired" %in% category) {
    reordered[3] <- "retired"
    props[3] <- sum(prop[category == "retired"], na.rm = TRUE)  # Aggregate scores
  }
  if ("other" %in% category) {
    reordered[4] <- "other"
    props[4] <- sum(prop[category == "other"], na.rm = TRUE)  # Aggregate scores
  }
  
# Add this block to remove duplicates
seen <- c()
for (i in seq_along(reordered)) {
  if (!is.na(reordered[i])) {
    if (reordered[i] %in% seen) {
      reordered[i] <- NA
      props[i] <- NA
    } else {
      seen <- c(seen, reordered[i])
    }
  }
}

return(list(reordered, props))
}


# Apply the reorder function to each row
employment_reordered <- employment_recategorised %>% 
  rowwise() %>%
  mutate(
    result = list(reorder_row(c(category1, category2, category3, category4, category5), 
                              c(prop1, prop2, prop3, prop4, prop5))),
    category1 = result[[1]][1], prop1 = result[[2]][1],
    category2 = result[[1]][2], prop2 = result[[2]][2],
    category3 = result[[1]][3], prop3 = result[[2]][3],
    category4 = result[[1]][4], prop4 = result[[2]][4],
    category5 = NA_character_,  #Clear leftovers
    prop5 = NA_real_           #Clear leftovers
  ) %>%
  select(-result) %>%
  ungroup()

# Make sure prop1–prop4 are numeric
employment_reordered <- employment_reordered %>%
  mutate(across(starts_with("prop"), ~ as.numeric(.)))

# Add counts based on props and sample size
employment_reordered <- employment_reordered %>%
  mutate(across(
    .cols = starts_with("prop"),
    .fns = ~ round((. / 100) * samplesize_total, 2),
    .names = "count{.col}"
  ))

# Reorder columns to interleave prop and count
employment_reordered <- employment_reordered %>%
  relocate(countprop1, .after = prop1) %>%
  relocate(countprop2, .after = prop2) %>%
  relocate(countprop3, .after = prop3) %>%
  relocate(countprop4, .after = prop4)

#remove NAs
employment_reordered <- employment_reordered %>%
  filter(!is.na(o_employment_report) & o_employment_report !="no")

#remove extra columns
employment_reordered <- employment_reordered %>%
  select(-category5,
         -prop5
         -countprop5)

#always remember to save, replace as appropriate
write.csv(employment_reordered, "Employment.csv", row.names = FALSE)
