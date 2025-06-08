#remove from environment
rm(df)
#remove all
rm(list = ls())


#always remember to save, replace as appropriate
write.csv(freq_table_sc_test_ordered, "sliceanddice_socialcapital210525.csv", row.names = FALSE)

#load
included_studies <- read.csv(file.choose (), header = TRUE)

#to remove excluded studies
included_studies <- included_studies %>%
  filter(!is.na(samplesize_report) & samplesize_report !="")

#to extract just social capital
sc_test <- included_studies %>%
  select(number,
         authors, 
         year,
         title,
         study_country,
         samplesize_total,
         sc_report,
         sc_marital_categories,
         sc_marital_desc,
         sc_marital_prop,
         sc_livingarrangement_categories,
         sc_livingarrangement_desc,
         sc_livingarrangement_prop)

#change to factors with dplyr to visualise
sc_test <-  sc_test %>%
  mutate_all(as.factor)

freq_table_sc_test<- sc_test %>%
  count(number,
        authors, 
        year,
        title,
        study_country,
        samplesize_total,
        sc_report,
        sc_marital_categories,
        sc_marital_desc,
        sc_marital_prop,
        sc_livingarrangement_categories,
        sc_livingarrangement_desc,
        sc_livingarrangement_prop)

#rearrange with n in second column
freq_table_sc_test <- freq_table_sc_test %>%
  select(number,
         authors, 
         year,
         title,
         study_country,
         sc_report,
         n,
         samplesize_total,
         sc_marital_categories,
         sc_marital_desc,
         sc_marital_prop,
         sc_livingarrangement_categories,
         sc_livingarrangement_desc,
         sc_livingarrangement_prop)

# Remove all instances of "or" from the 'sc_marital/livingarrangement_desc' column \\b is a word boundary so it makes sure that it is specific to just the word and not including "/", it's actually better to just substitute "/"
#freq_table_incometest<- freq_table_sc_test %>%
# mutate(status = gsub("\\bor\\b", " ", sc_desc))

#OR THIS WORKS BETTER

freq_table_sc_test <- freq_table_sc_test %>%
  mutate(sc_marital_desc = gsub(" or ", " ", sc_marital_desc))

#how to check max categories
# Calculate the maximum number of categories
max_categories <- max(sapply(strsplit(freq_table_sc_test$sc_marital_desc, " "), length))

#use dplyr and tidyr to split into separate categories
#create categories dynamically (should be cleaner)
column_names <- paste0("category", 1:max_categories)

# Print maximum number of categories and column names
print(paste("Maximum number of categories:", max_categories))
print(column_names)

# Split the 'status' column into the dynamically determined number of columns
freq_table_sc_test <- freq_table_sc_test %>%
  separate(sc_marital_desc, into = column_names, fill = "right")

#now do the same with the proportion column
#sub "v" to split values in _prop

freq_table_sc_test <- freq_table_sc_test %>%
  mutate(sc_marital_prop = gsub("v", " ", sc_marital_prop))


#how to check max categories
# Calculate the maximum number of categories
max_categories1 <- max(sapply(strsplit(freq_table_sc_test$sc_marital_prop, " "), length))


#use dplyr and tidyr to split into separate categories
#create categories dynamically (should be cleaner)
column_namesprop <- paste0("prop", 1:max_categories1)

# Print maximum number of categories and column names
print(paste("Maximum number of categories:", max_categories1))
print(column_namesprop)

# Split the 'status' column into the dynamically determined number of columns
freq_table_sc_test <- freq_table_sc_test %>%
  separate(sc_marital_prop, into = column_namesprop, fill = "right")


#REORDER TABLE DYNAMICALLY
# Generate a list of patterns from "1" to "5"
patterns <- as.character(1:5)

# Initialize an empty vector to store the ordered column names
ordered_cols <- c()

# Loop through each pattern and find matching columns
for (pattern in patterns) {
  matched_cols <- names(freq_table_sc_test)[str_detect(names(freq_table_sc_test), paste0(pattern, "$"))]
  ordered_cols <- c(ordered_cols, matched_cols)
}

# Reorder the dataframe based on the ordered columns
freq_table_sc_test_ordered <- freq_table_sc_test %>%
  select(number,
         authors, 
         year,
         title,
         study_country,
         sc_report,
         n,
         samplesize_total,
         sc_marital_categories,
         all_of(ordered_cols),
         sc_livingarrangement_categories,
         sc_livingarrangement_desc,
         sc_livingarrangement_prop)

#now repeat with living arrangement 
freq_table_sc_test <- freq_table_sc_test %>%
  mutate(sc_livingarrangement_desc = gsub(" or ", " ", sc_livingarrangement_desc))

max_categories2 <- max(sapply(strsplit(freq_table_sc_test$sc_livingarrangement_desc, " "), length))

column_names_living <- paste0("living", 1:max_categories2)

print(paste("Maximum number of categories:", max_categories2))

freq_table_sc_test <- freq_table_sc_test %>%
  separate(sc_livingarrangement_desc, into = column_names_living, fill = "right")


#now do the same with the proportion column
#sub "v" to split values in _prop

freq_table_sc_test <- freq_table_sc_test %>%
  mutate(sc_livingarrangement_prop = gsub("v", " ", sc_livingarrangement_prop))


#how to check max categories
# Calculate the maximum number of categories
max_categories3 <- max(sapply(strsplit(freq_table_sc_test$sc_livingarrangement_prop, " "), length))


#use dplyr and tidyr to split into separate categories
#create categories dynamically (should be cleaner)
living_prop <- paste0("living_prop", 1:max_categories3)

# Print maximum number of categories and column names
print(paste("Maximum number of categories:", max_categories3))
print(living_prop)

# Split the 'status' column into the dynamically determined number of columns
freq_table_sc_test <- freq_table_sc_test %>%
  separate(sc_livingarrangement_prop, into = living_prop, fill = "right")

#REORDER TABLE DYNAMICALLY
# Generate a list of numbers from "1" to "3"
numbers <- 1:3

# Initialize an empty vector to store the ordered column names
ordered_cols1 <- c()

# Loop through each pattern and find matching columns
for (num in numbers) {
  living_col <- paste0("living", num)
  living_prop_col <- paste0("living_prop", num)
  matched_cols <- c(living_col, living_prop_col)
  ordered_cols1 <- c(ordered_cols1, matched_cols)
}

# Reorder the dataframe based on the ordered columns
freq_table_sc_test_ordered <- freq_table_sc_test %>%
  select(number,
         authors, 
         year,
         title,
         study_country,
         sc_report,
         n,
         samplesize_total,
         sc_marital_categories,
         all_of(ordered_cols),
         sc_livingarrangement_categories,
         all_of(ordered_cols1), everything())

#to remove excluded studies i.e. "no"
freq_table_sc_test_ordered <- freq_table_sc_test_ordered %>%
  filter(!is.na(sc_report) & sc_report !="no")

#split df into two tables
#marital 
socialcapital_marital <- freq_table_sc_test_ordered %>%
  filter(!is.na(sc_marital_categories) & sc_marital_categories !="")

socialcapital_marital <- socialcapital_marital %>%
  select(-(20:26))

#living arrangement
socialcapital_living <- freq_table_sc_test_ordered %>%
  filter(!is.na(sc_livingarrangement_categories) & sc_livingarrangement_categories !="")
socialcapital_living <- socialcapital_living %>%
  select(-(9:19))



#now decide how the values should be categorised, in the case of marital, I want "married", "not married", "separated/divorced/widowed"
#now create a set of conditions to replace data categories THE ORDER MATTERS
# Define the categorization function
socialcapital_category_mapping1 <- function(category1) {
  case_when(
    is.na(category1) ~ NA_character_,  # Handle NA values
    
    
    str_detect(category1, "notm|single|unmarried|rma") ~ "not married",
    
    str_detect(category1, "married") ~ "married",
    
    TRUE ~ "separated/divorced/widowed"  # Return the original value for any other status
  )
}

#now use mutate to apply across the columns that have been identified
socialcapital_marital_recategorised <- socialcapital_marital %>%
  mutate(across(c(category1,
                  category2,
                  category3,
                  category4,
                  category5), 
                socialcapital_category_mapping1))

#change props back to numeric
socialcapital_marital_recategorised <-  socialcapital_marital_recategorised %>%
  mutate(across(contains("prop"),as.numeric))


#now to identify duplicates (+remove) and to reorder the dataframe i.e. category 1 = non-tertiary, category 2 = tertiary, category 3 = unclear
# Function to reorder and remove duplicates in a row
reorder_row <- function(category, prop) {
  reordered <- rep(NA, length(category))
  props <- rep(NA, length(prop))
  
  # Aggregate prop for "not married"
  if ("not married" %in% category) {
    reordered[1] <- "not married"
    props[1] <- sum(prop[category == "not married"], na.rm = TRUE)
  }
  
  # Aggregate prop for "married"
  if ("married" %in% category) {
    reordered[2] <- "married"
    props[2] <- sum(prop[category == "married"], na.rm = TRUE)
  }
  
  # Aggregate prop for "separated/divorced/widowed"
  if ("separated/divorced/widowed" %in% category) {
    reordered[3] <- "separated/divorced/widowed"
    props[3] <- sum(prop[category == "separated/divorced/widowed"], na.rm = TRUE)
  }
  
  # Handle remaining categories
  remaining_categories <- setdiff(category, c("not married", "married", "separated/divorced/widowed", NA, NA))
  remaining_props <- prop[category %in% remaining_categories]
  
  if (length(remaining_categories) > 0) {
    reordered[4:(3 + length(remaining_categories))] <- remaining_categories
    props[4:(3 + length(remaining_categories))] <- remaining_props
  }
  
  return(list(reordered = reordered, props = props))
}

# Apply the reorder function to each row
socialcapital_marital_reordered <- socialcapital_marital_recategorised %>% 
  rowwise() %>%
  mutate(
    result = list(reorder_row(c(category1, category2, category3, category4, category5), 
                              c(prop1, prop2, prop3, prop4, prop5))),
    # Assign the reordered values back to the original columns
    category1 = result$reordered[1], prop1 = result$props[1],
    category2 = result$reordered[2], prop2 = result$props[2],
    category3 = result$reordered[3], prop3 = result$props[3],
    category4 = result$reordered[4], prop4 = result$props[4],
    category5 = result$reordered[5], prop5 = result$props[5]
  ) %>%
  select(-result) %>%
  ungroup()

# Make sure prop1–prop4, and samplesize_total are numeric
socialcapital_marital_reordered <- socialcapital_marital_reordered %>%
  mutate(across(starts_with("prop"), ~ as.numeric(.)))

socialcapital_marital_reordered$samplesize_total <- as.numeric(socialcapital_marital_reordered$samplesize_total)

# Add counts based on props and sample size
socialcapital_marital_reordered<- socialcapital_marital_reordered %>%
  mutate(across(
    .cols = starts_with("prop"),
    .fns = ~ round((. / 100) * samplesize_total, 2),
    .names = "count{.col}"
  ))

# Reorder columns to interleave prop and count
socialcapital_marital_reordered <- socialcapital_marital_reordered %>%
  relocate(countprop1, .after = prop1) %>%
  relocate(countprop2, .after = prop2) %>%
  relocate(countprop3, .after = prop3) %>%
  relocate(countprop4, .after = prop4) %>%
  relocate(countprop5, .after = prop5)

#remove extra columns
socialcapital_marital_reordered <- socialcapital_marital_reordered %>%
  select(where(~ !all(is.na(.) | . == "")))

#save
write.csv(socialcapital_marital, "socialcapital_marital210525.csv", row.names = FALSE)
write.csv(socialcapital_marital_reordered, "socialcapital_marital_reordered210525.csv", row.names = FALSE)

#now for living arrangement
#now decide how the values should be categorised, in the case of marital, I want "living alone", "not living alone"
#now create a set of conditions to replace data categories THE ORDER MATTERS
# Define the categorization function
socialcapital_category_mapping2 <- function(living1) {
  case_when(
    is.na(living1) ~ NA_character_,  # Handle NA values
    
    living1 %in% c("livingalone") ~ "living alone",
    
    TRUE ~ "not living alone"  # otherwis all will be in this category
  )
}

#now use mutate to apply across the columns that have been identified
socialcapital_living_recategorised <- socialcapital_living %>%
  mutate(across(c(living1,
                  living2,
                  living3), 
                socialcapital_category_mapping2))

#change props back to numeric
socialcapital_living_recategorised <-  socialcapital_living_recategorised %>%
  mutate(across(contains("prop"),as.numeric))

#now to identify duplicates (+remove) and to reorder the dataframe i.e. living1 = living alone, living2 = not living alone
# Function to reorder and remove duplicates in a row
reorder_row1 <- function(living, living_prop) {
  # Remove NA values from both vectors
  non_na_indices <- !is.na(living) & !is.na(living_prop)
  living <- living[non_na_indices]
  living_prop <- living_prop[non_na_indices]
  
  reordered <- rep(NA, 3)
  props <- rep(NA, 3)
  
  # Aggregate prop for "living alone"
  if ("living alone" %in% living) {
    reordered[1] <- "living alone"
    props[1] <- sum(living_prop[living == "living alone"], na.rm = TRUE)
  }
  
  # Aggregate prop for "not living alone"
  if ("not living alone" %in% living) {
    reordered[2] <- "not living alone"
    props[2] <- sum(living_prop[living == "not living alone"], na.rm = TRUE)
  }
  
  # Handle remaining categories
  remaining_categories <- setdiff(living, c("living alone", "not living alone"))
  remaining_props <- living_prop[living %in% remaining_categories]
  
  if (length(remaining_categories) > 0) {
    reordered[3:(2 + length(remaining_categories))] <- remaining_categories
    props[3:(2 + length(remaining_categories))] <- remaining_props
  }
  
  return(list(reordered = reordered, props = props))
}

# Applying the function within mutate
socialcapital_living_reordered <- socialcapital_living_recategorised %>% 
  rowwise() %>%
  mutate(
    result = list(reorder_row1(c(living1, living2, living3), 
                               c(living_prop1, living_prop2, living_prop3))),
    living1 = result$reordered[1], living_prop1 = result$props[1],
    living2 = result$reordered[2], living_prop2 = result$props[2],
    living3 = result$reordered[3], living_prop3 = result$props[3]
  ) %>%
  select(-result) %>%
  ungroup()

# Make sure living_prop, and samplesize_total are numeric
socialcapital_living_reordered <- socialcapital_living_reordered %>%
  mutate(across(starts_with("living_prop"), ~ as.numeric(.)))

socialcapital_living_reordered$samplesize_total <- as.numeric(socialcapital_living_reordered$samplesize_total)

# Add counts based on props and sample size
socialcapital_living_reordered<- socialcapital_living_reordered %>%
  mutate(across(
    .cols = starts_with("living_prop"),
    .fns = ~ round((. / 100) * samplesize_total, 2),
    .names = "count{.col}"
  ))

# Reorder columns to interleave prop and count
socialcapital_living_reordered <- socialcapital_living_reordered %>%
  relocate(countliving_prop1, .after = living_prop1) %>%
  relocate(countliving_prop2, .after = living_prop2) 

#remove extra columns
socialcapital_living_reordered <- socialcapital_living_reordered %>%
  select(where(~ !all(is.na(.) | . == "")))


#save
write.csv(socialcapital_living, "socialcapital_living210525.csv", row.names = FALSE)
write.csv(socialcapital_living_reordered, "socialcapital_living_reordered210525.csv", row.names = FALSE)


