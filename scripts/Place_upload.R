#remove from environment
rm(df)
#remove all
rm(list = ls())


#always remember to save, replace as appropriate
write.csv(freq_table_place_test_ordered, "sliceanddice_place.csv", row.names = FALSE)

#load
included_studies <- read.csv(file.choose (), header = TRUE)

#to remove excluded studies
included_studies <- included_studies %>%
  filter(!is.na(samplesize_report) & samplesize_report !="")

#to extract just place 
place_test <- included_studies %>%
  select(number,
         authors, 
         year,
         title,
         study_country,
         samplesize_total,
         p_geog_report,
         p_geog_categories,
         p_geog_desc,
         p_geog_prop)

#change to factors with dplyr to visualise
place_test <-  place_test %>%
  mutate_all(as.factor)

freq_table_place_test<- place_test %>%
  count(number,
        authors, 
        year,
        title,
        study_country,
        samplesize_total,
        p_geog_report,
        p_geog_categories,
        p_geog_desc,
        p_geog_prop)

#rearrange with n in second column
freq_table_place_test <- freq_table_place_test %>%
  select(number,
         authors, 
         year,
         title,
         study_country,
         samplesize_total,
         p_geog_report,
         n,
         p_geog_categories,
         p_geog_desc,
         p_geog_prop)

# Remove all instances of "or" from the 'p_geog_desc' column \\b is a word boundary so it makes sure that it is specific to just the word and not including "/", it's actually better to just substitute "/"
#freq_table_place_test<- freq_table_place_test %>%
# mutate(status = gsub("\\bor\\b", " ", p_geog_desc))

#OR THIS WORKS BETTER

freq_table_place_test <- freq_table_place_test %>%
  mutate(p_geog_desc = gsub("/", "q", p_geog_desc))

freq_table_place_test <- freq_table_place_test %>%
  mutate(p_geog_desc = gsub(" or ", " ", p_geog_desc))


#how to check max categories
# Calculate the maximum number of categories
max_categories <- max(sapply(strsplit(freq_table_place_test$p_geog_desc, " "), length))


#use dplyr and tidyr to split into separate categories
#create categories dynamically (should be cleaner)
column_names <- paste0("category", 1:max_categories)

# Print maximum number of categories and column names
print(paste("Maximum number of categories:", max_categories))
print(column_names)

# Split the 'status' column into the dynamically determined number of columns
freq_table_place_test <- freq_table_place_test %>%
  separate(p_geog_desc, into = column_names, fill = "right")

#now do the same with the proportion column
#sub "v" to split values in _prop

freq_table_place_test <- freq_table_place_test %>%
  mutate(p_geog_prop = gsub("v", " ", p_geog_prop))


#how to check max categories
# Calculate the maximum number of categories
max_categories1 <- max(sapply(strsplit(freq_table_place_test$p_geog_prop, " "), length))


#use dplyr and tidyr to split into separate categories
#create categories dynamically (should be cleaner)
column_namesprop <- paste0("prop", 1:max_categories)

# Print maximum number of categories and column names
print(paste("Maximum number of categories:", max_categories1))
print(column_namesprop)

# Split the 'status' column into the dynamically determined number of columns
freq_table_place_test <- freq_table_place_test %>%
  separate(p_geog_prop, into = column_namesprop, fill = "right")

#REORDER TABLE DYNAMICALLY
# Generate a list of patterns from "1" to "5"
patterns <- as.character(1:5)

# Initialize an empty vector to store the ordered column names
ordered_cols <- c()

# Loop through each pattern and find matching columns
for (pattern in patterns) {
  matched_cols <- names(freq_table_place_test)[str_detect(names(freq_table_place_test), paste0(pattern, "$"))]
  ordered_cols <- c(ordered_cols, matched_cols)
}

# Reorder the dataframe based on the ordered columns
freq_table_place_test_ordered <- freq_table_place_test %>%
  select(number,
         authors, 
         year,
         title,
         study_country,
         samplesize_total,
         p_geog_report,
         n,
         p_geog_categories,
         all_of(ordered_cols))

#save 
write.csv(freq_table_place_test_ordered, "sliceanddice_place.csv", row.names = FALSE)

# To reorder, recategorise and aggregate reported values
# but suggest to clear environment to make things cleaner

#remove all
rm(list = ls())

#open slice and diced and reordered csv
place_categorised <- read.csv(file.choose (), header = TRUE)

#now decide how the values should be categorised, in the case of education, I want "metropolitan", "regional"
#now create a set of conditions to replace data categories THE ORDER MATTERS
# Define the categorization function
place_category_mapping1 <- function(category1) {
  case_when(
    is.na(category1) ~ NA_character_,  # Handle NA values
    
    
    str_detect(category1, "region|rural|remote") ~ "regional",
    
    TRUE ~ "metropolitan"  # Return the original value for any other status
  )
}

#now use mutate to apply across the columns that have been identified
place_recategorised <- place_categorised %>%
  mutate(across(c(category1,
                  category2,
                  category3,
                  category4,
                  category5), 
                place_category_mapping1))

#change props back to numeric
place_recategorised <-  place_recategorised %>%
  mutate(across(contains("prop"),as.numeric))


#now to identify duplicates (+remove) and to reorder the dataframe i.e. category 1 = non-tertiary, category 2 = tertiary, category 3 = unclear
# Function to reorder and remove duplicates in a row
reorder_row <- function(category, prop) {
  reordered <- rep(NA, length(category))
  props <- rep(NA, length(prop))
  
  # Aggregate prop for "regional"
  if ("regional" %in% category) {
    reordered[1] <- "regional"
    props[1] <- sum(prop[category == "regional"], na.rm = TRUE)
  }
  
  # Aggregate prop for "metropolitan"
  if ("metropolitan" %in% category) {
    reordered[2] <- "metropolitan"
    props[2] <- sum(prop[category == "metropolitan"], na.rm = TRUE)
  }
  
  # Handle remaining categories
  remaining_categories <- setdiff(category, c("regional", "metropolitan", NA))
  remaining_props <- prop[category %in% remaining_categories]
  
  if (length(remaining_categories) > 0) {
    reordered[3:(2 + length(remaining_categories))] <- remaining_categories
    props[3:(2 + length(remaining_categories))] <- remaining_props
  }
  
  return(list(reordered = reordered, props = props))
}

# Apply the reorder function to each row
place_reordered <- place_recategorised %>% 
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


#remove extra columns
place_reordered <- place_reordered %>%
  select(-(9:14))

#save
write.csv(place_reordered, "place_reordered.csv", row.names = FALSE)