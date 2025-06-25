# Create Disambiguation Summary
# This script takes the crosswalk file and creates a summary where each
# disambiguated organization name is shown with all its original variations

library(tidyverse)

# Read the crosswalk file
crosswalk <- read_csv("crosswalk.standardizenames.manualedits_clean.csv",
    col_types = cols(.default = "c")
)

# Create the disambiguation summary
disambiguation_summary <- crosswalk %>%
    # Group by the standardized (edited) name
    group_by(editedname) %>%
    # Create a list of all original variations for each standardized name
    summarise(
        # Count how many variations exist
        num_variations = n(),
        # Get all unique original names (remove duplicates)
        variations = list(unique(originalname)),
        .groups = "drop"
    ) %>%
    # Only include organizations that have multiple variations (disambiguated)
    filter(num_variations > 1) %>%
    # Convert the list column to separate columns for easier viewing
    mutate(
        # Create separate columns for each variation (up to max variations)
        variation_1 = map_chr(variations, ~ ifelse(length(.x) >= 1, .x[1], NA_character_)),
        variation_2 = map_chr(variations, ~ ifelse(length(.x) >= 2, .x[2], NA_character_)),
        variation_3 = map_chr(variations, ~ ifelse(length(.x) >= 3, .x[3], NA_character_)),
        variation_4 = map_chr(variations, ~ ifelse(length(.x) >= 4, .x[4], NA_character_)),
        variation_5 = map_chr(variations, ~ ifelse(length(.x) >= 5, .x[5], NA_character_)),
        variation_6 = map_chr(variations, ~ ifelse(length(.x) >= 6, .x[6], NA_character_)),
        variation_7 = map_chr(variations, ~ ifelse(length(.x) >= 7, .x[7], NA_character_)),
        variation_8 = map_chr(variations, ~ ifelse(length(.x) >= 8, .x[8], NA_character_)),
    ) %>%
    # Remove the list column since we've expanded it
    select(-variations) %>%
    # Arrange by number of variations (most disambiguated first)
    arrange(desc(num_variations))

# Display summary statistics
cat("Disambiguation Summary Statistics:\n")
cat("Total standardized organization names:", nrow(crosswalk %>% distinct(editedname)), "\n")
cat("Organizations with multiple variations:", nrow(disambiguation_summary), "\n")
cat("Maximum variations for single org:", max(disambiguation_summary$num_variations), "\n")
cat(
    "Average variations per disambiguated org:",
    round(mean(disambiguation_summary$num_variations), 2), "\n\n"
)

# Show top 10 most disambiguated organizations
cat("Top 10 Most Disambiguated Organizations:\n")
print(disambiguation_summary %>%
    select(editedname, num_variations) %>%
    head(10))

# Write the full results to CSV
write_csv(disambiguation_summary, "disambiguation_summary.csv", na = "")

cat("\nFull disambiguation summary saved to: disambiguation_summary.csv\n")
cat("Columns: editedname, num_variations, variation_1, variation_2, etc.\n")
