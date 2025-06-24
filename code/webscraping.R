library(pdftools)
library(dplyr)
library(tidyr)
library(stringr)

# Load and extract text
text1 <- pdf_text("/mnt/data/nisk.pdf")
text2 <- pdf_text("/mnt/data/201720180AB2447_Senate Floor Analyses.pdf")
all_text <- c(text1, text2)

# Initialize empty list to hold results
results <- list()

# Helper function to extract info from text
extract_info <- function(text, filename) {
  # Extract year from amendment date or footer
  year <- str_extract(text, "\\b20[0-9]{2}\\b") %>% na.omit() %>% first()
  
  # Extract bill number
  bill <- str_extract(text, "AB ?[0-9]{3,4}")
  
  # Extract support and opposition sections
  support <- str_match(text, "(?s)SUPPORT:.*?OPPOSITION:")[1]
  opposition <- str_match(text, "(?s)OPPOSITION:.*?(?=(ARGUMENTS|ASSEMBLY FLOOR|Prepared by|\\*\\*\\*))")[1]
  
  # Clean and separate organizations
  support_orgs <- support %>%
    str_remove("(?s)^.*?SUPPORT:") %>%
    str_remove("(?s)OPPOSITION:.*$") %>%
    str_split("\n") %>% unlist() %>%
    str_trim() %>%
    discard(~.x == "") %>%
    unique()
  
  opposition_orgs <- opposition %>%
    str_remove("(?s)^.*?OPPOSITION:") %>%
    str_split("\n") %>% unlist() %>%
    str_trim() %>%
    discard(~.x == "") %>%
    unique()
  
  # Create data frame
  df_support <- tibble(
    year = year,
    bill = bill,
    organization = support_orgs,
    position = "Support"
  )
  
  df_opposition <- tibble(
    year = year,
    bill = bill,
    organization = opposition_orgs,
    position = "Oppose"
  )
  
  bind_rows(df_support, df_opposition)
}

# Run for both documents
results[[1]] <- extract_info(paste(text1, collapse = "\n"), "nisk.pdf")
results[[2]] <- extract_info(paste(text2, collapse = "\n"), "ab2447.pdf")

# Combine and view
final_df <- bind_rows(results)
print(final_df)
