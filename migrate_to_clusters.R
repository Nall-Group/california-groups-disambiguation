# Migration script: Convert crosswalk CSV to org_clusters.json
# Run this script once to generate the initial JSON file

library(jsonlite)

# Read the crosswalk CSV
crosswalk <- read.csv("crosswalk.standardizenames.manualedits_clean.csv",
  stringsAsFactors = FALSE
)

# Get unique canonical names
canonical_names <- unique(crosswalk$editedname)

# Build clusters - one per canonical name
clusters <- lapply(canonical_names, function(canonical) {
  # Find all original names that map to this canonical name
  aliases <- crosswalk$originalname[crosswalk$editedname == canonical]

  # Filter out the canonical name itself (if it appears as an alias)
  aliases <- aliases[aliases != canonical]

  # Create children list - all are alternate_spelling initially
  children <- lapply(aliases, function(alias) {
    list(
      name = alias,
      relationship = "alternate_spelling"
    )
  })

  list(
    canonical = canonical,
    status = "active",
    children = children
  )
})

# Create the final output structure
output <- list(
  version = "1.0",
  clusters = clusters
)

# Write to JSON file
write_json(output, "2_webapp/org_clusters.json",
           pretty = TRUE,
           auto_unbox = TRUE)

cat("Migration complete!\n")
cat("Created:", length(clusters), "clusters\n")
cat("Output file: 2_webapp/org_clusters.json\n")
