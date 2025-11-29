# cluster_functions.R
# Functions for loading and querying organization clusters

library(jsonlite)

#' Load clusters and build lookup index
#' @param path Path to org_clusters.json file
#' @return List with clusters data and lookup index
load_clusters <- function(path = "org_clusters.json") {
  data <- fromJSON(path, simplifyVector = FALSE)

  # Build flat index for O(1) lookups
  index <- new.env(hash = TRUE)

  for (cluster in data$clusters) {
    # Skip empty canonical names
    if (is.null(cluster$canonical) || cluster$canonical == "") next

    # Index the canonical name
    index[[cluster$canonical]] <- list(
      canonical = cluster$canonical,
      path = list(),
      cluster = cluster
    )

    # Recursively index all children
    index_children(cluster$children, cluster$canonical, list(), index)
  }

  list(clusters = data$clusters, index = index)
}

#' Recursively index children in the tree
#' @param children List of child nodes
#' @param canonical The canonical (root) name for this cluster
#' @param parent_path Path from root to current node
#' @param index The index environment to populate
index_children <- function(children, canonical, parent_path, index) {
  if (is.null(children) || length(children) == 0) return()

  for (child in children) {
    if (is.null(child$name) || child$name == "") next

    current_path <- c(parent_path, list(child))
    index[[child$name]] <- list(
      canonical = canonical,
      path = current_path
    )

    # Recurse into grandchildren
    if (!is.null(child$children)) {
      index_children(child$children, canonical, current_path, index)
    }
  }
}

#' Look up any organization name
#' @param name The organization name to look up
#' @param data The loaded cluster data (from load_clusters)
#' @return List with lookup results or NULL if not found
lookup_org <- function(name, data) {
  entry <- data$index[[name]]
  if (is.null(entry)) return(NULL)

  list(
    found = TRUE,
    input = name,
    canonical = entry$canonical,
    path = entry$path,
    has_lineage = length(entry$path) > 0 &&
      any(sapply(entry$path, function(p) p$relationship %in% c("name_change", "merge", "split"))),
    narrative = build_narrative(name, entry)
  )
}

#' Build human-readable narrative from path
#' @param name The input name that was looked up
#' @param entry The index entry for this name
#' @return Character string describing the relationship
build_narrative <- function(name, entry) {
  if (length(entry$path) == 0) {
    return("")  # It's the canonical name, no narrative needed
  }

  # Build narrative by walking from the input name back to canonical
  parts <- c()
  for (i in seq_along(entry$path)) {
    node <- entry$path[[i]]
    rel <- node$relationship

    part <- switch(rel,
      "alternate_spelling" = paste0(node$name, " is an alternate spelling"),
      "name_change" = paste0(node$name, " was a former name (changed ", node$date, ")"),
      "merge" = paste0(node$name, " merged in ", node$date),
      "split" = paste0(node$name, " split off in ", node$date),
      paste0(node$name, " (", rel, ")")
    )
    parts <- c(parts, part)
  }

  # Return the narrative, building from leaf to root
  paste(rev(parts), collapse = " -> ")
}

#' Get all names in a cluster (for a given canonical org)
#' @param canonical The canonical organization name
#' @param data The loaded cluster data
#' @return Character vector of all names in this cluster
get_cluster_names <- function(canonical, data) {
  entry <- data$index[[canonical]]
  if (is.null(entry) || is.null(entry$cluster)) return(canonical)

  names <- canonical
  collect_names <- function(children) {
    if (is.null(children) || length(children) == 0) return()
    for (child in children) {
      names <<- c(names, child$name)
      if (!is.null(child$children)) {
        collect_names(child$children)
      }
    }
  }

  collect_names(entry$cluster$children)
  names
}
