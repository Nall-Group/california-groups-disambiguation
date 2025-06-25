# Setup script for California Groups Disambiguation Project
# Run this script to set up the R environment with required dependencies

cat("Setting up California Groups Disambiguation R Environment...\n")

# Install renv if not already installed
if (!requireNamespace("renv", quietly = TRUE)) {
    cat("Installing renv...\n")
    install.packages("renv", repos = "https://cran.r-project.org")
}

# Initialize renv if not already initialized
if (!file.exists("renv.lock")) {
    cat("Initializing renv...\n")
    renv::init()
} else {
    cat("Restoring packages from renv.lock...\n")
    renv::restore()
}

# Install required packages for the project
required_packages <- c("shiny", "tidyverse")

cat("Installing project dependencies...\n")
for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
        install.packages(pkg)
    }
}

# Create snapshot of current environment
cat("Creating snapshot of environment...\n")
renv::snapshot()

cat("Setup complete! You can now run the Shiny app in 2_webapp/\n")
cat("To run the app: shiny::runApp('2_webapp')\n")
