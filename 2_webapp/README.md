# California Groups Disambiguation - Web App

A Shiny web application that provides access to the California Groups disambiguation data with a clean, professional interface.

## Setup Instructions

### Prerequisites

- R (version 4.0 or higher)
- RStudio (recommended)

### Environment Setup with renv

This project uses `renv` for dependency management to ensure reproducible environments.

#### Option 1: Automatic Setup (Recommended)

From the **project root directory**:

```r
# Run the setup script
source("setup.R")
```

#### Option 2: Manual Setup

1. **Install renv** (if not already installed):

   ```r
   install.packages("renv")
   ```

2. **Restore the project environment** (from project root):

   ```r
   renv::restore()
   ```

3. **Install required packages**:
   ```r
   install.packages(c("shiny", "tidyverse"))
   ```

### Running the Web Application

Once the environment is set up, you can run the app in several ways:

#### From R Console:

```r
# Option 1: From project root
shiny::runApp("2_webapp")

# Option 2: From within 2_webapp directory
shiny::runApp()
```

#### From RStudio:

1. Open `2_webapp/app.R`
2. Click the "Run App" button in RStudio

#### From Command Line:

```bash
# Navigate to the 2_webapp directory
cd 2_webapp
R -e "shiny::runApp()"
```

The app will start a local server (typically at `http://127.0.0.1:XXXX`) and automatically open in your browser.

## App Features

- **Clean, responsive interface** with professional styling
- **Direct link** to the California Groups spreadsheet on GitHub
- **Opens in new tab** - GitHub link opens in a separate browser tab
- **Mobile-friendly** design with hover effects

## App Structure

- `app.R` - Main Shiny application file containing UI and server logic
- Uses **tidyverse** and **shiny** packages
- Minimal server logic (static page with styled HTML link)

## Customization

The app uses custom CSS styling defined in the UI. Key style elements:

- Bootstrap-style container layout
- Blue button styling with hover effects
- Centered layout with shadow effects
- Responsive design

## Troubleshooting

### Common Issues:

1. **Package not found**: Make sure you've run the setup script from the project root
2. **Port already in use**: Shiny will automatically find an available port
3. **renv not activated**: Make sure you're running R from the project root directory

### Adding New Dependencies:

If you need to add new R packages:

1. Install the package: `install.packages("package_name")`
2. Update the lock file: `renv::snapshot()`
3. The package will be available for all users who run `renv::restore()`

## Links

- [California Groups Spreadsheet](https://github.com/Nall-Group/california-groups-disambiguation/blob/main/crosswalk.standardizenames.manualedits_clean.csv) - Main data source accessed by the app
