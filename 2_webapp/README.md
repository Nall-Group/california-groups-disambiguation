# California Groups Disambiguation - Web Apps

There are two ways to access the disambiguation tool: a public GitHub Pages app and a local R Shiny app.

## GitHub Pages App (Public)

**URL:** https://nall-group.github.io/california-groups-disambiguation/

A static HTML/JavaScript app served from the `docs/` directory. It uses pre-built chunked lookup files so it only fetches the data it needs for each query (no large file downloads).

### Features

- **Single organization lookup** - type a name, get the canonical org name and relationship
- **Batch CSV disambiguation** - upload a CSV with org names in the first column, download results with disambiguated names and match status added

### Rebuilding After Crosswalk Changes

Whenever `2_webapp/org_clusters_crosswalk.json` is updated, regenerate the chunk files:

```bash
python3 scripts/build_chunks.py
```

This reads the JSON crosswalk and generates `docs/chunks/` (a manifest plus ~1,300 chunked JSON files). Commit the updated `docs/chunks/` directory and push to deploy.

### How It Works

The build script groups all name-to-canonical mappings by prefix:
- **2-character prefix** for most buckets (e.g. `chunks/AC.json`)
- **3-character prefix** for large buckets like CA, CO, SA (e.g. `chunks/CA/CAR.json`)
- **Hash-split** for very large 3-char buckets like CAL, THE, SAN (e.g. `chunks/CA/CAL_3.json`)

A small `chunks/manifest.json` tells the JavaScript which strategy to use for each prefix. The browser fetches only the relevant chunk file for each lookup.

## Local R Shiny App

For development or when you need the full relationship narratives (lineage info like "was a former name", "merged in [date]").

### Prerequisites

- R (version 4.0 or higher)
- RStudio (recommended)

### Environment Setup with renv

This project uses `renv` for dependency management.

#### Option 1: Automatic Setup (Recommended)

From the **project root directory**:

```r
source("setup.R")
```

#### Option 2: Manual Setup

1. Install renv: `install.packages("renv")`
2. Restore the project environment: `renv::restore()`
3. Install required packages: `install.packages(c("shiny", "jsonlite"))`

### Running the App

From R console:
```r
shiny::runApp("2_webapp")
```

From RStudio: open `2_webapp/app.R` and click "Run App".

From command line:
```bash
cd 2_webapp
R -e "shiny::runApp()"
```

The app starts a local server (typically `http://127.0.0.1:XXXX`) and opens in your browser.

### App Structure

- `app.R` - Main Shiny application (UI + server logic)
- `cluster_functions.R` - JSON crosswalk loader, lookup index builder, narrative generator
- `org_clusters_crosswalk.json` - Live crosswalk data (all updates go here)

## Data Source

Both apps use `2_webapp/org_clusters_crosswalk.json` as their data source. The CSV file `crosswalk.standardizenames.manualedits_clean.csv` is the original historical source and should not be edited.

## Troubleshooting

### Local App Issues

1. **Package not found**: Run the setup script from the project root
2. **Port already in use**: Shiny will automatically find an available port
3. **renv not activated**: Run R from the project root directory

## Links

- [GitHub Pages App](https://nall-group.github.io/california-groups-disambiguation/)
- [Report Issues](https://github.com/Nall-Group/california-groups-disambiguation/issues)
