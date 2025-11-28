# =============================
# Rwanda Nutrition Dashboard Setup
# =============================

# ----------------------------
# 1️⃣ List of Required Packages
# ----------------------------
packages <- c(
  "shiny", 
  "shinydashboard", 
  "shinydashboardPlus",  # Extended dashboard components
  "shinyWidgets",        # Extra UI widgets
  "shinyjs",             # Run JavaScript in Shiny
  "shinycssloaders",     # Loading animations
  "dplyr", 
  "ggplot2", 
  "plotly", 
  "echarts4r",
  "highcharter",
  "leaflet",             # Interactive maps
  "formattable",
  "reactable",
  "DT",                  # Interactive tables
  "wordcloud2",
  "networkD3",
  "treemap",
  "WDI", 
  "prophet", 
  "httr", 
  "jsonlite", 
  "yaml",
  "glue",                # Dynamic UI strings
  "lubridate",           # Date & time manipulation
  "corrplot",            # Correlation matrices
  "mvtnorm",
  "htmltools",
  "bslib","config",
  "shiny.i18n",
  "kableExtra",
  "rlang"
)

# ----------------------------
# 2️⃣ Function to Install Missing Packages
# ----------------------------
install_packages <- function(pkgs) {
  for (pkg in pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      message(paste("Installed:", pkg))
    } else {
      message(paste("Already installed:", pkg))
    }
    
  }
}

# Install all packages
install_packages(packages)

cat("\n✅ All required packages are ready!\n")

# ----------------------------
# 3️⃣ Async Support for Smooth UI
# ----------------------------
if (!requireNamespace("future", quietly = TRUE)) install.packages("future")
library(future)
plan(multisession)  # Enable background processing for long tasks

# ----------------------------
# 4️⃣ Optional: CSS Spinner for AI Recommendations
# ----------------------------
# Add this in your UI for a polished loading indicator
spinner_css <- "
  .spinner {
    margin: 20px auto;
    width: 50px;
    height: 50px;
    border: 6px solid #f3f3f3;
    border-top: 6px solid #3498db; /* blue */
    border-radius: 50%;
    animation: spin 1s linear infinite;
  }
  @keyframes spin {
    0% { transform: rotate(0deg); }
    100% { transform: rotate(360deg); }
  }
  .ai-message {
    text-align: center;
    font-style: italic;
    color: #555;
    margin-top: 10px;
  }
"
# Usage: tags$head(tags$style(HTML(spinner_css)))
