# ========================================================================
# Rwanda Nutrition Dashboard Launcher (Updated Version)
# ========================================================================

cat("\n 📌 Starting Rwanda Nutrition Dashboard...\n")

# ---- Install Dependencies ----
cat("📦 Checking dependencies...\n")

# ---- Load required libraries ----
suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(config)
  library(future)
  library(readr)
  library(dplyr)
  library(sf)
  library(here)
})

# ---- Set Project Root ----
# Set the working directory to the location of THIS script.
# This is the most reliable way to ensure all relative paths work correctly.
if (require("rstudioapi") && rstudioapi::isAvailable()) {
  project_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
} else {
  project_dir <- getwd() # Fallback for non-RStudio environments
}
setwd(project_dir)
cat(paste0("✅ Working directory set to: ", getwd(), "\n"))

# ---- Run Dependency Installer ----
tryCatch({
  source("install_dependencies.R")
  cat("✅ Dependencies checked/installed.\n")
}, error = function(e) {
  stop("❌ FATAL ERROR: Could not run install_dependencies.R:\n", e$message)
})

# ========================================================================
# 🔹 Load Custom Theme (nisr_theme.R)
# ========================================================================
theme_path <- file.path("R", "nisr_theme.R")
tryCatch({
  if (file.exists(theme_path)) {
    source(theme_path, local = TRUE) # Keep theme local to this block
    rwanda_theme <- nisr_theme # Assign the object directly
    cat("🎨 nisr_theme.R loaded.\n")
  } else {
    rwanda_theme <- NULL
    cat("⚠️ nisr_theme.R not found.\n")
  }
}, error = function(e) {
  rwanda_theme <- NULL
  cat("⚠️ Error loading custom theme:", e$message, "\n")
})

# ========================================================================
# 🔹 Load config.yml
# ========================================================================
config_path <- file.path("config", "config.yml")

if (file.exists(config_path)) {
  tryCatch({
    config <- config::get(file = config_path)
    options(config.file = config_path)
    cat("⚙️ config.yml loaded.\n")
  }, error = function(e) {
    cat("⚠️ config.yml found but error while loading.\n")
  })
} else {
  cat("⚠️ config.yml not found — using defaults.\n")
}

# ========================================================================
# 🔹 Enable Background Processing
# ========================================================================
tryCatch({
  future::plan(multisession)
  cat("💠 Background processing enabled.\n")
}, error = function(e) {
  cat("⚠️ Could not enable background processing.\n")
})

# ========================================================================
# 🔹 Pre-load Data into a Shared Environment
# ========================================================================

cat("💾 Pre-loading data...\n")

# Create an environment that will be passed to the Shiny app
app_env <- new.env()

# ========================================================================
# 🚀 Run Shiny App
# ========================================================================
app_path <- getwd() # The app is in the current working directory

cat("🚀 Launching Nutrition Dashboard...\n")
shiny::runApp(appDir = app_path, launch.browser = TRUE)