# ==============================================================================
# 🔧 TEST SETUP SCRIPT - Debug Your R Shiny App
# ==============================================================================
# Run this script to test if everything is set up correctly

cat("🔍 Testing R Shiny App Setup...\n")
cat("================================\n\n")

# Test 1: Check working directory
cat("1️⃣ Testing working directory:\n")
cat("Current directory:", getwd(), "\n")

# Test 2: Check if files exist
cat("\n2️⃣ Testing file existence:\n")
files_to_check <- c(
  "app.r",
  "R/nisr_theme.R", 
  "R/data_utils.R",
  "R/gemini_utils.R",
  "project/data/nutrition_data.csv"
)

for (file in files_to_check) {
  if (file.exists(file)) {
    cat("✅", file, "- EXISTS\n")
  } else {
    cat("❌", file, "- MISSING\n")
  }
}

# Test 3: Check if required packages are installed
cat("\n3️⃣ Testing required packages:\n")
required_packages <- c("shiny", "shinydashboard", "shinydashboardPlus", "dplyr", 
                      "plotly", "leaflet", "readr", "DT", "fresh", "bslib")

for (pkg in required_packages) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    cat("✅", pkg, "- INSTALLED\n")
  } else {
    cat("❌", pkg, "- NOT INSTALLED\n")
  }
}

# Test 4: Try to source the theme file
cat("\n4️⃣ Testing theme file:\n")
if (file.exists("R/nisr_theme.R")) {
  tryCatch({
    source("R/nisr_theme.R")
    cat("✅ nisr_theme.R - LOADED SUCCESSFULLY\n")
  }, error = function(e) {
    cat("❌ nisr_theme.R - ERROR:", e$message, "\n")
  })
} else {
  cat("❌ nisr_theme.R - FILE NOT FOUND\n")
}

# Test 5: Try to load CSV data
cat("\n5️⃣ Testing CSV data loading:\n")
if (file.exists("project/data/nutrition_data.csv")) {
  tryCatch({
    library(readr)
    data <- read_csv("project/data/nutrition_data.csv", n_max = 5)
    cat("✅ CSV file - LOADED SUCCESSFULLY\n")
    cat("📊 Sample data:\n")
    print(head(data, 3))
    cat("📋 Columns:", paste(colnames(data), collapse = ", "), "\n")
  }, error = function(e) {
    cat("❌ CSV file - ERROR:", e$message, "\n")
  })
} else {
  cat("❌ CSV file - NOT FOUND\n")
}

# Test 6: Try to source app.r
cat("\n6️⃣ Testing app copy.r syntax:\n")
if (file.exists("app copy.r")) {
  tryCatch({
    # Just check syntax without running
    parse("app copy.r")
    cat("✅ app copy.r - SYNTAX OK\n")
  }, error = function(e) {
    cat("❌ app copy.r - SYNTAX ERROR:", e$message, "\n")
  })
} else {
  cat("❌ app copy.r - FILE NOT FOUND\n")
}

cat("\n================================\n")
cat("🏁 Test completed!\n")
cat("If all tests pass, you can run: shiny::runApp('app copy.r')\n")
