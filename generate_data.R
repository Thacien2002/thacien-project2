# This script reads the original nutrition_data.csv,
# expands it to a desired number of rows, and saves it back.

library(readr)
library(dplyr)
library(here)

# --- Configuration ---
desired_rows <- 6000
input_file <- here("thacien project", "R project", "project", "data", "nutrition_data.csv")
output_file <- here("thacien project", "R project", "project", "data", "nutrition_data.csv")

# --- Read Original Data ---
cat("Reading original data from:", input_file, "\n")
original_data <- read_csv(input_file, show_col_types = FALSE)

if (nrow(original_data) > 0) {
  # --- Expand Data ---
  cat("Original data has", nrow(original_data), "rows. Expanding to", desired_rows, "rows.\n")
  
  # Calculate how many times to replicate the data
  replication_factor <- ceiling(desired_rows / nrow(original_data))
  
  # Replicate the data
  expanded_data <- bind_rows(replicate(replication_factor, original_data, simplify = FALSE))
  
  # Trim to the desired number of rows
  expanded_data <- expanded_data %>% slice(1:desired_rows)
  
  # --- Save Expanded Data ---
  cat("Writing", nrow(expanded_data), "rows to:", output_file, "\n")
  write_csv(expanded_data, output_file)
  
  cat("✅ Data generation complete.\n")
} else {
  cat("⚠️ Original data file is empty. No new data generated.\n")
}