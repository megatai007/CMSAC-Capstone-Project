# Load necessary package
library(baseballr)
library(dplyr)

# Define start and end dates
start_date <- as.Date("2022-03-30")
end_date <- as.Date("2022-10-01")

# Generate weekly date sequences
week_starts <- seq(start_date, end_date, by = "week")
week_ends <- pmin(week_starts + 6, end_date)  # Ensure end date does not exceed season end

# Initialize an empty list to store weekly data
all_data <- list()

# Loop through each week and fetch data
for (i in seq_along(week_starts)) {
  cat("Fetching data from", week_starts[i], "to", week_ends[i], "...\n")  # Print progress
  
  # Fetch data for the given week
  weekly_data <- tryCatch(
    statcast_search(start_date = as.character(week_starts[i]), 
                    end_date = as.character(week_ends[i])),
    error = function(e) NULL  # Handle errors gracefully
  )
  
  # Append if data is available
  if (!is.null(weekly_data) && nrow(weekly_data) > 0) {
    all_data[[i]] <- weekly_data
  }
}

# Combine all weeks into one DataFrame
final_data <- bind_rows(all_data)

# Save to CSV
write.csv(final_data, "statcast_2022.csv", row.names = FALSE)

cat("Data collection complete! Saved as statcast_2022.csv\n")