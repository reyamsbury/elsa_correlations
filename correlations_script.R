# Load necessary libraries
library(readr)       # For reading CSV files
library(dplyr)       # For data manipulation
library(purrr)       # For functional programming
library(tibble)      # For tibble support
library(writexl)     # For writing results to an Excel file

# Create a vector with the wave numbers (1 to 10)
wave_numbers <- 1:10

# Initialize a list to store the data
waves_data <- list()

# Loop over each wave number, build the URL, and load the data
for (wave in wave_numbers) {
  # Construct the URL for each wave
  url <- paste0("https://raw.githubusercontent.com/reyamsbury/elsa_correlations/refs/heads/main/wave", wave, ".csv")
  
  # Load the data from the URL using read_csv from readr
  wave_data <- read_csv(url)
  
  # Store the data in the list, using the wave number as the name
  waves_data[[paste0("wave", wave)]] <- wave_data
}

# Convert each wave's data to a tibble (if not already a tibble)
waves_data <- map(waves_data, as_tibble)

# List of corrected variable pairs to correlate
correlation_pairs <- list(
  c("totwq10_bu_s", "cfmetm"),
  c("totwq10_bu_s", "memtotb"),
  c("findiff", "memtotb"),
  c("findiff", "cfmetm"),
  c("findiff", "heyrc"),
  c("cfmetm", "memtotb"),
  c("dhdobyr", "memtotb"),
  c("dhdobyr", "cfmetm"),
  c("dhdobyr", "execnn"),
  c("dhdobyr", "heyrc")
)

# Function to perform correlations and significance testing for each dataset
perform_correlations <- function(data, correlation_pairs) {
  correlations <- list()
  
  # Loop over each correlation pair and calculate the correlation and p-value
  for (pair in correlation_pairs) {
    var1 <- pair[1]
    var2 <- pair[2]
    
    # Check if both variables are present in the data
    if (var1 %in% colnames(data) & var2 %in% colnames(data)) {
      # Perform the correlation test
      cor_test_result <- cor.test(data[[var1]], data[[var2]], use = "complete.obs")
      correlation <- cor_test_result$estimate  # Correlation coefficient
      p_value <- cor_test_result$p.value     # p-value for the correlation
      
      # Store the correlation coefficient and p-value
      correlations[[paste(var1, var2, sep = "_vs_")]] <- c(Correlation = correlation, P_value = p_value)
    } else {
      correlations[[paste(var1, var2, sep = "_vs_")]] <- c(Correlation = NA, P_value = NA)
    }
  }
  
  return(correlations)
}

# Create an empty list to store results
all_results <- list()

# Loop through each wave's data and perform correlations
for (wave in names(waves_data)) {
  # Retrieve the data for the current wave
  data <- waves_data[[wave]]
  
  # Perform the correlations for the current dataset
  results <- perform_correlations(data, correlation_pairs)
  
  # Store the results for the current wave
  all_results[[wave]] <- results
}

# Function to convert results to a data frame
convert_results_to_dataframe <- function(all_results) {
  result_list <- list()
  
  # Loop through all the results and prepare a data frame
  for (wave in names(all_results)) {
    results <- all_results[[wave]]
    for (correlation_name in names(results)) {
      result_list <- append(result_list, list(data.frame(
        Wave = wave,
        Pair = correlation_name,
        Correlation = results[[correlation_name]][1],
        P_value = results[[correlation_name]][2]
      )))
    }
  }
  
  # Combine all data frames into a single data frame
  result_df <- do.call(rbind, result_list)
  
  return(result_df)
}

# Convert the results into a data frame
result_df <- convert_results_to_dataframe(all_results)

# Write the data frame to an Excel file
write_xlsx(result_df, path = "correlation_results.xlsx")

getwd()

# Confirm the file has been saved
cat("Results saved to correlation_results.xlsx")
