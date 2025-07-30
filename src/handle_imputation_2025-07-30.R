# Function to prompt the user and handle imputation decision
handle_imputation <- function() {
  repeat {
    # Prompt the user to decide whether to impute missing data for ID3
    impute_choice <- readline(prompt = "Do you want to impute missing data for ID3? (y/n): ")
    cat("User input:", impute_choice, "\n")  # Debugging statement
    
    # Check the user's choice and proceed accordingly
    if (tolower(impute_choice) == "y") {
      cat("Imputation chosen.\n")  # Debugging statement
      
      
      # Count the number of 'Insecta' for each Ambient based on the selection
      data <<- data %>%
        filter(Device_type %in% selected_device)
      
      # Convert DateTime column to POSIXct format
      insecta_data <<- data %>%
        mutate(DateTime = ymd_hms(DateTime))
      
      # Create a binary variable for rain
      insecta_data$Rain <<- ifelse(insecta_data$PRECIPITATION > 0, 1, 0)
      
      # Count the number of 'Insecta' for each Ambient
      insecta_counts <<- insecta_data %>%
        group_by(DateTime, Day, Week, Device, Site, Device_type, Ambient, AIRTEMP, WINDSPEED, Rain, RAD, GROUNDTEMP, RH, AIRP) %>%
        summarise(Count = n(), .groups = "drop")
      
      # Duplicate ID4 rows and change Device to ID3
      id3_rows <<- insecta_counts %>%
        filter(Device == "ID4") %>%
        mutate(Device = "ID3",Site="Site3")
      
      # Append the new ID3 rows to insecta_counts
      insecta_counts <<- bind_rows(insecta_counts, id3_rows)
      
      # Calculate the mean count for devices in the Meadow and round the mean to the nearest integer
      mean_meadow <<- round(mean(insecta_counts$Count[insecta_counts$Ambient == "Meadow" & insecta_counts$Device != "ID3"], na.rm = TRUE))
      cat("Mean Meadow Count:", mean_meadow, "\n")  # Debugging statement
      
      # Impute the missing values for ID3
      insecta_counts$Count[insecta_counts$Device == "ID3"] <<- mean_meadow
      cat("Imputed counts for ID3:", insecta_counts$Count[insecta_counts$Device == "ID3"], "\n")  # Debugging statement
      
      
      
      # Generate dynamic output file names
      output_prefix <<- tools::file_path_sans_ext(basename(input_file))
      results_dir <<- "results"
      output_dir <<- file.path(results_dir, output_prefix, "_id_imputed")
      output_sufix <<- if (device_type_selection == 3) {
        "FAIRD&ID"
      } else {
        selected_device
      }
      
      cat("Imputation completed.\n")
      break
      
    } else if (tolower(impute_choice) == "n") {
      cat("No imputation chosen.\n")  # Debugging statement
      
      return(FALSE)  # Indicate that we should go to the parent else

    } else {
      cat("Error, invalid input. Please enter 'y' or 'n'.\n")
    }
  }
  return(TRUE)
}
