library(Matrix)
library(tidyverse)
library(readxl)
library(car)
library(broom)
library(writexl)
library(multcompView)
library(agricolae)
library(lme4)
library(dplyr)
library(ggplot2)
library(nnet)
library(effects)
library(RColorBrewer)
library(splines)
library(here)


# Print the current working directory
cat("Current working directory:", here(), "\n")

# Define the directory containing the source tables
source_directory <- here("source_tables")

# Print the contents of the source directory
cat("Contents of source directory:", list.files(path = source_directory), "\n")

# List all .csv files in the source directory
input_files <- list.files(path = source_directory, pattern = "\\.csv$", full.names = TRUE)

# Check if any .csv files are found
if (length(input_files) == 0) {
  stop("No .csv files found in the source directory.")
}

# Prompt the user to select an input file
cat("Select an input file:\n")
for (i in seq_along(input_files)) {
  cat(i, ":", basename(input_files[i]), "\n")
}
file_index <- as.integer(readline(prompt = "Enter the number of the file to process: "))

# Check if the input is valid
if (is.na(file_index) || file_index < 1 || file_index > length(input_files)) {
  stop("Invalid selection. Please run the script again and enter a valid number.")
}

# Read the selected data file
input_file <- input_files[file_index]
cat("Processing file:", input_file, "\n")
data <- read.csv(input_file)

# Prompt the user to select the device type to process
cat("Select the device type to process:\n1: FAIR-Device\n2: Insect Detect\n3: FAIR-Device and Insect Detect\n")
device_type_selection <- as.integer(readline(prompt = "Enter the number of the device type to process: "))

# Check if the input is valid
if (is.na(device_type_selection) || device_type_selection < 1 || device_type_selection > 3) {
  stop("Invalid selection. Please run the script again and enter a valid number.")
}

# Set the selected device type based on the user's choice
selected_device <- if (device_type_selection == 1) {
  "FAIRD"
} else if (device_type_selection == 2) {
  "ID"
} else {
  c("FAIRD", "ID")
}

selected_device_name <- if (device_type_selection == 1) {
  "FAIR-Device"
} else if (device_type_selection == 2) {
  "Insect Detect"
} else {
  c("FAIR-Device and Insect Detect")
}

# Filter out the not selected device, but also leave the empty cells in Device_type
data <- data %>%
  filter(Device_type %in% selected_device | Device_type == "")

# Prompt the user to select the time window for analysis
cat("Select the time window to analyze:\n")
cat("1: Week 1\n")
cat("2: Week 2\n")
cat("3: Week 3\n")
cat("4: Week 1 + Week 2\n")
cat("5: Week 1 + Week 2 + Week 3\n")

time_window_selection <- as.integer(readline(prompt = "Enter the number of the desired time window: "))

# Validate user input
if (is.na(time_window_selection) || time_window_selection < 1 || time_window_selection > 5) {
  stop("Invalid selection. Please run the script again and enter a valid number.")
}

# Filter the data for Class 'Insecta'
insecta_data <- data %>%
  filter(Order != '#N/C')

selection_result <- switch(time_window_selection,
                           list(data = data %>% filter(Week == 1), output_week_sufix = "week1", titlestring_week = "- Week 1 -"),           # Week 1
                           list(data = data %>% filter(Week == 2), output_week_sufix = "week2", titlestring_week = "- Week 2 -"),           # Week 2
                           list(data = data %>% filter(Week == 3), output_week_sufix = "week3", titlestring_week = "- Week 3 -"),           # Week 3
                           list(data = data %>% filter(Week %in% c(1, 2)), output_week_sufix = "week1&2", titlestring_week = "- Week 1 and 2 -"), # Week 1 + Week 2
                           list(data = data %>% filter(Week %in% c(1, 2, 3)), output_week_sufix = "", titlestring_week = "")      # Week 1 + Week 2 + Week 3 (no suffix)
)

# Extract the filtered data and suffix from the list
insecta_data <- selection_result$data
output_week_sufix <- selection_result$output_week_sufix
titlestring_week <- selection_result$titlestring_week


# Generate dynamic output file names
output_prefix <- tools::file_path_sans_ext(basename(input_file))
output_device_sufix <- if (device_type_selection == 3) {
  "FAIRD&ID"
} else {
  (selected_device)
}
results_dir <- "results"
output_dir <- file.path(results_dir, output_prefix, output_device_sufix, output_week_sufix)


# Create the results directory if it doesn't exist
if (!dir.exists(results_dir)) {
  dir.create(results_dir)
}

# Create the output directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

output_file_temp_windsp_heatmap_plot <- file.path(output_dir, paste0(output_prefix, "_temp_windsp_heatmap.png"))
output_file_windsp_loess_plot <- file.path(output_dir, paste0(output_prefix, "_windsp_loess_plot.png"))
output_file_temp_loess_plot <- file.path(output_dir, paste0(output_prefix, "_temp_loess_plot.png"))
output_file_temp_windsp_loess_plot <- file.path(output_dir, paste0(output_prefix, "_temp_loess_windsp_plot.png"))

#######


# Convert DateTime column to POSIXct format
insecta_data <- insecta_data %>%
  mutate(DateTime = ymd_hms(DateTime))

# Create a binary variable for rain
insecta_data$Rain <- ifelse(insecta_data$PRECIPITATION > 0, 1, 0)

# Recode empty Device values to a meaningful label
insecta_data <- insecta_data %>%
  mutate(Device = if_else(Device == "", "NoInsects", Device)) %>%
  mutate(Device_type = if_else(Device_type == "", "NoInsects", Device_type))

# Then, create a column that indicates an insect detection
# It is 1 if a device is reported (and not "NoInsects"), otherwise 0.
insecta_data <- insecta_data %>%
  mutate(Detection = if_else(Device == "NoInsects", 0, 1))

# Now, summarize while preserving every weather record
insecta_counts <- insecta_data %>%
  group_by(DateTime, Day, Week, Device_type, Device, Ambient, AIRTEMP, WINDSPEED, Rain, RAD, GROUNDTEMP, RH, AIRP) %>%
  summarise(Count = sum(Detection, na.rm = TRUE), .groups = "drop")

# # Count the number of 'Insecta' for each Ambient
# insecta_counts <- insecta_data %>%
#   group_by(DateTime, Day, Week, Device, Ambient, AIRTEMP, WINDSPEED, Rain, RAD, GROUNDTEMP, RH, AIRP) %>%
#   summarise(Count = n(), .groups = "drop")

# # Determine the dataframe for each week# Determine the dataframe for each week
# insecta_counts <- insecta_counts %>%
#   filter(Week == 1)
# insecta_counts_wk2 <- insecta_counts %>%
#   filter(Week == 2)
# insecta_counts_wk3 <- insecta_counts %>%
#   filter(Week == 3)

# Convert Device_type, Ambient, and Site to factors
insecta_counts$Day <- as.factor(insecta_counts$Day)
insecta_counts$Week <- as.factor(insecta_counts$Week)
insecta_counts$Ambient <- as.factor(insecta_counts$Ambient)
insecta_counts$Device <- as.factor(insecta_counts$Device)
insecta_counts$AIRTEMP <- as.numeric(as.character(insecta_counts$AIRTEMP))
insecta_counts$WINDSPEED <- as.numeric(as.character(insecta_counts$WINDSPEED))
insecta_counts$RAD <- as.numeric(as.character(insecta_counts$RAD))
insecta_counts$GROUNDTEMP <- as.numeric(as.character(insecta_counts$GROUNDTEMP))
insecta_counts$RH <- as.numeric(as.character(insecta_counts$RH))
insecta_counts$AIRP <- as.numeric(as.character(insecta_counts$AIRP))

#######

# Summarize data
summary_data <- insecta_counts %>%
  group_by(AIRTEMP, WINDSPEED) %>%
  summarize(Count = mean(Count, na.rm = TRUE), .groups = 'drop')

insecta_counts$AIRTEMP <- as.numeric(insecta_counts$AIRTEMP)
insecta_counts$WINDSPEED <- as.numeric(insecta_counts$WINDSPEED)

# Create a 2D density plot with fixed axis limits
plot0_clim <- ggplot(summary_data, aes(x = AIRTEMP, y = WINDSPEED)) +
  stat_density_2d(aes(fill = after_stat(density)), geom = "raster", contour = FALSE) +
  scale_fill_gradientn(colors = c("midnightblue","dodgerblue4", "limegreen", "yellow")) +
  scale_x_continuous(limits = c(0, 35)) +  # Fixed temperature range
  scale_y_continuous(limits = c(0, 12.5)) +  # Fixed wind speed range
  labs(title = str_wrap(paste("2D density plot of", selected_device_name, "Insecta Counts as function of Temperature and Wind Speed", titlestring_week)),
       x = "Air Temperature",
       y = "Wind Speed") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, lineheight = 1.2))  

print(plot0_clim)

# Save the plot as an image
ggsave(output_file_temp_windsp_heatmap_plot, plot = plot0_clim, width = 8, height = 6)

#######

# Fit the LOESS model
wind_speed_loess_model <- loess(Count ~ WINDSPEED, data = insecta_counts)

# Generate predictions
wind_speed_seq <- seq(min(insecta_counts$WINDSPEED), max(insecta_counts$WINDSPEED), length.out = 100)
wind_speed_predictions <- predict(wind_speed_loess_model, newdata = data.frame(WINDSPEED = wind_speed_seq))

# Find the peak
wind_speed_peak_index <- which.max(wind_speed_predictions)
wind_speed_peak <- wind_speed_seq[wind_speed_peak_index]
wind_speed_peak_insect_count <- wind_speed_predictions[wind_speed_peak_index]

# Round the peak wind speed to one decimal place
wind_speed_peak <- round(wind_speed_peak, 1)

# Create a scatter plot with jitter, LOESS line, and peak annotation
plot1_clim <- ggplot(insecta_counts, aes(x = WINDSPEED, y = Count)) +
  geom_jitter(height = 0.45, color = "darkgray") +
  geom_smooth(method = "loess", se = TRUE, color = "cyan", fill = "paleturquoise") +
  geom_vline(xintercept = wind_speed_peak, linetype = "dashed", color = "blue") +
  annotate("text", x = wind_speed_peak, y = wind_speed_peak_insect_count, label = paste("Peak:", wind_speed_peak, "m/s"),  
           vjust = -1, color = "blue") +
  labs(title = paste("Scatter Plot of", selected_device_name, "Insecta Count vs. Wind Speed with LOESS Line\n", titlestring_week),
       x = "Wind Speed (m/s)",
       y = "Abundance") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, lineheight = 1.2))  # Center the title and adjust line height

print(plot1_clim)

# Save the plot as an image
ggsave(output_file_windsp_loess_plot, plot = plot1_clim, width = 8, height = 6)

#######

# Fit the LOESS model
air_temp_loess_model <- loess(Count ~ AIRTEMP, data = insecta_counts)

# Generate predictions
air_temp_seq <- seq(min(insecta_counts$AIRTEMP), max(insecta_counts$AIRTEMP), length.out = 100)
air_temp_predictions <- predict(air_temp_loess_model, newdata = data.frame(AIRTEMP = air_temp_seq))

# Find the peak
air_temp_peak_index <- which.max(air_temp_predictions)
air_temp_peak <- air_temp_seq[air_temp_peak_index]
air_temp_peak_insect_count <- air_temp_predictions[air_temp_peak_index]

# Round the peak wind speed to one decimal place
air_temp_peak <- round(air_temp_peak, 1)

# Create a scatter plot with jitter, LOESS line, and peak annotation
plot2_clim <- ggplot(insecta_counts, aes(x = AIRTEMP, y = Count)) +
  geom_jitter(height = 0.45, color = "darkgray") +
  geom_smooth(method = "loess", se = TRUE, color = "red", fill = "pink") +
  geom_vline(xintercept = air_temp_peak, linetype = "dashed", color = "blue") +
  annotate("text", x = air_temp_peak, y = air_temp_peak_insect_count, label = paste("Peak:", air_temp_peak, "°C"), 
           vjust = -1, color = "blue") +
  labs(title = paste("Scatter Plot of", selected_device_name, "Insecta Count vs.Temperature with LOESS Line\n", titlestring_week),
       x = "Air Temperature (°C)",
       y = "Abundance") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, lineheight = 1.2))  # Center the title and adjust line height

print(plot2_clim)

# Save the plot as an image
ggsave(output_file_temp_loess_plot, plot = plot2_clim, width = 8, height = 6)

#######


# Interaction plot for temperature and wind speed
plot3_clim <- ggplot(insecta_counts, aes(x = AIRTEMP, y = Count, color = factor(WINDSPEED > wind_speed_peak))) +
  geom_jitter(height = 0.45) +
  geom_smooth(method = "loess") +
  labs(title = paste(selected_device_name, "Insect Count vs. Air Temperature by Wind Speed\n", titlestring_week), x = "Air Temperature", y = "Abundance", color = paste("Wind Speed >", wind_speed_peak, "m/s")) +
  theme(plot.title = element_text(hjust = 0.5, lineheight = 1.2))  # Center the title and adjust line height

print(plot3_clim)

# Save the plot as an image
ggsave(output_file_temp_windsp_loess_plot, plot = plot3_clim, width = 8, height = 6)

#######

# #Interaction plot for temperature and wind speed
# ggplot(insecta_counts, aes(x = WINDSPEED, y = Count, color = factor(AIRTEMP > 20))) +
#  geom_jitter(height = 0.45) +
#  geom_smooth(method = "loess") +
#  labs(title = "Insect Count vs. Wind Speed by Air Temperature", x = "Wind Speed", y = "Abundance", color = "Air Temperature > 20 °C")

#######
 
# Plot the distribution of temperature and wind speed
ggplot(insecta_counts, aes(x = AIRTEMP)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Distribution of Air Temperature\n", titlestring_week, x = "Air Temperature", y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5, lineheight = 1.2))  # Center the title and adjust line height

ggplot(insecta_counts, aes(x = WINDSPEED)) +
  geom_histogram(binwidth = 0.5) +
  labs(title = "Distribution of Wind Speed\n", titlestring_week, x = "Wind Speed", y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5, lineheight = 1.2))  # Center the title and adjust line height


quadratic_model <- lm(Count ~ poly(AIRTEMP, 2) + WINDSPEED + Rain + RAD + GROUNDTEMP + RH + AIRP, data = insecta_counts)
summary(quadratic_model)

cat("Results saved in:", output_dir, "\n")
