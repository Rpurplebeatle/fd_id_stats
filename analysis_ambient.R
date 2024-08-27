# Load necessary libraries
#install.packages(c("readxl", "dplyr", "ggplot2"))
library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(multcompView)
library(dunn.test)
library(here)  # Add the here package

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

# Count the number of 'Insecta' for each Ambient based on the selection
data <- data %>%
  filter(Device_type %in% selected_device)

# Generate dynamic output file names
output_prefix <- tools::file_path_sans_ext(basename(input_file))
output_sufix <- if (device_type_selection == 3) {
  "fd_id"
} else {
  (selected_device)
}
results_dir <- "results"
output_dir <- file.path(results_dir, output_prefix, output_sufix)


# Create the results directory if it doesn't exist
if (!dir.exists(results_dir)) {
  dir.create(results_dir)
}

# Create the output directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}



output_file_counts <- file.path(output_dir, paste0(output_prefix, "_insecta_counts.csv"))
output_file_devices_insecta_plot <- file.path(output_dir, paste0(output_prefix, "_devices_insecta_plot.png"))
output_file_devices_syrphoidea_plot <- file.path(output_dir, paste0(output_prefix, "_devices_syrphoidea_plot.png"))
output_file_chi_square <- file.path(output_dir, paste0(output_prefix, "_chi_square.csv"))
output_file_box_plot <- file.path(output_dir, paste0(output_prefix, "_insecta_box_plot.png"))

#######

# Filter the data for Order 'Insecta'
insecta_data <- data %>%
  filter(Class == 'Insecta')

# Convert DateTime column to POSIXct format
insecta_data <- insecta_data %>%
  mutate(DateTime = ymd_hms(DateTime))

#######

# Create a dataframe with the counts for each combination of Device_type and Ambient
insecta_counts_device_type <- as.data.frame(table(insecta_data$Device_type, insecta_data$Ambient))
names(insecta_counts_device_type) <- c("Device_type", "Ambient", "Count")

# Create a contingency table for 'Insecta'
insecta_table <- table(insecta_data$Device_type, insecta_data$Ambient)
print(insecta_table)

# Perform Chi-square test of independence for 'Insecta'
insecta_test_result <- chisq.test(insecta_table)

# Print the result for 'Insecta'
print(insecta_test_result)

# Filter the data for Superfamily == 'Syrphoidea'
syrphoidea_data <- data %>%
  filter(Superfamily == 'Syrphoidea')

# Create a bar plot
plot1_chi <- ggplot(insecta_counts_device_type, aes(x = Device_type, y = Count, fill = Ambient)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Device Type", y = "Insecta Count", fill = "Ambient") +
  theme_minimal() +
  scale_fill_manual(values = c("Maize" = "gold", "Meadow" = "yellowgreen"))+
  theme(plot.title = element_text(hjust = 0.5)) +  # Center the title
  ggtitle(str_wrap(paste("Comparison of", selected_device_name, "Insecta Counts by Ambient")))

print(plot1_chi)

# Save the plot as an image
ggsave(output_file_devices_insecta_plot, plot = plot1_chi, width = 8, height = 6)

#######

# Create a dataframe with the Syrphoidea counts for each combination of Device_type and Ambient
syrphoidea_counts <- as.data.frame(table(syrphoidea_data$Device_type, syrphoidea_data$Ambient))
names(syrphoidea_counts) <- c("Device_type", "Ambient", "Count")

# Create a contingency table for 'Syrphidae'
syrphoidea_table <- table(syrphoidea_data$Device_type, syrphoidea_data$Ambient)
print(syrphoidea_table)

# Perform Chi-square test of independence for 'Syrphidae'
syrphoidea_test_result <- chisq.test(syrphoidea_table)

# Print the result for 'Syrphidae'
print(syrphoidea_test_result)

# Create a bar plot
plot2_chi <- ggplot(syrphoidea_counts, aes(x = Device_type, y = Count, fill = Ambient)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Device Type", y = "Syrphoidea Count", fill = "Ambient") +
  theme_minimal() +
  scale_fill_manual(values = c("Maize" = "gold", "Meadow" = "yellowgreen"))+
  theme(plot.title = element_text(hjust = 0.5)) +  # Center the title
  ggtitle(str_wrap(paste("Comparison of", selected_device_name, "Superfamily 'Syrphoidea' Counts by Ambient")))

print(plot2_chi)

# Save the plot as an image
ggsave(output_file_devices_syrphoidea_plot, plot = plot2_chi, width = 8, height = 6)

#######

# Save the Chi-square test results to a CSV file with significance asterisks
chi_square_results <- data.frame(
  Test = c("Insecta", "Syrphoidea"),
  df = c(insecta_test_result$parameter, syrphoidea_test_result$parameter),
  Statistic = c(insecta_test_result$statistic, syrphoidea_test_result$statistic),
  p_value = c(insecta_test_result$p.value, syrphoidea_test_result$p.value),
  significance = case_when(
    c(insecta_test_result$p.value, syrphoidea_test_result$p.value) < 0.001 ~ "***",
    c(insecta_test_result$p.value, syrphoidea_test_result$p.value) < 0.01 ~ "**",
    c(insecta_test_result$p.value, syrphoidea_test_result$p.value) < 0.05 ~ "*",
    TRUE ~ ""
  )
)

print(chi_square_results)

#######

# Save the resulting tables to a new CSV file
write.csv(chi_square_results, output_file_chi_square, row.names = FALSE)

#######

# Count the number of 'Insecta' for each Ambient based on the selection
insecta_counts_box <- data %>%
  filter(Device_type != 0) %>%
  group_by(DateTime, Device_type, Ambient) %>%
  summarise(Count = n(), .groups = "drop")

# Perform Kruskal-Wallis test
kruskal_result <- kruskal.test(Count ~ interaction(Device_type, Ambient), data = insecta_counts_box)

# Perform post-hoc test
posthoc_result <- dunn.test(insecta_counts_box$Count, interaction(insecta_counts_box$Device_type, insecta_counts_box$Ambient), method = "bonferroni")

# Create a data frame with the p-values
p_values <- posthoc_result$P.adjusted
names(p_values) <- posthoc_result$comparisons

# Generate the letters
letters <- multcompLetters(p_values, threshold = 0.05)$Letters

# Ensure Device_type and Ambient are factors
insecta_counts_box$Ambient <- as.factor(insecta_counts_box$Ambient)
insecta_counts_box$Device_type <- as.factor(insecta_counts_box$Device_type)

# Check the levels of Device_type and Ambient
device_levels <- levels(insecta_counts_box$Device_type)
ambient_levels <- levels(insecta_counts_box$Ambient)

# Create a data frame for the letters
letter_df <- data.frame(
  Device_type = rep(device_levels, each = length(ambient_levels)),
  Ambient = rep(ambient_levels, times = length(device_levels)),
  Letters = letters[1:(length(device_levels) * length(ambient_levels))]
)

# Create the box plot with letters and facet_wrap without facet labels
plot0_box <- ggplot(insecta_counts_box, aes(x = Device_type, y = Count, fill = Ambient)) +
  geom_boxplot() +
  labs(x = "Device Type", y = "Count", fill = "Ambient") +
  scale_fill_manual(values = c("Maize" = "gold", "Meadow" = "yellowgreen")) +
  theme_minimal() +
  ggtitle(paste("Distribution of Insecta Counts for", selected_device_name, "by Ambient\n Kruskal-Wallis p-value:", format(kruskal_result$p.value, digits = 3))) + 
  theme(plot.title = element_text(hjust = 0.5, lineheight = 1.2),  # Center the title and adjust line height
        strip.background = element_blank(),  # Remove the strip background
        strip.text.x = element_blank()) +  # Remove the strip text
  geom_text(data = letter_df, aes(x = Device_type, y = max(insecta_counts_box$Count) + 10, label = Letters), 
            vjust = -0.2, position = position_dodge(width = 0.9)) +
  facet_wrap(~ Ambient)

print(plot0_box)

# Save the plot as an image
ggsave(output_file_box_plot, plot = plot0_box, width = 8, height = 6)


cat("Results saved in:", output_dir, "\n")