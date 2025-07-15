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

source(here("src/custom_colors.R"))

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
  "FAIRD&ID"
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


output_file_devices_insecta_plot <- file.path(output_dir, paste0(output_prefix, "_devices_insecta_plot.png"))
output_file_devices_syrphoidea_plot <- file.path(output_dir, paste0(output_prefix, "_devices_syrphoidea_plot.png"))
output_file_devices_insecta_tukey_plot <- file.path(output_dir, paste0(output_prefix, "_devices_insecta_tukey_plot.png"))
output_file_devices_insecta_box_plot <- file.path(output_dir, paste0(output_prefix, "_devices_insecta_box_plot.png"))

#######

# Filter the data for Order 'Insecta'
insecta_data <- data %>%
  filter(Class == 'Insecta')

# Convert DateTime column to POSIXct format
insecta_data <- insecta_data %>%
  mutate(DateTime = ymd_hms(DateTime))

#######

# Calculate the count for each observation
insecta_counts_device_type <- insecta_data %>%
  group_by(DateTime, Ambient, Device_type) %>%
  summarise(Count = n()) %>%
  ungroup()

# Create a table with factors, mean, standard deviation, and total count
dt_ambients <- insecta_counts_device_type %>%
  group_by(Ambient, Device_type) %>%
  summarise(mean_count = mean(Count), sd_count = sd(Count), total_count = sum(Count)) %>%
  mutate(Device_Ambient = paste(Device_type, Ambient, sep = ":")) %>%
  arrange(desc(mean_count))

# Create a bar plot
plot1_ambients <- ggplot(dt_ambients, aes(x = Device_type, y = total_count, fill = Device_Ambient)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = total_count - sd_count, ymax = total_count + sd_count), width = 0.2, position = position_dodge(0.9)) +
  geom_text(aes(label = total_count, y = total_count + sd_count), vjust = -0.5, position = position_dodge(0.9)) +
  labs(x = "Device Type", y = "Abundance", fill = "Ambient") +
  theme_minimal() +
  scale_fill_manual(values = custom_colors) +
  theme(plot.title = element_text(hjust = 0.5)) +  # Center the title
  ggtitle(str_wrap(paste("Comparison of", selected_device_name, "Insecta Counts by Ambient")))

print(plot1_ambients)

# Save the plot as an image
ggsave(output_file_devices_insecta_plot, plot = plot1_ambients, width = 8, height = 6)


#######

# Calculate the count for each observation
syrphoidea_counts_device_type <- insecta_data %>%
  filter(Superfamily == 'Syrphoidea') %>%
  group_by(DateTime, Ambient, Device_type) %>%
  summarise(Count = n()) %>%
  ungroup()

# Create a table with factors, mean, standard deviation, and total count
dt_ambients_syrphoidea <- syrphoidea_counts_device_type %>%
  group_by(Ambient, Device_type) %>%
  summarise(mean_count = mean(Count), sd_count = sd(Count), total_count = sum(Count)) %>%
  mutate(Device_Ambient = paste(Device_type, Ambient, sep = ":")) %>%
  arrange(desc(mean_count))

# Create a bar plot
plot2_ambients <- ggplot(dt_ambients_syrphoidea, aes(x = Device_type, y = total_count, fill = Device_Ambient)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = total_count - sd_count, ymax = total_count + sd_count), width = 0.2, position = position_dodge(0.9)) +
  geom_text(aes(label = total_count, y = total_count + sd_count), vjust = -0.5, position = position_dodge(0.9)) +
  labs(x = "Device Type", y = "Abundance", fill = "Ambient") +
  theme_minimal() +
  scale_fill_manual(values = custom_colors) +
  theme(plot.title = element_text(hjust = 0.5)) +  # Center the title
  ggtitle(str_wrap(paste("Comparison of", selected_device_name, "Syrphoidea Counts by Ambient")))

print(plot2_ambients)

# Save the plot as an image
ggsave(output_file_devices_syrphoidea_plot, plot = plot2_ambients, width = 8, height = 6)

#######

# Create a combined column for Device_type and Ambient
insecta_data <- insecta_data %>%
  mutate(Device_Ambient = paste(Device_type, Ambient, sep = ":"))

# Count the number of 'Insecta' for each combined Device_Ambient
insecta_counts_device_ambient <- insecta_data %>%
  group_by(DateTime, Day, Week, Device_Ambient) %>%
  summarise(Count = n(), .groups = "drop")

# Fit a three-way ANOVA model with interactions
anova_device_ambient <- aov(Count ~ Device_Ambient, data = insecta_counts_device_ambient)
summary(anova_device_ambient)

# Perform Tukey's HSD test for the interaction term
tukey_device_ambient <- TukeyHSD(anova_device_ambient, "Device_Ambient")
print(tukey_device_ambient)

# Create a data frame with the p-values
tukey_p_values <- tukey_device_ambient$Device_Ambient[, "p adj"]
names(tukey_p_values) <- rownames(tukey_device_ambient$Device_Ambient)
print(tukey_p_values)

# Extract the results and create a compact letter display for the interaction term
letters_tukey_device_ambient <- multcompLetters(tukey_p_values, threshold = 0.05)$Letters

# Create a table with factors, mean, standard deviation, and compact letter display
dt_device_ambient <- insecta_counts_device_ambient %>%
  group_by(Device_Ambient) %>%
  summarise(w = mean(Count), sd = sd(Count)) %>%
  arrange(desc(w))

# Convert the compact letter display to a data frame
cld_tukey_device_ambient <- data.frame(Device_Ambient = names(letters_tukey_device_ambient), 
                                       cld_tukey_device_ambient = letters_tukey_device_ambient)

# Merge the compact letter display with dt_device_ambient
dt_device_ambient <- dt_device_ambient %>%
  left_join(cld_tukey_device_ambient, by = "Device_Ambient")
            
# Extract the results and create a compact letter display for the interaction term
letters_tukey_device_ambient <- multcompLetters(tukey_p_values, threshold = 0.05)$Letters

# Extract the p-value from the ANOVA summary
anova_summary <- summary(anova_device_ambient)
p_value <- anova_summary[[1]]["Device_Ambient", "Pr(>F)"]

# Visualize the data with bar plots and letters for FAIR-D devices
plot0_device_type <- ggplot(dt_device_ambient, aes(x = Device_Ambient, y = w, fill = Device_Ambient)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_errorbar(aes(ymin = w - sd, ymax = w + sd), width = 0.2) +
  geom_text(aes(label = cld_tukey_device_ambient, y = w + sd), vjust = -0.5) +
  labs(x = "Device Type and Ambient", y = "Average Insecta Count") +
  scale_fill_manual(values = custom_colors) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, lineheight = 1.2),  # Center the title and adjust line height
        strip.background = element_blank(),  # Remove the strip background
        strip.text.x = element_blank()) +  # Remove the strip text
  theme(plot.title = element_text(hjust = 0.5)) +  # Center the title
  ggtitle(paste("Comparison of Insecta Counts for", selected_device_name, "by Ambient\nTukey HSD Letters, p-value =", round(p_value, 4)))

print(plot0_device_type)

# Save the plot as an image
ggsave(output_file_devices_insecta_tukey_plot, plot = plot0_device_type, width = 8, height = 6)


##########

# Count the number of 'Insecta' for each Ambient based on the selection
insecta_counts_box <- data %>%
  filter(Device_type != 0) %>%
  group_by(DateTime, Device_type, Ambient) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Device_Ambient = paste(Device_type, Ambient, sep = ":"))

# Perform Kruskal-Wallis test
kruskal_result <- kruskal.test(Count ~ interaction(Device_type, Ambient), data = insecta_counts_box)

# Perform post-hoc test
posthoc_result <- dunn.test(insecta_counts_box$Count, interaction(insecta_counts_box$Device_type, insecta_counts_box$Ambient), method = "bonferroni")

# Create a data frame with the p-values
p_values <- posthoc_result$P.adjusted
names(p_values) <- posthoc_result$comparisons

# Function to transform p-value names to Tukey format
transform_p_value_names <- function(p_value_names) {
  sapply(p_value_names, function(name) {
    parts <- strsplit(name, " - ")[[1]]
    parts <- gsub("\\.", ":", parts)
    paste(parts[2], parts[1], sep = "-")
  })
}

# Transform the names of p_values
names(p_values) <- transform_p_value_names(names(p_values))
print(names(p_values))

# Generate the letters
letters <- multcompLetters(p_values, threshold = 0.05)$Letters
print(letters)

# Create a data frame for the letters
letter_df <- data.frame(
  Device_Ambient = names(letters),
  Letters = letters
)

# Separate the Device_Ambient column into Device_type and Ambient
letter_df <- letter_df %>%
  separate(Device_Ambient, into = c("Device_type", "Ambient"), sep = ":") %>%
  mutate(Device_Ambient = paste(Device_type, Ambient, sep = ":"))

print(letter_df)

# Create the box plot with letters and facet_wrap without facet labels
plot0_box <- ggplot(insecta_counts_box, aes(x = Device_type, y = Count, fill = Device_Ambient)) +
  geom_boxplot() +
  labs(x = "Device Type", y = "Count", fill = "Ambient") +
  scale_fill_manual(values = custom_colors) +
  theme_minimal() +
  ggtitle(paste("Distribution of Insecta Counts for", selected_device_name, "by Ambient\n Kruskal-Wallis p-value:", format(kruskal_result$p.value, digits = 3), "- Dunn Test for Letters")) +  
  theme(plot.title = element_text(hjust = 0.5, lineheight = 1.2),  # Center the title and adjust line height
        strip.background = element_blank(),  # Remove the strip background
        strip.text.x = element_blank()) +  # Remove the strip text
  geom_text(data = letter_df, aes(x = Device_type, y = max(insecta_counts_box$Count) + 10, label = Letters), 
            vjust = -0.2, position = position_dodge(width = 0.9), fontface = "plain") +
  facet_wrap(~ Ambient)

print(plot0_box)

# Save the plot as an image
ggsave(output_file_devices_insecta_box_plot, plot = plot0_box, width = 8, height = 6)


cat("Results saved in:", output_dir, "\n")