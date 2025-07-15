
# Install and initialize renv
#install.packages("renv")
# library(renv)
# renv::init()
# 
# # Activate the project
# renv::activate()
# 
# # Install required packages
#install.packages(c("Matrix", "tidyverse", "readxl", "car", "broom", 
#                    "writexl", "multcompView", "agricolae", "lme4", 
#                    "dplyr", "ggplot2", "nnet", "effects", 
#                    "RColorBrewer", "splines", "ggrepel", "here"))
 

# Install renv on the new computer
#install.packages("renv")
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
library(ggrepel)
library(here)

# Restore the environment
renv::restore()

# Snapshot the environment
renv::snapshot()

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


output_file_order_prob_plot <- file.path(output_dir, paste0(output_prefix, "_order_prob_plot.png"))
output_file_order_pie_plot <- file.path(output_dir, paste0(output_prefix, "_order_pie_plot.png"))
output_file_order_pie_plot_diptera <- file.path(output_dir, paste0(output_prefix, "_order_pie_plot_diptera.png"))
#output_file_x2_plot <- file.path(output_dir, paste0(output_prefix, "_x2_plot.png"))
#output_file_x3_plot <- file.path(output_dir, paste0(output_prefix, "_x3_plot.png")) 

#######

# Filter the data for Class 'Insecta'
insecta_data <- data %>%
  filter(Class == 'Insecta' & Order != '#N/C')

# Convert DateTime column to POSIXct format
insecta_data <- insecta_data %>%
  mutate(DateTime = ymd_hms(DateTime))

# Create a binary variable for rain
insecta_data$Rain <- ifelse(insecta_data$PRECIPITATION > 0, 1, 0)

# Convert DateTime column to POSIXct format
insecta_data <- insecta_data %>%
  mutate(DateTime = ymd_hms(DateTime))

# Create a binary variable for rain
insecta_data$Rain <- ifelse(insecta_data$PRECIPITATION > 0, 1, 0)

# Count the number of 'Insecta' for each Ambient
insecta_data <- insecta_data %>%
  group_by(DateTime, Day, Week, Device, Ambient,Class, Order, Suborder, Superfamily, Family, Subfamily, Genus, Epithet, AIRTEMP, WINDSPEED, Rain, RAD, GROUNDTEMP, RH, AIRP, ) %>%
  summarise(Count = n(), .groups = "drop")

#######

# Create a numeric version of DateTime
insecta_data <- insecta_data %>%
  mutate(DateTime.num = as.numeric(DateTime))

# Fit the multinomial logistic regression model
m3 <- multinom(Genus ~ bs(DateTime.num, 3), data = insecta_data, maxit = 1000)

brachycera_data <- insecta_data %>%
  filter(Suborder == 'Brachycera')
  
# Dynamically create lists of unique values
superfamily_list <- unique(brachycera_data$Superfamily)
genus_list <- unique(brachycera_data$Genus)

# View the generated lists
print(superfamily_list)
print(genus_list)

# Generate a color palette with enough colors for each unique value
superfamily_colors <- setNames(colorRampPalette(brewer.pal(9, "Set3"))(length(superfamily_list)), superfamily_list)
genus_colors <- setNames(colorRampPalette(brewer.pal(9, "Set3"))(length(genus_list)), genus_list)

# Most frequent insect orders
rev(sort(colMeans(predict(m3, type = "probs"))))

# Perform ANOVA on the model
Anova(m3)

# # Compute the effect of DateTime.num
# eff <- Effect("DateTime.num", m3, xlevels = list(DateTime.num = seq(from = min(insecta_data$DateTime.num), to = max(insecta_data$DateTime.num), length = 200)))
# eff_df <- as.data.frame(eff)
# eff_df$DateTime <- as.POSIXct(eff_df$DateTime.num, origin = "1970-01-01")
# 
# # Reshape the dataframe from wide to long format
# eff_long <- eff_df %>%
#   pivot_longer(cols = starts_with("prob."), names_to = "variable", values_to = "prob") %>%
#   mutate(variable = str_replace(variable, "prob.", ""))
# 
# # Calculate mean probabilities for ordering
# mean_probs <- eff_long %>%
#   group_by(variable) %>%
#   summarize(mean_prob = mean(prob)) %>%
#   arrange(desc(mean_prob))
# 
# # Reorder factor levels based on mean probabilities
# eff_long$variable <- factor(eff_long$variable, levels = mean_probs$variable)
# 
# # Calculate the taxon with the highest mean probability
# taxon_high_prob <- mean_probs$variable[1]
# 
# # Plot the probability of insect Genus composition over time
# plot0_taxon <- ggplot(eff_long, aes(x = DateTime, y = prob, fill = variable)) +
#   geom_area(position = "stack") +
#   geom_path(aes(group = variable), color = "darkgray", linewidth = 0.5, position = "stack") +  # Add dark gray contour lines
#   scale_fill_manual(values = genus_colors) +
#   labs(title = paste("Taxonomic Composition Probability Over Time for", selected_device_name),
#        x = "Date",
#        y = "Probability",
#        fill = NULL) +  # Remove legend title
#   theme_minimal() +
#   theme(plot.title = element_text(hjust = 0.5)) +  # Center the title
#   scale_x_datetime(date_labels = "%d-%m-%Y", date_breaks = "1 week") +
#   scale_y_continuous(sec.axis = sec_axis(~ 1 - ., name = paste("Probability (", taxon_high_prob, ")")))
# 
# print(plot0_taxon)
# 
# # Save the plot as an image
# ggsave(output_file_order_prob_plot, plot = plot0_taxon, width = 8, height = 6)

#######

# Define the taxons of interest
#taxons_of_interest <- c("Sphaerophoria", "Syrphini", "Eupeodes", "Episyrphus", "Eristalis", "Dasysyrphus", "Scaeva")

# Filter the data for Class 'Insecta' and exclude "#N/C"
genus_data <- brachycera_data %>%
  filter(Genus != '#N/C') %>%
  group_by(Genus) %>%
  summarise(Count = n(), .groups = "drop") %>%
  arrange(desc(Count)) %>%  # Sort by Count in descending order
  slice_head(n = 10)  # Keep only the top 10 genera

# Calculate the total count of the top 10 genera
count <- sum(genus_data$Count)

# Calculate the percentage for each genus and reorder the Genus factor levels by Count in descending order
genus_data <- genus_data %>%
  mutate(Percentage = (Count / count) * 100) %>%
  mutate(Genus = factor(Genus, levels = Genus[order(-Count)]))

# View the resulting pivot table with percentages
print(genus_data)

# Adjust the color palette for the reduced number of genera
genus_colors <- setNames(colorRampPalette(brewer.pal(9, "Set3"))(nrow(genus_data)), genus_data$Genus)

# Create the pie chart for the top 10 genera
plot1 <- ggplot(genus_data, aes(x = "", y = Percentage, fill = Genus)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = paste("Top 10 Genus Taxon Distribution of Brachycera for", selected_device_name), x = "", y = "") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5)) +  # Center the title
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = genus_colors) +
  geom_text_repel(aes(label = paste0(round(Percentage, 1), "%")), 
                  position = position_stack(vjust = 0.5), 
                  box.padding = 0.5, 
                  point.padding = 0.5, 
                  segment.color = 'grey50')

# Print the pie chart
print(plot1)

# Save the pie chart as an image
ggsave(output_file_order_pie_plot, plot = plot1, width = 8, height = 6)

cat("Results saved in:", output_dir, "\n")

#####

# Add a column 'Sirphids' with three conditions
brachycera_data <- brachycera_data %>%
  mutate(Sirphids = case_when(
    Family == "Syrphidae" ~ "Syrphids",         # First condition: Family is "Syrphidae"
    Family == "#N/C" ~ "#N/C",                 # Second condition: Family is "#N/C"
    TRUE ~ "non Syrphids"                      # Default condition: Any other value
  ))

# View the updated data to confirm the changes
head(brachycera_data)
     
# Calculate the distribution of the 'Sirphids' column
sirphids_distribution <- brachycera_data %>%
  group_by(Sirphids) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Percentage = (Count / sum(Count)) * 100)

# Define a custom color palette for the 'Sirphids' categories
sirphids_colors <- setNames(c("#66C2A5", "#FC8D62", "lightgrey"), 
                            c("Syrphids", "non Syrphids", "#N/C"))

# Calculate the total count
total_count <- sum(sirphids_distribution$Count)

# Modify geom_text_repel to include both count and percentage
plot2 <- ggplot(sirphids_distribution, aes(x = "", y = Percentage, fill = Sirphids)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(
    title = paste("Percentage of Syrphids in Brachycera Suborder (Total:", total_count, ")"),
    x = NULL, y = NULL
  ) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = sirphids_colors) +
  geom_text_repel(
    aes(label = paste0(round(Percentage, 1), "% (", Count, ")")),  # Add absolute count inside parentheses
    position = position_stack(vjust = 0.5), 
    box.padding = 0.5, 
    point.padding = 0.5, 
    segment.color = 'grey50'
  )

# Print the pie chart
print(plot2)

# Save the pie chart
ggsave("sirphids_pie_chart.png", plot = plot2, width = 8, height = 6)



###########
# library(tidyr)
# library(scales)
# 
# # Convert DateTime to Date
# insecta_data <- insecta_data %>%
#   mutate(Date = as.Date(DateTime))
# 
# # Calculate the count of each Genus per Date
# abundance_data <- insecta_data %>%
#   group_by(Date, Genus) %>%
#   summarize(count = n()) %>%
#   ungroup()
# 
# # Calculate the total count per Date
# total_counts <- abundance_data %>%
#   group_by(Date) %>%
#   summarize(total_count = sum(count))
# 
# # Merge total counts with abundance data
# abundance_data <- abundance_data %>%
#   left_join(total_counts, by = "Date")
# 
# # Calculate proportional abundance
# abundance_data <- abundance_data %>%
#   mutate(proportion = count / total_count)
# 
# # Ensure proportions sum to 1 for each Date
# abundance_data <- abundance_data %>%
#   group_by(Date) %>%
#   mutate(proportion = proportion / sum(proportion)) %>%
#   ungroup()
# 
# # Calculate mean abundance for each Genus
# mean_abundance <- abundance_data %>%
#   group_by(Genus) %>%
#   summarize(mean_proportion = mean(proportion)) %>%
#   arrange(desc(mean_proportion))
# 
# # Reorder factor levels based on mean abundance
# abundance_data$Genus <- factor(abundance_data$Genus, levels = mean_abundance$Genus)
# 
# # Plot the proportional abundance over time
# plot_abundance <- ggplot(abundance_data, aes(x = Date, y = proportion, fill = Genus)) +
#   geom_area(position = "stack") +
#   scale_fill_manual(values = genus_colors) +
#   labs(title = "Proportional Abundance of Insect Genuss Over Time",
#        x = "Date",
#        y = "Proportion",
#        fill = "Insect Genus") +
#   theme_minimal() +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   scale_x_date(date_labels = "%d-%m-%Y", date_breaks = "1 week") +
#   scale_y_continuous(labels = percent_format())
# 
# print(plot_abundance)


###########

cat("Results saved in:", output_dir, "\n")

