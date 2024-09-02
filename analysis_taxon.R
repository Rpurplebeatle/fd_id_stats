#install.packages("ggrepel")
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
m3 <- multinom(Order ~ bs(DateTime.num, 3), data = insecta_data, maxit = 1000)

# Display color palettes and select one
#display.brewer.all()
mycols <- colorRampPalette(brewer.pal(9, "Set3"))(11)  # Create a custom palette with 10 colors

# Assign colors to Orders
order_colors <- setNames(mycols, c("Coleoptera", "Hymenoptera", "foo", "Dermaptera", "Diptera", "Hemiptera", "Lepidoptera", "Orthoptera", "Neuroptera", "Pterygota", "#N/C"))

# Most frequent insect orders
rev(sort(colMeans(predict(m3, type = "probs"))))

# Perform ANOVA on the model
Anova(m3)

# Compute the effect of DateTime.num
eff <- Effect("DateTime.num", m3, xlevels = list(DateTime.num = seq(from = min(insecta_data$DateTime.num), to = max(insecta_data$DateTime.num), length = 200)))
eff_df <- as.data.frame(eff)
eff_df$DateTime <- as.POSIXct(eff_df$DateTime.num, origin = "1970-01-01")

# Reshape the dataframe from wide to long format
eff_long <- eff_df %>%
  pivot_longer(cols = starts_with("prob."), names_to = "variable", values_to = "prob") %>%
  mutate(variable = str_replace(variable, "prob.", ""))

# Calculate mean probabilities for ordering
mean_probs <- eff_long %>%
  group_by(variable) %>%
  summarize(mean_prob = mean(prob)) %>%
  arrange(desc(mean_prob))

# Reorder factor levels based on mean probabilities
eff_long$variable <- factor(eff_long$variable, levels = mean_probs$variable)

# Calculate the taxon with the highest mean probability
taxon_high_prob <- mean_probs$variable[1]

# Plot the probability of insect Order composition over time
plot0_taxon <- ggplot(eff_long, aes(x = DateTime, y = prob, fill = variable)) +
  geom_area(position = "stack") +
  geom_path(aes(group = variable), color = "darkgray", linewidth = 0.5, position = "stack") +  # Add dark gray contour lines
  scale_fill_manual(values = order_colors) +
  labs(title = paste("Taxonomic Composition Probability Over Time for", selected_device_name),
       x = "Date",
       y = "Probability",
       fill = NULL) +  # Remove legend title
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +  # Center the title
  scale_x_datetime(date_labels = "%d-%m-%Y", date_breaks = "1 week") +
  scale_y_continuous(sec.axis = sec_axis(~ 1 - ., name = paste("Probability (", taxon_high_prob, ")")))

print(plot0_taxon)

# Save the plot as an image
ggsave(output_file_order_prob_plot, plot = plot0_taxon, width = 8, height = 6)

#######

# Define the taxons of interest
taxons_of_interest <- c("#N/C", "Coleoptera", "Dermaptera", "Hemiptera", "Hymenoptera", "Lepidoptera", "Orthoptera", "Pterygota", "Neuroptera")


# Filter the data for Class 'Insecta'
order_data <- data %>%
  filter(Order %in% taxons_of_interest) %>%
  group_by(Order) %>%
  summarise(Count = n(), .groups = "drop")

# Calculate the total count of all specified taxons
count <- sum(order_data$Count)

# Calculate the percentage for each taxon
order_data <- order_data %>%
  mutate(Percentage = (Count / count) * 100)

# View the resulting pivot table with percentages
print(order_data)

# Create the pie chart
plot1 <- ggplot(order_data, aes(x = "", y = Percentage, fill = Order)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = paste("Order Taxon Distribution without Diptera for", selected_device_name), x = "", y = "") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5)) +  # Center the title
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = order_colors) +
  geom_text_repel(aes(label = paste0(round(Percentage, 1), "%")), position = position_stack(vjust = 0.5), box.padding = 0.5, point.padding = 0.5, segment.color = 'grey50')

# Print the pie chart
print(plot1)

# Save the pie chart as an image
ggsave(output_file_order_pie_plot, plot = plot1, width = 8, height = 6)

cat("Results saved in:", output_dir, "\n")

############

# Define the taxons of interest
taxons_of_interest2 <- c("#N/C","Diptera", "Coleoptera", "Dermaptera", "Hemiptera", "Hymenoptera", "Lepidoptera", "Orthoptera", "Pterygota", "Neuroptera")


# Filter the data for Class 'Insecta'
order_data2 <- data %>%
  filter(Order %in% taxons_of_interest2) %>%
  group_by(Order) %>%
  summarise(Count = n(), .groups = "drop")

# Calculate the total count of all specified taxons
count <- sum(order_data2$Count)

# Calculate the percentage for each taxon
order_data2 <- order_data2 %>%
  mutate(Percentage = (Count / count) * 100)

# View the resulting pivot table with percentages
print(order_data2)

# Create the pie chart
plot2 <- ggplot(order_data2, aes(x = "", y = Percentage, fill = Order)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = paste("Order Taxon Distribution for", selected_device_name), x = "", y = "") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5)) +  # Center the title
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = order_colors) +
  geom_text_repel(aes(label = paste0(round(Percentage, 1), "%")), position = position_stack(vjust = 0.5), box.padding = 0.5, point.padding = 0.5, segment.color = 'grey50')

# Print the pie chart
print(plot2)

# Save the pie chart as an image
ggsave(output_file_order_pie_plot_diptera, plot = plot1, width = 8, height = 6)

cat("Results saved in:", output_dir, "\n")

