
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
library(tidyr)
library(forcats)


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


# Read the selected data file
input_file <- file.path(source_directory, "fd_id_1h_21_days.csv") 
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
data_filtered <- data %>%
  filter(Device_type %in% selected_device)

# Generate dynamic output file names
output_prefix <- tools::file_path_sans_ext(basename(input_file))
output_sufix <- if (device_type_selection == 3) {
  "FAIRD&ID"
} else {
  (selected_device)
}
results_dir <-  here("results")
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
insecta_data <- data_filtered %>%
  filter(Class == 'Insecta' & Order != '#N/C')

# Convert DateTime column to POSIXct format
insecta_data <- insecta_data %>%
  mutate(DateTime = ymd_hms(DateTime))

# Create a binary variable for rain
insecta_data$Rain <- ifelse(insecta_data$PRECIPITATION > 0, 1, 0)

# Count the number of 'Insecta' for each Ambient
insecta_data <- insecta_data %>%
  group_by(DateTime, Day, Week, Device, Device_type, Ambient, Class, Order, Suborder, Infraorder, Superfamily, Family, Subfamily, Genus, Species, AIRTEMP, WINDSPEED, Rain, RAD, GROUNDTEMP, RH, AIRP) %>%
  summarise(Count = n(), .groups = "drop")

#######

# Create a numeric version of DateTime
insecta_data <- insecta_data %>%
  mutate(DateTime.num = as.numeric(DateTime))

# Fit the multinomial logistic regression model
m3 <- multinom(Order ~ bs(DateTime.num, 3), data = insecta_data, maxit = 1000)

# Display color palettes and select one
#display.brewer.all()
mycols <- colorRampPalette(brewer.pal(11, "Set3"))(11)  # Create a custom palette with 10 colors

# Assign colors to Orders
order_colors <- setNames(mycols, c("Dermaptera", "Hymenoptera", "foo", "Diptera", "Coleoptera", "Hemiptera", "Orthoptera", "Lepidoptera", "#N/C", "Pterygota", "Neuroptera"))

print(order_colors)

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

# Add an Hour column
insecta_data <- insecta_data %>%
  mutate(Hour = hour(DateTime))

# Summarize data by Hour and Order
hourly_distribution <- insecta_data %>%
  group_by(Hour, Order) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Hour) %>%
  mutate(Probability = Count / sum(Count))

# Reorder orders by probability magnitude for stacking
hourly_distribution <- hourly_distribution %>%
  group_by(Hour) %>%
  mutate(order_rank = rank(-Probability, ties.method = "first")) %>%
  ungroup() %>%
  mutate(Order = fct_reorder(Order, order_rank))

# Define a color palette for the orders
#order_colors <- setNames(mycols, unique(hourly_distribution$Order))

# Create the plot
hourly_plot_ordered <- ggplot(hourly_distribution, aes(x = Hour, y = Probability, fill = Order)) +
  geom_area(position = "stack") +
  scale_fill_manual(values = order_colors) +
  labs(
    title = paste("Hourly Probability of Insect Orders (Stacked by Magnitude) for", selected_device_name),
    x = "Hour of the Day",
    y = "Probability",
    fill = "Insect Orders"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_x_continuous(breaks = seq(0, 24, by = 1))

# Display the plot
print(hourly_plot_ordered)

# Save the plot as an image
ggsave("hourly_probability_plot.png", plot = hourly_plot_ordered, width = 8, height = 6)

#######
##Hourly Taxa distribution##

#Define taxa counting threshold
threshold <- 5

# Extract base colors from Set3, excluding gray-like ones
safe_colors <- brewer.pal(12, "Set3")[1:8]  # Use first 8 vivid colors

#########

#Generate a color palette for Brachycera Genera
brachycera_genera_colors <- data %>%
  filter(Suborder == "Brachycera" & !is.na(Genus) & Genus != '#N/C')

# Check the Number of Unique Genera
unique_genera <- unique(brachycera_genera_colors$Genus)
length(unique_genera)

# Set the color palette
mycols_genus <- colorRampPalette(safe_colors)(length(unique_genera))
genus_colors <- setNames(mycols_genus, unique_genera)
print(genus_colors)


# Filter data for Order 'Brachycera' based on Genus information
brachycera_data_genus <- insecta_data %>%
  filter(Suborder == "Brachycera" & !is.na(Genus) & Genus != '#N/C')

# Add an Hour column
brachycera_data_genus <- brachycera_data_genus %>%
  mutate(Hour = hour(DateTime))

# Summarize data by Hour and Genus
hourly_distribution_genus <- brachycera_data_genus %>%
  group_by(Hour, Genus) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Hour) %>%
  mutate(Probability = Count / sum(Count))

# Create a complete sequence of hours
all_hours_genus <- sort(unique(hourly_distribution_genus$Hour))


# Create a data frame with all combinations of hours and genera
all_combinations_genus <- expand.grid(Hour = all_hours_genus, Genus = unique(hourly_distribution_genus$Genus))

# Merge with the original data to fill in missing combinations with zeros
expanded_data_genus <- merge(all_combinations_genus, hourly_distribution_genus, by = c("Hour", "Genus"), all.x = TRUE)
expanded_data_genus <- expanded_data_genus %>%
  mutate(across(c(Count, Probability), ~replace_na(., 0)))

# Reorder genera by probability magnitude for stacking
expanded_data_genus <- expanded_data_genus %>%
  group_by(Hour) %>%
  mutate(genus_rank = rank(-Probability, ties.method = "first")) %>%
  ungroup() %>%
  mutate(Genus = fct_reorder(Genus, genus_rank))

# Calculate total counts per Genus
genus_totals <- expanded_data_genus %>%
  group_by(Genus) %>%
  summarise(Total_Count = sum(Count), .groups = "drop")

# Identify genera that meet the threshold
valid_genera <- genus_totals %>%
  filter(Total_Count >= threshold) %>%
  pull(Genus)

# Filter the expanded_data_genus to retain only the valid genera
expanded_data_genus_filtered <- expanded_data_genus %>%
  filter(Genus %in% valid_genera)


# Plot the data
hourly_plot_genus <- ggplot(expanded_data_genus_filtered, aes(x = Hour, y = Probability, fill = Genus)) +
  geom_area(position = "identity") +
  scale_fill_manual(values = genus_colors) +
  labs(title = paste("Hourly Probability of Brachycera Genera with more than", threshold, "counts for", selected_device_name),
       x = "Hour of the Day", y = "Probability")+
  theme_minimal() +
  facet_wrap(~ Genus, scales = "free_y")

# Display the plot
print(hourly_plot_genus)

# Save the plot as an image
ggsave("hourly_probability_brachycera_genera.png", plot = hourly_plot_genus, width = 8, height = 6)


#######

#Generate a color palette for Brachycera Families
brachycera_families_colors <- data %>%
  filter(Suborder == "Brachycera" & !is.na(Family) & Family != '#N/C')

# Check the Number of Unique Families
unique_families <- unique(brachycera_families_colors$Family)
length(unique_families)

# Set the color palette
mycols_family <- colorRampPalette(safe_colors)(length(unique_families))
family_colors <- setNames(mycols_family, unique_families)
print(family_colors)

# Filter data for Order 'Brachycera' based on Family information
brachycera_data_family <- insecta_data %>%
  filter(Suborder == "Brachycera" & !is.na(Family) & Family != '#N/C')

# Add an Hour column
brachycera_data_family <- brachycera_data_family %>%
  mutate(Hour = hour(DateTime))

# Summarize data by Hour and Family
hourly_distribution_family <- brachycera_data_family %>%
  group_by(Hour, Family) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Hour) %>%
  mutate(Probability = Count / sum(Count))

# Create a complete sequence of hours
all_hours_family <- sort(unique(hourly_distribution_family$Hour))

# Create a data frame with all combinations of hours and families
all_combinations_family <- expand.grid(Hour = all_hours_family, Family = unique(hourly_distribution_family$Family))

# Merge with the original data to fill in missing combinations with zeros
expanded_data_family <- merge(all_combinations_family, hourly_distribution_family, by = c("Hour", "Family"), all.x = TRUE)
expanded_data_family <- expanded_data_family %>%
  mutate(across(c(Count, Probability), ~replace_na(., 0)))

# Reorder families by probability magnitude for stacking
expanded_data_family <- expanded_data_family %>%
  group_by(Hour) %>%
  mutate(family_rank = rank(-Probability, ties.method = "first")) %>%
  ungroup() %>%
  mutate(Family = fct_reorder(Family, family_rank))

# Calculate total counts per Family
family_totals <- expanded_data_family %>%
  group_by(Family) %>%
  summarise(Total_Count = sum(Count), .groups = "drop")

# Identify families that meet the threshold
valid_families <- family_totals %>%
  filter(Total_Count >= threshold) %>%
  pull(Family)

# Filter the expanded_data_family to retain only the valid families
expanded_data_family_filtered <- expanded_data_family %>%
  filter(Family %in% valid_families)

# Plot the data
hourly_plot_family <- ggplot(expanded_data_family_filtered, aes(x = Hour, y = Probability, fill = Family)) +
  geom_area(position = "identity") +
  scale_fill_manual(values = family_colors) +
  labs(title = paste("Hourly Probability of Brachycera Families with more than", threshold, "counts for", selected_device_name),
       x = "Hour of the Day", y = "Probability") +
  theme_minimal() +
  facet_wrap(~ Family, scales = "free_y")

# Display the plot
print(hourly_plot_family)

# Save the plot as an image
ggsave("hourly_probability_brachycera_families.png", plot = hourly_plot_family, width = 8, height = 6)


#######

#Generate a color palette for Brachycera Superfamilies
brachycera_superfam_colors <- data %>%
  filter(Suborder == "Brachycera" & !is.na(Superfamily) & Superfamily != '#N/C')

# Check the Number of Unique Superfamilies
unique_superfam <- unique(brachycera_superfam_colors$Superfamily)
length(unique_superfam)

# Set the color palette
mycols_superfamily <- colorRampPalette(safe_colors)(length(unique_superfam))
superfamily_colors <- setNames(mycols_superfamily, unique_superfam)
print(superfamily_colors)


# Filter data for Order 'Brachycera'
brachycera_data_superfam <- insecta_data %>%
  filter(Suborder == "Brachycera" & !is.na(Superfamily) & Superfamily != '#N/C')

# Add an Hour column
brachycera_data_superfam <- brachycera_data_superfam %>%
  mutate(Hour = hour(DateTime))

# Summarize data by Hour and Superfamily
hourly_distribution_superfamily <- brachycera_data_superfam %>%
  group_by(Hour, Superfamily) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Hour) %>%
  mutate(Probability = Count / sum(Count))

# Create a complete sequence of hours
all_hours_superfamily <- sort(unique(hourly_distribution_superfamily$Hour))

# Create a data frame with all combinations of hours and superfamilies
all_combinations_superfamily <- expand.grid(Hour = all_hours_superfamily, Superfamily = unique(hourly_distribution_superfamily$Superfamily))

# Merge with the original data to fill in missing combinations with zeros
expanded_data_superfam <- merge(all_combinations_superfamily, hourly_distribution_superfamily, by = c("Hour", "Superfamily"), all.x = TRUE)
expanded_data_superfam <- expanded_data_superfam %>%
  mutate(across(c(Count, Probability), ~replace_na(., 0)))

# Reorder superfamilies by probability magnitude for stacking
expanded_data_superfam <- expanded_data_superfam %>%
  group_by(Hour) %>%
  mutate(superfamily_rank = rank(-Probability, ties.method = "first")) %>%
  ungroup() %>%
  mutate(Superfamily = fct_reorder(Superfamily, superfamily_rank))

# Calculate total counts per Superfamily
superfamily_totals <- expanded_data_superfam %>%
  group_by(Superfamily) %>%
  summarise(Total_Count = sum(Count), .groups = "drop")

# Identify Superfamilies that meet the threshold
valid_superfam <- superfamily_totals %>%
  filter(Total_Count >= threshold) %>%
  pull(Superfamily)

# Filter the expanded_data_superfam to retain only the valid superfamilies
expanded_data_superfam_filtered <- expanded_data_superfam %>%
  filter(Superfamily %in% valid_superfam)

# Plot the data
hourly_plot_superfamily <- ggplot(expanded_data_superfam_filtered, aes(x = Hour, y = Probability, fill = Superfamily)) +
  geom_area(position = "identity") +
  scale_fill_manual(values = superfamily_colors) +
  labs(title = paste("Hourly Probability of Brachycera Superfamilies with more than", threshold, "counts for", selected_device_name),
       x = "Hour of the Day", y = "Probability") +
  theme_minimal() +
  facet_wrap(~ Superfamily, scales = "free_y")

# Display the plot
print(hourly_plot_superfamily)

# Save the plot as an image
ggsave("hourly_probability_brachycera_superfamilies.png", plot = hourly_plot_superfamily, width = 8, height = 6)


#######

# Define the taxons of interest
taxons_of_interest <- c("Coleoptera", "Dermaptera", "Hemiptera", "Hymenoptera", "Lepidoptera", "Orthoptera", "Pterygota", "Neuroptera")


# 1) Sum the existing Count field by Order
order_data <- insecta_data %>%
  filter(Order %in% taxons_of_interest) %>%
  group_by(Order) %>%
  summarise(Count = sum(Count, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    Percentage = Count / sum(Count) * 100,
    Order      = fct_reorder(Order, Percentage, .desc = TRUE),
    label      = paste0(sprintf("%.1f%%", Percentage), "\n(", Count, ")"),
    # Flag: TRUE para slices pequeños < umbral (e.g. 10%)
    small_slice = Percentage < 10
  )

print(order_data)

# Create the pie chart
order_pie_plot <- ggplot(order_data, aes(x = "", y = Percentage, fill = Order)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  # etiquetas con conteo y porcentaje
  geom_text_repel(
    aes(label = label),
    position      = position_stack(vjust = 0.5),
    box.padding   = 0.5,
    point.padding = 0.3,
    segment.color = "gray30",
    size          = 3
  ) +
  labs(
    title = paste("Order Taxon Distribution for", selected_device_name, "without Di"),
    x = NULL, y = NULL
  ) +
  theme_void() +
  theme(
    plot.title   = element_text(hjust = 0.5),
    legend.title = element_blank()
  ) +
  scale_fill_manual(values = c(order_colors, "Other"="gray50"))

# Print the pie chart
print(order_pie_plot)

# Save the pie chart as an image
ggsave(output_file_order_pie_plot, plot = order_pie_plot, width = 8, height = 6)

cat("Results saved in:", output_dir, "\n")

############

# Define the taxons of interest
taxons_of_interest2 <- c("Diptera", "Coleoptera", "Dermaptera", "Hemiptera", "Hymenoptera", "Lepidoptera", "Orthoptera", "Pterygota", "Neuroptera")


# 1) Sum the existing Count field by Order
order_data2 <- insecta_data %>%
  filter(Order %in% taxons_of_interest2) %>%
  group_by(Order) %>%
  summarise(Count = sum(Count, na.rm = TRUE), .groups = "drop")

# 2) Compute % of the grand total
total_global <- sum(order_data2$Count)

# 3) Re-lump tiny slices (<1%) into “Other”
order_data2 <- order_data2 %>%
  mutate(
    Order = if_else((Count / total_global * 100) < 1, "Other", Order)
  ) %>%
  group_by(Order) %>%
  summarise(Count = sum(Count), .groups = "drop")

# 4) Calculate the final percentage using the grand total
order_data2 <- order_data2 %>%
  mutate(
    Percentage = Count / total_global * 100,
    # reorder the factor levels so that the largest slice comes first
    Order = fct_reorder(Order, Percentage, .desc = TRUE)
  )

print(order_data2)

# Create the pie chart
order_pie_plot_diptera <- ggplot(order_data2, aes(x = "", y = Percentage, fill = Order)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = paste("Order Taxon Distribution for", selected_device_name), x = "", y = "") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5)) +  # Center the title
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c(order_colors, "Other" = "gray50")) +
  geom_text_repel(
    aes(label = paste0(round(Percentage, 2), "%")),
    position = position_stack(vjust = 0.5),
    box.padding = 0.5,
    point.padding = 0.5,
    segment.color = 'gray30',     # Color of the leader lines
    segment.size = 0.4,           # Thickness of the lines
    segment.curvature = 0.2,      # Optional: add curve to the lines
    segment.angle = 20,           # Angle at which the line emerges
    segment.ncp = 3               # Number of control points for curvature
  )

# Print the pie chart
print(order_pie_plot_diptera)

# Save the pie chart as an image
ggsave(output_file_order_pie_plot_diptera, plot = order_pie_plot_diptera, width = 8, height = 6)
###########


###########
write.csv(insecta_data, file.path(here("results"), "insecta_data.csv"), row.names = FALSE)


cat("Results saved in:", output_dir, "\n")

##########

# Summarize relative abundance by Ambient and Order
composition_by_ambient <- insecta_data %>%
  group_by(Ambient, Order) %>%
  summarise(Total = sum(Count), .groups = "drop") %>%
  group_by(Ambient) %>%
  mutate(Relative = Total / sum(Total))

# Plot
ggplot(composition_by_ambient, aes(x = Ambient, y = Relative, fill = Order)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = order_colors) +
  labs(title = paste("Relative Abundance of Insect Orders by Ambient for", selected_device_name),
       x = "Ambient",
       y = "Proportion",
       fill = "Order") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()

#########

# Step 1: Aggregate data per timestamp, ambient, and order
plot_data <- insecta_data %>%
  group_by(DateTime, Ambient, Order) %>%
  summarise(Count = sum(Count), .groups = "drop") %>%
  group_by(DateTime, Ambient) %>%
  mutate(Proportion = Count / sum(Count))  # Proportional stacking

# Step 2: Plot stacked proportions with facets
ggplot(plot_data, aes(x = DateTime, y = Proportion, fill = Order)) +
  geom_area(position = "stack") +
  facet_wrap(~ Ambient, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = order_colors) +
  labs(title = paste("Temporal Taxonomic Composition per Ambient for", selected_device_name),
       x = "Date",
       y = "Proportion",
       fill = "Order") +
  scale_y_continuous(labels = percent_format()) +
  scale_x_datetime(date_labels = "%d-%b", date_breaks = "3 days") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

