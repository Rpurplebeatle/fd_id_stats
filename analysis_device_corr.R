# Load necessary libraries
library(dplyr)
library(tidyr)
library(here)
library(ggplot2)

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

# Generate dynamic output file names
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

output_file_device_type_boxplot <- file.path(output_dir, paste0(output_prefix, "_device_type_boxplot.png"))
output_file_rank_corr_plot <- file.path(output_dir, paste0(output_prefix, "_rank_corr_plot.png"))

##########

# Function to calculate total insect counts for each week
calculate_insect_counts_per_week <- function(data) {
  weeks <- unique(data$Week)
  insect_counts_per_week <- list()
  
  for (week in weeks) {
    # Filter data for the specific week
    data_week <- data %>% filter(Week == week)
    
    # Group by DateTime and Device to count the number of detections per day for each device type
    data_grouped <- data_week %>%
      group_by(DateTime, Device) %>%
      summarise(insect_count = n(), .groups = 'drop') %>%
      spread(key = Device, value = insect_count, fill = 0)
    
    # Calculate total insect counts for the week
    total_insect_counts_week <- sum(data_grouped %>% select(-DateTime))
    
    # Store the total insect counts for the week
    insect_counts_per_week[[as.character(week)]] <- total_insect_counts_week
  }
  
  return(insect_counts_per_week)
}

# Run the function on your data
insect_counts_per_week <- calculate_insect_counts_per_week(data)

# Print the total insect counts for each week
print("Total Insect Counts Per Week:")
for (week in names(insect_counts_per_week)) {
  print(paste("Week", week, ":", insect_counts_per_week[[week]]))
}


# Aggregate data by date and device type
data_summary <- data %>%
  group_by(date = as.Date(DateTime), Device_type) %>%
  summarise(Count = n())

# Plot time series
plot_corr <- ggplot(data_summary, aes(x = date, y = Count, color = Device_type)) +
  geom_line(size = 1) +
  labs(title = "Daily Insect Abundance per Device Type",
       x = "Date",
       y = "Abundance",
       color = "Device type") +
  scale_color_manual(values = custom_colors) +
  theme_minimal()

print(plot_corr)

##########

#Summarize insect counts & compute RELATIVE abundance 
data_summary <- data %>%
  mutate(date = as.Date(DateTime)) %>%
  group_by(date, Device_type) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Device_type) %>%
  mutate(
    Count_rel = (Count - min(Count)) / (max(Count) - min(Count))
  ) %>%
  ungroup()


# Summarize climate & compute RELATIVE values 
env_summary <- data %>%
  mutate(date = as.Date(DateTime)) %>%
  group_by(date) %>%
  summarise(
    AIRTEMP      = mean(AIRTEMP,       na.rm = TRUE),
    WINDSPEED    = mean(WINDSPEED,     na.rm = TRUE),
    PRECIPITATION = mean(PRECIPITATION, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    AIRTEMP_rel       = (AIRTEMP - min(AIRTEMP)) / (max(AIRTEMP) - min(AIRTEMP)),
    WINDSPEED_rel     = (WINDSPEED - min(WINDSPEED)) / (max(WINDSPEED) - min(WINDSPEED)),
    PRECIPITATION_rel = (PRECIPITATION - min(PRECIPITATION)) / (max(PRECIPITATION) - min(PRECIPITATION))
  ) %>%
  select(date, AIRTEMP_rel, WINDSPEED_rel, PRECIPITATION_rel)


# insect  (one line per date × device)
insect_long <- data_summary %>%
  select(date, Device_type, Count_rel) %>%
  rename(
    metric = Device_type,
    value  = Count_rel
  )

# climate (one line per date × variable)
climate_long <- env_summary %>%
  pivot_longer(
    cols      = ends_with("_rel"),
    names_to  = "metric",
    values_to = "value"
  ) %>%
  # strip “_rel” suffix
  mutate(metric = sub("_rel$", "", metric))

# combined
plot_df <- bind_rows(insect_long, climate_long)


#Plot all variables on same Y-axis 
plot_combined <- ggplot() +
  # precipitation as semi‐transparent bars in the back, with width = 0.7
  geom_col(
    data  = filter(plot_df, metric == "PRECIPITATION"),
    aes(x = date, y = value),
    fill  = custom_colors["PRECIPITATION"],
    alpha = 0.5,
    width = 0.7
  ) +
  
  # temperature line at size = 0.7
  geom_line(
    data = filter(plot_df, metric == "AIRTEMP"),
    aes(x = date, y = value, color = metric),
    linetype = "dashed",
    size     = 0.8
  ) +
  geom_line(
    data = filter(plot_df, metric == "WINDSPEED"),
    aes(x = date, y = value, color = metric),
    linetype = "dashed",
    size     = 0.8
  ) +
  
  # insect & wind lines at size = 1
  geom_line(
    data = filter(plot_df, metric %in% selected_device),
    aes(x = date, y = value, color = metric),
    size = 1
  ) +
  
  scale_color_manual(
    name   = "Metric",
    values = custom_colors,
    breaks = c(selected_device, "AIRTEMP", "WINDSPEED", "PRECIPITATION")
  ) +
  
  labs(
    title = "Relative Insect Abundance and Climatic Conditions",
    x     = "Date",
    y     = "Relative values"
  ) +
  
  theme_minimal()

print(plot_combined)


######

library(scales)  # for percent formatting

# Aggregate total daily counts per Device_type
data_summary_scales <- data %>%
  group_by(date = as.Date(DateTime), Device_type) %>%
  summarise(Count = n(), .groups = "drop") %>%
  # For each device type, compute the relative daily count,
  # i.e., daily count divided by that device type's total count
  group_by(Device_type) %>%
  mutate(RelativeCount = Count / sum(Count))

plot_corr_scales <- ggplot(data_summary_scales, aes(x = date, y = RelativeCount, color = Device_type)) +
  geom_line(size = 1) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = "Relative Daily Insect Abundance per Device Type",
       x = "Date",
       y = "Relative Abundance",
       color = "Device type") +
  scale_color_manual(values = custom_colors) +
  theme_minimal()

print(plot_corr_scales)


if (device_type_selection == 3) { 
  #ANOVA test
  anova_test <- aov(Count ~ Device_type, data = data_summary)
  summary(anova_test)
  
  
  # Reshape data for correlation analysis
  data_wide <- data_summary %>%
    pivot_wider(names_from = Device_type, values_from = Count, values_fill = 0)
  
  
  #Cross-Correlation Function (CCF) helps check the lagged correlation between two time series.
  data_wide <- na.omit(data_wide)
  ccf(data_wide$ID, data_wide$FAIRD, lag.max = 30, main = "Cross-Correlation")
  
  
  # Rank comparison
  data_wide_rank <- data_summary %>%
    pivot_wider(names_from = Device_type, values_from = Count, values_fill = list(Count = 0)) %>%
    ungroup() %>%
    mutate(rank_ID = rank(ID),
           rank_FAIRD = rank(FAIRD))
  
  # Compute Pearson correlation coefficient
  absolute_corr <- cor(data_wide$FAIRD, data_wide$ID, use = "complete.obs", method = "pearson")
  print(paste("Pearson's correlation:", round(absolute_corr, 3)))
  
  # Fit a linear model to the absolute counts
  lm_absolute <- lm(ID ~ FAIRD, data = data_wide)
  abs_r2 <- summary(lm_absolute)$r.squared
  print(paste("R² for absolute counts:", round(abs_r2, 2)))
  
  
  spearman_rank_corr <- cor(data_wide_rank$rank_FAIRD, data_wide_rank$rank_ID, method = "spearman")
  print(paste("Spearman's rank correlation:", round(spearman_rank_corr, 3)))
  
  
  lm_rank <- lm(rank_ID ~ rank_FAIRD, data = data_wide_rank)
  rank_r2 <- summary(lm_rank)$r.squared
  print(paste("R² for rank counts:", round(rank_r2, 2)))
  
  
  # install.packages("ggrepel")
  library(ggrepel)
  
  rank_corr_plot <- ggplot(data_wide_rank, aes(x = rank_FAIRD, y = rank_ID)) +
    geom_point(size = 2) +
    geom_smooth(method = "lm", se = FALSE, color = "dimgrey") +
    # Add date labels. Converting date to character ensures proper display.
    geom_text_repel(aes(label = as.character(date)), size = 3) +
    annotate("text",
             x = max(data_wide_rank$rank_FAIRD) * 0.8,
             y = max(data_wide_rank$rank_ID) * 0.9,
             label = paste("R² =", round(rank_r2, 2)),
             size = 5,
             color = "black") +
    labs(x = "FAIRD Ranked Counts",
         y = "ID Ranked Counts",
         title = "Rank Correlation: FAIRD vs. ID Device Counts") +
    theme_minimal()
  
  print(rank_corr_plot)
  
  
  
  # Save the plot as an image
  ggsave(output_file_rank_corr_plot, plot = rank_corr_plot, width = 8, height = 6)
}





