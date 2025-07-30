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
library(here)  # Add the here package


# Use here() to define paths relative to the project root
source(here("src/reorder_rename_anova.R"))
source(here("src/add_significance.R"))
source(here("src/handle_imputation.R"))

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


#######
# Call the function to handle imputation if ID was selected
if (device_type_selection == 2 || device_type_selection == 3) {
  if (!handle_imputation()) {
    # This block executes if handle_imputation returns FALSE
    output_prefix <<- tools::file_path_sans_ext(basename(input_file))
    output_sufix <<- if (device_type_selection == 3) {
      "FAIRD&ID"
    } else {
      selected_device
    }
    
    # Count the number of 'Insecta' for each Ambient based on the selection
    data <- data %>%
      filter(Device_type %in% selected_device)
    
    # Convert DateTime column to POSIXct format
    insecta_data <- data %>%
      mutate(DateTime = ymd_hms(DateTime))
    
    # Create a binary variable for rain
    insecta_data$Rain <- ifelse(insecta_data$PRECIPITATION > 0, 1, 0)
    
    # Count the number of 'Insecta' for each Ambient; add Site:
    insecta_counts <- insecta_data %>%
      group_by(DateTime, Day, Week, Device, Site, Ambient, AIRTEMP, WINDSPEED, Rain, RAD, GROUNDTEMP, RH, AIRP) %>%
      summarise(Count = n(), .groups = "drop")
  }
} else {
  # This block executes if device_type_selection is not 2 or 3
  output_prefix <- tools::file_path_sans_ext(basename(input_file))
  output_sufix <- selected_device
  
    # Count the number of 'Insecta' for each Ambient based on the selection
  data <- data %>%
    filter(Device_type %in% selected_device)
  
  # Convert DateTime column to POSIXct format
  insecta_data <- data %>%
    mutate(DateTime = ymd_hms(DateTime))
  
  # Create a binary variable for rain
  insecta_data$Rain <- ifelse(insecta_data$PRECIPITATION > 0, 1, 0)
  
  # Count the number of 'Insecta' for each Ambient
  insecta_counts <- insecta_data %>%
    group_by(DateTime, Day, Week, Device, Site, Ambient, AIRTEMP, WINDSPEED, Rain, RAD, GROUNDTEMP, RH, AIRP) %>%
    summarise(Count = n(), .groups = "drop")
  
}
  
head(insecta_counts)

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
output_file_anova_devices_clim <- file.path(output_dir, paste0(output_prefix, "_anova_devices_clim.csv"))
output_file_anova_devices_clim_TII <- file.path(output_dir, paste0(output_prefix, "_anova_devices_clim_TII.csv"))
output_file_anova_devices <- file.path(output_dir, paste0(output_prefix, "_anova_devices.csv"))
output_file_anova_ambient_clim <- file.path(output_dir, paste0(output_prefix, "_anova_ambient_clim.csv"))
output_file_anova_ambient <- file.path(output_dir, paste0(output_prefix, "_anova_ambient.csv"))
output_file_anova_devices_wk1_clim <- file.path(output_dir, paste0(output_prefix, "_anova_devices_wk1_clim.csv"))
output_file_anova_devices_wk2_clim <- file.path(output_dir, paste0(output_prefix, "_anova_devices_wk2_clim.csv"))
output_file_anova_devices_wk3_clim <- file.path(output_dir, paste0(output_prefix, "_anova_devices_wk3_clim.csv"))
output_file_anova_weeks <- file.path(output_dir, paste0(output_prefix, "_anova_weeks.csv"))
output_file_anova_rain <- file.path(output_dir, paste0(output_prefix, "_anova_rain.csv"))
output_file_tukey_ambient <- file.path(output_dir, paste0(output_prefix, "_tukey_ambient.csv"))
output_file_tukey_devices <- file.path(output_dir, paste0(output_prefix, "_tukey_devices.csv"))
output_file_tukey_ambient <- file.path(output_dir, paste0(output_prefix, "_tukey_ambient.csv"))
output_file_tukey_weeks <- file.path(output_dir, paste0(output_prefix, "_tukey_weeks.csv"))
output_file_tukey_devices_plot <- file.path(output_dir, paste0(output_prefix, "_tukey_devices_plot.png"))
output_file_tukey_ambient_plot <- file.path(output_dir, paste0(output_prefix, "_tukey_ambient_plot.png"))
output_file_counts_weeks_plot <- file.path(output_dir, paste0(output_prefix, "_counts_weeks_plot.png"))
output_file_tukey_weeks_plot <- file.path(output_dir, paste0(output_prefix, "_tukey_weeks_plot.png"))
output_file_scatter_ambient_plot <- file.path(output_dir, paste0(output_prefix, "_scater_ambient_plot.png"))

#########


# Determine the dataframe for each week# Determine the dataframe for each week
insecta_counts_wk1 <- insecta_counts %>%
  filter(Week == 1)
insecta_counts_wk2 <- insecta_counts %>%
  filter(Week == 2)
insecta_counts_wk3 <- insecta_counts %>%
  filter(Week == 3)

# Convert Device_type, Ambient, and Site to factors
insecta_counts$Day <- as.factor(insecta_counts$Day)
insecta_counts$Week <- as.factor(insecta_counts$Week)
insecta_counts$Ambient <- as.factor(insecta_counts$Ambient)
insecta_counts$Device <- as.factor(insecta_counts$Device)
insecta_counts$Device_type=as.factor(insecta_counts$Device_type)
insecta_counts$Site <- as.factor(insecta_counts$Site)
insecta_counts$AIRTEMP <- as.numeric(as.character(insecta_counts$AIRTEMP))
insecta_counts$WINDSPEED <- as.numeric(as.character(insecta_counts$WINDSPEED))
insecta_counts$RAD <- as.numeric(as.character(insecta_counts$RAD))
insecta_counts$GROUNDTEMP <- as.numeric(as.character(insecta_counts$GROUNDTEMP))
insecta_counts$RH <- as.numeric(as.character(insecta_counts$RH))
insecta_counts$AIRP <- as.numeric(as.character(insecta_counts$AIRP))
# Convert DateTime to a factor for repeated measures
#insecta_counts$DateTime <- as.factor(insecta_counts$DateTime)

#######

# # Fit a mixed-effects model

subset(insecta_counts,Device=="ID3")

insecta_counts$Count

# check distributional assumptions:

#install.packages("fitdistrplus")
library(fitdistrplus)
f1=fitdist(insecta_counts$Count,"pois")
f2=fitdist(insecta_counts$Count,"nbinom")

denscomp(list(f1,f2))
lapply(list(f1,f2),function(x)summary(x)$aic)

#install.packages("tweedie")
library(tweedie)
t1=tweedie.profile(insecta_counts$Count~insecta_counts$Day)
# didn´t work.

library(glmmTMB)
library(lattice)

head(insecta_counts)

xyplot(Count~DateTime,groups=Device_type,insecta_counts,auto.key=T,
       pch=16,cex=2,type=c("p","smooth"),lwd=2)

# below model has wrong random effects structure
#mixed_model <- glmmTMB(Count ~ Device + (1|DateTime/Device), data = insecta_counts,
#                       family="nbinom2")

insecta_counts$Day.num=as.numeric(insecta_counts$Day)

unique(insecta_data[,2:7])
head(insecta_counts)

plot(Device_type~Site,insecta.df)
insecta_counts$Device_type
class(insecta_counts)


########################################################
# gam models####

library(mgcv)

# convert to data frame:
insecta.df=as.data.frame(insecta_counts)

# m1, a priori model only with day and device type
m1 <- gam(Count ~ Site+s(Day.num,by=Device_type,bs="ts",k=20),select=T,
          data=insecta.df,family=nb())


m2 <- gam(Count ~ Site+s(Day.num,by=Device_type,bs="ts",k=20),select=T,
          data=insecta.df,family=tw())

summary(m2)

AIC(m1)
AIC(m2) # slight improvement, but rather for positive continuous data

# for plotting these models:
library(gratia)

# exploratory plot:
xyplot(Count~DateTime,groups=Device_type,insecta_counts,auto.key=T,
       pch=16,cex=2,type=c("p","smooth"),lwd=2)

xyplot(AIRTEMP~DateTime,groups=Device_type,insecta_counts,auto.key=T,
       pch=16,cex=2,type=c("p","smooth"),lwd=2)

xyplot(WINDSPEED~DateTime,groups=Device_type,insecta_counts,auto.key=T,
       pch=16,cex=2,type=c("p","smooth"),lwd=2)

xyplot(AIRTEMP~WINDSPEED,insecta_counts,auto.key=T,
       pch=16,cex=2,type=c("p","smooth"),lwd=2)

# the below two models don´t contain the days:

w1 <- gam(Count ~ Site+s(AIRTEMP,WINDSPEED,by=Device_type,bs="ts"),select=T,
          data=insecta.df,family=nb())

# Airtemp as a normal smooth, Device type by day:
w2 <- gam(Count ~ Site+s(AIRTEMP,bs="ts")+
            s(Day.num,by=Device_type,bs="cs",k=20),select=T,
          data=insecta.df,family=nb())

w2a <- gam(Count ~ Site+s(AIRTEMP,by=Device_type,bs="ts")+
            s(Day.num,by=Device_type,bs="cs",k=20),select=T,
          data=insecta.df,family=nb())

xyplot(Count~AIRTEMP,groups=Device_type,insecta_counts,auto.key=T,
       pch=16,cex=2,type=c("p","smooth"),lwd=2)

# alternatively, put all in one big smooth term (not so elegant)
w3 <- gam(Count ~ Site+s(Day.num,AIRTEMP,by=Device_type,bs="ts",k=20),select=T,
          data=insecta.df,family=nb())


# model-based plots

# m1, the basic model without temp
draw(m1,constant = coef(m1)[1],grouped_by=T,fun=exp)

draw(w2,constant = coef(w2)[1],fun=exp,grouped_by=T)

# now with temp:
draw(w2a,constant = coef(w2)[1],fun=exp,select=1:2,grouped_by=T)

#optimal one, with temp?
draw(w2a,constant = coef(w2)[1],fun=exp,select=3:4,grouped_by=T)

# not so optimal:
draw(w3,constant = coef(w3)[1],fun=exp,grouped_by=T)

#draw(m2,constant = coef(m2)[1],grouped_by=T,fun=exp)



########################################################
# older gam models:####
########################################################


library(glmmTMB)
library(lattice)
library(mgcv)

# redefine device types:

insecta_counts$Devclass=factor(ifelse(grepl("FAIR",insecta_counts$Device),"FAIR","ID"))

head(insecta_counts)
xyplot(Count~DateTime,groups=Devclass,insecta_counts,type=c("p","smooth"),
       cex=2,pch=16,auto.key=list(columns=1))

# recode DateTime:

insecta_counts$Year=as.numeric(format(insecta_counts$DateTime,"%Y"))
insecta_counts$Month=as.numeric(format(insecta_counts$DateTime,"%m"))
insecta_counts$Day=as.numeric(format(insecta_counts$DateTime,"%d"))
insecta_counts$Hour=as.numeric(format(insecta_counts$DateTime,"%H"))
insecta_counts$Time=as.numeric(insecta_counts$DateTime)/1000

range(insecta_counts$Day)
range(insecta_counts$Month)

m1s=gam(Count~s(Month,bs="cc",k=2,by=Devclass)+s(Day,bs="cc",k=30,by=Devclass)+
         s(Time,by=Devclass),data=insecta_counts,family=nb())

m2s=gam(Count~s(Time,by=Devclass,bs="cc"),data=insecta_counts,family=nb())

# random effect still needs to be added

library(mgcViz)
#install.packages("gratia")
library(gratia)
#help(package="gratia")
plot(m2s)

g1=getViz(m2s)
print(plot(g1,allTerms=T),pages=1)

draw(g1)

######################################################################

# # Summary of the mixed-effects model
# summary(mixed_model)

# # Simplified ANOVA model
# anova_result_simple <- aov(Count ~ Device + DateTime, data = insecta_counts)
# 
# # Summary of the simplified ANOVA
# summary(anova_result_simple)

#######
# falsch, da hier Count data. 

# Fit a three-way ANOVA model with interactions
anova_devices_clim <- aov(Count ~ Device + Week + AIRTEMP + WINDSPEED + Rain + RAD + GROUNDTEMP + RH + AIRP, data = insecta_counts)

# Perform an ANOVA Type III
anova_devices_clim_TIII <- Anova(anova_devices_clim, type = "III")

# Apply the function to reorder and rename columns
anova_devices_clim_TIII <- reorder_rename_anova(anova_devices_clim_TIII)
print(anova_devices_clim_TIII)

#######

# Fit a three-way ANOVA model with interactions
anova_devices <- aov(Count ~ Device, data = insecta_counts)
summary(anova_devices) 

# Perform Tukey's HSD test for the different devices
tukey_devices <- TukeyHSD(anova_devices)
print(tukey_devices)

# Extract the results and create a compact letter display for devices
letters_tukey_devices <- multcompLetters4(anova_devices, tukey_devices)
#letters_tukey_devices <- multcompLetters(tukey_devices$Device)

# Create a table with factors, mean, standard deviation, and compact letter display
dt_devices <- insecta_counts %>%
  group_by(Device) %>%
  summarise(w = mean(Count), sd = sd(Count)) %>%
  arrange(desc(w))

# Extracting the compact letter display and adding to the table
cld_tukey_devices <- as.data.frame.list(letters_tukey_devices$Device)
dt_devices$cld_tukey_devices <- cld_tukey_devices$Letters

# Visualize the data with bar plots and letters for FAIR-D devices
plot0_device <- ggplot(dt_devices, aes(x = Device, y = w, fill = Device)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_errorbar(aes(ymin = w - sd, ymax = w + sd), width = 0.2) +
  geom_text(aes(label = cld_tukey_devices, y = w + sd), vjust = -0.5) +
  labs(x = "Device Nr.", y = "Average Insecta Count") +
  scale_fill_manual(values = c("FAIRD1" = "gold", "FAIRD2" = "yellow", "FAIRD3" = "greenyellow", "FAIRD4" = "yellowgreen", "ID1" = "gold", "ID2" = "yellow", "ID3" = "greenyellow", "ID4" = "yellowgreen")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +  # Center the title
  ggtitle(str_wrap(paste("Comparison of", selected_device_name, "Insecta Counts by Device Nr. with Tukey HSD Letters")))

print(plot0_device)

# Save the plot as an image
ggsave(output_file_tukey_devices_plot, plot = plot0_device, width = 8, height = 6)

########

# Fit a three-way ANOVA model with interactions
anova_ambient_clim <- aov(Count ~Ambient + Week + AIRTEMP + WINDSPEED + Rain + RAD + GROUNDTEMP + RH + AIRP, data = insecta_counts)

# Perform an ANOVA Type II
anova_ambient_clim_TIII <- Anova(anova_ambient_clim, type = "III")

# Apply the function to reorder and rename columns
anova_ambient_clim_TIII <- reorder_rename_anova(anova_ambient_clim_TIII)
print(anova_ambient_clim_TIII)

#######

# Fit a simple ANOVA model
anova_ambient <- aov(Count ~ Ambient, data = insecta_counts)
summary(anova_ambient)

# Perform Tukey's HSD test for the different devices
tukey_ambient <- TukeyHSD(anova_ambient, which='Ambient')
print(tukey_ambient)

# Extract the results and create a compact letter display for devices
letters_tukey_ambient <- multcompLetters4(anova_ambient, tukey_ambient)

# Create a table with factors, mean, standard deviation, and compact letter display
dt_ambient <- insecta_counts %>%
  group_by(Ambient) %>%
  summarise(w = mean(Count), sd = sd(Count)) %>%
  arrange(desc(w))

# Extracting the compact letter display and adding to the table
cld_tukey_ambient <- as.data.frame.list(letters_tukey_ambient $Ambient)
dt_ambient$cld_tukey_ambient <- cld_tukey_ambient$Letters

# Visualize the data with bar plots and letters for FAIR-D devices
plot1_device <- ggplot(dt_ambient, aes(x = Ambient, y = w, fill = Ambient)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_errorbar(aes(ymin = w - sd, ymax = w + sd), width = 0.2) +
  geom_text(aes(label = cld_tukey_ambient, y = w + sd), vjust = -0.5) +
  labs(x = "Ambient", y = "Average Insecta Count") +
  scale_fill_manual(values = c("Maize" = "gold", "Meadow" = "yellowgreen")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +  # Center the title
  ggtitle(str_wrap(paste("Comparison of", selected_device_name, "Insecta Counts by Ambient with Tukey HSD Letters"), width = 40))


print(plot1_device)

# Save the plot as an image
ggsave(output_file_tukey_ambient_plot, plot = plot1_device, width = 8, height = 6)

#######

library(lubridate)

# Determine the minimum time difference in seconds
min_time_diff <- as.numeric(min(diff(sort(unique(insecta_counts$DateTime)))), units = "secs")

# Create a function to generate time intervals
create_time_intervals <- function(datetime, interval) {
  cut(datetime, breaks = seq(min(datetime), max(datetime) + interval, by = interval), include.lowest = TRUE)
}

# Conditional resampling based on min_time_diff
if (min_time_diff <= 3600) {  # 1 hour in seconds
  interval_used <- "1 hour"  
  insecta_counts_no_device <- insecta_counts %>%
    mutate(DateTime = create_time_intervals(DateTime, min_time_diff)) %>%
    mutate(DateTime = as.POSIXct(DateTime)) %>%  # Ensure DateTime is POSIXct
    mutate(DateTime = floor_date(DateTime, interval_used)) %>%
    group_by(DateTime, Ambient) %>%
    summarise(Count = sum(Count, na.rm = TRUE))

} else {
  insecta_counts_no_device <- insecta_counts %>%
    group_by(DateTime, Ambient) %>%
    summarise(Count = sum(Count, na.rm = TRUE))
  interval_used <- paste(min_time_diff / 3600, "hours")
}

# Pivot the data to wide format for plotting
insecta_counts_wide <- insecta_counts_no_device %>%
  pivot_wider(names_from = Ambient, values_from = Count, values_fill = 0)

# Continue with the rest of your code
plot3_device <- ggplot(insecta_counts_wide, aes(x = Meadow, y = Maize)) +
  geom_jitter(color = "black", width = 0.3, height = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color = "dimgrey", linewidth = 1) +
  labs(title = str_wrap(paste("Scatter Plot of", selected_device_name, "Insecta Counts: Maize vs. Meadow/n (Interval:", interval_used, ")")),
       x = "Meadow Insecta Count",
       y = "Maize Insecta Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Center the title

# Fit the linear model
model <- lm(Maize ~ Meadow, data = insecta_counts_wide)

# Get the model summary
model_summary <- summary(model)

# Extract the R-squared value
r_squared <- model_summary$r.squared

# Extract the coefficients for the equation
intercept <- model_summary$coefficients[1, 1]
slope <- model_summary$coefficients[2, 1]

# Add the R-squared value and equation to the plot
plot3_device <- plot3_device +
  annotate("text", x = max(insecta_counts_wide$Meadow) * 0.37, y = max(insecta_counts_wide$Maize) * 0.53, label = paste("R² =", round(r_squared, 2)), hjust = 1, vjust = 1, size = 4, color = "dimgrey", fontface = "bold") +
  annotate("text", x = max(insecta_counts_wide$Meadow) * 0.5, y = max(insecta_counts_wide$Maize) * 0.6, label = paste("y =", round(intercept, 2), "+", round(slope, 2), "* x"), hjust = 1, vjust = 1, size = 4, color = "dimgrey", fontface = "bold")
print(plot3_device)

# Save the plot as an image
ggsave(output_file_scatter_ambient_plot, plot = plot3_device, width = 8, height = 6)

#######

#browser()

# Fit a three-way ANOVA model with interactions
#anova_devices_wk1_clim <- aov(Count ~ Ambient + AIRTEMP + WINDSPEED + Rain + RAD + GROUNDTEMP + RH + AIRP, data = insecta_counts_wk1)
#anova_devices_wk2_clim <- aov(Count ~ Ambient + AIRTEMP + WINDSPEED + Rain + RAD + GROUNDTEMP + RH + AIRP, data = insecta_counts_wk2)
#anova_devices_wk3_clim <- aov(Count ~ Ambient + AIRTEMP + WINDSPEED + RAD + GROUNDTEMP + RH + AIRP, data = insecta_counts_wk3)

anova_devices_wk1_clim <- aov(Count ~ Ambient + AIRTEMP + WINDSPEED + Rain + GROUNDTEMP + AIRP, data = insecta_counts_wk1)
anova_devices_wk2_clim <- aov(Count ~ Ambient + AIRTEMP + WINDSPEED + Rain + GROUNDTEMP + AIRP, data = insecta_counts_wk2)
anova_devices_wk3_clim <- aov(Count ~ Ambient + AIRTEMP + WINDSPEED + GROUNDTEMP + AIRP, data = insecta_counts_wk3)
summary(anova_devices_wk1_clim)

# Define a function to perform ANOVA and handle errors
perform_anova_TIII <- function(model) {
  tryCatch({
    Anova(model, type = "III")
  }, error = function(e) {
    warning("Aliased coefficients detected in the model. Skipping this ANOVA.")
    return(NULL)
  })
}

# Perform ANOVA for each week and handle potential errors
anova_devices_wk1_clim_TIII <- perform_anova_TIII(anova_devices_wk1_clim)
anova_devices_wk2_clim_TIII <- perform_anova_TIII(anova_devices_wk2_clim)
anova_devices_wk3_clim_TIII <- perform_anova_TIII(anova_devices_wk3_clim)

# Apply the function to reorder, rename columns, and print results if ANOVA results are not NULL
if (!is.null(anova_devices_wk1_clim_TIII)) {
  anova_devices_wk1_clim_TIII <- reorder_rename_anova(anova_devices_wk1_clim_TIII)
  print(anova_devices_wk1_clim_TIII)
}
if (!is.null(anova_devices_wk2_clim_TIII)) {
  anova_devices_wk2_clim_TIII <- reorder_rename_anova(anova_devices_wk2_clim_TIII)
  print(anova_devices_wk2_clim_TIII)
}
if (!is.null(anova_devices_wk3_clim_TIII)) {
  anova_devices_wk3_clim_TIII <- reorder_rename_anova(anova_devices_wk3_clim_TIII)
  print(anova_devices_wk3_clim_TIII)
}

anova_devices_wk1_clim_poly <- aov(Count ~ Ambient + poly(AIRTEMP, 2) + poly(WINDSPEED, 2) + Rain, data = insecta_counts_wk1)
anova_devices_wk2_clim_poly <- aov(Count ~ Ambient + poly(AIRTEMP, 2) + poly(WINDSPEED, 2) + Rain, data = insecta_counts_wk2)
anova_devices_wk3_clim_poly <- aov(Count ~ Ambient + poly(AIRTEMP, 2) + poly(WINDSPEED, 2), data = insecta_counts_wk3)

anova_devices_wk1_clim_poly_TIII <- perform_anova_TIII(anova_devices_wk1_clim_poly)
anova_devices_wk2_clim_poly_TIII <- perform_anova_TIII(anova_devices_wk2_clim_poly)
anova_devices_wk3_clim_poly_TIII <- perform_anova_TIII(anova_devices_wk3_clim_poly)

# Apply the function to reorder, rename columns, and print results if ANOVA results are not NULL
if (!is.null(anova_devices_wk1_clim_poly_TIII)) {
  print(anova_devices_wk1_clim_poly_TIII)
}
if (!is.null(anova_devices_wk2_clim_poly_TIII)) {
  print(anova_devices_wk2_clim_poly_TIII)
}
if (!is.null(anova_devices_wk3_clim_poly_TIII)) {
  print(anova_devices_wk3_clim_poly_TIII)
}


anova_devices_wk1_clim_inter <- aov(Count ~ Device + AIRTEMP * WINDSPEED + Rain + GROUNDTEMP + AIRP, data = insecta_counts_wk1)
anova_devices_wk2_clim_inter <- aov(Count ~ Device + AIRTEMP * WINDSPEED + Rain + GROUNDTEMP + AIRP, data = insecta_counts_wk2)
anova_devices_wk3_clim_inter <- aov(Count ~ Device + AIRTEMP * WINDSPEED + GROUNDTEMP + AIRP, data = insecta_counts_wk3)

anova_devices_wk1_clim_inter_TIII <- perform_anova_TIII(anova_devices_wk1_clim_inter)
anova_devices_wk2_clim_inter_TIII <- perform_anova_TIII(anova_devices_wk2_clim_inter)
anova_devices_wk3_clim_inter_TIII <- perform_anova_TIII(anova_devices_wk3_clim_inter)

# Apply the function to reorder, rename columns, and print results if ANOVA results are not NULL
if (!is.null(anova_devices_wk1_clim_inter_TIII)) {
  print(anova_devices_wk1_clim_inter_TIII)
}
if (!is.null(anova_devices_wk2_clim_inter_TIII)) {
  print(anova_devices_wk2_clim_inter_TIII)
}
if (!is.null(anova_devices_wk3_clim_inter_TIII)) {
  print(anova_devices_wk3_clim_inter_TIII)
}

#######

# Fit a three-way ANOVA model with interactions
anova_weeks <- aov(Count ~Week, data = insecta_counts)
summary(anova_weeks)

# Perform Tukey's HSD test for the different devices
tukey_weeks <- TukeyHSD(anova_weeks, which='Week')
print(tukey_weeks)

# Extract the results and create a compact letter display for devices
letters_tukey_weeks <- multcompLetters4(anova_weeks, tukey_weeks)

# Create a table with factors, mean, standard deviation, and compact letter display
dt_weeks <- insecta_counts %>%
  group_by(Week) %>%
  summarise(w = mean(Count), sd = sd(Count), Count = sum(Count)) %>%
  arrange(desc(w))

# Extracting the compact letter display and adding to the table
cld_tukey_weeks <- as.data.frame.list(letters_tukey_weeks $Week)
dt_weeks$cld_tukey_weeks <- cld_tukey_weeks$Letters

# Visualize the data with bar plots and letters for FAIR-D devices
plot2_device <- ggplot(dt_weeks, aes(x = Week, y = w, fill = Week)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_errorbar(aes(ymin = w - sd, ymax = w + sd), width = 0.2) +
  geom_text(aes(label = cld_tukey_weeks, y = w + sd), vjust = -0.5) +
  labs(x = "Week Nr.", y = "Average Insecta Count") +
  scale_fill_manual(values = c("1" = "salmon", "2" = "limegreen", "3" = "skyblue3")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +  # Center the title
  ggtitle(str_wrap(paste("Comparison of", selected_device_name, "Insecta average Counts by Week with Tukey HSD Letters")))

print(plot2_device)

# Save the plot as an image
ggsave(output_file_tukey_weeks_plot, plot = plot2_device, width = 8, height = 6)


# Visualize the data with bar plots and letters for FAIR-D devices
plot3_device <- ggplot(dt_weeks, aes(x = Week, y = Count, fill = Week)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_errorbar(aes(ymin = Count - sd, ymax = Count + sd), width = 0.2) +
  geom_text(aes(label = Count, y = Count + sd), vjust = -0.5) +
  labs(x = "Week Nr.", y = "Insecta Count") +
  scale_fill_manual(values = c("1" = "salmon", "2" = "limegreen", "3" = "skyblue3")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +  # Center the title
  ggtitle(str_wrap(paste("Comparison of", selected_device_name, "Insecta Counts")))

print(plot3_device)

# Save the plot as an image
ggsave(output_file_counts_weeks_plot, plot = plot3_device, width = 8, height = 6)

#######

anova_rain <- aov(Count ~ Rain, data = insecta_counts)
summary(anova_rain)

#######

# Convert the simple ANOVA results to tidy data frames
anova_devices  <- broom::tidy(anova_devices)
anova_ambient  <- broom::tidy(anova_ambient)
anova_weeks  <- broom::tidy(anova_weeks)
anova_rain  <- broom::tidy(anova_rain)

# Convert the tukey results to tidy data frames
tukey_devices  <- broom::tidy(tukey_devices)
tukey_ambient  <- broom::tidy(tukey_ambient)
tukey_weeks  <- broom::tidy(tukey_weeks)

# List of ANOVA models
anova_models <- list(
  anova_devices_clim_TIII,
  anova_devices,
  anova_ambient_clim_TIII,
  anova_ambient,
  anova_devices_wk1_clim_TIII,
  anova_devices_wk2_clim_TIII,
  anova_devices_wk3_clim_TIII,
  anova_weeks,
  anova_rain
)

# Apply the function to all ANOVA models, handling NULL values
anova_models_with_significance <- lapply(anova_models, function(model) {
  if (!is.null(model)) {
    add_significance(model)
  } else {
    NULL
  }
})

# Assign the modified tables back to their original names
anova_devices_clim_TIII <- anova_models_with_significance[[1]]
anova_devices <- anova_models_with_significance[[2]]
anova_ambient_clim_TIII <- anova_models_with_significance[[3]]
anova_ambient <- anova_models_with_significance[[4]]
anova_devices_wk1_clim_TIII <- anova_models_with_significance[[5]]
anova_devices_wk2_clim_TIII <- anova_models_with_significance[[6]]
anova_devices_wk3_clim_TIII <- anova_models_with_significance[[7]]
anova_weeks <- anova_models_with_significance[[8]]
anova_rain <- anova_models_with_significance[[9]]

# Modify the variable names for table display
# Create a mapping of variable names:
variable_mapping <- c(
  "Ambient" = "Ambient",
  "Week" = "Week",
  "AIRTEMP" = "Temperature",
  "WINDSPEED" = "Wind Speed",
  "Rain" = "Rain",
  "RAD" = "Radiation",
  "GROUNDTEMP" = "Ground temp.",
  "RH" = "Rel. humidity",
  "AIRP" = "Air pressure"
)


# Apply the mapping to the ANOVA tables
rename_variables <- function(df, mapping) {
  df$term <- sapply(df$term, function(x) ifelse(x %in% names(mapping), mapping[x], x))
  return(df)
}

# Apply the renaming function to all ANOVA tables
anova_devices_clim_TIII <- rename_variables(anova_devices_clim_TIII, variable_mapping)
anova_devices <- rename_variables(anova_devices, variable_mapping)
anova_ambient_clim_TIII <- rename_variables(anova_ambient_clim_TIII, variable_mapping)
anova_ambient <- rename_variables(anova_ambient, variable_mapping)
anova_devices_wk1_clim_TIII <- rename_variables(anova_devices_wk1_clim_TIII, variable_mapping)
anova_devices_wk2_clim_TIII <- rename_variables(anova_devices_wk2_clim_TIII, variable_mapping)
anova_devices_wk3_clim_TIII <- rename_variables(anova_devices_wk3_clim_TIII, variable_mapping)
anova_weeks <- rename_variables(anova_weeks, variable_mapping)
anova_rain <- rename_variables(anova_rain, variable_mapping)

# Save the resulting table to a new Excel file
write_excel_csv(insecta_counts, output_file_counts)

# Write the updated anova data frames to CSV files
write.csv(anova_devices_clim_TIII, output_file_anova_devices_clim, row.names = FALSE)
write.csv(anova_devices, output_file_anova_devices, row.names = FALSE)
write.csv(anova_ambient_clim_TIII, output_file_anova_ambient_clim, row.names = FALSE)
write.csv(anova_ambient, output_file_anova_ambient, row.names = FALSE)
write.csv(anova_devices_wk1_clim_TIII, output_file_anova_devices_wk1_clim, row.names = FALSE)
write.csv(anova_devices_wk2_clim_TIII, output_file_anova_devices_wk2_clim, row.names = FALSE)
write.csv(anova_devices_wk3_clim_TIII, output_file_anova_devices_wk3_clim, row.names = FALSE)
write.csv(anova_weeks, output_file_anova_weeks, row.names = FALSE)
write.csv(anova_rain, output_file_anova_rain, row.names = FALSE)

# Save the Tukey's HSD results with letters to a CSV file
write.csv(tukey_devices, output_file_tukey_devices, row.names = FALSE)
write.csv(tukey_ambient, output_file_tukey_ambient, row.names = FALSE)
write.csv(tukey_weeks, output_file_tukey_weeks, row.names = FALSE)

cat("Results saved in:", output_dir, "\n")

