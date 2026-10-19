# Step 1: Load and prepare the data

# Read the dataset
dat <- read.csv("Groundwater(17-22).csv")

# Convert relevant columns to numeric
dat$No. <- as.numeric(dat$No.)
dat$Year <- as.numeric(dat$Year)
dat$latitude <- as.numeric(dat$latitude)
dat$longitude <- as.numeric(dat$longitude)

# Subset the data by province and age (replace '시도' and 'Age' with actual column names)
subset_data <- dat %>% filter(시도 == '충청남도', Age == 'Adult')

# Subset the data to only necessary columns
ad <- subset_data

# Replace zeroes with a small value to avoid calculation issues
ad[c(10:17)][ad[c(10:17)] == 0] <- 0.000001

# Subset the dataset to remove rows with missing longitude/latitude data
ad.net <- ad[complete.cases(ad$longitude), ]

# Step 2: Create spatial weights
coords.ad <- cbind(ad.net$longitude, ad.net$latitude)  # Coordinates of locations
nb.ad <- knn2nb(knearneigh(coords.ad, k = 5))  # 5 nearest neighbors
w.ad <- nb2listw(nb.ad, style = "minmax")  # Create weights list

# Step 3: Function to calculate Local Moran's I and classify results
calculate_lisa <- function(variable, weights) {
  lisa <- localmoran(variable, weights)
  lisa <- as.data.frame(lisa)
  colnames(lisa) <- c("Ii", "E.Ii", "Var.Ii", "Z.Ii", "Pr")
  
  # Classify results into High-High, Low-Low, High-Low, and Low-High categories
  lisa$group <- NA
  lisa$group[lisa$Z.Ii > 1.96 & lisa$Ii > 0] <- "High-High"
  lisa$group[lisa$Z.Ii > 1.96 & lisa$Ii < 0] <- "High-Low"
  lisa$group[lisa$Z.Ii < -1.96 & lisa$Ii < 0] <- "Low-Low"
  lisa$group[lisa$Z.Ii < -1.96 & lisa$Ii > 0] <- "Low-High"
  
  # Calculate standard deviation
  lisa$std_dev <- sqrt(lisa$Var.Ii)
  
  return(lisa)
}

# Step 4: Calculate Local Moran's I for multiple variables

# General Bacteria
lisa.General_Bacteria <- calculate_lisa(ad.net$General_Bacteria, w.ad)

# NO3-N
lisa.NO3.N <- calculate_lisa(ad.net$NO3.N, w.ad)

# pH
lisa.pH <- calculate_lisa(ad.net$pH, w.ad)

# F
lisa.F <- calculate_lisa(ad.net$F, w.ad)

# As
lisa.As <- calculate_lisa(ad.net$As, w.ad)

# Al
lisa.Al <- calculate_lisa(ad.net$Al, w.ad)

# Mn
lisa.Mn <- calculate_lisa(ad.net$Mn, w.ad)

# Turbidity
lisa.Turbidity <- calculate_lisa(ad.net$Turbidity, w.ad)

# Fe
lisa.Fe <- calculate_lisa(ad.net$Fe, w.ad)

# Total Coliforms (T.C.)
lisa.TC <- calculate_lisa(ad.net$T.C., w.ad)

# Escherichia coli (E.C.)
lisa.EC <- calculate_lisa(ad.net$E.C., w.ad)

# Fecal Coliforms (F.C.)
lisa.FC <- calculate_lisa(ad.net$F.C., w.ad)

# Step 5: Add Local Moran's I results to the dataset with proper column names
ad.net <- cbind(ad.net, 
                General_Bacteria_Ii = lisa.General_Bacteria$Ii, 
                General_Bacteria_E_Ii = lisa.General_Bacteria$E.Ii, 
                General_Bacteria_Var_Ii = lisa.General_Bacteria$Var.Ii, 
                General_Bacteria_Z_Ii = lisa.General_Bacteria$Z.Ii, 
                General_Bacteria_Pr = lisa.General_Bacteria$Pr, 
                General_Bacteria_group = lisa.General_Bacteria$group,
                General_Bacteria_std_dev = lisa.General_Bacteria$std_dev,
                NO3_N_Ii = lisa.NO3.N$Ii, 
                NO3_N_E_Ii = lisa.NO3.N$E.Ii, 
                NO3_N_Var_Ii = lisa.NO3.N$Var.Ii, 
                NO3_N_Z_Ii = lisa.NO3.N$Z.Ii, 
                NO3_N_Pr = lisa.NO3.N$Pr, 
                NO3_N_group = lisa.NO3.N$group,
                NO3_N_std_dev = lisa.NO3.N$std_dev,
                pH_Ii = lisa.pH$Ii, 
                pH_E_Ii = lisa.pH$E.Ii, 
                pH_Var_Ii = lisa.pH$Var.Ii, 
                pH_Z_Ii = lisa.pH$Z.Ii, 
                pH_Pr = lisa.pH$Pr, 
                pH_group = lisa.pH$group,
                pH_std_dev = lisa.pH$std_dev,
                F_Ii = lisa.F$Ii, 
                F_E_Ii = lisa.F$E.Ii, 
                F_Var_Ii = lisa.F$Var.Ii, 
                F_Z_Ii = lisa.F$Z.Ii, 
                F_Pr = lisa.F$Pr, 
                F_group = lisa.F$group,
                F_std_dev = lisa.F$std_dev,
                As_Ii = lisa.As$Ii, 
                As_E_Ii = lisa.As$E.Ii, 
                As_Var_Ii = lisa.As$Var.Ii, 
                As_Z_Ii = lisa.As$Z.Ii, 
                As_Pr = lisa.As$Pr, 
                As_group = lisa.As$group,
                As_std_dev = lisa.As$std_dev,
                Al_Ii = lisa.Al$Ii, 
                Al_E_Ii = lisa.Al$E.Ii, 
                Al_Var_Ii = lisa.Al$Var.Ii, 
                Al_Z_Ii = lisa.Al$Z.Ii, 
                Al_Pr = lisa.Al$Pr, 
                Al_group = lisa.Al$group,
                Al_std_dev = lisa.Al$std_dev,
                Mn_Ii = lisa.Mn$Ii, 
                Mn_E_Ii = lisa.Mn$E.Ii, 
                Mn_Var_Ii = lisa.Mn$Var.Ii, 
                Mn_Z_Ii = lisa.Mn$Z.Ii, 
                Mn_Pr = lisa.Mn$Pr, 
                Mn_group = lisa.Mn$group,
                Mn_std_dev = lisa.Mn$std_dev,
                Turbidity_Ii = lisa.Turbidity$Ii, 
                Turbidity_E_Ii = lisa.Turbidity$E.Ii, 
                Turbidity_Var_Ii = lisa.Turbidity$Var.Ii, 
                Turbidity_Z_Ii = lisa.Turbidity$Z.Ii, 
                Turbidity_Pr = lisa.Turbidity$Pr, 
                Turbidity_group = lisa.Turbidity$group,
                Turbidity_std_dev = lisa.Turbidity$std_dev,
                Fe_Ii = lisa.Fe$Ii, 
                Fe_E_Ii = lisa.Fe$E.Ii, 
                Fe_Var_Ii = lisa.Fe$Var.Ii, 
                Fe_Z_Ii = lisa.Fe$Z.Ii, 
                Fe_Pr = lisa.Fe$Pr, 
                Fe_group = lisa.Fe$group,
                Fe_std_dev = lisa.Fe$std_dev,
                TC_Ii = lisa.TC$Ii, 
                TC_E_Ii = lisa.TC$E.Ii, 
                TC_Var_Ii = lisa.TC$Var.Ii, 
                TC_Z_Ii = lisa.TC$Z.Ii, 
                TC_Pr = lisa.TC$Pr, 
                TC_group = lisa.TC$group,
                TC_std_dev = lisa.TC$std_dev,
                EC_Ii = lisa.EC$Ii, 
                EC_E_Ii = lisa.EC$E.Ii, 
                EC_Var_Ii = lisa.EC$Var.Ii, 
                EC_Z_Ii = lisa.EC$Z.Ii, 
                EC_Pr = lisa.EC$Pr, 
                EC_group = lisa.EC$group,
                EC_std_dev = lisa.EC$std_dev,
                FC_Ii = lisa.FC$Ii, 
                FC_E_Ii = lisa.FC$E.Ii, 
                FC_Var_Ii = lisa.FC$Var.Ii, 
                FC_Z_Ii = lisa.FC$Z.Ii, 
                FC_Pr = lisa.FC$Pr, 
                FC_group = lisa.FC$group,
                FC_std_dev = lisa.FC$std_dev)

# Step 6: Write the updated dataset to a CSV file with encoding for Korean characters
write.csv(ad.net, "gangwondo-local.csv", row.names = FALSE, fileEncoding = "euc-kr")

# Load necessary library
library(dplyr)

# Step 7: Filter to only include rows with "High-High" pattern
high_high_data <- ad.net %>% filter(
  General_Bacteria_group == "High-High" |
    NO3_N_group == "High-High" |
    pH_group == "High-High" |
    F_group == "High-High" |
    As_group == "High-High" |
    Al_group == "High-High" |
    Mn_group == "High-High" |
    Turbidity_group == "High-High" |
    Fe_group == "High-High" |
    TC_group == "High-High" |
    EC_group == "High-High" |
    FC_group == "High-High"
)
# Step 8: Create a summary data frame for visualization
summary_chart <- data.frame(
  Parameter = rep("NO3-N", nrow(high_high_data)),
  Region = high_high_data$시군구,
  Concentration_range = paste0(
    round(high_high_data$NO3.N, 1), " ~ ", round(high_high_data$NO3_N_std_dev, 1)
  ),
  Avg_Concentration = high_high_data$NO3_N_Ii,
  Std_deviation = high_high_data$NO3_N_std_dev,
  Pattern = rep("High-High", nrow(high_high_data))
)

# View the summary chart
print(summary_chart)


# Load necessary library
library(dplyr)

# Step 5: Filter to only include rows with "High-High" pattern for each variable
high_high_data <- ad.net %>% filter(
  General_Bacteria_group == "High-High" |
    NO3_N_group == "High-High" |
    pH_group == "High-High" |
    F_group == "High-High" |
    As_group == "High-High" |
    Al_group == "High-High" |
    Mn_group == "High-High" |
    Turbidity_group == "High-High" |
    Fe_group == "High-High" |
    TC_group == "High-High" |
    EC_group == "High-High" |
    FC_group == "High-High"
)

# Step 6: Filter and compile the summary for High-High patterns only

compiled_data <- data.frame()

# Define the parameters and their group columns
parameters <- list(
  "NO3.N" = lisa.NO3.N,
  "pH" = lisa.pH,
  "Mn" = lisa.Mn,
  "T.C." = lisa.TC,
  "F" = lisa.F,
  "Fe" = lisa.Fe,
  "As" = lisa.As,
  "Al" = lisa.Al,
  "E.C." = lisa.EC,
  "F.C." = lisa.FC,
  "General_Bacteria" = lisa.General_Bacteria
)

# Loop through each parameter to filter and summarize "High-High" areas
for (param in names(parameters)) {
  # Merge the LISA data with ad.net data
  param_data <- cbind(ad.net, parameters[[param]])
  
  # Filter the data for High-High group only
  filtered_data <- param_data %>% filter(group == "High-High")
  
  # Create a summary for the parameter if there is data available
  if (nrow(filtered_data) > 0) {
    summary <- filtered_data %>%
      group_by(시군구) %>%
      summarize(
        Parameter = param,
        Concentration_range = paste0(min(get(param), na.rm = TRUE), " ~ ", max(get(param), na.rm = TRUE)),
        Avg_Concentration = mean(Ii, na.rm = TRUE),
        Std_deviation = paste0(min(std_dev, na.rm = TRUE), " ~ ", max(std_dev, na.rm = TRUE)),
        Avg_Std_Deviation = mean(std_dev, na.rm = TRUE),
        Pattern = "High-High"
      )
    
    # Add the summary to the compiled data
    compiled_data <- rbind(compiled_data, summary)
  }
}

# View the compiled summary
print(compiled_data)

# Convert all character columns in compiled_data to handle encoding properly
compiled_data_converted <- compiled_data
compiled_data_converted[] <- lapply(compiled_data_converted, function(x) {
  if (is.character(x)) {
    iconv(x, from = "UTF-8", to = "CP949")
  } else {
    x
  }
})

# Write the converted data to CSV
write.csv(compiled_data_converted, "Compiled_High_High_Summary_Chart_CP949.csv", row.names = FALSE, fileEncoding = "CP949")

