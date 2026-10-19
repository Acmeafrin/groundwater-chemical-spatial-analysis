# Load required packages
if (!require(data.table)) install.packages("data.table", dependencies=TRUE)
if (!require(dplyr)) install.packages("dplyr", dependencies=TRUE)
if (!require(tidyr)) install.packages("tidyr", dependencies=TRUE)
if (!require(ggplot2)) install.packages("ggplot2", dependencies=TRUE)
if (!require(reshape2)) install.packages("reshape2", dependencies=TRUE)

library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)

# Load the data
if (!require(readr)) install.packages("readr", dependencies=TRUE)
library(readr)

# Load the translated geological data, specifying column types to handle parsing issues
data <- read_csv(
  "C:/Users/Afrin/OneDrive/Desktop/chung_geo_corrected.csv",
  col_types = cols(
    T.C. = col_double(),
    F.C. = col_double(),
    E.C. = col_double(),
    Cd = col_double(),
    `...63` = col_skip(),
    `...64` = col_skip(),
    .default = col_guess()
  )
)

# Convert non-numeric values to NA for numeric columns
numeric_cols <- c("T.C.", "F.C.", "E.C.", "Cd", "General_Bacteria", "Pb", "F", "As", "Se", "Hg", "Cr", "NH3.N", "NO3.N", "B", "Trichloroethylene", "Dichloromethane", "Benzene", "Toluene", "Ethylbenzene", "Xylene", "TH", "COD", "Cu", "Color", "pH", "Zn", "Cl", "Fe", "Mn", "Turbidity", "SO4", "Al")

data <- data %>%
  mutate(across(all_of(numeric_cols), ~ as.numeric(.), .names = "{col}"))

# Define geological categories mapping function
categorize_geology <- function(geology_name) {
  if (grepl("Limestone|Dolomite", geology_name, ignore.case = TRUE)) {
    return("Carbonate Rock")
  } else if (grepl("Granite|Basalt|Igneous|Acidic Rocks|Andesitic|Biotite Granite|Porphyry", geology_name, ignore.case = TRUE)) {
    return("Igneous Rock")
  } else if (grepl("Sandstone|Shale|Conglomerate|Quartzite|Limesilicate", geology_name, ignore.case = TRUE)) {
    return("Sedimentary Rock")
  } else if (grepl("Schist|Gneiss|Metamorphic|Amphibolite|Arc Gneiss|Biotite Gneiss|Black Slate|Quartz Gneiss|Granitic Gneiss|Metamorphic Complex|Gneiss", geology_name, ignore.case = TRUE)) {
    return("Metamorphic Rock")
  } else if (grepl("Alluvium|Soil", geology_name, ignore.case = TRUE)) {
    return("Alluvium")
  } else {
    return("Other")
  }
}

# Apply geological categorization to the REFROCK column
data$GeoType <- sapply(data$REFROCK, categorize_geology)

# Ensure Well_ID column exists (creating it if missing)
if (!"Well_ID" %in% colnames(data)) {
  data$Well_ID <- seq_len(nrow(data))  # Create Well_ID if it does not exist
}

print(colnames(data))

# Calculate area percentages for each well without using double grouping
final_results <- data %>%
  group_by(Well_ID, GeoType) %>%
  summarise(Totalarea_lithology = sum(area_lithology, na.rm = TRUE), .groups = 'drop') %>%
  group_by(Well_ID) %>%
  mutate(area_lithologyPercentage = ifelse(sum(Totalarea_lithology, na.rm = TRUE) == 0, 0, (Totalarea_lithology / sum(Totalarea_lithology, na.rm = TRUE)) * 100)) %>%
  ungroup()

# Reshape the data to have one row per well with geological type percentages as columns
reshaped_results <- final_results %>%
  select(Well_ID, GeoType, area_lithologyPercentage) %>%
  pivot_wider(names_from = GeoType, values_from = area_lithologyPercentage, values_fill = list(area_lithologyPercentage = 0))

# Summarize the reshaped data to ensure one row per well
summarized_results <- reshaped_results %>%
  group_by(Well_ID) %>%
  summarise(across(everything(), sum), .groups = 'drop')

# Save the final results to a CSV file
write.csv(data, "data.csv", row.names = FALSE)

# Display the first few rows of the reshaped data to verify
head(summarized_results)




#

write.csv(data, "data_cp949.csv", row.names = FALSE, fileEncoding = "CP949")
