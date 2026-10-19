# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(extrafont)
library(spdep)



#font_import()
#fonts()
#"Times New Roman" %in% fonts()


# Read the dataset
data <- read.csv("Groundwater(17-22).csv")

data$No. = as.numeric(data$No.)


data$Year = as.numeric(data$Year)
data$latitude = as.numeric(data$latitude)
data$longitude = as.numeric(data$longitude)

# Subset the data by province and age
# Replace the placeholders with your actual column names and filter values
subset_data <- data %>% filter(시도 == '충청남도', Age == 'Adult')

# Replace zero values in specific columns
# Adjust the column indices (17:56) according to your dataset
subset_data[, 17:56] <- lapply(subset_data[, 17:56], function(x) ifelse(x == 0, 0.000001, x))
str(subset_data)

# Remove rows with NA in 'longitude'
subset_data <- subset_data %>% filter(complete.cases(longitude))

# Standardize the data (scaling)
# Adjust the column indices according to your dataset
scaled_data <- scale(subset_data[, c(17, 19, 20, 22, 23:56)])
scaled_data <- as.data.frame(scaled_data)
# Combine scaled data with other columns
subset_data <- cbind(subset_data[, 1:11], scaled_data)


#Check the standardization
summary(subset_data$General_Bacteria)
summary(subset_data$NO3.N)


#Display first rows of net dataset
head(subset_data)
#create neighbor list

coords <- cbind(subset_data$longitude,subset_data$latitude)
nb <- knn2nb(knearneigh(coords, k = 20)) #k- nearest neighbors

             
#Create spatial weights matrix

w.ad <- nb2listw(nb, style = "minmax")
             


#w.ad is my spatial weight matrix & subset_data is my dataframe
calculate_contributions <- function(df, param, w) {
  # Calculate Moran's I for neighbor contribution
  lag_param <- lag.listw(w, df[[param]])
  neighbor_contribution <- cor(df[[param]], lag_param)
  local_contribution <- 1 - neighbor_contribution
  
  # Add the contributions to the data frame
  df[[paste0(param, "_neighbor_contribution")]] <- neighbor_contribution
  df[[paste0(param, "_local_contribution")]] <- local_contribution
  
  return(df)
}

# Define your parameters
parameters <- c("General_Bacteria")

# Loop through the parameters and apply the function
for (param in parameters) {
  subset_data <- calculate_contributions(subset_data, param, w.ad)
}

# Reshape data for visualization
long_data <- subset_data %>%
  pivot_longer(
    cols = contains("_contribution"),
    names_to = c("parameter", "contribution_type"),
    names_sep = "_",
    values_to = "contribution_value"
  ) %>%
  mutate(contribution_type = case_when(
    contribution_type == "neighbor" ~ "Neighbor",
    contribution_type == "local" ~ "Local",
    TRUE ~ as.character(contribution_type)
  ))
# Inspect the first few rows
head(long_data)


#final touch up on ggplot
# Set the order of the parameters
desired_order <- c("pH", "NO3.N", "F", "Al", "As", "Mn", "Fe", "Turbidity")
# Define custom labels for the x-axis, replacing "NO3.N" with "NO3-N"
custom_labels <- c("pH", expression(NO[3]*"-N"), "F", "Al", "As", "Mn", "Fe", "Turbidity")
# Update long_data to have this order
long_data$parameter <- factor(long_data$parameter, levels = desired_order)


library(ggplot2)

final_plot <- ggplot(long_data, aes(x = parameter, y = contribution_value, fill = contribution_type)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0, 0)) +
  scale_fill_manual(values = c("#FF857E", "#000BC4"), labels = c("Local contribution", "Neighbor contribution")) +
  labs(title = NULL, x = NULL, y = "Contribution (%)", fill = NULL) +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1.1, color = "black", size = 13, family = "Times New Roman"),
    axis.text.y = element_text(color = "black", size = 13, family = "Times New Roman"),
    axis.ticks = element_line(color = "black",size = 1.2),
    axis.line = element_line(color = "black",size = 1),
    axis.title.y = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_x_discrete(labels = custom_labels)


print(final_plot)

#GEN

# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(spdep)

# Read the dataset
data <- read.csv("Groundwater(17-22).csv")

# Convert relevant columns to numeric
data$No. <- as.numeric(data$No.)
data$Year <- as.numeric(data$Year)

data$latitude <- as.numeric(data$latitude)
data$longitude <- as.numeric(data$longitude)

# Subset the data by province and age (replace '시도' and 'Age' with actual column names)
subset_data <- data %>% filter(시도 == '충청남도', Age == 'Adult')

# Replace zero values in specific columns with a small positive number to avoid log/scale issues
subset_data[, 17:56] <- lapply(subset_data[, 17:56], function(x) ifelse(x == 0, 0.000001, x))

# Remove rows with NA in 'longitude'
subset_data <- subset_data %>% filter(complete.cases(longitude))

# Standardize the data (scaling)
scaled_data <- scale(subset_data[, c(17, 19, 20, 22, 23:56)])
scaled_data <- as.data.frame(scaled_data)

# Combine scaled data with other columns
subset_data <- cbind(subset_data[, 1:11], scaled_data)

# Create neighbor list
coords <- cbind(subset_data$longitude, subset_data$latitude)
nb <- knn2nb(knearneigh(coords, k = 20)) # k-nearest neighbors

# Create spatial weights matrix
w.ad <- nb2listw(nb, style = "minmax")

# Function to calculate contributions
calculate_contributions <- function(df, param, w) {
  # Calculate Moran's I for neighbor contribution
  lag_param <- lag.listw(w, df[[param]])
  neighbor_contribution <- cor(df[[param]], lag_param)
  local_contribution <- 1 - neighbor_contribution
  
  # Add the contributions to the data frame
  df[[paste0(param, "_neighbor_contribution")]] <- neighbor_contribution
  df[[paste0(param, "_local_contribution")]] <- local_contribution
  
  return(df)
}

# Calculate contributions specifically for General_Bacteria
subset_data <- calculate_contributions(subset_data, "General_Bacteria", w.ad)

# Check the structure of the data to verify if the columns were added
print("Column names in subset_data:")
print(names(subset_data))

# Verify the columns before reshaping
print(names(subset_data))  # Check column names to ensure correct selection

# Reshape data for visualization if columns exist
long_data_gb <- subset_data %>%
  pivot_longer(
    cols = c("General_Bacteria_neighbor_contribution", "General_Bacteria_local_contribution"),
    names_to = "contribution_type",  # Capture only the contribution type (neighbor/local)
    names_pattern = "General_Bacteria_(.*)_contribution",  # Capture 'neighbor' or 'local' as contribution type
    values_to = "contribution_value"
  ) %>%
  mutate(
    parameter = "General_Bacteria",  # Set parameter column explicitly
    contribution_type = case_when(
      contribution_type == "neighbor" ~ "Neighbor",
      contribution_type == "local" ~ "Local",
      TRUE ~ as.character(contribution_type)
    )
  )

# Inspect the first few rows to confirm the reshaping
print("Reshaped data for General_Bacteria:")
head(long_data_gb)

# Visualization for General_Bacteria
final_plot_gb <- ggplot(long_data_gb, aes(x = parameter, y = contribution_value, fill = contribution_type)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0, 0)) +
  scale_fill_manual(values = c("#FF857E", "#000BC4"), labels = c("Local contribution", "Neighbor contribution")) +
  labs(title = "Contribution of Local and Neighbor for General_Bacteria", x = NULL, y = "Contribution (%)", fill = NULL) +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 11),
    axis.text.x = element_text(angle = 0, hjust = 0.5, color = "black", size = 13, family = "Times New Roman"),
    axis.text.y = element_text(color = "black", size = 13, family = "Times New Roman"),
    axis.ticks = element_line(color = "black", size = 1.2),
    axis.line = element_line(color = "black", size = 1),
    axis.title.y = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Print the plot
print(final_plot_gb)

#pie
# Summarize the contributions for a pie chart
pie_data <- long_data_gb %>%
  group_by(contribution_type) %>%
  summarise(total_contribution = sum(contribution_value)) %>%
  mutate(proportion = total_contribution / sum(total_contribution))  # Normalize to sum to 1

# Create the pie chart with corrected proportions
pie_chart <- ggplot(pie_data, aes(x = "", y = proportion, fill = contribution_type)) +
  geom_bar(width = 1, stat = "identity", color = "black") +
  coord_polar("y") +
  scale_fill_manual(values = c("#f6511d", "#004b95"), labels = c("Local contribution", "Neighbor contribution")) +
  labs(title = "General_Bacteria", fill = NULL) +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right",
    plot.title = element_text(size = 16, hjust = 0.5)
  ) +
  geom_text(aes(label = paste0(round(proportion * 100, 1), "%")), position = position_stack(vjust = 0.5), color = "white")

# Print the corrected pie chart
print(pie_chart)

#donut
# Summarize the contributions for a donut chart
donut_data <- long_data_gb %>%
  group_by(contribution_type) %>%
  summarise(total_contribution = sum(contribution_value)) %>%
  mutate(proportion = total_contribution / sum(total_contribution))  # Normalize to sum to 1

# Create the donut chart
donut_chart <- ggplot(donut_data, aes(x = 2, y = proportion, fill = contribution_type)) +
  geom_bar(stat = "identity", color = "black", width = 1) +
  coord_polar(theta = "y") +
  xlim(0.5, 2.5) +  # Adjust the x-axis to create the donut hole
  scale_fill_manual(values = c("#f69256", "#18BEBC"), labels = c("Local contribution", "Neighbor contribution")) +
  labs(title = "General_Bacteria", fill = NULL) +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right",
    plot.title = element_text(size = 16, hjust = 0.5)
  ) +
  geom_text(aes(label = paste0(round(proportion * 100, 1), "%")), position = position_stack(vjust = 0.5), color = "white")

# Print the donut chart
print(donut_chart)

