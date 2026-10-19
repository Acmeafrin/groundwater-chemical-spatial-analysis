library(readxl)
library(ggplot2)
library(dplyr)
library(forcats)
library(maps)
library(mapdata)


# Load the dataset

df <- read_excel("wonju_parameter.xlsx", sheet = 'Sheet1')  


# Filter for the contaminants in columns 7 to 19
contaminants <- df[, 2:11]  

# the sum of each contaminant
contaminant_counts <- colSums(contaminants, na.rm = TRUE)

# Convert to a data frame for plotting
contaminant_counts_df <- data.frame(Contaminant = names(contaminant_counts), Counts = contaminant_counts)



# Reordering the dataframe to make sure 'others' is last
contaminant_counts_df$Contaminant <- factor(contaminant_counts_df$Contaminant,
                                            levels = c(names(sort(contaminant_counts[contaminant_counts_df$Contaminant != 'others'], decreasing = TRUE)), 'others'))


custom_labels <- setNames(nm = levels(contaminant_counts_df$Contaminant),
                          obj = levels(contaminant_counts_df$Contaminant))
# Replace the label for "NO3-N" with a string that ggplot can parse as an expression
custom_labels[custom_labels == "NO3-N"] <- expression(NO[3]*"-N")


# Define a vector of colors for the bars
colors <- c("red", "#1874CD", "green", "purple", "brown", "grey", "navy","seagreen","maroon2","darkorange1")

# Adjusted parts of the ggplot code

ggplot(contaminant_counts_df, aes(x = Contaminant, y = Counts, fill = Contaminant)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = colors) +
  scale_x_discrete(labels = custom_labels) +  # Use the custom labels here
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", colour = "black"),
    plot.background = element_rect(fill = "white", colour = "white"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
    axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.line = element_line(color = "black"),
    legend.position = "none",  # Remove the legend
    legend.text = element_blank(),
    legend.title = element_blank(),
    legend.key.size = unit(0, 'lines')  # Effectively remove legend keys
  ) +
  guides(fill = guide_legend(override.aes = list(shape = 21, size = 2))) + # Change shape to adjust size
  labs(x = 'Contaminants', y = 'Counts')

# Display the plot
print(ggplot_object) # Replace ggplot_object with the name of your ggplot variable

# Save the plot without the legend
ggsave('contaminants_barplot_no_legend.png', width = 12, height = 8, bg = "white")

 
# Display the plot
ggsave('contaminants_barplot_legend_inside.png', width = 12, height = 8, bg = "white")



# only chemical

# Load the dataset
setwd("C:/Users/ACMEAFRINJAHAN/Documents/Groundwater files- ACME")
df <- read_excel("summary table.xlsx", sheet = 'Sheet4')  


# Filter for the contaminants in columns 7 to 15
contaminants <- df[, 7:15]  

# the sum of each contaminant
contaminant_counts <- colSums(contaminants, na.rm = TRUE)

# Convert to a data frame for plotting
contaminant_counts_df <- data.frame(Contaminant = names(contaminant_counts), Counts = contaminant_counts)

# Reordering the dataframe to make sure 'others' is last
contaminant_counts_df$Contaminant <- factor(contaminant_counts_df$Contaminant,
                                            levels = c(names(sort(contaminant_counts[contaminant_counts_df$Contaminant != 'others'], decreasing = TRUE)), 'others'))

custom_labels <- setNames(nm = levels(contaminant_counts_df$Contaminant),
                          obj = levels(contaminant_counts_df$Contaminant))
# Replace the label for "NO3-N" with a string that ggplot can parse as an expression
custom_labels[custom_labels == "NO3-N"] <- expression(NO[3]*"-N")
# Define a vector of colors for the bars
colors <- c("red", "#1874CD", "#228B22", "purple", "#FFA500", "navy","#8B1A1A","#ff61c3","grey","orange")
ggplot(contaminant_counts_df, aes(x = Contaminant, y = Counts, fill = Contaminant)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = colors) +
  scale_x_discrete(labels = custom_labels) +  # Use the custom labels here
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", colour = "black"),
    plot.background = element_rect(fill = "white", colour = "white"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
    axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.line = element_line(color = "black"),
    legend.position = "none",  # Remove the legend
    legend.text = element_blank(),
    legend.title = element_blank(),
    legend.key.size = unit(0, 'lines')  # Effectively remove legend keys
  ) +
  guides(fill = guide_legend(override.aes = list(shape = 21, size = 2))) + # Change shape to adjust size
  labs(x = 'Contaminants', y = 'Counts')

# Display the plot
print(contaminant_counts_df) # Replace ggplot_object with the name of your ggplot variable

# Save the plot without the legend
ggsave('contaminants_barplot_no_legend.png', width = 12, height = 8, bg = "white")



#Gangwon do

# Load the dataset

df <- read_excel("contaminant- gangwon do", sheet = 'Sheet1')  


# Filter for the contaminants in columns 7 to 19
contaminants <- df[, 7:20]  

# the sum of each contaminant
contaminant_counts <- colSums(contaminants, na.rm = TRUE)

# Convert to a data frame for plotting
contaminant_counts_df <- data.frame(Contaminant = names(contaminant_counts), Counts = contaminant_counts)



# Reordering the dataframe to make sure 'others' is last
contaminant_counts_df$Contaminant <- factor(contaminant_counts_df$Contaminant,
                                            levels = c(names(sort(contaminant_counts[contaminant_counts_df$Contaminant != 'others'], decreasing = TRUE)), 'others'))


custom_labels <- setNames(nm = levels(contaminant_counts_df$Contaminant),
                          obj = levels(contaminant_counts_df$Contaminant))
# Replace the label for "NO3-N" with a string that ggplot can parse as an expression
custom_labels[custom_labels == "NO3-N"] <- expression(NO[3]*"-N")


# Define a vector of colors for the bars
colors <- c("red", "#1874CD", "green", "purple", "brown", "grey", "navy","seagreen","maroon2","darkorange1","steelblue","tomato1","lightblue")

# Adjusted parts of the ggplot code

ggplot(contaminant_counts_df, aes(x = Contaminant, y = Counts, fill = Contaminant)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = colors) +
  scale_x_discrete(labels = custom_labels) +  # Use the custom labels here
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", colour = "black"),
    plot.background = element_rect(fill = "white", colour = "white"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
    axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.line = element_line(color = "black"),
    legend.position = "none",  # Remove the legend
    legend.text = element_blank(),
    legend.title = element_blank(),
    legend.key.size = unit(0, 'lines')  # Effectively remove legend keys
  ) +
  guides(fill = guide_legend(override.aes = list(shape = 21, size = 2))) + # Change shape to adjust size
  labs(x = 'Contaminants', y = 'Counts')

# Display the plot
print(ggplot_object) # Replace ggplot_object with the name of your ggplot variable

# Save the plot without the legend
ggsave('contaminants_barplot_no_legend.png', width = 12, height = 8, bg = "white")


# Display the plot
ggsave('contaminants_barplot_legend_inside.png', width = 12, height = 8, bg = "white")



# only chemical

# Load the dataset
setwd("C:/Users/ACMEAFRINJAHAN/Documents/Groundwater files- ACME")
df <- read_excel("summary table.xlsx", sheet = 'Sheet4')  


# Filter for the contaminants in columns 7 to 15
contaminants <- df[, 7:15]  

# the sum of each contaminant
contaminant_counts <- colSums(contaminants, na.rm = TRUE)

# Convert to a data frame for plotting
contaminant_counts_df <- data.frame(Contaminant = names(contaminant_counts), Counts = contaminant_counts)

# Reordering the dataframe to make sure 'others' is last
contaminant_counts_df$Contaminant <- factor(contaminant_counts_df$Contaminant,
                                            levels = c(names(sort(contaminant_counts[contaminant_counts_df$Contaminant != 'others'], decreasing = TRUE)), 'others'))

custom_labels <- setNames(nm = levels(contaminant_counts_df$Contaminant),
                          obj = levels(contaminant_counts_df$Contaminant))
# Replace the label for "NO3-N" with a string that ggplot can parse as an expression
custom_labels[custom_labels == "NO3-N"] <- expression(NO[3]*"-N")
# Define a vector of colors for the bars
colors <- c("red", "#1874CD", "#228B22", "purple", "#FFA500", "navy","#8B1A1A","#ff61c3","grey")
ggplot(contaminant_counts_df, aes(x = Contaminant, y = Counts, fill = Contaminant)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = colors) +
  scale_x_discrete(labels = custom_labels) +  # Use the custom labels here
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", colour = "black"),
    plot.background = element_rect(fill = "white", colour = "white"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
    axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.line = element_line(color = "black"),
    legend.position = "none",  # Remove the legend
    legend.text = element_blank(),
    legend.title = element_blank(),
    legend.key.size = unit(0, 'lines')  # Effectively remove legend keys
  ) +
  guides(fill = guide_legend(override.aes = list(shape = 21, size = 2))) + # Change shape to adjust size
  labs(x = 'Contaminants', y = 'Counts')

# Display the plot
print(ggplot_object) # Replace ggplot_object with the name of your ggplot variable

# Save the plot without the legend
ggsave('contaminants_barplot_no_legend.png', width = 12, height = 8, bg = "white")


#gangwon-do water quality pararmeter chart



#wonju only
library(readxl)
library(ggplot2)
library(dplyr)
library(forcats)
library(maps)
library(mapdata)

# Load the dataset
df <- read_excel("wonju_parameter.xlsx", sheet = 'Sheet1')

# Filter for the contaminants in columns 2 to 11
contaminants <- df[, 2:11]

# the sum of each contaminant
contaminant_counts <- colSums(contaminants, na.rm = TRUE)

# Convert to a data frame for plotting
contaminant_counts_df <- data.frame(Contaminant = names(contaminant_counts), Counts = contaminant_counts)

# Define the custom order
custom_order <- c("T.C.", "E.C.", "pH", "General_Bacteria", "F.C.", "NO3.N", "Turbidity", "As", "F", "Others")

# Reordering the dataframe to follow the custom order
contaminant_counts_df$Contaminant <- factor(contaminant_counts_df$Contaminant, levels = custom_order)

# Define custom labels for the x-axis
custom_labels <- setNames(nm = custom_order, obj = custom_order)
custom_labels[custom_labels == "NO3.N"] <- expression(NO[3]*"-N")

# Define a vector of colors for the bars
colors <- c("red", "#1874CD", "green", "purple", "brown", "grey", "navy", "seagreen", "maroon2", "darkorange1")

# Create the bar plot
bar_plot <- ggplot(contaminant_counts_df, aes(x = Contaminant, y = Counts, fill = Contaminant)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = colors) +
  scale_x_discrete(labels = custom_labels) +  # Use the custom labels here
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", colour = "black"),
    plot.background = element_rect(fill = "white", colour = "white"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
    axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.line = element_line(color = "black"),
    legend.position = "none",  # Remove the legend
    legend.text = element_blank(),
    legend.title = element_blank(),
    legend.key.size = unit(0, 'lines')  # Effectively remove legend keys
  ) +
  guides(fill = guide_legend(override.aes = list(shape = 21, size = 2))) + # Change shape to adjust size
  labs(x = 'Contaminants', y = 'Counts')

# Print the plot
print(bar_plot)

# Save the plot without the legend
ggsave('contaminants_barplot_no_legend.png', plot = bar_plot, width = 12, height = 8, bg = "white")

# Save the plot with the legend inside
ggsave('contaminants_barplot_legend_inside.png', plot = bar_plot + theme(legend.position = "right"), width = 12, height = 8, bg = "white")
