# Proper Imports
install.packages("tidyverse")
library(ggplot2)

install.packages("gridExtra")
library(gridExtra)

install.packages("tidyverse")
library(tidyverse)

# Visual 1
# Load data
data_text <- read.csv("/Users/rens/Github/Y2BA-Retake--Rens---Stijn-/Data/filtered_data_text_new.csv", encoding = "UTF-8")

# Filter the data
data_filtered <- subset(data_text, demo_role %in% c("Educator", "Student", "Supporting Staff"))

# Make 'demo_ai_know' a factor with specified levels
data_filtered$demo_ai_know <- factor(data_filtered$demo_ai_know, 
                                     levels = c("Extremely bad", "Somewhat bad", "Neither good nor bad", "Somewhat good", "Extremely good"))

# Add numeric column for calculations
data_filtered$demo_ai_know_numeric <- as.numeric(data_filtered$demo_ai_know)

# Calculate average and median per role
average_awareness <- aggregate(demo_ai_know_numeric ~ demo_role, data=data_filtered, mean)
colnames(average_awareness) <- c("demo_role", "average_knowledge")

median_awareness <- aggregate(demo_ai_know_numeric ~ demo_role, data=data_filtered, median)
colnames(median_awareness) <- c("demo_role", "median_knowledge")

# Increase text size in the graphs
median_graph <- ggplot(data=median_awareness, aes(x=demo_role, y=median_knowledge, fill=demo_role)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=sprintf("%.2f", median_knowledge)), vjust=-0.5, size=5) +  # Enlarge text labels
  ylab("Median knowledge of AI") +
  ggtitle("Median knowledge of AI by Role") +
  theme_minimal(base_size = 14) +  # Increase base text size
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),  # Enlarge X-axis text
    axis.text.y = element_text(size = 14),  # Enlarge Y-axis text
    axis.title = element_text(size = 16),   # Enlarge axis titles
    plot.title = element_text(size = 18, face = "bold")  # Enlarge graph title
  ) +
  scale_fill_brewer(palette = "Set1")

average_graph <- ggplot(data=average_awareness, aes(x=demo_role, y=average_knowledge, fill=demo_role)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=sprintf("%.2f", average_knowledge)), vjust=-0.5, size=5) +  # Enlarge text labels
  ylab("Average knowledge of AI") +
  ggtitle("Average knowledge of AI by Role") +
  theme_minimal(base_size = 14) +  # Increase base text size
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),  # Enlarge X-axis text
    axis.text.y = element_text(size = 14),  # Enlarge Y-axis text
    axis.title = element_text(size = 16),   # Enlarge axis titles
    plot.title = element_text(size = 18, face = "bold")  # Enlarge graph title
  ) +
  scale_fill_brewer(palette = "Set1")

# Display graphs side by side
grid.arrange(median_graph, average_graph, ncol=2)

# Visual 2
# Filter data for relevant roles
data_filtered <- subset(data_text, demo_role %in% c("Educator", "Student", "Supporting Staff"))

# Create a factor variable for demo_experience
data_filtered$experience <- factor(data_filtered$demo_experience, 
                                   levels = c("0 - 6 months", "1 - 2 years", "2 - 5 years", "5 - 10 years", "20 + years"))

# Convert aware_everyday to a numeric scale
awareness_numeric <- c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree")
data_filtered$aware_everyday <- factor(data_filtered$aware_everyday, levels = awareness_numeric)
data_filtered$aware_everyday <- as.numeric(data_filtered$aware_everyday)

# Calculate average and median AI awareness
average_awareness <- aggregate(aware_everyday ~ demo_role + experience, data=data_filtered, mean)
median_awareness <- aggregate(aware_everyday ~ demo_role + experience, data=data_filtered, median)

# Function to format labels
format_labels <- function(x) {
  return(sprintf("%.2f", x))
}

# Bar plot for average AI awareness
average_plot <- ggplot(data=average_awareness, aes(x=demo_role, y=aware_everyday, fill=experience)) +
  geom_bar(stat="identity", position=position_dodge(width=0.8), width=0.7) +
  geom_text(aes(label=format_labels(aware_everyday)), position=position_dodge(width=0.8), vjust=-0.5, size=5) +  # Larger text
  ylab("Average Awareness of AI") +
  ggtitle("Average AI Awareness by Role and Experience") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),  # Larger text labels
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18, face = "bold")) +
  scale_fill_brewer(palette = "Set1")

# Bar plot for median AI awareness
median_plot <- ggplot(data=median_awareness, aes(x=demo_role, y=aware_everyday, fill=experience)) +
  geom_bar(stat="identity", position=position_dodge(width=0.8), width=0.7) +
  geom_text(aes(label=format_labels(aware_everyday)), position=position_dodge(width=0.8), vjust=-0.5, size=5) +  # Larger text
  ylab("Median Awareness of AI") +
  ggtitle("Median AI Awareness by Role and Experience") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),  # Larger text labels
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18, face = "bold")) +
  scale_fill_brewer(palette = "Set1")

# Display graphs side by side
grid.arrange(average_plot, median_plot, ncol=2)

# Convert demo_ai_know to a numeric scale
knowledge_numeric <- c("Extremely bad", "Somewhat bad", "Neither good nor bad", "Somewhat good", "Extremely good")
data_filtered$demo_ai_know <- factor(data_filtered$demo_ai_know, levels = knowledge_numeric)
data_filtered$demo_ai_know <- as.numeric(data_filtered$demo_ai_know)

# Calculate average and median AI knowledge
average_knowledge <- aggregate(demo_ai_know ~ demo_role + experience, data=data_filtered, mean)
median_knowledge <- aggregate(demo_ai_know ~ demo_role + experience, data=data_filtered, median)

# Bar plot for average AI knowledge
average_knowledge_plot <- ggplot(data=average_knowledge, aes(x=demo_role, y=demo_ai_know, fill=experience)) +
  geom_bar(stat="identity", position=position_dodge(width=0.8), width=0.7) +
  geom_text(aes(label=format_labels(demo_ai_know)), position=position_dodge(width=0.8), vjust=-0.5, size=5) +  # Larger text
  ylab("Average Knowledge of AI") +
  ggtitle("Average AI Knowledge by Role and Experience") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),  # Larger text labels
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18, face = "bold")) +
  scale_fill_brewer(palette = "Set1")

# Bar plot for median AI knowledge
median_knowledge_plot <- ggplot(data=median_knowledge, aes(x=demo_role, y=demo_ai_know, fill=experience)) +
  geom_bar(stat="identity", position=position_dodge(width=0.8), width=0.7) +
  geom_text(aes(label=format_labels(demo_ai_know)), position=position_dodge(width=0.8), vjust=-0.5, size=5) +  # Larger text
  ylab("Median Knowledge of AI") +
  ggtitle("Median AI Knowledge by Role and Experience") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),  # Larger text labels
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18, face = "bold")) +
  scale_fill_brewer(palette = "Set1")

# Display graphs side by side
grid.arrange(average_knowledge_plot, median_knowledge_plot, ncol=2)

# Visual 3
# Load required libraries
library(ggplot2)
library(dplyr)
library(stats)

# Filter data for students
data_student <- data_text %>% filter(demo_role == "Student")

# Remove rows with missing values in relevant columns
data_student <- data_student %>%
  filter(!is.na(Q226), !is.na(demo_ai_know))

# Process data by converting scores to numeric values
data_student <- data_student %>%
  mutate(Q226_numeric = case_when(
    Q226 == "Under 6" ~ 5.5,
    Q226 == "Between 6 and 7" ~ 6.5,
    Q226 == "Between 7 and 8" ~ 7.5,
    Q226 == "Between 8 and 9" ~ 8.5,
    Q226 == "Between 9 and 10" ~ 9.5,
    TRUE ~ NA_real_  # Handle other cases or missing values as NA
  ))

# Ensure demo_ai_know is a factor or numeric value for regression
data_student$demo_ai_know <- factor(data_student$demo_ai_know,
                                    levels = c("Extremely bad", "Somewhat bad", "Neither good nor bad", 
                                               "Somewhat good", "Extremely good"))

# Perform linear regression
lm_model <- lm(Q226_numeric ~ demo_ai_know, data = data_student)

# Extract fitted values and residuals from the model
data_student$fitted_values <- lm_model$fitted.values
data_student$residuals <- lm_model$residuals

# Create plot of residuals vs fitted values
ggplot(data_student, aes(x = fitted_values, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Residuals") +
  ggtitle("Residuals vs Fitted Values") +
  theme_minimal()
