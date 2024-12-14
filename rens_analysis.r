# Proper Imports
install.packages("tidyverse")
library(ggplot2)

install.packages("gridExtra")
library(gridExtra)

install.packages("tidyverse")
library(tidyverse)

# Visual 1
# Data laden
data_text <- read.csv("/Users/rens/Github/Y2BA-Retake--Rens---Stijn-/Data/filtered_data_text_new.csv", encoding = "UTF-8")

# Filteren van de data
data_filtered <- subset(data_text, demo_role %in% c("Educator", "Student", "Supporting Staff"))

# Maak 'demo_ai_know' een factor met gespecificeerde levels
data_filtered$demo_ai_know <- factor(data_filtered$demo_ai_know, 
                                     levels = c("Extremely bad", "Somewhat bad", "Neither good nor bad", "Somewhat good", "Extremely good"))

# Voeg numerieke kolom toe voor berekeningen
data_filtered$demo_ai_know_numeric <- as.numeric(data_filtered$demo_ai_know)

# Bereken gemiddelde en mediaan per rol
average_awareness <- aggregate(demo_ai_know_numeric ~ demo_role, data=data_filtered, mean)
colnames(average_awareness) <- c("demo_role", "average_knowledge")

median_awareness <- aggregate(demo_ai_know_numeric ~ demo_role, data=data_filtered, median)
colnames(median_awareness) <- c("demo_role", "median_knowledge")

# Vergroten van tekst in de grafieken
median_graph <- ggplot(data=median_awareness, aes(x=demo_role, y=median_knowledge, fill=demo_role)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=sprintf("%.2f", median_knowledge)), vjust=-0.5, size=5) +  # Tekstlabels groter maken
  ylab("Median knowledge of AI") +
  ggtitle("Median knowledge of AI by Role") +
  theme_minimal(base_size = 14) +  # Basistekstgrootte verhogen
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),  # X-as tekst groter
    axis.text.y = element_text(size = 14),  # Y-as tekst groter
    axis.title = element_text(size = 16),   # Assentitels groter
    plot.title = element_text(size = 18, face = "bold")  # Grafiektitel groter
  ) +
  scale_fill_brewer(palette = "Set1")

average_graph <- ggplot(data=average_awareness, aes(x=demo_role, y=average_knowledge, fill=demo_role)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=sprintf("%.2f", average_knowledge)), vjust=-0.5, size=5) +  # Tekstlabels groter maken
  ylab("Average knowledge of AI") +
  ggtitle("Average knowledge of AI by Role") +
  theme_minimal(base_size = 14) +  # Basistekstgrootte verhogen
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),  # X-as tekst groter
    axis.text.y = element_text(size = 14),  # Y-as tekst groter
    axis.title = element_text(size = 16),   # Assentitels groter
    plot.title = element_text(size = 18, face = "bold")  # Grafiektitel groter
  ) +
  scale_fill_brewer(palette = "Set1")

# Grafieken naast elkaar tonen
grid.arrange(median_graph, average_graph, ncol=2)

# Visual 2
# Filter de data voor de relevante rollen
data_filtered <- subset(data_text, demo_role %in% c("Educator", "Student", "Supporting Staff"))

# Maak een factorvariabele voor demo_experience
data_filtered$experience <- factor(data_filtered$demo_experience, 
                                   levels = c("0 - 6 months", "1 - 2 years", "2 - 5 years", "5 - 10 years", "20 + years"))

# Converteer aware_everyday naar een numerieke schaal
awareness_numeric <- c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree")
data_filtered$aware_everyday <- factor(data_filtered$aware_everyday, levels = awareness_numeric)
data_filtered$aware_everyday <- as.numeric(data_filtered$aware_everyday)

# Gemiddelde en mediaan AI awareness berekenen
average_awareness <- aggregate(aware_everyday ~ demo_role + experience, data=data_filtered, mean)
median_awareness <- aggregate(aware_everyday ~ demo_role + experience, data=data_filtered, median)

# Functie om labels te formatteren
format_labels <- function(x) {
  return(sprintf("%.2f", x))
}

# Barplot voor gemiddelde AI awareness
average_plot <- ggplot(data=average_awareness, aes(x=demo_role, y=aware_everyday, fill=experience)) +
  geom_bar(stat="identity", position=position_dodge(width=0.8), width=0.7) +
  geom_text(aes(label=format_labels(aware_everyday)), position=position_dodge(width=0.8), vjust=-0.5, size=5) +  # Grotere tekst
  ylab("Average Awareness of AI") +
  ggtitle("Average AI Awareness by Role and Experience") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),  # Grotere tekstlabels
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18, face = "bold")) +
  scale_fill_brewer(palette = "Set1")

# Barplot voor mediaan AI awareness
median_plot <- ggplot(data=median_awareness, aes(x=demo_role, y=aware_everyday, fill=experience)) +
  geom_bar(stat="identity", position=position_dodge(width=0.8), width=0.7) +
  geom_text(aes(label=format_labels(aware_everyday)), position=position_dodge(width=0.8), vjust=-0.5, size=5) +  # Grotere tekst
  ylab("Median Awareness of AI") +
  ggtitle("Median AI Awareness by Role and Experience") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),  # Grotere tekstlabels
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18, face = "bold")) +
  scale_fill_brewer(palette = "Set1")

# Grafieken naast elkaar tonen
grid.arrange(average_plot, median_plot, ncol=2)

# Converteer demo_ai_know naar een numerieke schaal
knowledge_numeric <- c("Extremely bad", "Somewhat bad", "Neither good nor bad", "Somewhat good", "Extremely good")
data_filtered$demo_ai_know <- factor(data_filtered$demo_ai_know, levels = knowledge_numeric)
data_filtered$demo_ai_know <- as.numeric(data_filtered$demo_ai_know)

# Gemiddelde en mediaan AI kennis berekenen
average_knowledge <- aggregate(demo_ai_know ~ demo_role + experience, data=data_filtered, mean)
median_knowledge <- aggregate(demo_ai_know ~ demo_role + experience, data=data_filtered, median)

# Barplot voor gemiddelde AI kennis
average_knowledge_plot <- ggplot(data=average_knowledge, aes(x=demo_role, y=demo_ai_know, fill=experience)) +
  geom_bar(stat="identity", position=position_dodge(width=0.8), width=0.7) +
  geom_text(aes(label=format_labels(demo_ai_know)), position=position_dodge(width=0.8), vjust=-0.5, size=5) +  # Grotere tekst
  ylab("Average Knowledge of AI") +
  ggtitle("Average AI Knowledge by Role and Experience") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),  # Grotere tekstlabels
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18, face = "bold")) +
  scale_fill_brewer(palette = "Set1")

# Barplot voor mediaan AI kennis
median_knowledge_plot <- ggplot(data=median_knowledge, aes(x=demo_role, y=demo_ai_know, fill=experience)) +
  geom_bar(stat="identity", position=position_dodge(width=0.8), width=0.7) +
  geom_text(aes(label=format_labels(demo_ai_know)), position=position_dodge(width=0.8), vjust=-0.5, size=5) +  # Grotere tekst
  ylab("Median Knowledge of AI") +
  ggtitle("Median AI Knowledge by Role and Experience") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),  # Grotere tekstlabels
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18, face = "bold")) +
  scale_fill_brewer(palette = "Set1")

# Grafieken naast elkaar tonen
grid.arrange(average_knowledge_plot, median_knowledge_plot, ncol=2)

# Visual 3
# Laad de benodigde bibliotheken
library(ggplot2)
library(dplyr)
library(stats)

# Filter de data voor studenten
data_student <- data_text %>% filter(demo_role == "Student")

# Verwijder de rijen met missende waarden in de relevante kolommen
data_student <- data_student %>%
  filter(!is.na(Q226), !is.na(demo_ai_know))

# Verwerk de data door de cijfers om te zetten naar numerieke waarden
data_student <- data_student %>%
  mutate(Q226_numeric = case_when(
    Q226 == "Under 6" ~ 5.5,
    Q226 == "Between 6 and 7" ~ 6.5,
    Q226 == "Between 7 and 8" ~ 7.5,
    Q226 == "Between 8 and 9" ~ 8.5,
    Q226 == "Between 9 and 10" ~ 9.5,
    TRUE ~ NA_real_  # Handle other cases or missing values as NA
  ))

# Zorg ervoor dat demo_ai_know een factor of numerieke waarde is voor de regressie
data_student$demo_ai_know <- factor(data_student$demo_ai_know,
                                    levels = c("Extremely bad", "Somewhat bad", "Neither good nor bad", 
                                               "Somewhat good", "Extremely good"))

# Voer de lineaire regressie uit
lm_model <- lm(Q226_numeric ~ demo_ai_know, data = data_student)

# Haal de fitted waarden en residuen uit het model
data_student$fitted_values <- lm_model$fitted.values
data_student$residuals <- lm_model$residuals

# Maak de plot van de residuen versus fitted waarden
ggplot(data_student, aes(x = fitted_values, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Residuals") +
  ggtitle("Residuals vs Fitted Values") +
  theme_minimal()