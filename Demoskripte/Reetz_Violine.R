# Library (müssen vorher installiert werden)
library(ggplot2)

# Classification project on heart disease dataset [1]
heart_data <- read.csv("https://raw.githubusercontent.com/massaid01/HeartDisease/refs/heads/main/heart_2020_cleaned.csv")

# template aus Most basic violin chart [2]
# Verteilung des BMI nach Geschlecht
ggplot(heart_data, aes(x=Sex, y=BMI, fill=Sex)) + 
  geom_violin() + 
  theme_minimal() + 
  labs(title = "Verteilung des BMI nach Geschlecht")

# Verteilung des BMI nach ethnischer Herkunft und Geschlecht
ggplot(heart_data, aes(x=Race, y=BMI, fill=Sex)) + 
  geom_violin() + 
  theme_minimal() +
  labs(title = "Verteilung des BMI nach ethnischer Herkunft und Geschlecht")

# Mit Boxplot
ggplot(heart_data, aes(x = Sex, y = BMI, fill = Sex)) + 
  geom_violin() + 
  geom_boxplot(width = 0.1, color = "black") +  # Boxplot innerhalb der Violine
  theme_minimal() + 
  labs(title = "Verteilung des BMI nach Geschlecht")

# Quellen
# [1] https://github.com/massaid01/HeartDisease/tree/main
# [2] https://r-graph-gallery.com/95-violin-plot-with-ggplot2.html
