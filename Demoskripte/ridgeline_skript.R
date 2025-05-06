# Pakete installieren
install.packages(c("ggplot2", "ggridges", "readr"))

# Pakete laden
library(ggplot2)
library(ggridges)
library(readr)

# Daten importieren
penguins <- read_csv("https://raw.githubusercontent.com/allisonhorst/palmerpenguins/master/inst/extdata/penguins.csv")
penguins_clean <- na.omit(penguins)

# Ridgeline-Plot: Körpermasse nach Art und Insel
plot_species_island <- ggplot(penguins_clean, aes(x = body_mass_g, y = species, fill = island)) +
  geom_density_ridges(alpha = 0.8, scale = 1.2) +
  theme_minimal() +
  labs(
    title = "Körpermasse der Pinguinarten auf verschiedenen Inseln",
    subtitle = "Vergleich der Arten und Inseln",
    x = "Körpermasse (Gramm)",
    y = "Pinguinart",
    fill = "Insel"
  ) +
  scale_fill_brewer(palette = "Set2")

# Plot anzeigen (Körpermasse nach Art und Insel)
print(plot_species_island)

# Adelie-Pinguine auswählen
adelie_penguins <- subset(penguins_clean, species == "Adelie")

# Ridgeline-Plot: Körpermasse Adelie-Pinguine nach Geschlecht und Insel
plot_adelie_sex_island <- ggplot(adelie_penguins, aes(x = body_mass_g, y = interaction(sex, island), fill = sex)) +
  geom_density_ridges(alpha = 0.8, scale = 1.2) +
  theme_minimal() +
  labs(
    title = "Körpermasse der Adelie-Pinguine",
    subtitle = "Vergleich nach Geschlecht und Insel",
    x = "Körpermasse (Gramm)",
    y = "Geschlecht & Insel",
    fill = "Geschlecht"
  ) +
  scale_fill_brewer(palette = "Pastel1")

# Plot anzeigen (Körpermasse Adelie-Pinguine nach Geschlecht und Insel)
print(plot_adelie_sex_island)