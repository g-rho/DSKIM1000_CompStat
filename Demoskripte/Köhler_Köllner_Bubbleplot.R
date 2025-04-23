################################################################################
#                           BUBBLE PLOT (BLASENDIAGRAMM)                       #
################################################################################

## 1. Was ist ein Bubble Plot?
# ------------------------------------------------------------------------------
# - wie ein Scatterplot, aber mit einer dritten Dimension durch Punktgröße
# - 3 numerische Variablen:
#     ▪ X-Achse
#     ▪ Y-Achse
#     ▪ Punktgröße


################################################################################
#                           2. EINFACHES BEISPIEL                              #
################################################################################

## 2.1 Pakete installieren und laden
# ------------------------------------------------------------------------------
# install.packages("ggplot2")
# install.packages("dplyr")

library(ggplot2)
library(dplyr)

## 2.2 Beispiel-Daten für einen einfachen Bubble Plot
# ------------------------------------------------------------------------------
df <- data.frame(
  x = c(1, 2, 3, 4, 5),                 
  y = c(10, 15, 13, 20, 18),            
  size = c(5, 10, 8, 12, 6),            
  label = c("A", "B", "C", "D", "E")    
)


## 2.3 Einfacher Bubble Plot
# ------------------------------------------------------------------------------
ggplot(df, aes(x = x, y = y, size = size)) +
  geom_point(alpha = 0.6)


## 2.4 Anpassungen: Farbe, Skalierung, Labels, Layout
# ------------------------------------------------------------------------------
ggplot(df, aes(x = x, y = y, size = size)) +  # Mapping: x- und y-Achse + Blasengröße
  geom_point(alpha = 0.6, color = "steelblue") +  # Punkte halbtransparent und blau
  scale_size(range = c(3, 15)) + # Punktgröße skalieren 
  labs(                                            
    title = "Einfacher Bubble Plot", # Haupttitel
    x = "X-Achse",  # Beschriftung x-Achse
    y = "Y-Achse",  # Beschriftung y-Achse                              
    size = "Größe" # Legendentitel für Punktgröße
  ) +
  theme_minimal() # schlichtes Design


################################################################################
#                           3. ERWEITERTES BEISPIEL                            #
################################################################################

## 3.1 Weitere Pakete für optionale Anpassungen
# ------------------------------------------------------------------------------
# install.packages("hrbrthemes")
# install.packages("viridis")
# install.packages("ggrepel")
# install.packages("plotly")

library(hrbrthemes) # für theme_ipsum()
library(viridis) # für die Farbpalette
library(ggrepel) # für Beschriftung der Bubbles
library(plotly) # für interaktive Bubble Plots

## 3.2 Realwelt-Beispiel: gapminder Datensatz
# ------------------------------------------------------------------------------
# - Quelle: https://www.data-to-viz.com/graph/bubble.html 
# - Ziel: Visualisierung von BIP, Lebenserwartung und Bevölkerung (2007)

# install.packages("gapminder")
library(gapminder)

# Datensatz anpassen
data <- gapminder %>%
  filter(year == 2007) %>%  # Jahr 2007 filtern
  select(-year) %>%  # "year"-Spalte entfernen
  mutate(
    pop = round(pop / 1000000, 2), # Bevölkerung in Millionen (2 Dezimalstellen)
    gdpPercap = round(gdpPercap, 0), # BIP pro Kopf runden (volle Zahlen)
    lifeExp = round(lifeExp, 1), # Lebenserwartung auf 1 Stelle runden
    country = factor(country, country) # Länder als geordneter Faktor (für Plotreihenfolge)
  ) %>%
  arrange(desc(pop)) # nach Bevölkerungsgröße sortieren


# Bubble Plot erstellen
ggplot(data, aes(x = gdpPercap, y = lifeExp, size = pop, color = continent)) + # Achsen zuordnen
  geom_point(alpha = 0.7) + # Punkte mit leichter Transparenz
  scale_size(range = c(1.4, 19), name = "Population (M)") +  # Punktgrößen-Skalierung + Legendenname
  scale_color_viridis(discrete = TRUE) + # Farbpalette
  theme_minimal() +
  theme(legend.position = "right") + # Legende rechts anzeigen
  labs(  
    title = "Life expectancy vs. GDP per capita (2007)", # Überschrift
    x = "GDP per capita",  # Beschriftung x-Achse
    y = "Life expectancy"  # Beschriftung y-Achse
  )

## Interpretation:
# - klarer Zusammenhang: höheres BIP → höhere Lebenserwartung
# - afrikanische Länder unten links, europäische Länder oben rechts
# - China und Indien als große Kreise auffällig durch hohe Bevölkerung


## Text einfügen
# Daten vorbereiten
tmp <- data %>%
  mutate(
    annotation = case_when( # neue Spalte: automatisch bestimmte Länder zu markieren (beschriften).case_when() prüft mehrere Bedingungen: 
      gdpPercap > 5000 & lifeExp < 60 ~ "yes", # Länder mit relativ hohem BIP aber niedriger Lebenserwartung
      lifeExp < 30 ~ "yes", # Länder mit extrem niedriger Lebenserwartung
      gdpPercap > 40000 ~ "yes" #Länder mit extrem hohem BIP pro Kopf
    )  # Wenn eine dieser Bedingungen zutrifft, wird "yes" in der Spalte annotation eingetragen, sonst bleibt es NA.
  ) 

# Plot
ggplot( tmp, aes(x=gdpPercap, y=lifeExp, size = pop, color = continent)) +             #Hier wird wieder der Bubble Plot erstellt: x = gdpPercap: BIP pro Kopf; y = lifeExp: Lebenserwartung; size = pop: Punktgröße = Bevölkerung; color = continent: Farbe = Kontinent
  geom_point(alpha=0.7) +                                                              #Zeichnet die Punkte mit 70% Deckkraft (leichte Transparenz für bessere Lesbarkeit).
  scale_size(range = c(1.4, 19), name="Population (M)") +                              #Setzt die Größenskala für die Punkte – von sehr klein (1.4) bis groß (19). Außerdem wird die Legende passend benannt: „Population (M)“.
  scale_color_viridis(discrete=TRUE) +                                                 #Verwendet die Viridis-Farbpalette für kategoriale (diskrete) Werte -> gut unterscheidbar, farbenblind-freundlich
  theme_minimal() +                                                                    #theme_ipsum() sorgt für ein cleanes, modernes Design (aus dem Paket hrbrthemes). ACHTUNG! Kann zu Problemen kommen, weil die Standard-Schriftart IBM Plex Sans nicht umbedingt auf Windows vorinstalliert ist. Entweder seperat installieren, eine andere Schriftart angeben (theme_ipsum(base_family = "Arial")), oder anderes Theme benutzen (z.b.theme_minimal() oder theme_light())
  theme(legend.position="none") +                                                      #legend.position="none" entfernt alle Legenden (z.B. für Farbe, Größe), um den Plot minimalistischer zu halten.
  geom_text_repel(data=tmp %>% filter(annotation=="yes"), aes(label=country), size=4 ) #Hier ist der Unterschied! Nur bestimmte Länder werden automatisch beschriftet, basierend auf der annotation-Spalte.
#geom_text_repel = Text-Labels, die sich nicht überlappen


################################################################################
#                         4. EIGNUNG EINES BUBBLE PLOTS                        #
################################################################################

## 4.1 Wann ist ein Bubble Plot sinnvoll?
# ------------------------------------------------------------------------------
# - drei numerische Variablen darstellen 
# - Daten mit starkem Kontrast bei der dritten Variable 
# - wenn sich die Blasengrößen deutlich unterscheiden, wird der Effekt klar sichtbar. 
#    --> Beispiel: Bevölkerung von China vs. Schweiz
# - geringe bis mittlere Datenmenge (< 100 Punkten)


## 4.2 Wann ist ein Bubble Plot NICHT sinnvoll?
# ------------------------------------------------------------------------------
# - wenn die dritte Variable nicht wichtig ist (stattdessen Scatterplot)
# - wenn die Blasengröße schwer vergleichbar ist
#    --> Bubble-Fläche als Maßstab verwenden, nicht den Durchmesser
# - wenn man viele kleine Unterschiede zeigen will
# - bei sehr vielen Datenpunkte 
#    --> Lösungen: filtern, clustern oder andere Visualisierungen wie Heatmaps, Density Plots, Facets
# - wenn alle drei Variablen stark korreliert sind

 
################################################################################
#                          5. INTERAKTIVER BUBBLE PLOT                         #
################################################################################

p <- data %>%

  # Text für tooltip vorbereiten
  mutate(text = paste("Country: ", country, "\nPopulation (M): ", pop, "\nLife Expectancy: ", lifeExp, "\nGdp per capita: ", gdpPercap, sep="")) %>%
  
  # Bubble Plot erstellen
  ggplot( aes(x=gdpPercap, y=lifeExp, size = pop, color = continent, text=text)) +
  geom_point(alpha=0.7) +
  scale_size(range = c(1.4, 19), name="Population (M)") +
  scale_color_viridis(discrete=TRUE, guide=FALSE) +
  theme_ipsum() +
  theme(legend.position="none")

# turn ggplot interactive with plotly
pp <- ggplotly(p, tooltip="text")
pp

