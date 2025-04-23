###########################################################################
#Erste Möglichkeit mit csv Datei
###########################################################################

# Benötigte Pakete laden
library(sf)
library(ggplot2)
library(dplyr)
library(rnaturalearth)

#Deutschlandkarte laden
#Bibliothek rnaturalearth
germany <- ne_states(country = "Germany", returnclass = "sf")
germany
#Einlesen der CSV
data <- read.table("C:/Users/morit/Desktop/Master/2. Semester/Computergestützte Statistik/Gero/Vortrag/statistic_id1256_abwanderung-ins-ausland-in-den-bundeslaendern-2023.csv", header = T, sep = ";")

#Abwadnerungsdaten mit Geodaten verknüpfen
germany <- germany %>%
  left_join(data, by = "name")

#Choropleth-Karte erstellen
ggplot(data = germany) +
  geom_sf(aes(fill = Abwanderung)) +
  scale_fill_gradient(name = "Abwanderung",
                      low = "orange", high = "darkblue") +
  labs(title = "Abwanderung aus Bundesländern 2023") +
  theme_minimal()

#Zweite Tabelle einlesen
data2 <- read.table("C:/Users/morit/Desktop/Master/2. Semester/Computergestützte Statistik/Gero/Vortrag/statistic_id71085_einwohnerzahl-der-bundeslaender-in-deutschland-2023.csv", header = T, sep = ";")

#Namensänderung durch Fehlerhaftes Einlesen
data2$name[3] <- "Baden-Württemberg"
data2$name[12]<- "Thüringen"

#joinen der Tabellen
germany <- germany %>%
  left_join(data2, by = "name")
#Errechnen des Verhältnis von Abwanderung und Einwohnern
germany$AbwanderungProzent <- (germany$Abwanderung/germany$Einwohner)*100

#Choropleth-Karte erstellen
ggplot(data = germany) +
  geom_sf(aes(fill = AbwanderungProzent)) +
  scale_fill_gradient(name = "Abwanderung in Relation zu Einwohnerzahl (Prozent)",
                      low = "orange", high = "darkblue") +
  labs(title = "Abwanderung zu Einwohnerzahl aus Bundesländern 2023") +
  theme_minimal()




###########################################################################
#Zweite Möglichkeit mit geo.json Datei
###########################################################################

# Pakete laden
library(sf)
library(ggplot2)
library(dplyr)
library(viridis)
library(scales)

#Datensatzladen
co2_data <- read.csv("C:/Users/morit/Desktop/Master/2. Semester/Computergestützte Statistik/Gero/Vortrag/owid-co2-data.csv",sep = ",",header = T)

#nach 2021 Filtern
co2_latest <- co2_data %>%
  filter(year == 2021, !is.na(co2), nchar(iso_code) == 3) %>%
  select(iso_code, country, co2)

#GEOJSON Downloaden
# Donwloaden der Karte
tmp_geojson <- tempfile(fileext = ".geojson")#erstellt eine temporaere Datei die anschließend eingelesen wird
download.file(
  "https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json",#spezieller Dateityp
  tmp_geojson
)
world_sf <- read_sf(tmp_geojson)
world_sf[1,1]$geometry

#Mergen der beiden Tabllen
world_merged <- world_sf %>%
  left_join(co2_latest, by = c("id" = "iso_code"))
co2_latest
world_merged

#Für Log-Skala fehlende Werte als sehr klein einstellen
world_merged$co2[is.na(world_merged$co2)] <- 0.01

#Plotten
ggplot(world_merged) +  # Starte ggplot mit dem sf-Datensatz, der Ländergrenzen enthält
  geom_sf(aes(fill = co2), color = "white", linewidth = 0.1) +#Zeichne Ländergrenzen, färbe nach CO2-Werten
  scale_fill_viridis_c(  #Farbskala mit viridis
    trans = "log",  #logarithmische Skala, für große Werteunterschiede
    name = "CO2-Emissionen\n(Mio. Tonnen)",
    option = "inferno",#Farbverlauf "plasma" aus dem viridis-Paket  "veridis", "plasma", "inferno"
    labels = label_number(scale_cut = cut_short_scale()),#z.B. 1k statt 1.000 – kürzere Zahlenformate
    guide = guide_colorbar(#Gestaltung der Farbleiste (Legende)
      title.position = "top",
      title.hjust = 0.5,
      barwidth = unit(10, "cm"),
      barheight = unit(0.5, "cm") 
    )
  ) +
  labs(
    title = "Globale CO2-Emissionen pro Land (2021)",
    subtitle = "Logarithmische Skala zur besseren Darstellung",  
    caption = "Datenquelle: Our World in Data" 
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),    
    plot.subtitle = element_text(size = 12),               
    plot.caption = element_text(size = 9),                 
    legend.position = "bottom",                           
    legend.title = element_text(size = 10),                  
    legend.text = element_text(size = 8)                     
  )


