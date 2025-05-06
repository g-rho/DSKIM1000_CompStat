#install.packages("treemap") Paket 'treemap'installieren
# library paket laden
library(treemap)

data <- read.csv2("dax.csv") # Die CSV-Datei "dax.csv" wird eingelesen und in das Dataframe 'data' gespeichert
data$Marktkapitalisierung = as.numeric(as.character(data$Marktkapitalisierung)) 
# Die Spalte 'Marktkapitalisierung' wird von Faktor zu numerischem Wert umgewandelt, damit sie für Berechnungen verwendet werden kann

# treemap
treemap(data,
        force.print.labels = TRUE, #damit wird sichergestellt, dass alle labels angezeigt werden.
        index="Aktiengesellschaft", # Erstelle eine Treemap, die die Aktiengesellschaften visualisiert.
        vSize="Marktkapitalisierung",# Die Größe der Rechtecke.
        type="index" # Die Aktiengesellschaften werden anhand ihres Namens im Index dargestellt.
)


# treemap Gruppen, Untergruppe
treemap(data,
        force.print.labels = TRUE, #damit wird sichergestellt, dass alle labels angezeigt werden.
        index=c("Sitz","Aktiengesellschaft"), # Erstelle eine Treemap, die die Daten nach den Kategorien "Sitz" und "Aktiengesellschaft" gruppiert.
        vSize="Marktkapitalisierung", # Die Größe der einzelnen Rechtecke 
        type="index" # Darstellung der Hierarchie anhand der angegebenen Indizes
) 
#Treemap individuell anpassen (customize)
data_tree <- treemap(data,
        force.print.labels = TRUE, #damit wird sichergestellt, dass alle labels angezeigt werden.
        index=c("Sitz","Aktiengesellschaft"),# Erstelle eine Treemap, die die Daten nach den Kategorien "Sitz" und "Aktiengesellschaft" gruppiert. 
        vSize="Marktkapitalisierung",# Die Größe der einzelnen Rechtecke
        type="index",# Darstellung der Hierarchie anhand der angegebenen Indizes
        fontsize.labels = c(15, 12), #Größe der Beschriftungen.Größe für die Gruppe, Größe für die Untergruppe
        fontcolor.labels = c("#36486b", "#2A3132"), # Farbe der Beschriftungen 
        fontface.labels = c(2, 1), # Schriftart der Beschriftungen: 1,2,3,4 für normal,fett, kursiv,fett-kursiv
        bg.labels = c("transparent"), #Hintergrundfarbe der Beschriftungen
        align.labels = list(
          c("left", "top"),
          c("center", "center")
        ), #Wo die Beschriftungen im Rechteck platziert werden sollen Gruppe: links oben und Untergruppe zentriert sowohl horizontal auch vertikal.
        inflate.labels = F, # Wenn es wahr ist, sind die Beschriftungen größer, wenn das Rechteck größer ist. 
        border.col = c("black", "white"), # Farbe der Grenzen von Gruppen, Untergruppen
        border.lwds = c(4, 2), # Breite der Farbe
        palette = "Set3", #Farbpalette aus den RColorBrewer-Voreinstellungen oder eine eigene erstellen. 
        title = "Deutscher Aktienindex (DAX)", # Titel 
        fontsize.title = 22, # Schriftgröße des Titels 
        aspRatio = 5/3 # Das Seitenverhältnis (Breite zu Höhe))
) 

#Interaktives Treemap-Diagramm mit dem Package "d3treeR"

#Installiere zuerst das Package "d3treeR"
#install.packages("remotes")
#remotes::install_github("d3treeR/d3treeR",force = TRUE)
library(d3treeR)

#Erzeuge die interaktive Treemap
#Als Parameter wird ein treemap übergeben, hier der Treemap "data_tree"
#Der Parameter "rootname" ist die Bezeichnung der Treemap, hier passenderweise "DAX"
d3tree2(data_tree , width = "100%", height ="400px", rootname = "DAX")





