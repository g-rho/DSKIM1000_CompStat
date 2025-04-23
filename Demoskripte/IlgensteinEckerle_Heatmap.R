# Erik Ilgenstein
# Achim Eckerle

# Heatmap

##############################################################################
# Beispiel: Heatmap eines Korrelationsmatrix mit mtcars
##############################################################################

# Basis-Pakete laden
# install.packages("reshape2")
# install.packages("ggplot2")
# install.packages("plotly")
# install.packages("lattice")
# install.packages("RColorBrewer")

library(reshape2)    # für melt()
library(ggplot2)     # für geom_tile()
library(plotly)      # für Interaktivität
library(lattice)     # levelplot()
library(RColorBrewer)# Farbpaletten

# --------------------------------------------------------------------
# Datensatz und Korrelationsmatrix vorbereiten
# --------------------------------------------------------------------
data("mtcars")   # Lädt den Datensatz mtcars (32 Zeilen, 11 Spalten)
head(mtcars)

# Spaltennamen verständlicher machen
colnames(mtcars) <- c(
  "Miles per gallon",
  "Zylinder",
  "Hubraum",
  "Leistung",
  "Gewicht_vorher",       # drat = Rear axle ratio (optional anpassen)
  "Gewicht",              # wt = Gewicht (in 1000 lbs)
  "Beschleunigung",
  "Motor",
  "Getriebe",
  "Gänge",
  "Vergaser"
)

head(mtcars)

# Wir berechnen die Korrelationsmatrix
mtcars_cor <- cor(mtcars)

# Struktur der Korrelationsmatrix ansehen
print(mtcars_cor)



# --------------------------------------------------------------------
# Heatmap mit Base R
# --------------------------------------------------------------------
# Einfachster Aufruf
heatmap(mtcars_cor)

# Ohne Clustering:
heatmap(mtcars_cor, 
        Rowv = NA, 
        Colv = NA,
        main = "Heatmap (einfach) mtcars")

# Mit eigener Farbpalette (z.B. von blau bis weiß bis rot)
# hier mit 50 Abstufungen
heatmap(
  mtcars_cor,
  Rowv = NA, 
  Colv = NA,
  col = colorRampPalette(c("blue", "white", "red"))(50),
  main = "Heatmap (einfach) mtcars"
)


# --------------------------------------------------------------------
# Heatmap mit ggplot2
# --------------------------------------------------------------------
# Für ggplot2 müssen wir die Matrix in ein 'long format' schmelzen
mtcars_melt <- melt(mtcars_cor)
head(mtcars_melt)

# Spalten heißen jetzt: Var1 (Zeile), Var2 (Spalte), value (Korrelationswert)

# Basische Darstellung
ggplot(mtcars_melt, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  labs(
    title = "Heatmap (ggplot2) mtcars",
    x = "",
    y = ""
  )

# Farbskala anpassen, z.B. für einen Korrelations-Plot nimmt man gerne
# eine divergierende Palette (Mitte=0):
ggplot(mtcars_melt, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red", midpoint = 0
  ) +
  labs(
    title = "Heatmap (ggplot2) mtcars",
    x = "Eigenschaften",
    y = "Eigenschaften"
  ) +
  theme_minimal()

# --------------------------------------------------------------------
# 4) Interaktive Heatmap mit plotly
# --------------------------------------------------------------------
p <- ggplot(mtcars_melt, aes(Var2, Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(
    title = "Heatmap interaktiv (plotly) mtcars",
    x = "Eigenschaften",
    y = "Eigenschaften"
  ) +
  theme_minimal()

# Einfach per ggplotly() interaktiv machen:
ggplotly(p)

# --------------------------------------------------------------------
# 5) Heatmap mit lattice (levelplot())
# --------------------------------------------------------------------
levelplot(
  mtcars_cor,
  main = "Heatmap (levelplot) mtcars",
  xlab = "Eigenschaften",
  ylab = "Eigenschaften",
  col.regions = colorRampPalette(brewer.pal(9, "RdBu"))(100)
)

