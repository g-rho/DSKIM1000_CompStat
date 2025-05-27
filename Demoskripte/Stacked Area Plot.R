#library(tidyverse)  .
#library(readr)       
#library(ggplot2)
#library(dplyr)

url <- "https://ourworldindata.org/grapher/global-energy-substitution.csv?v=1&csvType=full&useColumnShortNames=false"
energy <- read_csv(url, show_col_types = FALSE)

# ------------------------------
# 1) Nur die benötigten Spalten
# ------------------------------
raw_data <- energy[ , c(3, 4:13) ]   # 3 = "Year", 4–13 = die 10 Quell-Spalten
  #select(Year, 4:13)            # 76 Zeilen × 11 Spalten (Jahr + 10 Quellen)

# ----------------------------------------
# 2) Daten
# ------------------------------

time  <- rep(raw_data$Year,                    # jeden Jahreswert …
             times = ncol(raw_data) - 1)       # … so oft wiederholen wie es Quell-Spalten gibt (10)
# → 76 × 10 = 760 Werte

value <- as.vector(                            # Matrix flach in Vektor umwandeln
  as.matrix(
    raw_data[ , -1]                   # alle Spalten außer Year (die 10 Quellen)
  )
)
# → 760 Zahlen: zuerst komplette Spalte 2, dann Spalte 3, … (spaltenweise Aufklappen)

group <- rep(names(raw_data)[-1],              # Spaltennamen der 10 Quellen
             each = nrow(raw_data))            # jeden Namen 76-mal hintereinander wiederholen
# → 760 Zeichen: Source-Label passt 1-zu-1 zum korrespondierenden value

data  <- data.frame(time, value, group)        # alles in ein Data-Frame packen
# 760 Zeilen × 3 Spalten (time | value | group) – bereit für ggplot2, plotly, …




# ------------------------------------------------------------
# 3) Gestapelten Area-Plot zeichnen
# ------------------------------------------------------------
ggplot(data,aes(x = time, y = value, fill = group)) +
  geom_area() +
  labs(x = "Jahr", y = "TWh", fill = "Quelle")

