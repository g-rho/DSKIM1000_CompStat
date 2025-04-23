# Hochschule Stralsund
# DSKIM1110 Statistische Grundlagen von Machine-Learning
# SoSe 2025
# 
# Eingereicht bei:
# Prof. Dr. G. Szepannek
# Fakultät für Wirtschaft
# 
# Eingereicht  durch:
# Muhterem Firatli
# Christian Delfs

# Boxplot

# Die Verwendung des Boxplot wird im Folgenden unter Verwendung von Wetterdaten 
# für die Station Barth und das Jahr 2023 demonstriert.
# 
# Es wird die R Standard Boxplot-Funktion sowie das Paket ggplot2 für die 
# Erstellung der Grafiken verwendet. 

# Daten

# DWD - Wetterdaten

# https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/daily/kl/historical/

# ID, Station-Name, URL
# 00183, Greifswald, https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/daily/kl/historical/tageswerte_KL_00298_19470101_20231231_hist.zip
# 00298, Barth, https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/daily/kl/historical/tageswerte_KL_00298_19470101_20231231_hist.zip

# Parameter-Beschreibung: ./tageswerte_KL_00298_19470101_20231231_hist/Metadaten_Parameter_klima_tag_00298.txt
# TMK;Tagesmittel der Temperatur °C
# TNK;Tagesminimum der Lufttemperatur in 2m Hoehe °C
# TXK;Tagesmaximum der Lufttemperatur in 2m Höhe °C
# FM;Tagesmittel der Windgeschwindigkeit m/s
# FX;Maximum der Windspitze m/s
# NM;Tagesmittel des Bedeckungsgrades
# RSK;tgl. Niederschlagshoehe mm
# SDK;Sonnenscheindauer Tagessumme Stunde

# Daten einlesen

w_barth <- read.table("./tageswerte_KL_00298_19470101_20231231_hist/produkt_klima_tag_19470101_20231231_00298.txt", sep = ";", header = T)
w_greifswald <- read.table("./tageswerte_KL_00183_19470101_20231231_hist/produkt_klima_tag_19470101_20231231_00183.txt", sep = ";", header = T)

# Information über Variablen
str(w_barth)

# Statistische Kennzahlen zu Werten
summary(w_barth)

# Sämtliche Merkmale enthalten zu einen geringen Teil den Wert "-999". 
# Betroffene Beobachtungen werden entfernt.

# Spalte CITY hinzufügen (Gruppierungsmerkmal)
w_barth$CITY <- 'Barth'
w_greifswald$CITY <- 'Greifswald'

# Monat aus MESS_DATUM extrahieren und als Factor zu neuer Spalte hinzufügen
# 1. MESS_DATUM von integer in character umwandeln
# 2. Substring Zeichen 5 und 6
# 3. Typ von character in factor umwandeln
# 4. Neue Spalte (Monat) mit factor erzeugen
w_barth$MONTH <- as.factor(substring(as.character(w_barth$MESS_DATUM), first = 5, last = 6))
w_greifswald$MONTH <- as.factor(substring(as.character(w_greifswald$MESS_DATUM), first = 5, last = 6))

# Daten für Jahr 2023 und Station Barth selektieren
w_2023 <- w_barth[which(w_barth$MESS_DATUM >= 20230101 & w_barth$MESS_DATUM <= 20231231), ]

# Daten für Barth und Greifswald vereinen
w_2023_complete <- rbind(w_2023,w_greifswald[which(w_greifswald$MESS_DATUM >= 20230101 & w_greifswald$MESS_DATUM <= 20231231), ])

# Maximum der Windspitze (FX) selektieren und fehlende Werte (-999) ausschließen
w_2023_fx <- w_2023[which(w_2023$FX >= 0.0), ]$FX

# Anwendung

# 1. Eine numerische Variable

# Maximum der Windspitze pro Tag für das Jahr 2023

# R Standard Boxplot-Funktion

graphics::boxplot(w_2023_fx,
        #notch=T,
        #outline=F,
        #horizontal=T,
        xlab='Maximum Windspitze/Tag',
        ylab='Windspitze m/s')#Maximum der Windspitze m/s

# 2. Mehrere numerische Variablen

# Minimum, Mittel und Maximum der Tages-Temperatur für das Jahr 2023

boxplot(w_2023$TNK, w_2023$TMK, w_2023$TXK, 
        names=c('Min', 'Avg', 'Max'), 
        xlab='Tageswerte Temperatur',
        ylab='Temperatur °C')

# 3. Ein oder mehrere kategoriale+numerische variablen und Zeitreihen

# Tagesmittel-Temperatur gruppiert nach Monat (ordinal, kategorial) für das Jahr 2023

boxplot(formula=TMK~MONTH, 
        data=w_2023, 
        xlab='Tagesmittel-Temperatur je Monat', 
        ylab='Temperatur °C')

###

# Beispiel-Rechnung

# Maximum der Windspitze November 2023
w_2023_11_fx <- w_2023[which(w_2023$FX >= 0.0 & w_2023$MESS_DATUM >= 20231101 & w_2023$MESS_DATUM <= 20231130), ]$FX

#w_2023_11_fx
#w_2023_11_fx[order(w_2023_11_fx)]

boxplot(w_2023_11_fx,
        horizontal=T,
        xlab='Maximum Windspitze/Tag')

range(w_2023_11_fx)

# Min, Max
# 4.5, 21.3

# Das Minimum der Werte liegt bei 4.5 und das Maximum bei 21.3.

quantile(w_2023_11_fx)

#    0%    25%    50%    75%   100% 
# 4.500  8.725 10.700 12.725 21.300

# Der Median beträgt 10.7 und das 25% und 75% Quantil haben die Werte 8.725 und
# 12.725

#boxplot(w_2023_11_fx, plot=F)
boxplot.stats(w_2023_11_fx)

# $stats
# 4.5  8.7 10.7 12.8 18.9

# Wie hier bereits zu sehen ist, entspricht der obere Whisker nicht dem Maximum.

# Inter-Quantile-Range
# Q3 - Q1
#12.8 - 8.7 = 4.1

# unterer Whisker/Minimum
# Q1 - 1.5 * IQR
#8.7 - 1.5 * 4.1 = 2.55 -> Minimum 4.5 = unterer Whisker

# Da der berechnete Wert von 2.55 für den unteren Whisker kleiner als das 
# Minimum von 4.5 ist, sind unterer Whisker und Minimum damit gleich.

# oberer Whisker/Maximum
# Q3 + 1.5 * IQR
#12.8 + 1.5 * 4.1 = 18.95 -> oberer Whisker, Maximum 21.3, Outlier > 18.95

# Der berechnete Wert von 18.95 für den oberen Whisker ist kleiner als das 
# Maximum mit 21.3. Oberer Whisker und Maximum sind hier nicht gleich und 
# Punkte oberhalb von 18.95 stellen mögliche Ausreißer dar.

###

# ggplot2

#install.packages('ggplot2')

library(ggplot2)

# Tagesmittel-Temperatur gruppiert nach Monat (ordinal, diskret/kategorial) für das Jahr 2023
# Verwendung Jitter

ggplot(w_2023, aes(x=MONTH, y=TMK)) + 
  geom_boxplot() + 
  geom_jitter(color="red", size=0.7, alpha=0.9) + 
  xlab("Tagesmittel-Temperatur je Monat") + 
  ylab("Temperatur °C")

# Tagesmittel-Temperatur gruppiert nach Monat (ordinal, kategorial) und CITY für das Jahr 2023

ggplot(w_2023_complete, aes(x=MONTH, y=TMK, fill=CITY)) + 
  geom_boxplot() + 
  geom_jitter(color="red", size=0.7, alpha=0.9) + 
  xlab("Tagesmittel-Temperatur je Monat") + 
  ylab("Temperatur °C")

# Facet
# Tagesmittel-Temperatur gruppiert nach MONTH (ordinal, kategorial) und als Facet=CITY für das Jahr 2023

ggplot(w_2023_complete, aes(x=MONTH, y=TMK)) + 
  geom_boxplot() + 
  facet_wrap(~CITY) + #, scale="free"
  geom_jitter(color="red", size=0.7, alpha=0.9) + 
  xlab("Tagesmittel-Temperatur je Monat") + 
  ylab("Temperatur °C")

# grouping

# ggplot(w_2023, aes(x=NM, y=TMK)) +#, fill=name
#   geom_boxplot(aes(group=cut_width(NM, 3.0))) + #, notch = TRUE
#   geom_jitter(color="black", size=0.4, alpha=0.9) +
#   ggtitle("") +
#   xlab("Maximum Windspitze m/s") +
#   ylab("Temperatur °C")

# marginal boxplot

# #install.packages('ggExtra')
# library('ggExtra')
# 
# p <- ggplot(w_2023[which(w_2023$FX >= 0.0 & w_2023$SDK >= 0.0), ], aes(x=FX, y=TXK, color=MONTH, size=SDK)) +
#   geom_point() +
#   scale_colour_brewer(palette = "Paired") + 
#   xlab("Maximum Windspitze m/s") +
#   ylab("Temperatur °C")
# 
# ggMarginal(p, type="boxplot")

