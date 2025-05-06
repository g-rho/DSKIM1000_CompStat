#************************************Korrelogram************************************

#-------------------------------------
# Eingereicht bei: Prof. Dr. Szepannek
# Eingereicht durch:
# 1) Mopong Foguem, Danielle
# 2) Le, Quang Ninh
#-------------------------------------

# ------------------------------------------------------------------------
#*****************************Definition**********************************
# Ein Korrelogramm ist eine grafische Darstellung der Beziehungen zwischen 
# verschiedenen numerischen Variablen in einem Datensatz.
#
# Die Beziehung zwischen jedem Variablenpaar wird durch ein Streudiagramm 
# oder ein Symbol(Quadrat / Kreis / Ellipse / Pie ) visualisiert
# ------------------------------------------

#---------------------------------------------------------------------------
#********************die Bedeutung des Korrelogramms:***********************
# EDA ist einer der wichtigsten Schritte im Data-Science-Prozess.
# Im EDA-Schritt werden die Daten mithilfe von zusammenfassenden Statistiken und Visualisierungen untersucht,
# um ihre Eigenschaften besser zu verstehen und Muster, Beziehungen und Ausreißer zu identifizieren.
# Korrelogramm ermöglicht die Visualisierung der Beziehungen des gesamten Datensatzes auf einen Blick. 
# Damit wird die Multikollinearität erkannt. 
# Weil in vielen Modellen (insbesondere linearen Modellen wie der linearen oder logistischen Regression) 
# können stark korrelierte Prädiktoren die Varianz der Koeffizientenschätzungen erhöhen, 
# das Modelltraining instabil machen und die Interpretierbarkeit beeinträchtigen.
# Das heißt: Mit Hilfe von Implementation des Korrelogramms wird die Multikollinearität erkannt und dann führt das Beheben von Multikollinearität 
# (z. B. durch Weglassen oder Kombinieren von Variablen)zu robusteren Modellen.
#-------------------------------------------
# ------------------------------------------
# Pakete laden (und ggf. installieren)
# ------------------------------------------
if (!require(corrplot)) install.packages("corrplot")
if (!require(gridGraphics)) install.packages("gridGraphics")
if (!require(ggplotify)) install.packages("ggplotify")
if (!require(GGally)) install.packages("GGally")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(ggcorrplot)) install.packages("ggcorrplot")  
if (!require(PerformanceAnalytics)) install.packages("PerformanceAnalytics")
install.packages("grid")

library(MASS)
library(corrplot)
library(corrplot)
library(GGally)
library(ggplotify)
library(gridGraphics)
library(ggplot2)
library(ggcorrplot)
library(PerformanceAnalytics)

# -------------------------------------------------------------------------------------------
# Daten vorbereiten, wir fangen erstmal mit Iris Datensatz an ( wenige Variable)
# -------------------------------------------------------------------------------------------
# Iris-Datensatz laden
data(iris)
df <- iris # dataframe extrahieren
head(df)
str(df)

# Nur numerische Spalten
c_numeric <- which(sapply(df, is.numeric))

#----------------------------------------------------------------------------------
#***************************I ) Visualisierung durch scatterplot*******************
#----------------------------------------------------------------------------------
# Die Beziehung zwischen jedem Variablenpaar wird durch ein Streudiagramm visualisiert. 
#----------------------------------------------------------------------------------
#***************************I.1 ) ggpairs from GGally*****************************
#---------------------------------------------------------------------------------

# Standard-Paardiagramm der numerischen Variablen des Iris-Datensatzes 
# (ohne Unterscheidung der Arten).
ggpairs(df,
        columns = c_numeric, # Nur numerische Spalten
        title = "GGally: ggpairs auf den Iris-Datensatz(Standard)")
#----------------------------------------------------------------------------------------------------------------
# Im Standardmodus von GGally::ggpairs werden im oberen Dreieck die paarweisen Korrelationskoeffizienten angezeigt, 
# im unteren Dreieck finden sich die Scatterplots aller Kombinationen von Variablen,
# und auf der Diagonalen ist für jede einzelne Variable jeweils ein Histogramm abgebildet,
# das deren univariate Verteilung darstellt.
#----------------------------------------------------------------------------------------------------------------


# Erstellen ein Paardiagramm mit Regressionslinien im oberen Dreieck (ohne Farbe und Form der Arten).
ggpairs(df,                               
        columns = c_numeric,                        # Nur numerische Spalten auswählen
        
        upper = list(continuous = wrap("smooth", 
                                       method = "lm",  # Lineares Regressionsmodell
                                       se = TRUE)),    # Konfidenzintervall hinzufügen
        lower = list(continuous = wrap("points")),     # Scatterplots in der unteren Hälfte
        diag = list(continuous = "barDiag")) +        # Histogramm auf der Diagonale
  labs(title = "Iris Dataset: Pairwise Scatter Plot without Color and Shape by Species") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))             # Titel zentrieren und formatieren
#----------------------------------------------------------------------------------------------------------------
# Im obigen ggpairs-Plot werden im oberen Dreieck für jede Paarung numerischer Variablen glatte Regressionslinien 
# (mit Konfidenzintervall) dargestellt, um lineare Zusammenhänge zu visualisieren.
# Im unteren Dreieck zeigt jede Zelle einen Streudiagramm (Scatterplot) der entsprechenden Variablenkombination,
# was die tatsächliche Punktverteilung offenlegt. 
# Auf der Diagonalen findet sich für jede einzelne numerische Variable ein Histogramm,
# das deren univariate Häufigkeitsverteilung abbildet. 
# Zusammen bietet dieser Plot somit eine kompakte Übersicht über lineare Trends,
# Rohdatenpunkte und Einzelverteilungen aller betrachteten Merkmale.
#----------------------------------------------------------------------------------------------------------------


# Erstellen ein Paardiagramm mit Regressionslinien im oberen Dreieck (mit Farbe und Form nach Speicies).
ggpairs(df,
             columns = c_numeric,
             mapping = aes(color = Species, shape = Species),            # Farbe und Form nach Speicies
             upper = list(continuous = "cor"),    
             lower = list(continuous = wrap("smooth", 
                                            method = "lm",               # Regression im unteren
                                            se = TRUE)),                 # Konfidenzintervall hinzufügen
             diag = list(continuous = "densityDiag")) +                  # Dichte auf der Diagonalen
  labs(title = "Iris Dataset: Pairwise Scatter Plot with Color and Shape by Species") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))

#----------------------------------------------------------------------------------------------------------------
# Durch das Mapping werden Art-spezifische Zusammenhänge und Verteilungen visualisiert,
# anstatt alle Punkte und Verläufe zu einem Gesamtergebnis zu verschmelzen.
# Jede Spezies (Setosa, Versicolor, Virginica) bekommt eine eigene Farbe.
# Zusätzlich zum Formcode werden die Punkte in den Scatterplots mit unterschiedlichen Symbolen(Kreis,Dreiecke, Quadrat)
# Die linearen Modelle werden für jede Spezies separat berechnet und dargestellt, inklusive Konfidenzintervall.
# Diagonale: Es erscheinen drei überlagerte Dichtekurven (eine je Spezies), sodass man die Verteilungen der Arten direkt vergleichen kann.
# Obere Dreiecksmatrix: Diagramm zeigt nicht nur gesamt Korrelationskoeffizienten  sondern auch Korrelationskoeffizienten mehrere Gruppen
#----------------------------------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
#**********************I.2) PerformanceAnalytics*******************************
#-------------------------------------------------------------------------------

#Farbe definieren und Form Mapping
cols   <- c("setosa"     = "#F8766D",
            "versicolor" = "#00BA38",
            "virginica"  = "dodgerblue2")

shapes <- c("setosa"     = 16,   # ausgefüllter Kreis
            "versicolor" = 17,   # ausgefüllter Dreieck
            "virginica"  = 15)   # ausgefüllter Diamant

#Vektoren pro Punkt erstellen
colourVec <- cols[ as.character(df$Species) ]
pchVec    <- shapes[ as.character(df$Species) ]

chart.Correlation(df[c_numeric],
                  histogram = TRUE,
                  col = colourVec,
                  pch = pchVec,
                  main = "PerformanceAnalytics Correlogramm using chart.Correlation()")

#---------------------------------
# Alternativ Code(kürze): nur Farbcode, kein Formcode
# chart.Correlation(df[-5], bg=df$Species, pch=21)
# https://www.r-bloggers.com/2011/08/graphically-analyzing-variable-interactions-in-r/
#---------------------------------


#-------------------------------------------------------------------------------
# In diesem Diagramm werden die Zusammenhänge der numerischen Merkmale des Iris‐Datensatzes auf drei Arten fast genau so wie ggpairs von oben visualisiert
# Entlang der Diagonale sind für jede Variable sowohl Histogramme als auch Dichteplots abgebildet.
# Obere Dreiecksmatrix: Diagramm zeigt nur gesamt Korrelationskoeffizienten (je Größe von Ziffer desto stärke Korrelation_ nach absolut Wert)
# Anstatt die linearen Modelle werden für jede Spezies separat berechnet und dargestellt wie ggpairs,
# chart.Correlation zeigt Dichtekurve.
# schließlich, bei char.Correlation gibt es keine aes (color = ...,shape=…) wie ggpairs, man muss Farbe definieren und Form Mapping erstellen
# dann Vektoren pro Punkt erstellen um Form und Farbe nach Arten darzustellen. 
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
#**********************Vorteile:( im Fall Scatterplot)****************
#-------------------------------------------------------------------------------
# Sowohl ggpairs als auch chart.Correlation bieten den großen Vorteil, 
# dass sie in einem einzigen Diagramm umfangreiche Informationen zu den Beziehungen und Verteilungen Ihrer Variablen bündeln.
# die Kombination von Korrelation, Scatterplots und Verteilungsdarstellungen macht die Tools extrem informativ 
# und erlaubt es, in einem Blick sowohl Zusammenhänge als auch Muster und Ausreißer in den Daten zu erkennen, 
# ohne mehrere separate Plots anfertigen zu müssen.
#-------------------------------------------------------------------------------

# -------------------------------------------------------------------------------------------
# Daten vorbereiten,ab jetzt nutzen  wir Boston Datensatz ( 14 numerischen Variablen)
# -------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#**********************I.3) Häufige Fehler:( im Fall Scatterplot)****************
#-------------------------------------------------------------------------------
# Simulieren Sie einen Datenframe mit 14 numerischen Variablen
data(Boston)
df1 <- Boston
# Paarweises Diagramm für alle 14 Variablen
ggpairs(df1, title = "Too Many Variables in Pairwise Plot")

#--------------------------------------------------------------------------------
# Ein häufiger Nachteil von umfangreichen Paarplots wie etwa dem ggpairs-Plot mit allen 84 Scatterplots
# im unteren Dreieck (und noch mehr, sobald auch im oberen Dreieck weitere Darstellungen hinzukommen)
# ist die Überfrachtung der Grafik: Zu viele Punktewolken, überlagernde Farben und Formen erschweren die Interpretation,
# und Legenden sowie Achsenbeschriftungen können sich gegenseitig überlappen und unlesbar werden.
# Um dem entgegenzuwirken, empfiehlt es sich in der explorativen Datenanalyse (EDA),
# zunächst eine Merkmalsauswahl durchzuführen – etwa nur die Top N der am stärksten miteinander korrelierenden Variablen auszuwählen
# oder kleinere, inhaltlich zusammengehörige Variablengruppen zu bilden.
# Statt eines unübersichtlichen Paarplots kann man dann auf eine Korrelations-Heatmap ausweichen
# oder gezielt Symbol-Darstellungen (z. B. in einem corrplot) nutzen, um die wesentlichen Zusammenhänge kompakt und klar lesbar zu visualisieren.
#--------------------------------------------------------------------------------


#-------------------------------------------------------------------------
#***************************II ) Visualisierung durch Symbol (corrplot)********************
#-------------------------------------------------------------------------

# cor matrix berechnnen (Der Korrelationskoeffizient nach Bravais-Pearson als Standard)
cor_mat <- cor(df1)

corrplot(cor_mat,
         method = "ellipse",       # Darstellungsform: Ellipsen zeigen Korrelation Stärke & Richtung
         type = "lower",           # Nur untere Dreieckshälfte anzeigen (oberes bleibt leer)
         order = "hclust",         # Variablen nach hierarchischem Clustering gruppieren
         addrect = 2,              # 2 Cluster-Boxen im Plot einzeichnen
         tl.col = "black",         # Achsenbeschriftungen in Schwarz
         tl.srt = 45,              # Text drehen
         title = "Pearson Korrelogramm des Boston-Datensatzes mit corrplot",
         addCoef.col = "black",    # Korrelationskoeffizienten als Zahlen in Schwarz hinzufügen
         col = colorRampPalette(c("#3B9AB2", "#EEEEEE", "#F21A00"))(200),  # Farbverlauf
         mar = c(0,0,3,0),        # Ränder: oben Platz für den Titel, sonst keine Ränder
         diag = FALSE)             # Diagonale (Selbstkorrelationen) ausblenden

#--------------------------------------------------------------------------------------------
# Das corrplot-Diagramm stellt die Einträge einer Korrelationsmatrix auf intuitive Weise dar,
# indem es jede Zelle entweder als farbcodierte Fläche oder in Form eines Symbols (Ellipse, Kreis, Quadrat u. Ä.) visualisiert.
# Zusätzlich können die numerischen Korrelationskoeffizienten direkt als Zahlen in die Zellen eingeblendet werden,
# was eine präzise Ablesung ermöglicht. Dank der Option, die Variablenreihenfolge
# anhand eines hierarchischen Clusterings zu ordnen und Cluster-Boxen einzufügen, 
# lassen sich stark korrelierte Gruppen auf einen Blick erkennen. 
# Insgesamt bietet corrplot damit eine sehr übersichtliche und zugleich detaillierte Darstellung selbst großer Korrelationsmatrizen
# und erleichtert sowohl das Erkennen globaler Muster (durch Clustering) als auch lokaler Zusammenhänge.
#--------------------------------------------------------------------------------------------


#-------------------------------------------------------------------------
#***************************III ) Vergleich mit Standard-Heatmap********************
#-------------------------------------------------------------------------
# Erstelle eine Korrelations-Heatmap mit ggcorr aus dem Paket GGally
ggcorr(data = NULL,                         # kein Rohdaten-Frame, wir nutzen direkt die Korrelationsmatrix
       cor_matrix =cor_mat,                 # zuvor berechnete Korrelationsmatrix (z. B. mit cor(df))
       label = TRUE,                        # zeige die numerischen Korrelationskoeffizienten in den Zellen an
       label_round = 2,                     # runde die angezeigten Werte auf 2 Dezimalstellen
       label_size = 4,                      # Schriftgröße der Labels
       name = "Correlation Coef") +          # Legendentitel für die Farbskala
  ggtitle("Pearson Korrelogramm des Boston-Datensatzes mit ggcorr von GGally")

#------------------------------------------------------------------------------------------
# Das ggcorr-Diagramm aus GGally visualisiert eine Korrelationsmatrix als farblich codiertes Raster,
# in dem jeder Zelle der passende Korrelationskoeffizient numerisch angezeigt wird. 
# Zugleich erhält man so eine leicht erfassbare Übersicht auch sehr großer Matrizen,
# da positive und negative Zusammenhänge durch abgestufte Farben schnell unterschieden werden können.
# Im Gegensatz zu corrplot bietet ggcorr jedoch keine automatische Cluster-Ordnung der Variablen
# und keine alternativen Symbol-Formen (Ellipse, Kreis  usw.), 
# sondern konzentriert sich auf eine schlichte Heatmap-Darstellung. 
# Dafür ist es vollständig in das ggplot2-Ökosystem integriert, 
# was die einheitliche Anwendung von Themes, Facetten und weiteren ggplot-Funktionen ermöglicht 
# und so die nahtlose Einbindung in komplexere Visualisierungspipelines erleichtert.
#------------------------------------------------------------------------------------------


# ggcorrplot ist die spezialisierte Erweiterung für Korrelations‐Heatmaps,
# die Ihnen mehr Kontrolle über Clustering, Teil‐Darstellung, Signifikanz und Feintuning bietet,
# während ggcorr eher eine schnelle, einfache ggplot‐native Standard‐Heatmap liefert.
ggcorrplot(cor_mat,                         # Übergibt die zuvor berechnete Korrelationsmatrix
           method = "square",               # Darstellungsform: "square" zeichnet quadratische Tiles (alternativ: "circle")
           type = "lower",                  # Zeigt nur die untere Hälfte der Matrix an (‚upper‘ für obere Hälfte, ‚full‘ für beide) 
           hc.order = TRUE,                 # Sortiert Variablen per hierarchischem Clustering neu, um Cluster sichtbar zu mache
           lab = TRUE,                      # Beschriftet die Zellen mit den Korrelationskoeffizienten
           lab_size = 4,                    # Schriftgröße der Koeffizienten-Labels
           colors = c("#3B9AB2", "#EEEEEE", "#F21A00"),  # Farbverlauf
           title = "Pearson Korrelogramm des Boston-Datensatzes mit ggcorrplot)",
           ggtheme = ggplot2::theme_minimal())   ## Minimalistisches Theme aus ggplot2 für ein sauberes Design

#-------------------------------------------------------------------------
#***************************IV ) Zusammenfassung*************************
#-------------------------------------------------------------------------
# Der im Präsentation gezeigte Entscheidungsbaum fasst zusammen, wie man Korrelogramm in der EDA bei numerischen Variablen vorgehen kann:

# 1. Fall:  Wenige Variablen (ca. 3–6/7)
# Hier bieten sich Scatterplots an, um Paarbeziehungen direkt zu inspizieren.
# Zunächst kann man mit chart.Correlation (PerformanceAnalytics) ein kompaktes Korrelogramm erzeugen,
# das sowohl Streudiagramme als auch Histogramme/Dichteanzeigen kombiniert.
# Für noch detailliertere Ansichten und Gruppierungen (z. B. nach einer Faktorvariable) verwendet man anschließend ggpairs (GGally),
# das moderne ggplot2-Ästhetik mit Regressionslinien, Dichten oder weiteren Facetten-Optionen verbindet.

# 2. Fall : Viele Variablen (≈ 10 oder mehr)
# Ein riesiger Paarplot würde schnell unübersichtlich. Stattdessen nutzt man Heatmaps der Korrelationsmatrix:
  
  # 2.1 : Mit ggcorr/ ggcorrplot (GGally bzw. ggcorrplot) erstellt man nahtlos im ggplot2-Stil eine farbcodierte Heatmap,
  # optional mit Clustering (hc.order) und beschrifteten Korrelationswerten.

  # 2.2 : Für noch flexiblere Low-Level-Optionen—etwa unterschiedliche Symbolformen 
  # (Ellipse, Kreis, Quadrat, usw) oder präzises Clustering—greift man auf das spezialisierte Paket corrplot zurück.

# Ein wesentlicher Vorbehalt bei der Analyse rein korrelativer Zusammenhänge besteht darin,
# dass Kausalität nicht aus der Korrelation abgeleitet werden kann: Auch starke statistische Zusammenhänge zwischen Variablen müssen nicht bedeuten,
# dass die eine die andere ursächlich beeinflusst.
# Ohne zusätzliche Methoden oder Experimente zur Prüfung von Ursache-Wirkungs-Beziehungen liefern Korrelationsanalysen
# somit nur Hinweise auf Assoziationen, nicht aber auf zugrundeliegende Mechanismen.


