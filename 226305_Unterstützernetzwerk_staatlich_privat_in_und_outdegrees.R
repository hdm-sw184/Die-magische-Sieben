# Beispiel Gesamtnetzwerk Studierende (Work- und Help-Netzwerk)
library(igraph)
# liest die Dateien direkt aus dem github-Verzeichnis ein
el <- read.csv("https://raw.githubusercontent.com/hdm-sw184/Die-magische-Sieben/master/226395_Unterst%C3%BCtzernetzwerk_Staat_Privat_Edgelist.csv", header=T, as.is=T, sep = ",")
nodes <- read.csv("https://raw.githubusercontent.com/hdm-sw184/Die-magische-Sieben/master/226305_Unterst%C3%BCtzernetzwerk_Staat_Privat_Nodelist.csv", header=T, as.is=T, sep = ",")
# prüft, ob alle Variablen eingelesen wurden
head(el)
head(nodes)
# wandelt die edgelist in eine Matrix um und baut das igraph-Objekt
hties <- as.matrix(el)
s <- graph_from_data_frame(d=hties, vertices=nodes, directed=T)
s
#Es gibt 38 Knoten, die 152 Beziehungen abbilden 

# einfache Visualisierung
plot(s)


#Visualisierung des Netzwerkes 
# EDGE/KANTEN-Attribute festlegen
# kleinere und schönere Kanten mit Pfeilspitzen mit Gewichten

E(s)$arrow.size <- .2 # definiert die Pfeilspitze auf 20% des Ursprungswerts
E(s)$color="grey60" # definiert die Kantenfarbe auf schwarz
E(s)$curved=.2


#Unterschiedliche Farben der Knoten 
#FARBEN nach Vertex-Attributen definieren
colrs <- c("blue", "lightblue", "grey", "grey60")

V(s)$color <- colrs[V(s)$type]
# weist dem Vertex-Attribut color die Funktion colrs zu, die wir vorher mit zwei Farben definiert haben.
plot(s)



# EXTRA: TWO-MODE Netzwerke (bi-partite)
# bei einem bi-partite Netzwerk haben Sie in der Regel die Rubrik "type" angelegt und mit 0 und 1 kodiert. Damit könnten Sie das Netzwerk wie folgt visualisieren (funktioniert in unserem Beispiel nicht, da wir kein bi-partite Netzwerk angelegt haben.)

vertex_attr(s)
V(s)[V(s)$type == 1]$shape <- "square"
V(s)[V(s)$type == 2]$shape <- "circle"
V(s)[V(s)$type == 4]$shape <- "circle"

plot(s)



#Visualisierung
# Das sieht noch immer schrecklich aus. Je mehr Knoten (und Kanten), desto schlechter kann man das Netzwerk erkennen.
# Ein weiterer Tipp für den plot-Befehl wäre es, das Netzwerk erst einmal zu entzerren.

coords <- layout_with_kk(s)*0.2

plot(s, edge.arrow.size=0.4, layout=coords, rescale=FALSE, edge.color="grey80", edge.arrow.size=0.4, 
     vertex.frame.color="transparent", 
     layout=layout_with_kk, 
     edge.color="grey80",
     vertex.label.dist=0.5,
     vertex.label.color="black")


#Nun wollen rausfinden,wer hier die größten in und outdegrees hat  

degree(s, mode="in")
degree(s, mode="out")

#Degree Werte in Prozent Zahlen 
degree(s, mode="all", normalized = TRUE)







