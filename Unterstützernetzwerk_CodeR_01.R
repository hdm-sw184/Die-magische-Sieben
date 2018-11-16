#Unterstützernetzwerk nur staatliche und private Unterstützer 

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
colrs <- c("turquoise", "lightblue", "grey", "grey60")

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


plot(s, edge.arrow.size=.1, layout=coords, rescale=FALSE, edge.color="grey80", edge.arrow.size=.1, 
     vertex.frame.color="transparent", 
     edge.color="grey80",
     vertex.label.dist=0.2,
     vertex.label.cex=.55,
     vertex.size=9,
     vertex.label.color="black",
     ylim=c(-2,3), xlim=c(-2,3))


#Nun wollen rausfinden,wer hier die größten in und outdegrees hat  

degree(s, mode="in")
degree(s, mode="out")

#Degree Werte in Prozent Zahlen 
degree(s, mode="all", normalized = TRUE)

#NEUES NETZWERK
#NEUES NETZWERK
#NEUES NETZWERK 
#NEUES NETZWERK

#Netzwerk staatliche und private Unterstützer mit allen Indegrees >19 


library(igraph)
# liest die Dateien direkt aus dem github-Verzeichnis ein
el <- read.csv("https://raw.githubusercontent.com/hdm-sw184/Die-magische-Sieben/master/226305_Unterstuetzernetzwerk_edgelist_indegrees_groe%C3%9Fer19.csv", header=T, as.is=T, sep = ",")
nodes <- read.csv("https://raw.githubusercontent.com/hdm-sw184/Die-magische-Sieben/master/226305_Unterst%C3%BCtzernetzwerk_nodelist_indegreesab19_Bezeichnung_mit_ID.csv", header=T, as.is=T, sep = ",")
# prüft, ob alle Variablen eingelesen wurden
head(el)
head(nodes)
# wandelt die edgelist in eine Matrix um und baut das igraph-Objekt
hties <- as.matrix(el)
s <- graph_from_data_frame(d=hties, vertices=nodes, directed=T)
s


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
colrs <- c("turquoise", "lightblue", "grey", "grey")

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

coords <- layout_with_kk(s)*0.15


plot(s, edge.arrow.size=.1, layout=coords, rescale=FALSE, edge.color="grey80", edge.arrow.size=.1, 
     vertex.frame.color="transparent", 
     edge.color="grey80",
     vertex.label.dist=0.2,
     vertex.label.cex=.55,
     vertex.size=9,
     vertex.label.color="black",
     ylim=c(-0.85,0.85), xlim=c(-0.85,0.85))

#Ego-Netzwerk IOG 
#Auflistung der Verbindungen der Knoten (entspricht degree)
ego_size(s)

#Detaillierte Aufstellung aller Knoten und deren direkte Verbindung
ego(s)

ego(s, order = 1, nodes = V(s)$name == "IOG", mode = "all")
#selektiert das Ego-Netzwerk von IOG. In einem gerichteten Netzwerken kann "mode" durch in und out unterschieden werden. Mit dem Befehl "order" werden die angrenzenden Knoten ausgewählt, bei 1 sind dies alle Knoten, die direkt mit ego verbunden sind. Bei order = 2 werden auch Knoten erfasst, die zwei Schritte von Ego entfernt sind.
#jedes Netzwerk in einzelne Egonetzwerke zerlegen; 1 Akteur den ich auswähle steht im Zentrum, 


# mit make_ego_graph lassen sichn spezifische Knoten des Netzwerks auflisten.

c
IOG <- make_ego_graph(s, order = 1, nodes = V(s)$name == "IOG", mode = "all")
IOG
plot(IOG[[1]], main="Egonetzwerk IOG") # der Visualisierungsbefehl ist hier etwas umständlich.

#Neues Netzwerk 
#Neues Netzwerk
#Neues Netzwerk

#Netzwerk staatliche und private Unterstützer mit allen Indegrees >19 ohne das Egonetzwerk von IOG!!!!

library(igraph)
# liest die Dateien direkt aus dem github-Verzeichnis ein
el <- read.csv("https://raw.githubusercontent.com/hdm-sw184/Die-magische-Sieben/master/226305_Unterst%C3%BCtzernetzwerk_edgelist_indegreesab19_ID_ohneEGO_vonIOG.csv", header=T, as.is=T, sep = ",")
nodes <- read.csv("https://raw.githubusercontent.com/hdm-sw184/Die-magische-Sieben/master/226305_Unterst%C3%BCtzernetzwerk_nodelist_indegreesab19_ID_ohneEgo_vonIOG.csv", header=T, as.is=T, sep = ",")
# prüft, ob alle Variablen eingelesen wurden
head(el)
head(nodes)
# wandelt die edgelist in eine Matrix um und baut das igraph-Objekt
hties <- as.matrix(el)
s <- graph_from_data_frame(d=hties, vertices=nodes, directed=T)
s


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
colrs <- c("turquoise", "lightblue", "grey", "grey")

V(s)$color <- colrs[V(s)$type]
# weist dem Vertex-Attribut color die Funktion colrs zu, die wir vorher mit zwei Farben definiert haben.
plot(s)



# EXTRA: TWO-MODE Netzwerke (bi-partite)

vertex_attr(s)
V(s)[V(s)$type == 1]$shape <- "square"
V(s)[V(s)$type == 2]$shape <- "circle"
V(s)[V(s)$type == 4]$shape <- "circle"

plot(s)



#Visualisierung
# Das sieht noch immer schrecklich aus. Je mehr Knoten (und Kanten), desto schlechter kann man das Netzwerk erkennen.
# Ein weiterer Tipp für den plot-Befehl wäre es, das Netzwerk erst einmal zu entzerren.

coords <- layout_with_kk(s)*0.15

plot(s, edge.arrow.size=.1, layout=coords, rescale=FALSE, edge.color="grey80", edge.arrow.size=.1, 
     vertex.frame.color="transparent", 
     edge.color="grey80",
     vertex.label.dist=0.2,
     vertex.label.cex=.55,
     vertex.size=9,
     vertex.label.color="black",
     ylim=c(-0.85,0.85), xlim=c(-0.85,0.85))


#Nun wollen rausfinden,wer hier die größten in und outdegrees hat  

degree(s, mode="in")
degree(s, mode="out")

#Degree Werte in Prozent Zahlen 
degree(s, mode="all", normalized = TRUE)

#Netzwerkmaße

# Betweenness
betweenness(s) # Wie wahrscheinlich ist es, dass dieser Knoten die Verbindung zu anderen Knoten im Netzwerk herstellen kann? - Bedeutung der schwachen Beziehungen

# Durchmesser
diameter(s)

# Density
edge_density(s)

# Triadenzenus (eigentlich nur bei gerichteten Netzwerken)
triad_census(s)


