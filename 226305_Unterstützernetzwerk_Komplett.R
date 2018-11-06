# Beispiel Gesamtnetzwerk Studierende (Work- und Help-Netzwerk)
library(igraph)
# liest die Dateien direkt aus dem github-Verzeichnis ein
el <- read.csv("https://raw.githubusercontent.com/hdm-sw184/Die-magische-Sieben/master/226305_Unterst%C3%BCtzernetzwerk_Edgelist_WashNetzwerk.csv", header=T, as.is=T, sep = ",")
nl <- read.csv("https://raw.githubusercontent.com/hdm-sw184/Die-magische-Sieben/master/226305_Unterst%C3%BCtzernetzwerk_Nodelist_WashNetzwerk.csv", header=T, as.is=T, sep = ",")
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


# Mit den Befehlen haben Sie die Edge-Attribute um 3 neue Werte ergänzt. Diese sind nun dauerhaft im igraph-Objekt hinterlegt.
list.edge.attributes(s)

edge.attributes(s)

plot(s)

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
V(s)[V(s)$type == 3]$shape <- "circle"
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


















#Triadencensus
triad_census(s)

#Ein bis zwei Egonetzwerke bilden

#Auflistung der Verbindungen der Knoten (entspricht degree)
ego_size(s)

#Detaillierte Aufstellung aller Knoten und deren direkte Verbindung
ego(s)

ego(s, order = 1, nodes = V(s)$name == "WHH", mode = "all")
#selektiert das Ego-Netzwerk von Mr. Hi. In einem gerichteten Netzwerken kann "mode" durch in und out unterschieden werden. Mit dem Befehl "order" werden die angrenzenden Knoten ausgewählt, bei 1 sind dies alle Knoten, die direkt mit ego verbunden sind. Bei order = 2 werden auch Knoten erfasst, die zwei Schritte von Ego entfernt sind.
#jedes Netzwerk in einzelne Egonetzwerke zerlegen; 1 Akteur den ich auswähle steht im Zentrum, 
#Mach mir ein egonetz werk aus Netzwerk Karate, aus dem Knoten, Oder 1= direkt verbunden; Order 2= indirekt verbunden; order 3 wären dann alle
#

# Variante mit zwei Beziehungen ausgehend von Mr Hi
ego(s, order = 2, nodes = V(s)$name == "WHH", mode = "all")

# mit make_ego_graph lassen sichn spezifische Knoten des Netzwerks auflisten.

c
ww <- make_ego_graph(s, order = 1, nodes = V(s)$name == "Welthungerhilfe", mode = "all")
ww
plot(ww[[1]]) # der Visualisierungsbefehl ist hier etwas umständlich.


#Nun das Teilnetzwerk von Plan Deutschland

#Auflistung der Verbindungen der Knoten (entspricht degree)
ego_size(s)

#Detaillierte Aufstellung aller Knoten und deren direkte Verbindung
ego(s)

ego(s, order = 1, nodes = V(s)$name == "PD", mode = "all")
#selektiert das Ego-Netzwerk von Mr. Hi. In einem gerichteten Netzwerken kann "mode" durch in und out unterschieden werden. Mit dem Befehl "order" werden die angrenzenden Knoten ausgewählt, bei 1 sind dies alle Knoten, die direkt mit ego verbunden sind. Bei order = 2 werden auch Knoten erfasst, die zwei Schritte von Ego entfernt sind.
#jedes Netzwerk in einzelne Egonetzwerke zerlegen; 1 Akteur den ich auswähle steht im Zentrum, 
#Mach mir ein egonetz werk aus Netzwerk Karate, aus dem Knoten, Oder 1= direkt verbunden; Order 2= indirekt verbunden; order 3 wären dann alle
#

# Variante mit zwei Beziehungen ausgehend von Mr Hi
ego(s, order = 1, nodes = V(s)$name == "PlanDeutschland", mode = "all")

# mit make_ego_graph lassen sichn spezifische Knoten des Netzwerks auflisten.

pd <- make_ego_graph(s, order = 1, nodes = V(s)$name == "PlanDeutschland", mode = "all")
pd
plot(pd[[1]]) 
# der Visualisierungsbefehl ist hier etwas umständlich.



# erstellt das Netzwerk k2, indem alle Knoten des Netzwerks karate gelöscht werden, die nicht den Wert "1" im Vertex-Attribut "Faction" haben. Es bleiben also nur nochn Mitglieder der Fraktion 2 übrig.
#WIr löschen alle Knoten raus, die eine bestimmte Eigenschaft nicht erfüllen 
#Gut für Teilnetzwerke
#Gelb hatte hier den Wert 2

s1 <- delete_vertices(pd[[1]], V(pd[[1]])[type == "2"])
plot(s1)

#Hier nochmal schauen 





