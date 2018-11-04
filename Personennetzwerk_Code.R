library(igraph)
library(RColorBrewer)
# liest die Dateien direkt aus dem github-Verzeichnis ein
el <- read.csv("https://raw.githubusercontent.com/hdm-sw184/Die-magische-Sieben/master/226305_Personennetzwerk_Edgelist_WashNetzwerk.csv", header=T, as.is=T, sep = ",")
nodes <- read.csv("https://raw.githubusercontent.com/hdm-sw184/Die-magische-Sieben/master/226305_Personennetzwerk_Nodelist_WashNetzwerk.csv", header=T, as.is=T, sep = ",")
# prüft, ob alle Variablen eingelesen wurden
head(el)
head(nodes)
# wandelt die edgelist in eine Matrix um und baut das igraph-Objekt
hties <- as.matrix(el)
personen <- graph_from_data_frame(d=hties, vertices=nodes, directed=F)
personen
# addiert edges auf, wenn sie auf der gleichen Beziehung sind - BRAUCHEN WIR DAS?
p <- simplify(personen, edge.attr.comb = list(weight="sum"))
# ruft das finale igraph-Objekt auf.
p
# einfache Visualisierung
plot(p)

# Visualisierung Geschlecht
# 1 männlich: Rechteck; 2 weiblich: Kreis; 3 kein Geschlecht: Kreis
maennlich <- V(p)[sex == "1"] # wählt alle Knoten aus, die sex 1 haben
V(p)[maennlich]$shape = "square" # weist diesen Knoten Rechteck zu

weiblich <- V(p)[sex == "2"] 
V(p)[weiblich]$shape = "circle" 

neutral <- V(p)[sex == "3"] 
V(p)[neutral]$shape = "circle" 

# Farben der Partei zuweisen 
# 1 = CDU - pink/schwarz 2 = CSU - blau 3 = SPD - rot 4 = Bündnis 90 Grünen- grün 5 = FDP - gelb 6 = Die Linke - lila  7 = NGO - grau 8 = Keine Partei - weiß

colrs <- c("pink", "blue", "red", "green", "yellow", "purple", "darkgrey", "white")
V(p)$color <- colrs[V(p)$Partei]

# Plot des Gesamtnetzwerks

coords <- layout_with_kk(p)*0.3 # Entzerren

plot(p, layout=coords, vertex.label.cex=.7, 
     vertex.label=V(p)$name, 
     vertex.label.color = "black",
     vertex.label.dist =0)

coords <- layout_with_kk(p)*0.25

plot(p, edge.arrow.size=0.4, layout=coords, rescale=FALSE, edge.color="grey80", main="Personennetzwerk")

# Cluster
gcw <- cluster_walktrap(p) # Zuordnung der Knoten zueinander
modularity(gcw)
membership(gcw)

plot(gcw, p)

# Degree BRINGT NICHT WIRKLICH WAS
degree(p)

degree(p, mode="all", normalized = TRUE) # prozentualer Wert

# Betweenness
betweenness(p) # Wie wahrscheinlich ist es, dass dieser Knoten die Verbindung zu anderen Knoten im Netzwerk herstellen kann? - Bedeutung der schwachen Beziehungen

# Durchmesser
diameter(p)

# Density
edge_density(p)

# Teilnetzwerk Rainer Funke
ego(p, order = 1, nodes = V(p)$name == "Funke", mode = "all")
funke <- make_ego_graph(p, order = 1, nodes = V(p)$name == "Funke", mode = "all")
plot(funke[[1]], main="Ego-Netzwerk Rainer Funke", vertex.label.cex=.7)

# Teilnetzwerk Gesine Schwan
ego(p, order = 1, nodes = V(p)$name == "Schwan", mode ="all")
schwan <- make_ego_graph(p, order = 1, nodes = V(p)$name == "Schwan", mode = "all")
plot(schwan[[1]], main="Ego-Netzwerk Gesine Schwan", vertex.label.cex=.7)

# Teilnetzwerk Barbel Dieckmann
ego(p, order = 1, nodes = V(p)$name == "Dieckmann", mode = "all")
dieckmann <- make_ego_graph(p, order = 1, nodes = V(p)$name == "Dieckmann", mode = "all")
dieckmann
plot(dieckmann[[1]], main="Ego-Netzwerk Bärbel Dieckmann", vertex.label.cex=.7)

# Teilnetzwerk Baerbel Kofler
ego(p, order = 1, nodes = V(p)$name == "Kofler", mode = "all")
kofler <- make_ego_graph(p, order = 1, nodes = V(p)$name == "Kofler", mode = "all")
plot(kofler[[1]], main="Ego-Netzwerk Bärbel Kofler", vertex.label.cex=.7)

# Teilnetzwerk HeinzJoachim Kersting
ego(p, order = 1, nodes = V(p)$name == "Kersting", mode = "all")
kersting <- make_ego_graph(p, order = 1, nodes = V(p)$name == "Kersting", mode = "all")
kersting
plot(kersting[[1]], main="Ego-Netzwerk Heinz-Joachim Kersting", vertex.label.cex=.7)

# Teilnetzwerk Karin Roth
ego(p, order = 1, nodes = V(p)$name == "Roth", mode = "all")
roth <- make_ego_graph(p, order = 1, nodes = V(p)$name == "Roth", mode = "all")
roth
plot(kersting[[1]], main="Ego-Netzwerk Karin Roth", vertex.label.cex=.7)
