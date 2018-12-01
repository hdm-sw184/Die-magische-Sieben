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
# 1 = CDU - pink 2 = CSU - hellblau 3 = SPD - rot 4 = Bündnis 90 Grünen- grün 5 = FDP - gelb 6 = Die Linke - lila  7 = NGO - grau 8 = Keine Partei - orange

colrs <- c("pink", "lightblue", "red", "green", "yellow", "purple", "darkgrey", "orange")
V(p)$color <- colrs[V(p)$Partei]

# Plot des Gesamtnetzwerks

coords <- layout_with_kk(p)*1.0 # Entzerren

plot(p,
     layout = coords,
     vertex.size = 40,
     vertex.label.cex=.5, 
     vertex.frame.color="transparent",
     vertex.label.color="black",
     rescale=FALSE,
     main="Personennetzwerk",
     ylim=c(-3.5,4.5), xlim=c(-4.5,4.5))


# Betweenness
betweenness(p) 

# Durchmesser
diameter(p)

# Density/Dichte
edge_density(p)
