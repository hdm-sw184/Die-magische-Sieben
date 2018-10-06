# Beispiel Gesamtnetzwerk Studierende (Work- und Help-Netzwerk)
library(igraph)
# liest die Dateien direkt aus dem github-Verzeichnis ein
el <- read.csv("https://raw.githubusercontent.com/hdm-sw184/Die-magische-Sieben/master/226305_Projekt_WashNetzwerk_Personennetzwerk_Edgelist%20-%20Tabellenblatt1.csv", header=T, as.is=T, sep = ",")
nodes <- read.csv("https://raw.githubusercontent.com/hdm-sw184/Die-magische-Sieben/master/226305_Projekt_WashNetzwerk_Personennetzwerk_Nodelist%20-%20Tabellenblatt1.csv", header=T, as.is=T, sep = ",")
# prüft, ob alle Variablen eingelesen wurden
head(el)
head(nodes)
# wandelt die edgelist in eine Matrix um und baut das igraph-Objekt
hties <- as.matrix(el)
personen <- graph_from_data_frame(d=hties, vertices=nodes, directed=F)
personen
# addiert edges auf, wenn sie auf der gleichen Beziehung sind
p <- simplify(personen, edge.attr.comb = list(weight="sum"))
# ruft das finale igraph-Objekt auf.
p
# einfache Visualisierung
plot(p)

# Teilnetzwerk Rainer Funke
ego(p, order = 1, nodes = V(p)$name == "Rainer Funke", mode = "all")
funke <- make_ego_graph(p, order = 1, nodes = V(p)$name == "Rainer Funke", mode = "all")
plot(funke[[1]])

# Teilnetzwerk Gesine Schwan
ego(p, order = 1, nodes = V(p)$name == "Gesine Schwan", mode ="all")
schwan <- make_ego_graph(p, order = 1, nodes = V(p)$name == "Gesine Schwan", mode = "all")
plot(schwan[[1]])

# Teilnetzwerk Barbel Dieckmann
ego(p, order = 1, nodes = V(p)$name == "Baerbel Dieckmann", mode = "all")
dieckmann <- make_ego_graph(p, order = 1, nodes = V(p)$name == "Baerbel Dieckmann", mode = "all")
dieckmann
plot(dieckmann[[1]])

# Teilnetzwerk Baerbel Kofler
ego(p, order = 1, nodes = V(p)$name == "Baerbel Kofler", mode = "all")
kofler <- make_ego_graph(p, order = 1, nodes = V(p)$name == "Baerbel Kofler", mode = "all")
kofler
plot(kofler[[1]])

# Teilnetzwerk HeinzJoachim Kersting
ego(p, order = 1, nodes = V(p)$name == "HeinzJoachim Kersting", mode = "all")
kersting <- make_ego_graph(p, order = 1, nodes = V(p)$name == "HeinzJoachim Kersting", mode = "all")
kersting
plot(kersting[[1]])

# Vergleich von 4
par(mfrow=c(2,2), mar=c(0,0,3,0))
plot(funke[[1]], main="Rainer Funke")
plot(schwan[[1]], main="Gesine Schwan")
plot(kofler[[1]], main="Baerbel Kofler")
plot(dieckmann[[1]], main="Baerbel Dieckmann")
