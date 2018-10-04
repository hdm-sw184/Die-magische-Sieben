# Beispiel Gesamtnetzwerk Studierende (Work- und Help-Netzwerk)
library(igraph)
# liest die Dateien direkt aus dem github-Verzeichnis ein
el <- read.csv("https://raw.githubusercontent.com/hdm-sw184/Die-magische-Sieben/master/226305_Projekt_WashNetzwerk_Unterst%C3%BCtzernetzwerk_Edgelist.csv", header=T, as.is=T, sep = ",")
nodes <- read.csv("https://raw.githubusercontent.com/hdm-sw184/Die-magische-Sieben/master/226305_Projekt_WashNetzwerk_Unterst%C3%BCtzernetzwerk_Nodelist.csv", header=T, as.is=T, sep = ",")
# prüft, ob alle Variablen eingelesen wurden
head(el)
head(nodes)
# wandelt die edgelist in eine Matrix um und baut das igraph-Objekt
hties <- as.matrix(el)
students <- graph_from_data_frame(d=hties, vertices=NULL, directed=T)
students
# addiert edges auf, wenn sie auf der gleichen Beziehung sind
s <- simplify(students, edge.attr.comb = list(weight="sum"))
# ruft das finale igraph-Objekt auf.
s
# einfache Visualisierung
plot(s)