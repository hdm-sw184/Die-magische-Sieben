# Challenge
# Tag 3
# Wenn Sie die Anforderungen erfüllt haben und Ihr Skript mit Lösungen auf Moodle hochgeladen haben, können Sie gehen (alternativ natürlich mir noch Fragen stellen!)

# Aufgabe: Beantworten Sie mit einer Visualisierung aus dem vorgegebenen Studentennetzwerk folgende Fragen:

# 1. Welche Personen sind im Arbeits- und Unterstützungsnetzwerk am populärsten? (Wer wird am meisten um Rat gefragt? Mit wem wird am liebsten zusammengearbeitet?) Sind es die gleichen Personen? Die Visualisierung soll beide Netzwerke vergleichen (beide Visualisierungen mit Überschriften nebeneinander.

# 2. Belegen Sie dies mit einem Ego-Netzwerk der am besten vernetzten Raucher. Wer ist direkt mit dem Raucher verbunden? Sind Raucher die besseren Broker zwischen Netzwerken? (Tipp: Betweenness).
# Bonus-Frage: Sind Raucher/innen auch tätowiert? Wenn ja, wie viele?

# 3. Welche Vertiefungsrichtung arbeitet mehr/weniger? CR oder PR? Visualisieren Sie das bitte mit einem Farbverlauf (Tipp: Sie müssen vorher die Netzwerke trennen)

# 4. Vergleichen Sie die Cluster des Arbeits- und Ratsuche-Netzwerk, nachdem Sie die Gewichte der jeweiligen Netzwerke mit dem simplify Befehl aufaddiert haben. Visualisieren Sie die Cluster nebeneinander mit dem walktrap Algorithmus. Was fällt Ihnen dabei auf.

# Bitte laden Sie das R-Skript mit Antworten auf die Fragen auf Moodle hoch.


# Bitte verwenden ausschließlich folgenden Datensatz
# Codebuch ist hier hinterlegt
# https://github.com/hdm-crpr/226305/blob/master/data/students/codierung.rmd

library("igraph")
el <- read.csv("https://raw.githubusercontent.com/hdm-crpr/226305/master/data/students/edges.csv", header=T, as.is=T, sep = ",")
nodes <- read.csv("https://raw.githubusercontent.com/hdm-crpr/226305/master/data/students/nodes.csv", header=T, as.is=T, sep = ",")
eties <-as.matrix(el)
s <- graph_from_data_frame(d=eties, vertices=nodes, directed=T)

# Frage 1

# Selektion in Teilnetzwerk w (work) und h (help)
edge.attributes(s_all)
edge.attributes(s_all)$relation
# das edge attribut relation ist in 6 und 7 kodiert.
# 6 ist das Arbeitsnetzwerk (Projektarbeit), 7 das Hilfsnetzwerk (Ratsuche)

w <- subgraph.edges(s_all, E(s_all)[relation == 6])
w
edge.attributes(w)$relation
plot(w, edge.arrow.size=0.1, main="Arbeitsnetzwerk")

h <- subgraph.edges(s_all, E(s_all)[relation == 7])
h
edge.attributes(h)$relation
plot(h, edge.arrow.size=0.1, main="Hilfsnetzwerk")


# direkter Vergleich der beiden Netzwerke herstellen

par(mfrow=c(1,2), mar=c(0,0,2,0))
plot(w, layout = layout_with_fr, edge.arrow.size=0.1, main="Arbeitsnetzwerk")
plot(h, layout = layout_with_fr, edge.arrow.size=0.1, main="Hilfsnetzwerk")
?par

# Jetzt interessieren uns aber die Indegrees, da wir ja herausfinden wollen, wer am meisten um Rat gefragt wird
ins <- degree(s, mode = "in")
ins
plot(w, layout = layout_nicely, vertex.size=ins*1.5, edge.color="darkgrey", main="Arbeitsnetzwerk")


# Jetzt interessieren uns aber die Indegrees, da wir ja herausfinden wollen, wer am meisten um Rat gefragt wird
ins <- degree(s, mode = "in")
ins
plot(h, layout = layout_nicely, vertex.size=ins*1.5, edge.color="darkgrey", main="Hilfsnetzwerk")

# Antwort: Im Arbeits- und Unterstützungsnetzwerk ist die Person 18 am populärsten.

# Frage 2

V(s)$color = "red"

smokers <- V(s)[smoke == "2"]

smokers

V(s)[smokers]$color = "blue"

#Durch Ermittlung des Betweenness-Wertes der Raucher herausfinden, wer von ihnen am besten vernetzt ist
plot(s)
bs <- betweenness(s, v = V(s)[smokers],
                  directed = TRUE,
                  weights = NULL,
                  nobigint = TRUE,
                  normalized = TRUE)
bs

plot(s,
     vertex.size=bs*100,
     edge.arrow.size=.2,
     main="Verteilung Betweenness im Netzwerk")
#Person 19 hat einen Betweenness-Wert von 0,177, damit den höchsten Wert aller Raucher. 

smoker19 <- make_ego_graph(s,
                           order = 1,
                           nodes = V(s)$name == 19,
                           mode ="in")
plot(smoker19[[1]],
     edge.arrow.size=.2,
     edge.curved=.2,
     main="Der bestvernetzte Raucher",
     sub="Ego-Netzwerk des Rauchers mit dem höchsten Betweenness-Wert")
#Direkt mit Raucher 19 verbunden sind die Personen 4,7,9,11,27,31,33,38. 

nonsmokers <- V(s)[smoke == "1"]
bs <- betweenness(s, v = V(s)[nonsmokers],
                  directed = TRUE,
                  weights = NULL,
                  nobigint = TRUE,
                  normalized = TRUE)
bs
#Wir errechnen den Betweenness-Wert der Nichtraucher, um ihn mit dem der Raucher zu vergleichen.

smokers <- delete_vertices(s, V(s)[smoke == "1"])
smokers
plot(smokers)

nonsmokers <- delete_vertices(s, V(s)[smoke == "2"])
nonsmokers
plot(nonsmokers)
#Wir erstellen ein Raucher- und ein Nichtrauchernetzwerk. 

edge_density(smokers)
edge_density(nonsmokers)
#Die Dichte des Rauchernetzwerks ist höher als die des Nichtrauchernetzwerks. Also sind die Raucher besser vernetzt.

# Frage 3

s_all <- s

vertex.attributes(s_all)
vertex.attributes(s_all)$crpr

# CR Netzwerk

# Trennen
cr <- delete_vertices(s_all, V(s_all)[crpr == "1"])
cr
plot(cr, edge.arrow.size=0.1, main="CR Netzwerk")

# Unterscheidung job: 1 = keinen, 2 = bis 5h/Woche, 3 = bis 10h/Woche, 4 = mehr als 10h/Woche
vertex.attributes(cr)$job
colrs <- c("yellow", "green", "orange", "red")
V(cr)$color <- colrs[V(cr)$job]
plot(cr, edge.arrow.size=0.1, main="CR Netzwerk")

# PR Netzwerk
pr <- delete_vertices(s_all, V(s_all)[crpr == "2"])
pr
plot(pr, edge.arrow.size=0.1, main="PR Netzwerk")

# Unterscheidung job
vertex.attributes(pr)$job
colrs <- c("yellow", "green", "orange", "red")
V(pr)$color <- colrs[V(pr)$job]
plot(pr, edge.arrow.size=0.1, main="PR Netzwerk")

# Vergleich

par(mfrow=c(1,2), mar=c(0,0,2,0))
plot(cr, edge.arrow.size=0.1, main="CR Netzwerk")
plot(pr, edge.arrow.size=0.1, main="PR Netzwerk")

# Antwort
# In Der CR Vertiefung arbeiten 7 Leute mehr als 10h/Woche, in der PR Vertiefung 12.
# Das heißt, in der PR Vertiefungsrichtung arbeiten mehr.

# Frage 4

s_all <- s

edge.attributes(s_all)
edge.attributes(s_all)$relation

# Arbeitsnetzwerk

# Trennen
w <- subgraph.edges(s_all, E(s_all)[relation == 6])
w
edge.attributes(w)$relation
plot(w, edge.arrow.size=0.1, main="Arbeitsnetzwerk")

# Simplify (Weights addieren)
edge.attributes(w)$weight
w1 <- simplify(w, edge.attr.comb = list(weight="sum"))
edge.attributes(w1)$weight

# Cluster
gcw <- cluster_walktrap(w) # Zuordnung der Knoten zueinander
modularity(gcw)
membership(gcw)
plot(gcw, w, edge.arrow.size=.2, main="Clusteranalyse des Arbeitsnetzwerks")

# Cluster Unterstützernetzwerk

h <- subgraph.edges(s_all, E(s_all)[relation == 7])
h
edge.attributes(h)$relation
plot(h, edge.arrow.size=0.1, main="Hilfsnetzwerk")

# Simplify (Weights addieren)
edge.attributes(h)$weight
h1 <- simplify(h, edge.attr.comb = list(weight="sum"))
edge.attributes(h1)$weight

# Cluster
gch <- cluster_walktrap(h) # Zuordnung der Knoten zueinander
modularity(gch)
membership(gch)
plot(gch, h, edge.arrow.size=.2, main="Clusteranalyse des Unterstützernetzwerks")

# Vergleich Arbeits- und Unterstützernetzwerk

par(mfrow=c(1,2), mar=c(0,0,2,0))
plot(gcw, w, edge.arrow.size=.2, main="Clusteranalyse des Arbeitsnetzwerks")
plot(gch, h, edge.arrow.size=.2, main="Clusteranalyse des Unterstützernetzwerks")

# Was fällt auf?
# Es fällt auf, dass man die Personen, mit denen man gerne zusammenarbeiten, generell auch um Hilfe fragt.
# Allerdings nicht immer, da es beim Unterstützernetzwerk ein sehr großes Cluster gibt, das aus mehreren Clustern des Arbeitsnetzwerks besteht.
