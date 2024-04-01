#einlesen des Excel- Datensatzes
install.packages("readxl")
library(readxl)
data_esc <- read_excel("data/eurovision_song_contest_1957_2023.xlsx")

#Subset erstellen um nur die Ergebnisse der Finale miteinander zu vergleichen.
#Anschließend sanity check der ersten Zeilen
data_esc_subset <- subset(data_esc, `(semi-) final` == "f" )
head(data_esc_subset)

#Knoten des Datensatzes anschließend bereinigen (Duplikate entfernen),
#so dass jedes Land nur einmal auftritt. Anschließend: Sanity check
knoten_fromlaender <- data_esc_subset$`From country`
countries_frombereinigt <- unique(knoten_fromlaender)
print(length(countries_frombereinigt))
print(countries_frombereinigt)

#Dataframe für die Knoten erstellen 
knoten <- data.frame(countries_frombereinigt)
knoten_fromlaender <- data_esc_subset$`From country`
countries_frombereinigt <- unique(knoten_fromlaender)

#sanity check, Länge und Länderübersicht
print(length(countries_frombereinigt))
print(countries_frombereinigt)

#Kanten erstellen als eigenes Datenframe mit der addierten Punkteanzahl,
#aufaddiert seit Aufzeichnungsbeginn.
#aggregate() Funktion bei "Stackoverflow" gefunden und angewandt
install.packages("dplyr")
library(dplyr)
sum_jahre <- aggregate(
  data_esc_subset$Points,
  by=list(`From country`=data_esc_subset$`From country`,`To country`=data_esc_subset$`To country`),
  FUN = sum)
sum_jahre <- sum_jahre %>%
  rename(addedpoints = x)

#Alle Zeilen die 0 summierte Punkte haben, werden entfernt.
#-> alle die nie für einander gevotet haben
sum_jahre <- subset(sum_jahre, `addedpoints` != 0 )

#Graph erstellen
install.packages("igraph")
library(igraph)

G <- graph.data.frame(sum_jahre, directed = TRUE, vertices = knoten)


#Erstellen eines Data Frames unter Beruecksichtigung des Kantengewichts (Hilfsmittel: Stackoverflow- Website)
kanten_weights <- data.frame(name= sum_jahre$`From country`, weight = sum_jahre$addedpoints)

#Hinzufügen der Gewichtung an die Kanten
#E() gibt die kanten zurück
E(G)$weight <- sum_jahre$addedpoints

#Sanity check 
V(G)$weight
E(G)$weight


print(G)
plot(G)

#Exportieren des R plots zu einer Datei, um diese dann in Gephi zu importieren
write_graph(G,"graph_escneu.gml", format = "gml")




#Degreezentralität
degree.G <- degree(G)
print(degree.G)

G <- set_vertex_attr(G, "degree", "index"= V(G), degree.G)
print(paste("Highest degree: ", sort(degree.G, decreasing = T)[1]))

sort(degree.G, decreasing = T)[1:35]

#Dichte
density_directed <- edge_density(G)
print(density_directed)


#Modularität: Identifikation von Gruppen (auskommentiert, da leider nicht aussagekräftig)
#G1 <- cluster_leading_eigen(G)
#G <- set_vertex_attr(G, "modularity", index = V(G), membership(G1))
#modularity(G1)
#head(membership(G1))