##Subset um Disqualifikation einiger Länder auf Richtigkeit zu überprüfen

data_esc_2022 <- subset(data_esc, 
                        Year == 2022 & Points != 0 & `(semi-) final` == "f")
#Aggregation der Punkte nach Ländern aller Jahre
install.packages("dplyr")
library(dplyr)
sum_jahre2 <- aggregate(
  data_esc_2022$Points,
  by=list(`From country`=data_esc_2022$`From country`,`To country`= data_esc_2022$`To country`),
  FUN = sum)
sum_jahre2 <- sum_jahre2 %>%
  rename(addedpoints = x)

knoten_fromlaender <- data_esc_2022$`From country`
countries_frombereinigt <- unique(knoten_fromlaender)
print(length(countries_frombereinigt))
print(countries_frombereinigt)

#Dataframe für die Knoten erstellen 
knoten <- data.frame(countries_frombereinigt)
knoten_fromlaender <- data_esc_2022$`From country`
countries_frombereinigt <- unique(knoten_fromlaender)
####
install.packages("igraph")
library(igraph)

G <- graph.data.frame(sum_jahre2, directed = TRUE, vertices = knoten)
print(G)


#Erstellen eines Data Frames unter Beruecksichtigung des Kantengewichts
kanten_weights2 <- data.frame(name= sum_jahre2$`From country`, weight = sum_jahre2$addedpoints)

#Hinzufügen der Gewichtung an die Kanten
#E() gibt die kanten zurück
E(G)$weight <- sum_jahre2$addedpoints

# Sanity check 
#V(G)$weight
#E(G)$weight

plot(G)




#Exportieren des R plots zu einer Datei, um diese dann in Gephi zu importieren
write_graph(G,"graph_escfinale22.gml", format = "gml")

