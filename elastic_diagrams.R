library(igraph)
library(magrittr)
library(visNetwork)
library(data.table)

setwd("C:/Users/vincent/Desktop/Westpac/R code/Awesome picture")

mydata <- read.csv("VisNetwork for Beginners.csv", head = T)

move = names(mydata)[names(mydata) == "originator" | names(mydata)== "beneficiary"]
setcolorder(mydata, c(move, setdiff(names(mydata), move))) 
#Identify the networks using the igraph package.

graph <- graph.data.frame(mydata, directed=F)
graph <- simplify(graph)

# Metrics
DC = degree(graph)
BC = betweenness(graph)


networks <- clusters(as.undirected(graph))
V(graph)$network <- networks$membership
#Create the network graph (i.e. the interactive visual) using the visNetwork package.

nodes <- data.frame(id = V(graph)$name, title = V(graph)$name, group = V(graph)$network)
nodes <- nodes[order(nodes$id, decreasing = F),]

edges <- get.data.frame(graph, what="edges")
# 
# visNetwork(nodes, edges) %>%
#   visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)%>%
#   visGroups(groupname = "1", color = "maroon") 

#If you would like to save the interactive network visual as a standalone html, then simply use the following commands. Saving the visual as a standalone html will enable you to email your fantastic interactive network visual to your friend or colleague.


saveWidget(my_visNetwork, "My Network Graph.html")
# Network is a vertex attribute. Therefore, you can obtain the unique identifier using the vertex attribute as follows.

mynodes<-  data.frame(name = V(graph)$name, network = V(graph)$network)
# To merge the network unique identifier with your orginal dataset, use the following code:
  
  newdata<- merge(mydata, mynodes, by.x=c("originator"), by.y=c("name"))
# Move the network variable to the first column using the following code:
  
  move = names(newdata)[names(newdata) == "network"]
setcolorder(newdata, c(move, setdiff(names(newdata), move))) 
# To output your dataset with the network unique identifier, run the following code:
  
  write.csv(newdata, "Data with Network Unique ID.csv", row.names =F)








graph <- graph.data.frame(mydata, directed=T)
graph <- simplify(graph)

V(graph)$indegree <- centr_degree(graph, mode = "in")$res

nodes <- get.data.frame(graph, what="vertices")
nodes <- data.frame(id = nodes$name, title = nodes$name, group = nodes$indegree, indegree = nodes$indegree)
setnames(nodes, "indegree", "in-degree centrality")
nodes <- nodes[order(nodes$id, decreasing = F),]

edges <- get.data.frame(graph, what="edges")[1:2]

visNetwork(nodes, edges, height = "600px", width = "100%") %>%
  visOptions(selectedBy = "in-degree centrality", highlightNearest = TRUE, nodesIdSelection = TRUE)%>%
  visPhysics(stabilization = FALSE)