
library(tidyverse)
library(readr)
library(igraph)

# get s1 edgelist

edgefile <- "C:/Users/Jacob/Desktop/Orgaimi.csv"
edgelist <- readr::read_csv(edgefile)

View(edgelist)

seasons <- 1:5 # <- adjust if you want to focus on specific seasons

edgelist <- edgelist %>% 
  dplyr::filter(Season %in% seasons)

# create igraph network with weighted edges

edgelist_matrix <- as.matrix(edgelist[ ,1:2])

axiom_graph <- igraph::graph_from_edgelist(edgelist_matrix, directed = FALSE) %>% 
  igraph::set.edge.attribute("weight", value = edgelist$Weight)


#Quick look at Axiom graph.  

l <- igraph::layout_with_mds(axiom_graph)
plot(axiom_graph, vertex.label = NA, vertex.size = 5, rescale = F, layout = l*0.02)

## Using the Louvain community detection algorithm

# run louvain with edge weights

louvain_partition <- igraph::cluster_louvain(axiom_graph, weights = E(axiom_graph)$weight)

axiom_graph$community <- louvain_partition$membership

sizes(louvain_partition) %>% 
  knitr::kable()

#So we have a couple of very small communities here - lets take a look at who is in these.  
membership(louvain_partition)[which(membership(louvain_partition) %in% c(6,7))] 

## Characterizing the main communities



high_degree_nodes <- c()

for (i in 1:8) {
  subgraph <- induced_subgraph(axiom_graph, v = which(axiom_graph$community == i))
  degree <-  igraph::degree(subgraph)
  high_degree_nodes[i] <- names(which(degree == max(degree)))
}

high_degree_nodes[c(1:5, 8)]

high_btwn_nodes <- c()

for (i in 1:8) {
  subgraph <- induced_subgraph(axiom_graph, v = which(axiom_graph$community == i))
  btwn <-  igraph::betweenness(subgraph)
  high_btwn_nodes[i] <- names(which(btwn == max(btwn)))
}

high_btwn_nodes[c(1:5, 8)]

## Visualizing the communities

# give our nodes some properties, incl scaling them by degree and coloring them by community

V(axiom_graph)$size <- degree(axiom_graph)/10
V(axiom_graph)$frame.color <- "white"
V(axiom_graph)$color <- axiom_graph$community
V(axiom_graph)$label <- V(axiom_graph)$name

# also solor edges according to their starting node
edge.start <- ends(axiom_graph, es = E(axiom_graph), names = F)[,1]
E(axiom_graph)$color <- V(axiom_graph)$color[edge.start]
E(axiom_graph)$arrow.mode <- 0

# only label central characters

v_labels <- which(V(axiom_graph)$name %in% high_degree_nodes[c(1:5, 8)])

for (i in 1:length(V(axiom_graph))) {
  if (!(i %in% v_labels)) {
    V(axiom_graph)$label[i] <- ""
  }
}

# plot network
l1 <- layout_on_sphere(axiom_graph)
plot(axiom_graph, rescale = F, layout = l1)

#multidimensional Layout
l2 <- layout_with_mds(axiom_graph)
plot(axiom_graph, rescale = F, layout = l2*0.02)


