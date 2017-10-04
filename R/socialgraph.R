#'Visualize a social network
#'
#'Function for plotting social networks with options to size nodes based on centrality values and/or edge widths
#'sized based on edge widths. (NOTE: centrality values are based on an unweighted network)
#'
#'@param edgelist a dataframe that contains a list of people and their associates. The first column represents
#'source nodes and the second column represents target nodes.
#'@param sizeby custom option to size nodes based on either "degree", "eigenvector", "betweenness", or "closeness" centrality.
#' Default to "none".
#'@param weighted custom option to size edges based on weighted values. Default to "FALSE".
#'
#'@details
#'Degree centrality identifies well-connected nodes.
#'Eigenvector centrality identifies nodes that are connected to well-connected nodes.
#'Betweenness centrality identifies nodes that serve as bridges.
#'Closeness centrality identifies nodes that are close to all other nodes in a network.
#'
#'@examples
#'# minimal example
#'source <- c("a", "a", "b", "c", "d", "d", "d")
#'target <- c("b", "c", "c", "d", "e", "f", "g")
#'associations <- data.frame(source, target)
#'social_graph(edgelist = associations, sizeby = "betweenness")
#'
#'# FM 3-24 example affiliation matrix
#'data("fm3_24_affiliation")
#'affiliations <- as.data.frame(fm3_24_affiliation)
#'associations <- transform2social(edgelist = affiliations)
#'social_graph(edgelist = associations, sizeby = "betweenness", weighted = TRUE)
#'
#'@rdname socialgraph
#'@export
social_graph <- function(edgelist, sizeby = "none", weighted = FALSE){

require(igraph)
require(visNetwork)

#Create a graph using igraph
g<-graph.edgelist(as.matrix(edgelist[,1:2]), directed=FALSE)
g<-as.undirected(g,mode="collapse")

#Calculate centrality measures
ifelse(sizeby=="degree", size<-degree(g),
       ifelse(sizeby=="eigenvector",size<-eigen_centrality(g)$vector,
       ifelse(sizeby=="betweenness",size<-betweenness(g,normalized=TRUE),
       ifelse(sizeby=="closeness",size<-closeness(g),size<-degree(g)/degree(g)
       ))))

#Incorporate weighted edges
ifelse(weighted==TRUE, E(g)$weight<-as.numeric(edgelist[,3]), E(g)$weight<-1)
edge.weights<-get.edge.attribute(g,'weight')

#Visualize the network
NamesEL<-get.edgelist(g,names=TRUE)
nodelabels<-unique(c(NamesEL[,1],NamesEL[,2]))
igraphEL<-get.edgelist(g, names=FALSE)
nodes<-data.frame(id=unique(c(igraphEL[,1],igraphEL[,2])),
                  label=nodelabels,
                  value=size[nodelabels])
ifelse(weighted==TRUE, edges<-data.frame(from=igraphEL[,1], to=igraphEL[,2],
                  label=edge.weights,
                  width=edge.weights),
                  edges<-data.frame(from=igraphEL[,1], to=igraphEL[,2])
)
visNetwork(nodes,edges)%>%
  visOptions(highlightNearest=list(enabled=TRUE, hover=FALSE),nodesIdSelection=TRUE)

}
