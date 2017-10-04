#'Community detection using edge betweenness
#'
#'Function that identifies structurally-based groups using the Girvan–Newman, edge betweenness community detection algorithm
#'and then produces a plot where nodes are colored based on group membership.
#'
#'@param edglist a dataframe that contains a list of people and their associates. The first column represents
#'source nodes and the second column represents target nodes.
#'@param sizeby custom option to size nodes based on either "degree", "eigenvector", "betweenness", or "closeness" centrality.
#' Default to "none".
#'@param weighted custom option to size edges based on weighted values. Default to "FALSE".
#'
#'@examples
#'# minimal example
#'source <- c("a", "a", "b", "c", "d", "d", "d")
#'target <- c("b", "c", "c", "d", "e", "f", "g")
#'associations <- data.frame(source, target)
#'graph_communities(edgelist = associations, sizeby = "betweenness")
#'
#'# FM 3-24 example affiliation matrix
#'data("fm3_24_affiliation")
#'affiliations <- as.data.frame(fm3_24_affiliation)
#'associations <- transform2social(edgelist = affiliations)
#'graph_communities(edgelist = associations, sizeby = "betweenness", weighted = TRUE)
#'
#'@rdname graphcommunities
#'@export

graph_communities <- function(edgelist, sizeby = "none", weighted=FALSE){

require(igraph)
require(visNetwork)

#Create a graph using igraph
g<-graph_from_edgelist(as.matrix(edgelist[,1:2]),directed=FALSE)
g<-as.undirected(g,mode="collapse")

#Identify communities using edge betweeness
grps<-cluster_edge_betweenness(g)
grptable<-data.frame(grps$names,grps$membership)

#Visualize the network in visNetwork
ifelse(sizeby=="degree",size<-degree(g),
       ifelse(sizeby=="eigenvector",size<-eigen_centrality(g)$vector,
       ifelse(sizeby=="betweenness",size<-betweenness(g,normalized=TRUE),
       ifelse(sizeby=="closeness",size<-closeness(g),size<-degree(g)/degree(g)
                            ))))

#Incorporate weighted edges
ifelse(weighted==TRUE, E(g)$weight<-as.numeric(edgelist[,3]), E(g)$weight<-1)
edge.weights<-get.edge.attribute(g,'weight')

igraphEL<-get.edgelist(g,names=FALSE)
nodes<-data.frame(id=c(1:length(grps$names)),
                  label=grps$names,
                  group=grps$membership,
                  value=size[grps$names])
ifelse(weighted==TRUE, edges<-data.frame(from=igraphEL[,1], to=igraphEL[,2],
                                         label=edge.weights,
                                         width=edge.weights),
       edges<-data.frame(from=igraphEL[,1], to=igraphEL[,2])
)
visNetwork(nodes,edges)%>%
  visOptions(highlightNearest=list(enabled=TRUE, hover=FALSE),nodesIdSelection=TRUE)
}

