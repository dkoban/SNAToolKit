#'Create a table of centrality values
#'
#'Function for generating a table of the nodes ranked by four common centrality measures (degree,
#'eigenvector, betweenness, and closeness centrality)
#'
#'@param edgelist a dataframe that contains a list of people and their associates. The first column represents
#'source nodes and the second column represents target nodes.
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
#'centralitytable(edgelist = associations)
#'
#'# FM 3-24 example association matrix
#'data("fm3_24_affiliations")
#'affiliations <- as.data.frame(fm3_24_affiliations)
#'associations <- transform2social(edgelist = affiliations)
#'centralitytable(edgelist = associations)
#'
#'@rdname centralitytable
#'@export

centralitytable<-function(edgelist){
require(igraph)

#Create a graph using igraph
g<-graph.edgelist(as.matrix(edgelist[,1:2]), directed=FALSE)
g<-as.undirected(g,mode="collapse")

#Calculate centrality measures
deg<-degree(g)
degdf<-data.frame(degree=sort(deg,decreasing=TRUE))
eig<-eigen_centrality(g)$vector
eigdf<-data.frame(eigenvector=sort(eig,decreasing=TRUE))
btwn<-betweenness(g,normalized=TRUE)
btwndf<-data.frame(betweenness=sort(btwn,decreasing=TRUE))
cls<-closeness(g)
clsdf<-data.frame(closeness=sort(cls,decreasing=TRUE))

all<-data.frame(rownames(degdf),degdf[,1],
                  rownames(eigdf),round(eigdf[,1],3),
                  rownames(btwndf),round(btwndf[,1],3),
                  rownames(clsdf),round(clsdf[,1],3)
                  )
colnames(all)<-c(" ","deg"," ","eig"," ","btwn"," ","cls")
all
}

