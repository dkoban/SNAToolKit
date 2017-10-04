#'Visualize an affiliation network
#'
#'Function for plotting affiliation networks.
#'
#'@param edgelist a dataframe that contains a list of people and their associated activities. The first column
#'should list the name of the source node; the second column should list associated activities.
#'
#'@examples
#'# minimal example
#'agents <-  c("Bill", "Bill", "Bill", "Bob", "Bob", "Bob", "John", "John")
#'activities <- c("Act1","Act2", "Act3","Act1", "Act2","Act4","Act3","Act4")
#'affiliations <- data.frame(agents, activities)
#'affiliation_graph(edgelist = affiliations)
#'
#'# FM 3-24 example affiliation matrix
#'data("fm3_24_affiliation")
#'affiliations <- as.data.frame(fm3_24_affiliation)
#'affiliation_graph(edgelist = affiliations)
#'
#'@rdname affiliationgraph
#'@export
#'

affiliation_graph <- function(edgelist){
require(igraph)

#Assign unique IDs for each node
sourceIdx=as.numeric(as.factor(edgelist[,1]))
targetIdx=max(sourceIdx)+as.numeric(as.factor(edgelist[,2]))

#Count the number of nodes by type.
#Since the network is bipartite, the source nodes are "agents" and the
#target nodes are "activities"
n=length(unique(sourceIdx))
m=length(unique(targetIdx))

#Visualize the network using the visNetwork package
nodelabels=c(paste(unique(edgelist[,1])),
             paste(unique(edgelist[,2])))
nodes=data.frame(id=c(unique(sourceIdx),unique(targetIdx)),
                 label=nodelabels,
                 group=c(rep("Agent",n),rep("Activities",m)))
edges=data.frame(from=sourceIdx,
                 to=targetIdx)

visNetwork(nodes,edges)%>%
  visEdges(arrows=list(to=list(enabled=TRUE,scaleFactor=2)))%>%
  visOptions(highlightNearest=list(enabled=TRUE,hover=FALSE),nodesIdSelection=TRUE)%>%
  visGroups(groupname="Activities",color="green",shape="square")%>%
  visLegend(position="right")

}

