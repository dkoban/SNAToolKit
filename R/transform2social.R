#'Convert an affiliation network to an association network
#'
#'Function for transforming an edgelist consisting of affiliaiton data to an edgelist
#'containing association data. The resulting edgelist generates edge weights that correspond
#'to the number of shared activities
#'
#'@param edgelist a dataframe that contains a list of people and their associated activities. The first column
#'should list the name of the source node; the second column should list associated activities.
#'
#'@examples
#'# minimal example
#'agents <-  c("Bill", "Bill", "Bill", "Bob", "Bob", "Bob", "John", "John")
#'activities <- c("Act1","Act2", "Act3","Act1", "Act2","Act4","Act3","Act4")
#'affiliations <- data.frame(agents, activities)
#'transform2social(edgelist = affiliations)
#'
#'# FM 3-24 example affiliation matrix
#'data("fm3_24_affiliation")
#'affiliations <- as.data.frame(fm3_24_affiliation)
#'transform2social(edgelist = affiliations)
#'
#'@rdname transform2social
#'@export

transform2social <- function(edgelist){
require(igraph)
#Assign unique IDs for each node
sourceIdx<-as.numeric(as.factor(edgelist[,1]))
targetIdx<-max(sourceIdx)+as.numeric(as.factor(edgelist[,2]))

#Count the number of nodes by type.
n<-length(unique(sourceIdx))
m<-length(unique(targetIdx))

#Create a bipartite graph in igraph
biGraph<-graph.empty()
biGraph<-add.vertices(biGraph,nv=n,
                attr=list(name=paste0(unique(edgelist[,1])),
                type=rep(TRUE,n)))
biGraph<-add.vertices(biGraph,nv=m,
                attr=list(name=paste0(unique(edgelist[,2])),
                type=rep(FALSE,m)))
edgeListVec<-t(edgelist)
biGraph<-add.edges(biGraph,edgeListVec)

#Extract the affiliation matrix and transform it into a social network.
B<-get.incidence(biGraph)
A<-t(B)%*%B
sociogram<-graph.adjacency(A,diag=FALSE,weighted=TRUE) #remove self loops
sociogram<-as.undirected(sociogram,mode="collapse")
sourceTarget<-get.edgelist(sociogram,names=TRUE)
edge.weights<-get.edge.attribute(sociogram,'weight')/2
df<-data.frame(sourceTarget[,1],sourceTarget[,2],edge.weights)
colnames(df)<-c("source","target","weight")
df
}

