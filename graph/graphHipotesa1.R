library(igraph)

setwd("D:\\Project\\hipotesis-data-graph\\graph")

# relation = as.matrix(read.csv("relasi.csv",header=FALSE, sep=",",stringsAsFactors=FALSE))
nodes <- read.csv("nodes.csv",header=TRUE, sep=",",as.is=TRUE)
links <- read.csv("relasiGraph.csv", header=FALSE, sep=",",as.is=TRUE)

net <- graph_from_data_frame(d=links, vertices=nodes, directed=FALSE)
# deg <- degree(net, mode="all")
print(V(net))
plot(net, vertex.size=deg*3)
