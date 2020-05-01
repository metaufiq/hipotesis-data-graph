library(igraph)

setwd("D:\\Project\\hipotesis-data-graph")

# relation = as.matrix(read.csv("relasi.csv",header=FALSE, sep=",",stringsAsFactors=FALSE))
data = as.matrix(read.csv("atribut.csv", sep=","))

productId = data[,1]
reviewUserId = data[,2]


relasi = as.matrix(read.csv("relasi.csv",header=FALSE, sep=","))
#buat graph
gpokec <- graph_from_adjacency_matrix(relasi)

#hipotesa 1
gpokec.productId <- productId
gpokec.reviewUserId <- reviewUserId


gpokec <- set.vertex.attribute(gpokec,'productId', index = V(gpokec), 
                               gpokec.productId)
gpokec <- set.vertex.attribute(gpokec,'reviewUserId', index = V(gpokec),
                               gpokec.reviewUserId)

V(gpokec)$reviewUserId[grepl("A3UCN2RGY7O6S1",V(gpokec)$reviewUserId)] <- 1
V(gpokec)$reviewUserId[V(gpokec)$reviewUserId != 1] <- 2



colrs <- c("tomato", "gold")
V(gpokec)$color <- colrs[as.integer(V(gpokec)$reviewUserId)]
deg <- degree(gpokec, mode="all")
print(deg)
V(gpokec)$size <- deg
V(gpokec)$label <- NA

E(gpokec)$arrow.size <- .2
E(gpokec)$edge.color <- "gray80"
l <- layout_with_graphopt(gpokec)
plot(gpokec,layout=l)
legend(x=-1.5, y=-1.1, c("Direview","Tidak direview"), pch=21,
       col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1)