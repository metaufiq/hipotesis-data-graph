library(igraph)

setwd("D:\\Project\\hipotesis-data-graph")

# mengambil data atribut
data = as.matrix(read.csv("atribut.csv", sep=","))


#mengambil atribut productId dan reviewUserId 
productId = data[,1]
reviewUserId = data[,2]

#mengambil data relasi dari adjency matrix yang ada di relasi.csv
relasi = as.matrix(read.csv("relasi.csv",header=FALSE, sep=","))

#buat graph
gpokec <- graph_from_adjacency_matrix(relasi)

#hipotesa 1: Apakah makanan yang direview oleh A3UCN2RGY7O6S1 populer ?

#menambahkan attribute yang sudah dibuat pada masing-masing node
gpokec.productId <- productId
gpokec.reviewUserId <- reviewUserId
gpokec <- set.vertex.attribute(gpokec,'productId', index = V(gpokec), 
                               gpokec.productId)
gpokec <- set.vertex.attribute(gpokec,'reviewUserId', index = V(gpokec),
                               gpokec.reviewUserId)


#jika suatu produk direview oleh A3UCN2RGY7O6S1 maka nilai reviewUserId menjadi 1,jika tidak maka dirubah menjadi 2
V(gpokec)$reviewUserId[grepl("A3UCN2RGY7O6S1",V(gpokec)$reviewUserId)] <- 1
V(gpokec)$reviewUserId[V(gpokec)$reviewUserId != 1] <- 2

#konfigurasi hasil graph,termasuk pewarnaan
#jika warna node adalah tomat,maka node tersebut 
colrs <- c("tomato", "gold")
V(gpokec)$color <- colrs[as.integer(V(gpokec)$reviewUserId)]
deg <- degree(gpokec, mode="all")

#semakin banyak relasi dengan suatu node,semakin besar node tersebut direview oleh A3UCN2RGY7O6S1, jika tidak maka warna node tersebut adalah emas(kuning)
V(gpokec)$size <- deg

V(gpokec)$label <- NA
E(gpokec)$arrow.size <- .2
E(gpokec)$edge.color <- "gray80"
l <- layout_with_graphopt(gpokec)
png(file="D:\\Project\\hipotesis-data-graph/graph_hipotesa1.png",
width=600, height=350)
plot(gpokec,layout=l)
legend(x=-1.5, y=-1.1, c("Direview","Tidak direview"), pch=21,
       col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1)
dev.off()