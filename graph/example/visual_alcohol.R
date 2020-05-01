library(igraph)
library(visNetwork)
#Ambil Data
data = as.matrix(read.csv("atribut_fix.csv", sep=","))
relasi = as.matrix(read.csv("relasi.csv",header=FALSE, sep=","))
#kolom 1=id, kolom 2= gender, kolom 3=age,kolom 4=smoking, 
#kolom5=alcoholic,kolom6=jumTeman
sum_nei=data[,6]
attr_smoke=data[,4]
attr_gender=data[,2]
attr_age=data[,3]
attr_alcohol=data[,5]
#buat graph
gpokec <- graph_from_adjacency_matrix(relasi)


#hipotesa 3 (apakah jumlah teman seseorang dipengaruhi oleh popularitas orang tersebut)
#hitung indegree outdegreee masing masing node

gpokec.alcohol <- attr_alcohol
gpokec.age <- attr_age


gpokec <- set.vertex.attribute(gpokec,'alcohol', index = V(gpokec), 
                               gpokec.alcohol)
gpokec <- set.vertex.attribute(gpokec,'age', index = V(gpokec),
                               gpokec.age)

V(gpokec)$alcohol[V(gpokec)$alcohol == 0] <- 2
colrs <- c("tomato", "gold")
V(gpokec)$color <- colrs[V(gpokec)$alcohol]
V(gpokec)$size <- V(gpokec)$age*0.1
V(gpokec)$label <- NA

E(gpokec)$arrow.size <- .2
E(gpokec)$edge.color <- "gray80"
l <- layout_with_graphopt(gpokec)
plot(gpokec,layout=l)
legend(x=-1.5, y=-1.1, c("Minum Alkohol","Tidak Minum Alkohol"), pch=21,
       col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1)
