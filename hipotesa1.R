library(igraph)
setwd("D:\\Project\\hipotesis-data-graph")
#hipotesa 1: Apakah makanan yang direview oleh A3UCN2RGY7O6S1 populer ?

#mengambil data relasi
relation = as.matrix(read.csv("relasi.csv",header=FALSE, sep=",",stringsAsFactors=FALSE))

#jika ada relasi antara index baris dan kolomnya maka nilainya 1, jika tidak maka 1
relation[grepl("0",relation)] <- 0
relation[grepl("1",relation)] <- 1
attrb = as.matrix(read.csv("atribut.csv",header=FALSE, sep=","))


#mengambil attribut userId,jika nilainya 1 maka makanan tersebut direview oleh A3UCN2RGY7O6S1,jika tidak maka 0
attr_userId = attrb[,2]
attr_userId[grepl("A3UCN2RGY7O6S1",attr_userId)] <- 1
attr_userId[!attr_userId %in% c(1)] <- 0

gfoods <- graph_from_adjacency_matrix(relation)
gfoods.userId <- attr_userId


gfoods.degree <- degree(gfoods)
png(file="D:\\Project\\hipotesis-data-graph/scatterplot_hipotesa1.png",
width=600, height=350)
plot(gfoods.degree, gfoods.userId, main="Scatterplot reviedByUser ~ degree", 
  	xlab="degree", ylab="reviedByUser (0/1)", pch=19)
#hasil hipotesa: direview oleh A3UCN2RGY7O6S1 atau tidak,tidak mempengaruhi sebuah makanan populer pada amazon

gfoods.clustcoeff <- transitivity(gfoods, type="local", isolates = "zero")
plot(gfoods.clustcoeff, gfoods.userId, main="Scatterplot reviedByUser ~ clustcoeff", 
  	xlab="clustcoeff", ylab="reviedByUser (0/1)", pch=19)


data_cek <- as.data.frame(cbind(gfoods.userId, gfoods.indegree, gfoods.clustcoeff, attrb[,2]))
colnames(data_cek) <- c('reviedByUser', 'indegree', 'clustcoeff', 'gender')

set.seed(100)
trainingRowIndex <- sample(1:nrow(data_cek), 0.8*nrow(data_cek))
trainingData <- data_cek[trainingRowIndex, ]
testData  <- data_cek[-trainingRowIndex, ]

lm1  <- lm(reviedByUser ~ indegree, data=trainingData)
lm2  <- lm(reviedByUser ~ clustcoeff, data=trainingData)
lm3  <- lm(reviedByUser ~ indegree+clustcoeff, data=trainingData)
lm4  <- lm(reviedByUser ~ indegree+clustcoeff+gender, data=trainingData)

lm1.pred <- predict(lm1, testData)
lm2.pred <- predict(lm2, testData)
lm3.pred <- predict(lm3, testData)
lm4.pred <- predict(lm3, testData)

# display boxplots of predicted values for all models
pred_data <- as.data.frame(cbind(testData$reviedByUser,lm1.pred,lm2.pred,lm3.pred, lm4.pred))
colnames(pred_data) <- c('reviedByUser', 'indeg', 'clustcoeff', 'indeg+clust', 'indeg+clust+gender')
boxplot(pred_data, main="smoking behaviour")

library(stargazer) 
stargazer(lm1,lm2,lm3,lm4,type="text", 
	dep.var.labels=c("Smoking Behaviour"),	
	column.labels=c("indeg","clust","indeg+clust", "indeg+clust+gender"),
	covariate.labels=c("In-Degrees", "Clustering Coefficient", "Gender"),
	omit.stat=c("LL","ser","f")) 
