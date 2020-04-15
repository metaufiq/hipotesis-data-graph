#Data berisi 197 siswa --> data relasi, ukuran matriks 199x197
#cell(i,j) = 1 berarti siswa-i berteman dengan siswa-j
library(igraph)
setwd("D:\\Project\\hipotesis-data-graph")

relation = as.matrix(read.csv("relasi.csv",header=FALSE, sep=",",stringsAsFactors=FALSE))
relation[grepl("0",relation)] <- 0
relation[grepl("1",relation)] <- 1
attrb = as.matrix(read.csv("atribut.csv",header=FALSE, sep=","))


attr_userId = attrb[,2]
attr_userId[grepl("A3UCN2RGY7O6S1",attr_userId)] <- 1
attr_userId[!attr_userId %in% c(1)] <- 0

gfoods <- graph_from_adjacency_matrix(relation)
gfoods.userId <- attr_userId


gfoods.indegree <- degree(gfoods)
plot(gfoods.indegree, gfoods.userId, main="Scatterplot smoke ~ indegree", 
  	xlab="indegree", ylab="smoke (0/1)", pch=19)

gfoods.clustcoeff <- transitivity(gfoods, type="local", isolates = "zero")
plot(gfoods.clustcoeff, gfoods.userId, main="Scatterplot smoke ~ clustcoeff", 
  	xlab="clustcoeff", ylab="smoke (0/1)", pch=19)


data_cek <- as.data.frame(cbind(gfoods.userId, gfoods.indegree, gfoods.clustcoeff, attrb[,2]))
colnames(data_cek) <- c('smoke', 'indegree', 'clustcoeff', 'gender')

set.seed(100)
trainingRowIndex <- sample(1:nrow(data_cek), 0.8*nrow(data_cek))
trainingData <- data_cek[trainingRowIndex, ]
testData  <- data_cek[-trainingRowIndex, ]

lm1  <- lm(smoke ~ indegree, data=trainingData)
lm2  <- lm(smoke ~ clustcoeff, data=trainingData)
lm3  <- lm(smoke ~ indegree+clustcoeff, data=trainingData)
lm4  <- lm(smoke ~ indegree+clustcoeff+gender, data=trainingData)

lm1.pred <- predict(lm1, testData)
lm2.pred <- predict(lm2, testData)
lm3.pred <- predict(lm3, testData)
lm4.pred <- predict(lm3, testData)

# display boxplots of predicted values for all models
pred_data <- as.data.frame(cbind(testData$smoke,lm1.pred,lm2.pred,lm3.pred, lm4.pred))
colnames(pred_data) <- c('smoke', 'indeg', 'clustcoeff', 'indeg+clust', 'indeg+clust+gender')
boxplot(pred_data, main="smoking behaviour")

library(stargazer) 
stargazer(lm1,lm2,lm3,lm4,type="text", 
	dep.var.labels=c("Smoking Behaviour"),	
	column.labels=c("indeg","clust","indeg+clust", "indeg+clust+gender"),
	covariate.labels=c("In-Degrees", "Clustering Coefficient", "Gender"),
	omit.stat=c("LL","ser","f")) 
