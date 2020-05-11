#hipotesa 2:Apakah helpfulness yang tinggi menentukan produk populer ?
library(igraph)
setwd("D:\\Project\\hipotesis-data-graph")

#mengambil data relasi
relation = as.matrix(read.csv("relasi.csv",header=FALSE, sep=",",stringsAsFactors=FALSE))

#jika ada relasi antara index baris dan kolomnya maka nilainya 1, jika tidak maka 1
relation[grepl("0",relation)] <- 0
relation[grepl("1",relation)] <- 1
attrb = as.matrix(read.csv("atribut.csv",header=FALSE, sep=","))

#mengambil attribut  helpfulness
attr_reviewHelpfulness = attrb[,3]

gfoods <- graph_from_adjacency_matrix(relation)
gfoods.reviewHelpfulness <- attr_reviewHelpfulness


gfoods.indegree <- degree(gfoods)
png(file="D:\\Project\\hipotesis-data-graph/scatterplot_hipotesa2.png",
width=600, height=350)
plot(gfoods.indegree, gfoods.reviewHelpfulness, main="Scatterplot helpfulness ~ indegree", 
  	xlab="indegree", ylab="helpfulness", pch=19)
	  
#hasil hipotesa: tinggi tidaknya rata-rata helpfullness review pada makanan tidak mempengaruhi sebuah makanan populer pada amazon
dev.off()
gfoods.clustcoeff <- transitivity(gfoods, type="local", isolates = "zero")
plot(gfoods.clustcoeff, gfoods.reviewHelpfulness, main="Scatterplot helpfulness ~ clustcoeff", 
  	xlab="clustcoeff", ylab="helpfulness", pch=19)


data_cek <- as.data.frame(cbind(gfoods.reviewHelpfulness, gfoods.indegree, gfoods.clustcoeff, attrb[,2]))
colnames(data_cek) <- c('helpfulness', 'indegree', 'clustcoeff', 'gender')

set.seed(100)
trainingRowIndex <- sample(1:nrow(data_cek), 0.8*nrow(data_cek))
trainingData <- data_cek[trainingRowIndex, ]
testData  <- data_cek[-trainingRowIndex, ]

lm1  <- lm(helpfulness ~ indegree, data=trainingData)
lm2  <- lm(helpfulness ~ clustcoeff, data=trainingData)
lm3  <- lm(helpfulness ~ indegree+clustcoeff, data=trainingData)
lm4  <- lm(helpfulness ~ indegree+clustcoeff+gender, data=trainingData)

lm1.pred <- predict(lm1, testData)
lm2.pred <- predict(lm2, testData)
lm3.pred <- predict(lm3, testData)
lm4.pred <- predict(lm3, testData)

# display boxplots of predicted values for all models
pred_data <- as.data.frame(cbind(testData$helpfulness,lm1.pred,lm2.pred,lm3.pred, lm4.pred))
colnames(pred_data) <- c('helpfulness', 'indeg', 'clustcoeff', 'indeg+clust', 'indeg+clust+gender')
boxplot(pred_data, main="smoking behaviour")

library(stargazer) 
stargazer(lm1,lm2,lm3,lm4,type="text", 
	dep.var.labels=c("Smoking Behaviour"),	
	column.labels=c("indeg","clust","indeg+clust", "indeg+clust+gender"),
	covariate.labels=c("In-Degrees", "Clustering Coefficient", "Gender"),
	omit.stat=c("LL","ser","f")) 
