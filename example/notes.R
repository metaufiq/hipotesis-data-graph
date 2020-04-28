#Data berisi 197 siswa --> data relasi, ukuran matriks 199x197
#cell(i,j) = 1 berarti siswa-i berteman dengan siswa-j
library(igraph)
setwd("D:\\Project\\hipotesis-data-graph\\example")

data_friends = as.matrix(read.csv("friendship.csv",header=FALSE, sep=","))
attrb = as.matrix(read.csv("attrb.csv",header=FALSE, sep=","))

#data atribut: 1-6, jumlah baris dalam data atribut sama dengan data relasi
#(1=never smoked, 2=only ever tried smoking once, 3=used to smoke but never smoke now, 
# 4=<1 cigarette a week, 5=1-6 cigarettes a week, 6=>6 cigarettes a week)
attr_smoke = attrb[,1] #kolom 1: utk smoking dan kolom 2: utk gender (1=boy, 2=girl)
attr_smoke[attr_smoke %in% c(1,2,3)] <- 0 #... no need
attr_smoke[attr_smoke %in% c(4,5,6)] <- 1


gfriends <- graph_from_adjacency_matrix(data_friends)
gfriends.smoke <- attr_smoke

#Hypotheses about the effect popularity on adolescent’s smoking level
#H1: Are popular students more or less likely to start smoking?
#Noted that popularity status is showed by in-degree value of nodes (students).

#plotting to check Linear relationship ... model assumptions for regression
gfriends.indegree <- degree(gfriends, mode = "in")
plot(gfriends.indegree, gfriends.smoke, main="Scatterplot smoke ~ indegree", 
  	xlab="indegree", ylab="smoke (0/1)", pch=19)
break
# hasilnya tdk terbukti karena tdk terlihat jelas kecenderungan jumlah teman dgn kebiasaan smoke

# check berdasarkan clustering coefficient or transitivity
# ... Transitivity in igraph measures the probability of a student for having friendship relation
gfriends.clustcoeff <- transitivity(gfriends, type="local", isolates = "zero")
plot(gfriends.clustcoeff, gfriends.smoke, main="Scatterplot smoke ~ clustcoeff", 
  	xlab="clustcoeff", ylab="smoke (0/1)", pch=19)
break
# siswa dgn probability berteman (clustcoeff) rendah cenderung tdk merokok

data_cek <- as.data.frame(cbind(gfriends.smoke, gfriends.indegree, gfriends.clustcoeff, attrb[,2]))
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
# dibandingkan data testing, hasil prediksi msh cenderung salah, mgkn krn data belajar atau salah model
# tapi model lm2 lbh stabil krn variance tdk terlalu besar dibandingkan lm1 dan lm3, jika dilihat dari data testing

library(stargazer) 
stargazer(lm1,lm2,lm3,lm4,type="text", 
	dep.var.labels=c("Smoking Behaviour"),	
	column.labels=c("indeg","clust","indeg+clust", "indeg+clust+gender"),
	covariate.labels=c("In-Degrees", "Clustering Coefficient", "Gender"),
	omit.stat=c("LL","ser","f")) 
# the model with the highest Adjusted R2 is preferred, 
# lm4 lbh baik tp tdk sesuai dgn data testing jadi perlu diuji coba dgn variasi jumlah data belajar
