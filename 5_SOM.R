##coolBlueHotRed.R 필요
##plotHeatMap.R 필요
##dublin.csv: 아일랜드 더블린 지역의 센서스 데이터, 4046 rec * 14 var

##0. setwd 경로 설정
setwd("/Users/kimsojeong/DA")

##1. 데이터 전처리
#패키지 읽기
#install.packages("kohonen")
#install.packages("dummies")
library(kohonen)
library(dummies)

#팔레트 불러오기
pretty_palette <- c("#1f77b4", "#ff7f0e", "#2ca02c",
                    "#d62728", "#9467bd", "#8c564b",
                    "#e377c2")

#데이터 불러오기
data <- read.csv("dublin.csv", header=TRUE)

#분석에 사용할 변수 선정
data_train <- data[,c(2,4,5,8)]

#정규화 후, 매트릭스 만들기
data_train_matrix <- as.matrix(scale(data_train))

##2. SOM 학습

#xdim: x의 차원 수, ydim: y의 차원 수
#topo: 형태 hexagonal, rectangular

som_grid <- somgrid(xdim = 20, ydim = 20, topo = "hexagonal")

#Train the SOM model
#rlen: 학습 횟수, alpha: 학습률, radius: 이웃 노드의 반경, keep.data: 원 자료를 같이 저장할지?

set.seed(31)
som_model <- som(data_train_matrix,
                 grid=som_grid,
                 rlen=1000,
                 alpha=c(0.3, 0.01),
                 keep.data=TRUE)

##3. 시각화
#custom palette as per kohonen package (not compulsory)
source("coolBlueHotRed.R")
plot(som_model, type = "changes")

# counts within nodes
plot(som_model, type = "counts", 
     main="Node Counts",
     palette.name = coolBlueHotRed)

#map quality
plot(som_model, type = "quality", 
     main="Node Quality/Distance",
     palette.name = coolBlueHotRed)

#neighbour distances
plot(som_model, type = "dist.neighbours", 
     main="SOM neighbour dist",
     palette.name = coolBlueHotRed)

#code spread
plot(som_model, type = "codes", 
     main="Code",
     palette.name = coolBlueHotRed)

#Plot the heatmap for a variable at scaled / normalised values

#define the variable to plot
#4번째 변수 =  unemployment_percent 변수에 대한 군집은 이렇다.
var <- 4 

plot(som_model, 
     type = "property", 
     property = getCodes(som_model)[,var],
     main=colnames(getCodes(som_model))[var],
     palette.name = coolBlueHotRed)

#plot a variable from the original data set (will be uncapped etc.)
#This function produces a menu for multiple heatmaps if a factor or character is chosen
source("plotHeatMap.R")

#A menu of all variables should be displayed if variable=0
#(note on Mac this will required working XQuartz installation.)
#앞에서는 2,4,5,8번째 변수에 대해 돌렸지만 여기선 다 돌릴것이다.
plotHeatMap(som_model, data, variable=2)
plotHeatMap(som_model, data, variable=3)
plotHeatMap(som_model, data, variable=4)
plotHeatMap(som_model, data, variable=5)
plotHeatMap(som_model, data, variable=6)
plotHeatMap(som_model, data, variable=7)
plotHeatMap(som_model, data, variable=8)
plotHeatMap(som_model, data, variable=9)
plotHeatMap(som_model, data, variable=10)
plotHeatMap(som_model, data, variable=11)
plotHeatMap(som_model, data, variable=12)
plotHeatMap(som_model, data, variable=13)
plotHeatMap(som_model, data, variable=14)

#Form clusters on grid
#use hierarchical clustering to cluster the codebook vectors
som_cluster <- cutree(hclust(dist(getCodes(som_model))), 6)

#Show the map with different colours for every cluster
plot(som_model, type = "mapping", bgcol = pretty_palette[som_cluster],
     main="Clusters")
add.cluster.boundaries(som_model, som_cluster)

#show the same plot with the codes instead of just colours
plot(som_model, type = "codes", bgcol = pretty_palette[som_cluster],
     main="Clusters")
add.cluster.boundaries(som_model, som_cluster)



















