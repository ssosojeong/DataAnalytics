# SOM demonstration for Irish Census data in Dublin Area.
#
# Set your R working directory to the location of this file.
# - you will require subdirectories "boundary_files" and "census_data"
#
# Developed with R 3.0.2.
# Accompanying blog post and slides at:
# - http://www.shanelynn.ie/
# - http://www.slideshare.net/shanelynn/2014-0117-dublin-r-selforganising-maps-for-customer-segmentation-shane-lynn
#
# Shane Lynn 12/01/2014
# @shane_a_lynn

### LOAD LIBRARIES - install with:
install.packages(c("kohonen", "dummies", "ggplot2", "maptools", "sp", "reshape2", "rgeos", "rgdal"))
library(kohonen)
library(dummies)
library(ggplot2)
library(sp)
library(maptools)
library(reshape2)
library(rgeos)
library(rgdal)

# Colour palette definition
pretty_palette <- c("#1f77b4", '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2')

# ------------------- SOM TRAINING ---------------------------

#choose the variables with which to train the SOM
data <- read.csv('dublin.csv', header=TRUE)
data_train <- data[, c(2,4,5,8)]

# now train the SOM using the Kohonen method
data_train_matrix <- as.matrix(scale(data_train))
som_grid <- somgrid(xdim = 20, ydim=20, topo="hexagonal")

# Train the SOM model!
set.seed(31)
som_model <- som(data_train_matrix, 
                 grid=som_grid, 
                 rlen=1000, 
                 alpha=c(0.3,0.01), 
                 keep.data = TRUE )

# -------------------- SOM VISUALISATION -----------------

#Visualise the SOM model results
# Plot of the training progress - how the node distances have stabilised over time.

## custom palette as per kohonen package (not compulsory)
source('coolBlueHotRed.R')

plot(som_model, type = "changes")
#counts within nodes
plot(som_model, type = "counts", main="Node Counts", palette.name=coolBlueHotRed)
#map quality
plot(som_model, type = "quality", main="Node Quality/Distance", palette.name=coolBlueHotRed)
#neighbour distances
plot(som_model, type="dist.neighbours", main = "SOM neighbour distances", palette.name=grey.colors)
#code spread
plot(som_model, type = "codes")

# Plot the heatmap for a variable at scaled / normalised values
var <- 4 #define the variable to plot
plot(som_model, type = "property", property = getCodes(som_model)[,var], main=colnames(getCodes(som_model))[var], palette.name=coolBlueHotRed)


#plot a variable from the original data set (will be uncapped etc.)
# This function produces a menu for multiple heatmaps if a factor or character is chosen
source('plotHeatMap.R')
# A menu of all variables should be displayed if variable=0 
# (note on Mac this will required working XQuartz installation.)
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


# ------------------ Clustering SOM results -------------------

# Form clusters on grid
## use hierarchical clustering to cluster the codebook vectors
som_cluster <- cutree(hclust(dist(getCodes(som_model))), 6)

# Show the map with different colours for every cluster						  
plot(som_model, type="mapping", bgcol = pretty_palette[som_cluster], main = "Clusters")
add.cluster.boundaries(som_model, som_cluster)

#show the same plot with the codes instead of just colours
plot(som_model, type="codes", bgcol = pretty_palette[som_cluster], main = "Clusters")
add.cluster.boundaries(som_model, som_cluster)

