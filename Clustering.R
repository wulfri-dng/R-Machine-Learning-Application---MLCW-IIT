library("readxl") # readxl package used to import excel files
library(NbClust)
library(factoextra)

vehicle_list <- read_excel("D:\\Coding Area\\University Projects\\Courseworks\\R-Machine-Learning-Application---MLCW-IIT\\vehicles.xlsx")

# Remove 1st and last columns
vehicle_list_inputs = vehicle_list[, -1]
vehicle_list_inputs = vehicle_list_inputs[, -19]

# Calculate outliers and remove them automatically
while(TRUE) {
  outliers_found = FALSE
  
  for(i in colnames(vehicle_list_inputs)){
    outliers = boxplot.stats(vehicle_list_inputs[[i]])$out
    
    if(length(outliers) != 0) {
      outliers_found = TRUE
      
      for(j in 1:length(outliers)) {
        vehicle_list_inputs = subset(vehicle_list_inputs, vehicle_list_inputs[[i]] != outliers[j])
      }
    }
  }
  
  if(!outliers_found) {
    break
  }
}

# To check whether the all outliers have removed or not
boxplot(vehicle_list_inputs)

# Scale the data set
scaled_vehicle_list_inputs <- scale(vehicle_list_inputs)

# NbClust to identify clusters
set.seed(26)
NbClust(scaled_vehicle_list_inputs, distance="euclidean", min.nc=2,max.nc=5,method="kmeans",index="all")

# Elbow method to identify clusters
k = 2:5
set.seed(42)	
WSS_of_scaled_vehicle_list_inputs = sapply(k, function(k) {kmeans(scaled_vehicle_list_inputs, centers=k)$tot.withinss})
plot(k, WSS_of_scaled_vehicle_list_inputs, type="l", xlab= "Number of k", ylab="Within sum of squares")

# Average Silhouette method to identify clusters
fviz_nbclust(scaled_vehicle_list_inputs, kmeans, method = 'silhouette')

# Gap Static Algorithm to identify clusters
fviz_nbclust(scaled_vehicle_list_inputs, kmeans, method = 'gap_stat')

kmean_vehicle_list = kmeans(scaled_vehicle_list_inputs, centers = 3, nstart = 10)
kmean_vehicle_list
fviz_cluster(kmean_vehicle_list, data=scaled_vehicle_list_inputs)
