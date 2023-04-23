library("readxl") # readxl package used to import excel files
library(NbClust)
library(factoextra)
library(cluster)

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

# Calculate WSS, TSS and BSS to identify the optimal cluster
kmean_vehicle_list_cluster_2 = kmeans(scaled_vehicle_list_inputs, centers = 2)
kmean_vehicle_list_cluster_3 = kmeans(scaled_vehicle_list_inputs, centers = 3)

wss_cluster_2 = kmean_vehicle_list_cluster_2$tot.withinss
wss_cluster_3 = kmean_vehicle_list_cluster_3$tot.withinss

tss_cluster_2 = kmean_vehicle_list_cluster_2$totss
tss_cluster_3 = kmean_vehicle_list_cluster_3$totss

bss_cluster_2 = kmean_vehicle_list_cluster_2$betweenss
bss_cluster_3 = kmean_vehicle_list_cluster_3$betweenss

bss2_to_tss2_ratio = bss_cluster_2/tss_cluster_2
bss3_to_tss3_ratio = bss_cluster_3/tss_cluster_3

# When compared to k=2 and k=3, in k=3, BSS is higher and WSS is lower than the k=2. 
# Additionally k=3 has a higher BSS/TSS ratio. 
# Then k=3 is the better option.

# Visualize cluster(k=3)
fviz_cluster(kmean_vehicle_list_cluster_3, data=scaled_vehicle_list_inputs)

# Generate silhouette plot
silhouette_of_vehicle_list_cluster_2 <- silhouette(kmean_vehicle_list_cluster_2$cluster, dist(scaled_vehicle_list_inputs))
silhouette_of_vehicle_list_cluster_3 <- silhouette(kmean_vehicle_list_cluster_3$cluster, dist(scaled_vehicle_list_inputs))
fviz_silhouette(silhouette_of_vehicle_list_cluster_2)
fviz_silhouette(silhouette_of_vehicle_list_cluster_3)
