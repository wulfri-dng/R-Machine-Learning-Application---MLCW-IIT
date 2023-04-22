library("readxl") # readxl package used to import excel files
library("ggplot2")

vehicle_list <- read_excel("D:\\Coding Area\\University Projects\\Courseworks\\R-Machine-Learning-Application---MLCW-IIT\\vehicles.xlsx")

# Remove 1st and last column and scale the data set
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


boxplot(vehicle_list_inputs)
  
summary(vehicle_list_inputs)

scaled_vehicle_list_inputs <- scale(vehicle_list_inputs)


scaled_vehicle_list_inputs
head(scaled_vehicle_list_inputs, 200)
head(vehicle_list_inputs, 10)
print(scaled_vehicle_list_inputs)

hist(scaled_vehicle_list_inputs)

data <- iris[,1:4]
dim(data)
quartiles <- quantile(data$Sepal.Width, probs=c(.25, .75), na.rm = FALSE)
quartiles

IQR <- IQR(data$Sepal.Width)
