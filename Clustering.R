library("readxl") # readxl package used to import excel files
library("ggplot2")

vehicle_list <- read_excel("D:\\Coding Area\\University Projects\\Courseworks\\R-Machine-Learning-Application---MLCW-IIT\\vehicles.xlsx")

# Remove 1st and last column and scale the data set
vehicle_list_inputs = vehicle_list[, -1]
vehicle_list_inputs = vehicle_list_inputs[, -19]

for(i in colnames(vehicle_list_inputs)){
  outliers = boxplot.stats(vehicle_list_inputs[[i]])$out
  
  if(length(outliers) != 0) {
    out_row = which(vehicle_list_inputs[[i]] %in% c(outliers))

    print(vehicle_list_inputs[out_row, ])
  }
}
  
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
