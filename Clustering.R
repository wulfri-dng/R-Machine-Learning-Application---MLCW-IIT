library("readxl") # readxl package used to import excel files
library("ggplot2")

vehicle_list <- read_excel("D:\\Coding Area\\University Projects\\Courseworks\\R-Machine-Learning-Application---MLCW-IIT\\vehicles.xlsx")

# Remove 1st and last column and scale the data set
vehicle_list_inputs = vehicle_list[, -1]
vehicle_list_inputs = vehicle_list_inputs[, -19]

dat <- ggplot2::mpg
print(dat$hwy)
summary(dat$hwy)

boxplot(dat$hwy,
  ylab = "hwy"
)

boxplot.stats(dat$hwy)$out
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
