library("readxl") # readxl package used to import excel files

vehicle_list <- read_excel("D:\\Coding Area\\University Projects\\Courseworks\\R-Machine-Learning-Application---MLCW-IIT\\vehicles.xlsx")

# Remove 1st and last column and scale the data set
vehicle_list_inputs = vehicle_list[, -1]
vehicle_list_inputs = vehicle_list_inputs[, -19]
scaled_vehicle_list_inputs <- scale(vehicle_list_inputs)

scaled_vehicle_list_inputs
head(scaled_vehicle_list_inputs, 20)
head(vehicle_list_inputs, 10)
