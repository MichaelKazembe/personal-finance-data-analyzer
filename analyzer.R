# Function to load data from a CSV file give a file path and return a data frame
load_data <- function(file_path) {
    data <- read.csv(file_path, header = TRUE, sep = ",")

    print("Data Loaded Successfully")
    print(data) 

    return(data)
}

load_data("finance.csv") 