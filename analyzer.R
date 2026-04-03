# Function to load data from a CSV file give a file path and return a data frame
load_data <- function(file_path) {
    data <- read.csv(file_path, header = TRUE, sep = ",")

    print("Data Loaded Successfully")
    print(data) 
    return(data)
}

# Function to Clean Data
clean_data <- function(data) {
    # convert date column to date datatype
    data$date <- as.Date(data$date, format = "%Y-%m-%d")

    # Remove Rows with Missing Values (NA values)
    data <- na.omit(data)

    print("Data Cleaned Successfully")
    print(data)
    return(data)
}

# Example Usage
my_data <- load_data("finance.csv") 
cleaned_data <- clean_data(my_data)
