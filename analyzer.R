# Function to load data from a CSV file give a file path and return a data frame
load_data <- function(file_path) {
    data <- read.csv(file_path, header = TRUE, sep = ",")

    print("Data Loaded Successfully")
    print('\n')
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
    print('\n')
    print(data)
    return(data)
}

# Example Usage
my_data <- load_data("finance.csv") 
cleaned_data <- clean_data(my_data)

# Function to Summarize Finances

summarize_finances <- function(cleaned_data) {
    print("===== FINANCIAL SUMMARY =====")

    # Filter Income and Expenses

    # Total Income
    total_income <- sum(cleaned_data$amount[cleaned_data$category == "income"])

    # Total Expenses (absolute value since amounts are negative)
    total_expenses <- sum((cleaned_data$amount[cleaned_data$category == "expense"]))

    # Net Balance
    balance <- total_income + total_expenses

    # Print Summary
    print(paste("Total Income: $", total_income))
    print(paste("Total Expenses: $", total_expenses))
    print(paste("Net Balance: $", balance))

    return(list(total_income = total_income, 
        total_expenses = total_expenses, 
        balance = balance))
}

# Example Usage
summary_results <- summarize_finances(cleaned_data)