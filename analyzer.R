options(repos = c(CRAN = "https://cloud.r-project.org")) # Set CRAN repository for package installation

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

#  Usage
my_data <- load_data("finance.csv") 
cleaned_data <- clean_data(my_data)

# Function to Summarize Finances

summarize_finances <- function(cleaned_data) {
    print('\n')
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

#  Usage
summary_results <- summarize_finances(cleaned_data)

# Function to Analyze Income and Expenses by Category

category_totals <- function(cleaned_data) {
    print('\n')
    print("===== CATEGORY TOTALS =====")

    # Find Unique Categories in the Data
    categories <- unique(cleaned_data$category)
    # create an empty list
    totals <- list()
    # Loop through each category and calculate total amount
    for (category in categories) {
        total <- sum(cleaned_data$amount[cleaned_data$category == category])
        totals[[category]] <- total
        print(paste("Total for", category, ": $", total))
    }

    return(totals)
}

#  Usage
category_summary <- category_totals(cleaned_data)

# Function to Visualize Income and Expenses by Category
plot_bar_chart <- function(category_summary) {
    print('\n')
    print("Generating Bar Chart...")

    # Convert List to Numbers
    amounts <- unlist(category_summary)

    # Extract Categories: "Income", "Expense"
    categories <- names(category_summary)

    # Create Bar Plot
    barplot(
        abs(amounts),
        names.arg = categories, # Use category names as labels
        main = "Expenses by Category", # Title of the Chart
        xlab = "Category", # Label for X-axis
        ylab = "Amount Spent ($)", # Label for Y-axis
        col = rainbow(length(categories))) # Colors for the bars
}

#  Usage
plot_bar_chart(category_summary)