options(repos = c(CRAN = "https://cloud.r-project.org")) # Set CRAN repository for package installation

# Function to load data from a CSV file give a file path and return a data frame
load_data <- function(file_path) {
    data <- read.csv(file_path, header = TRUE, sep = ",")

    print("Data Loaded Successfully")
    print('\n')
    print(data) 
    return(data)
}

# Function to Create a Sample Data Frame
create_sample_data <- function() {
 # Create sample data frame
finance_data <- data.frame(
  date = c("2026-01-01","2026-01-02","2026-01-03","2026-01-04","2026-01-05","2026-01-06", NA, "2026-01-08"),
  description = c("Salary","Groceries","Transport","Restaurant","Freelance","Rent","Unknown","Cinema"),
  amount = c(500,-50,-20,-30,200,-150,NA,-40),
  type = c("Income","Expense","Expense","Expense","Income","Expense","Expense","Expense"),
  category = c("Salary","Food","Transport","Food","Freelance","Housing",NA,"Entertainment")
)

# Write CSV to disk
write.csv(finance_data, "finance.csv", row.names = FALSE)
}

# Function to Clean Data
clean_data <- function(data) {
    # Convert date column to Date type
    data$date <- as.Date(data$date, format = "%Y-%m-%d")   
    # Normalize 'type' column
    data$type <- ifelse(tolower(data$type) == "income","Income","Expense")  
    # Ensure 'category' is character
    data$category <- as.character(data$category)
    # Ensure 'amount' is numeric
    data$amount <- as.numeric(data$amount)
    # Remove rows with missing values
    data <- na.omit(data)  

    print("Data Cleaned Successfully")
    print('\n')
    print(data)
    
    return(data)
}

# Function to Summarize Finances
summarize_finances <- function(data) {
    print('\n')
    print("===== FINANCIAL SUMMARY =====")
    
    total_income <- sum(data$amount[data$type == "Income"])
    total_expenses <- sum(data$amount[data$type == "Expense"])
    balance <- total_income + total_expenses
    
    print(paste("Total Income: $", total_income))
    print(paste("Total Expenses: $", total_expenses))
    print(paste("Net Balance: $", balance))
    
    return(list(total_income = total_income,
                total_expenses = total_expenses,
                balance = balance))
}


# Function to Analyze Income and Expenses by Category
category_totals <- function(data) {
    print('\n')
    print("===== CATEGORY TOTALS =====")
    
    categories <- unique(data$category)
    totals <- list()
    
    for(category in categories){
        total <- sum(data$amount[data$category == category])
        totals[[category]] <- total
        print(paste("Total for", category, ": $", total))
    }
    
    return(totals)
}

# Function to Visualize Income and Expenses by Category
plot_bar_chart <- function(category_summary) {
    print('\n')
    print("Generating Bar Chart...")
    # Convert category summary to numeric values for plotting
    amounts <- abs(unlist(category_summary))
    # Get category names
    categories <- names(category_summary)
    # Generate colors for the bars
    colors <- rainbow(length(categories))
    
    barplot(amounts,
        names.arg = categories, # Add category names as labels on the x-axis
        main = "Income and Expenses by Category", # Add title to the bar chart
        xlab = "Category", # Add x-axis label
        ylab = "Amount ($)", # Add y-axis label
        col = colors) # Add colors to the bars
}

# Function to Plot a Pie Chart for Expenses
plot_pie_chart <- function(data) {
    print('\n')
    print("Generating Pie Chart for Expenses...")  
    # Filter only expenses
    expense_data <- subset(data, type == "Expense")  
    # Sum per category
    expense_summary <- tapply(expense_data$amount, expense_data$category, sum) 
    # Generate colors for the pie chart
    colors <- rainbow(length(expense_summary))

    # Create pie chart
    pie(abs(expense_summary), # Use absolute values for the pie chart
        labels = names(expense_summary), # Add category names as labels
        col = colors, # Add colors to the pie chart
        main = "Expense Distribution by Category") # Add title to the pie chart
}

# Main function to run the analysis
main <- function() {
    # Load
    my_data <- load_data("finance.csv") 

    # Clean
    cleaned_data <- clean_data(my_data)

    # Summarize
    summary_results <- summarize_finances(cleaned_data)

    # Category totals
    category_summary <- category_totals(cleaned_data)

    # Visualize
    plot_bar_chart(category_summary)
    plot_pie_chart(cleaned_data)
}

# Run the main function
main()