options(repos = c(CRAN = "https://cloud.r-project.org")) # Set CRAN repository for package installation

library(dplyr)  # Load dplyr for case_when function

# --------- Function to load data from a CSV file ---------
load_data <- function(file_path) {
    # Check if the file exists before attempting to read it
    if(!file.exists(file_path)) {
        print(paste("File not found:", file_path))
        return(NULL)
    }
    # Read from file
    data <- read.csv(file_path, header = TRUE, sep = ",")

    print("Data Loaded Successfully")
    print('\n')
    print(data) 
    return(data)
}

# --------- Function to Create a Sample Data Frame ---------
create_sample_data <- function() {
 # Create sample data frame
finance_data <- data.frame(
  date = c("2026-01-01","2026-01-02","2026-01-03","2026-01-04","2026-01-05","2026-01-06", NA, "2026-01-08"),
  description = c("Salary","Groceries","Transport","Restaurant","Freelance","Rent","Unknown","Cinema"),
  amount = c(500,-50,-20,-30,200,-150,NA,-40),
  type = c("Income","Expense","Expense","Expense","Income","Expense","Expense","Expense"),
  category = c("Salary","Food","Transport","Food","Freelance","Housing",NA,"Entertainment")
)
  return(finance_data)
}

# --------- Categories List ---------
category_dictionary <- list(
    Salary = c("salary", "bonus"),
    Freelance = c("freelance"),
    Food = c("grocery","restaurant","coffee","snacks","kfc","shoprite"),
    Transport = c("taxi","transport","fuel","uber"),
    Housing = c("rent"),
    Utilities = c("electricity","internet","phone","subscription","airtel","tnm"),
    Health = c("doctor","hospital","gym"),
    Entertainment = c("movie","cinema","netflix"),
    Debt = c("loan"),
    Charity = c("donation"),
    Clothing = c("shopping")
)

# --------- Function to Clean Data ---------
clean_data <- function(data) {
    
    # Replace "/" with "-" to standardize format
    data$date <- gsub("/", "-", data$date)
    
    # Convert to Date (invalid dates become NA)
    data$date <- as.Date(data$date, format = "%Y-%m-%d")
    
    # Trim white spaces
    data$description <- trimws(data$description)
    data$category <- trimws(data$category)
    data$type <- trimws(data$type)
    
    # Standardize type to "Income" or "Expense"
    data$type <- ifelse(tolower(data$type) == "income", "Income", "Expense")
    
    # Convert amount to numeric (invalid entries become NA)
    data$amount <- as.numeric(data$amount)

    # Categorize transactions based on description keywords
    data$category <- case_when(
        grepl("salary|bonus|freelance|interest|refund|gift received", data$description, ignore.case = TRUE) ~ "Income",
        grepl("grocery|coffee|dinner|snacks|restaurant", data$description, ignore.case = TRUE) ~ "Food",
        grepl("taxi|transport", data$description, ignore.case = TRUE) ~ "Transport",
        grepl("rent", data$description, ignore.case = TRUE) ~ "Housing",
        grepl("electricity|internet|phone|subscription", data$description, ignore.case = TRUE) ~ "Utilities",
        grepl("doctor|gym", data$description, ignore.case = TRUE) ~ "Health",
        grepl("movie|cinema", data$description, ignore.case = TRUE) ~ "Entertainment",
        grepl("loan", data$description, ignore.case = TRUE) ~ "Debt",
        grepl("donation", data$description, ignore.case = TRUE) ~ "Charity",
        grepl("shopping", data$description, ignore.case = TRUE) ~ "Clothing",
        TRUE ~ "Other"
    )

    # Remove rows with NA values in critical columns (date, amount, type) 
    data <- na.omit(data)

    print("Data Cleaned and Categorized Successfully")
    print('\n')
    print(data)

    return(data)
}

# --------- Function to Summarize Finances ---------
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


# --------- Function to Analyze Income and Expenses by Category ---------
category_totals <- function(data) {
    print('\n')
    print("===== CATEGORY TOTALS =====")
    
    # Get unique categories
    categories <- unique(data$category)
    totals <- list()
    
    # Calculate total for each category
    for(category in categories){
        total <- sum(data$amount[data$category == category]) # Sum amounts for the current category
        totals[[category]] <- total # Store total in a list with category as the key
        print(paste("Total for", category, ": $", total))
    }
    
    return(totals)
}

# --------- Function to Visualize Income and Expenses by Category ---------
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

# --------- Function to Plot a Pie Chart for Expenses ---------
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

# --------- Main function to run the analysis ---------
main <- function() {

    # Load data
    my_data <- load_data("finance.csv") 

    # Clean data
    cleaned_data <- clean_data(my_data)

    # Summarize finances
    summary_results <- summarize_finances(cleaned_data)

    # Category totals
    category_summary <- category_totals(cleaned_data)

    # Visualizations
    plot_bar_chart(category_summary) # Generate bar chart for income and expenses by category
    # plot_pie_chart(cleaned_data) # Generate pie chart for expenses
}

# Run the main function to execute the analysis
main()