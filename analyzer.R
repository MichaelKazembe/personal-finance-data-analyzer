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

# --------- Function to Automatically Categorize Transactions Based on Description ---------
auto_categorize <- function(description) {
    # Loop through each category and its associated keywords
    for(category in names(category_dictionary)){
        keywords <- category_dictionary[[category]]
        # Check if any of the keywords for the current category are present in the description (case-insensitive)
        if(any(grepl(paste(keywords, collapse="|"),
                     description,
                     ignore.case = TRUE))){
            return(category)
        }
    }

    return("Other")
}

# --------- Function to Clean Data ---------
clean_data <- function(data) {

    # Standardize dates
    data$date <- gsub("/", "-", data$date)
    data$date <- as.Date(data$date, format="%Y-%m-%d")

    # Trim spaces
    data$description <- trimws(data$description)
    data$category <- trimws(data$category)
    data$type <- trimws(data$type)

    # Type classification
    data$type <- case_when(
        tolower(data$type) == "income" ~ "Income",
        tolower(data$type) == "expense" ~ "Expense",
        TRUE ~ NA_character_
    )

    # Convert amount
    data$amount <- as.numeric(data$amount)

    # ---- Preserve existing categories ----
    missing_category <- is.na(data$category) | data$category == ""

    data$category[missing_category] <-
        sapply(data$description[missing_category], auto_categorize)

    # ---- Remove only unusable rows ----
    data <- data[!is.na(data$date) &
                 !is.na(data$amount) &
                 !is.na(data$type), ]

    print("Data Cleaned Successfully\n")
    print(data)

    return(data)
}

# --------- Function to Summarize Finances ---------
summarize_finances <- function(data) {
    print('\n')
    print("===== FINANCIAL SUMMARY =====")
    
    # Calculate totals
    total_income <- sum(data$amount[data$type == "Income"])
    total_expenses <- sum(data$amount[data$type == "Expense"])
    # Calculate net balance
    balance <- total_income + total_expenses
    
    print(paste("Total Income: $", total_income))
    print(paste("Total Expenses: $", total_expenses))
    print(paste("Net Balance: $", balance))
    
    return(list(total_income = total_income,
                total_expenses = total_expenses,
                balance = balance))
}


# --------- Function to Analyze Income and Expenses by Category ---------
category_totals <- function(data){

    print("\n===== CATEGORY TOTALS =====")
    #  Group by type and category, then summarize the total amount for each group
    summary <- data %>% 
        group_by(type, category) %>%
        summarise(total = sum(amount), .groups="drop")

    print(summary)

    return(summary)
}

# --------- Function to Visualize Income and Expenses by Category ---------
plot_bar_chart <- function(summary){

    print("\nGenerating Bar Chart...")

    income <- summary[summary$type=="Income",]
    expense <- summary[summary$type=="Expense",]

    # Set up the plotting area to have 1 row and 2 columns for side-by-side plots
    par(mfrow=c(1,2))

    # Plot Income and Expenses as bar charts
    barplot(income$total,
            names.arg=income$category,
            main="Income by Category",
            col=rainbow(nrow(income)),
            las=2)

    barplot(abs(expense$total),
            names.arg=expense$category,
            main="Expenses by Category",
            col=rainbow(nrow(expense)),
            las=2)

    par(mfrow=c(1,1))
}

# --------- Function to Plot a Pie Chart for Expenses ---------
plot_pie_chart <- function(data){

    print("\nGenerating Expense Pie Chart...")
    # Filter only expenses for the pie chart
    expense_data <- data[data$type=="Expense",]

    # Group by category and summarize total expenses for each category
    expense_summary <- expense_data %>%
        group_by(category) %>%
        summarise(total = abs(sum(amount)))

    # Plot the pie chart
    pie(expense_summary$total,
        labels=expense_summary$category,
        col=rainbow(nrow(expense_summary)),
        main="Expense Distribution")
}

# --------- Function to Display Menu Options ---------
show_menu <- function(){

    cat("\n==============================\n")
    cat(" PERSONAL FINANCE MANAGER\n")
    cat("==============================\n")
    cat("1. View Financial Summary\n")
    cat("2. View Category Totals\n")
    cat("3. Show Charts\n")
    cat("4. Reload Data\n")
    cat("5. Exit\n")
    cat("==============================\n")
}

# --------- Function to Run the Command-Line Interface ---------
run_cli <- function(cleaned_data){

    repeat {

        # Show menu
        show_menu()

        # Get user choice
        choice <- readline("Select an option: ")

        # Decision system
        switch(choice,

            "1" = {
                summarize_finances(cleaned_data)
            },

            "2" = {
                category_totals(cleaned_data)
            },

            "3" = {
                plot_pie_chart(cleaned_data)
            },

            "4" = {
                cat("\nReloading data...\n")
                raw_data <- load_data("finance.csv")
                cleaned_data <- clean_data(raw_data)
            },

            "5" = {
                cat("\nExiting program... Goodbye!\n")
                break
            },

            {
                cat("\nInvalid option. Please try again.\n")
            }
        )
    }
}

# --------- Main function to run the analysis ---------
main <- function(){

    # Load data once
    raw_data <- load_data("finance.csv")

    # Clean data once
    cleaned_data <- clean_data(raw_data)

    # Start interactive CLI
    run_cli(cleaned_data)
}

# Run the main function to execute the analysis
main()