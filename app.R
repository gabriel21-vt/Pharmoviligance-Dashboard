
#source("logic.r")

library(shiny)
library(openxlsx)
library(bslib)
library(DT)
library(psych)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyverse)
library(shinyjs)
library(glue)
library(pvm)
library(trend)
library(reshape2)

# Load and preprocess data
#setwd("C:/Users/gabel/allergy-allies")
df <- read.csv("./Data/cleaned_target_drugs.csv", header = TRUE)
data_dictionary <- "./Data/faers_analysis_dataset_dictionary.xlsx"
df_faers_data_dictionary <- read.xlsx(data_dictionary, sheet = 1)
df_faers_data_dictionary$Note <- NULL
#Rename columns of df_faers_data_dictionary
df_faers_data_dictionary <- df_faers_data_dictionary %>% `colnames<-`(c("Variable Name", "Description", "Type of Variable"))

#Rename columns in df_target_drugs
df_target_drugs <- read.xlsx(data_dictionary, sheet = 2) %>%
  mutate(Brand.Name = str_replace(Brand.Name, "Singulaira", "Singulair")) %>%
  filter(!is.na(Brand.Name)) %>%
  mutate(Brand.Name = str_to_lower(Brand.Name))
df_target_drugs <- df_target_drugs %>% `colnames<-`(c("Generic Name", "Brand Name", "Initial Approval Date", "Note"))

#Rename columns in df_psychiatric
df_psychiatric <- read.xlsx(data_dictionary, sheet = 3)
df_psychiatric <- df_psychiatric %>% `colnames<-`(c("Reaction Group", "Reaction"))

# Create Description Table
description_table <- describe(df)
definition_table <- data.frame(
  Index = colnames(description_table),
  Definition = c(
    "Variables analyzed",
    "Number of valid cases (non-missing values)",
    "Mean of the variable",
    "Standard deviation of the variable",
    "Median of the variable",
    "Trimmed mean of the variable",
    "Mad of the variable",
    "Minimum value in the variable",
    "Maximum value in the variable",
    "Range of the variable (Max - Min)",
    "Skewness of the variable",
    "Kurtosis of the variable",
    "Standard error of the mean"
  )
)

df_copy <- df %>%
  filter(ps_drugname %in% unique(df_target_drugs$`Brand Name`)) %>%
  select(ps_drugname, age)

# Step 2: Define age brackets
brackets <- seq(0, 110, by = 10)

df_copy <- df_copy %>%
  mutate(age_bracket = cut(age, breaks = brackets, right = FALSE, include.lowest = TRUE))

df_copy$age_start <- as.numeric(sub("\\[([0-9]+),.*", "\\1", df_copy$age_bracket))
df_copy$age_end <- as.numeric(sub(".*,([0-9]+)\\)", "\\1", df_copy$age_bracket))

# Step 4: Group by 'age_bracket' and 'ps_drugname', then count
age_bracket_counts <- df_copy %>%
  group_by(age_bracket, ps_drugname) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = ps_drugname, values_from = Count, values_fill = list(Count = 0))

# Step 5: Reshape to long format
age_bracket_counts_long <- age_bracket_counts %>%
  pivot_longer(
    cols = -age_bracket,
    names_to = "Drug",
    values_to = "Count"
  )


#Remove rows where ps_drugname is null
df = df %>% filter(!is.null(ps_drugname))
#Filter df to only drugs in df_target_drugs
df = df %>%
  filter(ps_drugname %in% unique(df_target_drugs$`Brand Name`))

#Need to convert things to datetime
df$fda_dt <- as.Date(df$fda_dt)
df$event_dt <- as.Date(df$event_dt)
df$init_fda_dt <- as.Date(df$init_fda_dt)

df <- df %>%
  mutate(across(where(is.character), ~ na_if(., "")))

#Sum the total number of na values per column in df
null_values <- sapply(df, function(x) sum(is.na(x)))

null_values_df <- data.frame(
  column_name = names(null_values),
  null_values = ((null_values / nrow(df)) * 100),
  row.names = NULL,
  stringsAsFactors = FALSE
)

#sort null_values_df by null_values
null_values_df <- arrange(null_values_df, desc(null_values))

#Get the year of the fda case report
years <- format(df$fda_dt, "%Y")
#Put into a new column
df$years = years

#creating a lag column
df = df %>% mutate(lag = as.numeric(fda_dt - event_dt))

#explode reaction_pt into multiple rows
df_outc_cod <- df %>% separate_rows(reaction_pt, sep = ";")

#for reaction_pt column, every entry should be stripped of leading and trailing whitespace, and converted to lowercase
df_outc_cod$reaction_pt <- tolower(str_trim(df_outc_cod$reaction_pt))
#same thing for df_psychiatric
df_psychiatric$Reaction <- tolower(str_trim(df_psychiatric$Reaction))
#Filter df_outc_cod to only include reactions from df_psychiatric
df_outc_cod <- df_outc_cod %>% filter(reaction_pt %in% df_psychiatric$Reaction)



#then unexplode the reaction_pt column and set delimiter to ;
df_outc_cod <- df_outc_cod %>%
  group_by(caseid) %>%
  mutate(reaction_pt = paste(reaction_pt, collapse = ";")) %>%  # Collapse reaction_pt
  ungroup() %>%  # Ungroup after mutating
  distinct()  # Remove duplicate rows if needed

#limit df_outc_cod to columns having less than 55% null values
null_value_limit <- 55
columns_to_keep <- null_values_df %>% filter(null_values < null_value_limit) %>% 
  pull(column_name)
#Use in "length_greater_15" for choices
df_outc_cod_limit_null <- df_outc_cod %>% select(all_of(columns_to_keep))


#################################################

#Seperate df_outc_cod by outc_cod and use delimiter semicolon
df_outc_cod <- df_outc_cod %>% separate_rows(outc_cod, sep = ";")
#Remove the other column, visually not useful.
df_outc_cod <- df_outc_cod %>% filter(outc_cod != "OT")

#Recollapse df_outc_cod back into delimited data for accurate visualizations used by multiple selectInput(s)
df_outc_cod <- df_outc_cod %>%
  group_by(caseid) %>%
  mutate(outc_cod = paste(outc_cod, collapse = ";")) %>%  # Collapse reaction_pt
  ungroup() %>%  # Ungroup after mutating
  distinct()  # Remove duplicate rows if needed

#Use in ROR-PRR function
df_reactions <- df_outc_cod %>% separate_rows(reaction_pt, sep = ";")



#---------------ROR FUNCTION-----------------#
##################################################
ROR_PRR_func <- function(Drug, ADR, df, CI=0.95, column = c("Drug_ADR", "year")){
  
  #check if column is of length 2
  if (length(column) != 2) {
    return("Invalid column. Please provide a column with two elements.")
  }
  
  #check if columns exist in df
  if (!(column[1] %in% colnames(df)) || !(column[2] %in% colnames(df))) {
    return("Invalid column. Please provide valid column names.")
  }
  
  independent_column <- sym(column[1])
  response_column <- sym(column[2])
  
  a <- df %>% filter(!!independent_column == Drug & !!response_column != ADR)
  a <- a %>% count(!!response_column, name = "Frequency") %>% arrange(desc(Frequency))
  a <- sum(a$Frequency)
  
  b <- df %>% filter(!!independent_column != Drug & !!response_column != ADR)
  b <- b %>% count(!!response_column, name = "Frequency") %>% arrange(desc(Frequency))
  b <- sum(b$Frequency) 
  
  
  c <- df %>% filter(!!independent_column == Drug & !!response_column == ADR)
  c <- c %>% count(!!response_column, name = "Frequency") %>% arrange(desc(Frequency))
  c <- c %>% filter(!!response_column == ADR) %>% pull(Frequency)
  
  d <- df %>% filter(!!independent_column != Drug & !!response_column == ADR)
  d <- d %>% count(!!response_column, name = "Frequency") %>% arrange(desc(Frequency))
  d <- d %>% filter(!!response_column == ADR) %>% pull(Frequency)
  
  ROR_value <- ROR(a, b, c, d)
  PRR_value <- PRR(a, b, c, d, alpha = CI)
  #BCPNN_value <- BCPNN(a, b, c, d, alpha = CI, version = 'alternative')
  
  #return (glue("The Reporting Odds Ratio for {Drug}-{ADR} is: {ROR_value}"))
  return (c(ROR = ROR_value, PRR = PRR_value))
  
}

Running_ROR_PRR <- function(df, data_column = c("Drug_ADR", "year")){
  rows <- unique(df %>% select(data_column[1]) %>% pull())
  rows <- as.character(rows)
  
  #extract unique years and sort them
  columns <- unique(df %>% select(data_column[2]) %>% pull())
  columns <- sort(as.character(columns))
  
  # Initialize an empty data frame with ADRs as rows and drugs as columns
  ROR_table <- data.frame(matrix(NA, ncol= length(columns), nrow = length(rows)))
  rownames(ROR_table) <- rows
  colnames(ROR_table) <- columns
  
  PRR_table <- ROR_table
  
  for (row in rows) {
    for (column in columns) { 
      
      # Attempt to calculate ROR and PRR with error handling
      ROR_PRR <- tryCatch({
        result <- ROR_PRR_func(row, column, df, column = data_column)
        
        # Check if the result is numeric and has non-missing values
        if (is.null(result) || length(result) == 0 || any(is.na(result))) {
          c(ROR = 0, PRR = 0)  # Return zeros for missing values
        } else {
          result
        }
      }, error = function(e) {
        c(ROR = 0, PRR = 0)  # Handle errors by returning zeros
      })
      
      # Assign the ROR and PRR values to their respective tables
      ROR_table[row, column] <- ifelse("ROR" %in% names(ROR_PRR), ROR_PRR["ROR"], 0)
      PRR_table[row, column] <- ifelse("PRR" %in% names(ROR_PRR), ROR_PRR["PRR"], 0)
    }
  }
  
  ROR_table <- as.data.frame(ROR_table)
  PRR_table <- as.data.frame(PRR_table)
  
  return(list(ROR_table = ROR_table, PRR_table = PRR_table))
}




################################################################################
ROR_func <- function(Drug, ADR, df, CI=0.95, column = c("Drug_ADR", "year")){
  
  #check if column is of length 2
  if (length(column) != 2) {
    return("Invalid column. Please provide a column with two elements.")
  }
  
  #check if columns exist in df
  if (!(column[1] %in% colnames(df)) || !(column[2] %in% colnames(df))) {
    return("Invalid column. Please provide valid column names.")
  }
  
  independent_column <- sym(column[1])
  response_column <- sym(column[2])
  
  a <- df %>% filter(!!independent_column == Drug & !!response_column != ADR)
  a <- a %>% count(!!response_column, name = "Frequency") %>% arrange(desc(Frequency))
  a <- sum(a$Frequency)
  
  b <- df %>% filter(!!independent_column != Drug & !!response_column != ADR)
  b <- b %>% count(!!response_column, name = "Frequency") %>% arrange(desc(Frequency))
  b <- sum(b$Frequency) 
  
  
  c <- df %>% filter(!!independent_column == Drug & !!response_column == ADR)
  c <- c %>% count(!!response_column, name = "Frequency") %>% arrange(desc(Frequency))
  c <- c %>% filter(!!response_column == ADR) %>% pull(Frequency)
  
  d <- df %>% filter(!!independent_column != Drug & !!response_column == ADR)
  d <- d %>% count(!!response_column, name = "Frequency") %>% arrange(desc(Frequency))
  d <- d %>% filter(!!response_column == ADR) %>% pull(Frequency)
  
  ROR_value <- ROR(a, b, c, d)
  
  #return (glue("The Reporting Odds Ratio for {Drug}-{ADR} is: {ROR_value}"))
  return (ROR_value)
  
}

Running_ROR <- function(df, data_column = c("Drug_ADR", "year"), updateProgress = NULL) {
  
  # Extract unique rows (Drug_ADR) and columns (years)
  rows <- unique(df %>% select(data_column[1]) %>% pull())
  rows <- as.character(rows)
  
  columns <- unique(df %>% select(data_column[2]) %>% pull())
  columns <- sort(as.character(columns))
  
  # Initialize an empty data frame with ADRs as rows and years as columns
  ROR_table <- data.frame(matrix(NA, ncol = length(columns), nrow = length(rows)))
  rownames(ROR_table) <- rows
  colnames(ROR_table) <- columns
  
  # Loop through each Drug_ADR and year
  for (row in rows) {
    if (is.function(updateProgress)) {
      text <- paste0("Adding row: ", row)
      updateProgress(detail = text)
    }
    for (column in columns) {
      # Attempt to calculate ROR with error handling
      ROR_value <- tryCatch({
        result <- ROR_func(row, column, df, column = data_column)
        
        # Ensure result is valid
        if (is.null(result) || is.na(result)) {
          0  # Return 0 for missing or invalid values
        } else {
          result
        }
      }, error = function(e) {
        0  # Handle errors by returning 0
      })
      
      # Assign the ROR value to the table
      ROR_table[row, column] <- ROR_value
    }
  }
  
  # Convert to a data frame and return the results
  ROR_table <- as.data.frame(ROR_table)
  
  return(list(ROR_table = ROR_table))
}



###############################################################################
##########################PRR Function ########################################
PRR_func <- function(Drug, ADR, df, CI=0.95, column = c("Drug_ADR", "year")){
  
  #check if column is of length 2
  if (length(column) != 2) {
    return("Invalid column. Please provide a column with two elements.")
  }
  
  #check if columns exist in df
  if (!(column[1] %in% colnames(df)) || !(column[2] %in% colnames(df))) {
    return("Invalid column. Please provide valid column names.")
  }
  
  independent_column <- sym(column[1])
  response_column <- sym(column[2])
  
  a <- df %>% filter(!!independent_column == Drug & !!response_column != ADR)
  a <- a %>% count(!!response_column, name = "Frequency") %>% arrange(desc(Frequency))
  a <- sum(a$Frequency)
  
  b <- df %>% filter(!!independent_column != Drug & !!response_column != ADR)
  b <- b %>% count(!!response_column, name = "Frequency") %>% arrange(desc(Frequency))
  b <- sum(b$Frequency) 
  
  
  c <- df %>% filter(!!independent_column == Drug & !!response_column == ADR)
  c <- c %>% count(!!response_column, name = "Frequency") %>% arrange(desc(Frequency))
  c <- c %>% filter(!!response_column == ADR) %>% pull(Frequency)
  
  d <- df %>% filter(!!independent_column != Drug & !!response_column == ADR)
  d <- d %>% count(!!response_column, name = "Frequency") %>% arrange(desc(Frequency))
  d <- d %>% filter(!!response_column == ADR) %>% pull(Frequency)
  
  PRR_value <- PRR(a, b, c, d, alpha = CI)
  #BCPNN_value <- BCPNN(a, b, c, d, alpha = CI, version = 'alternative')
  
  #return (glue("The Reporting Odds Ratio for {Drug}-{ADR} is: {ROR_value}"))
  return (PRR_value)
  
}

Running_PRR <- function(df, data_column = c("Drug_ADR", "year"), updateProgress = NULL){
  rows <- unique(df %>% select(data_column[1]) %>% pull())
  rows <- as.character(rows)
  
  #extract unique years and sort them
  columns <- unique(df %>% select(data_column[2]) %>% pull())
  columns <- sort(as.character(columns))
  
  # Initialize an empty data frame with ADRs as rows and drugs as columns
  PRR_table <- data.frame(matrix(NA, ncol= length(columns), nrow = length(rows)))
  rownames(PRR_table) <- rows
  colnames(PRR_table) <- columns
  
  
  for (row in rows) {
    if (is.function(updateProgress)) {
      text <- paste0("Adding row: ", row)
      updateProgress(detail = text)
    }
    for (column in columns) {
      # Attempt to calculate ROR with error handling
      PRR_value <- tryCatch({
        result <- PRR_func(row, column, df, column = data_column)
        
        # Ensure result is valid
        if (is.null(result) || is.na(result)) {
          0  # Return 0 for missing or invalid values
        } else {
          result
        }
      }, error = function(e) {
        0  # Handle errors by returning 0
      })
      
      # Assign the ROR value to the table
      PRR_table[row, column] <- PRR_value
    }
  }
  
  PRR_table <- as.data.frame(PRR_table)
  
  return(list(PRR_table = PRR_table))
}

###############################################################################
##########################BCPNN Function ########################################
BCPNN_func <- function(Drug, ADR, df, CI=0.95, column = c("Drug_ADR", "year")){
  
  #check if column is of length 2
  if (length(column) != 2) {
    return("Invalid column. Please provide a column with two elements.")
  }
  
  #check if columns exist in df
  if (!(column[1] %in% colnames(df)) || !(column[2] %in% colnames(df))) {
    return("Invalid column. Please provide valid column names.")
  }
  
  independent_column <- sym(column[1])
  response_column <- sym(column[2])
  
  a <- df %>% filter(!!independent_column == Drug & !!response_column != ADR)
  a <- a %>% count(!!response_column, name = "Frequency") %>% arrange(desc(Frequency))
  a <- sum(a$Frequency)
  
  b <- df %>% filter(!!independent_column != Drug & !!response_column != ADR)
  b <- b %>% count(!!response_column, name = "Frequency") %>% arrange(desc(Frequency))
  b <- sum(b$Frequency) 
  
  
  c <- df %>% filter(!!independent_column == Drug & !!response_column == ADR)
  c <- c %>% count(!!response_column, name = "Frequency") %>% arrange(desc(Frequency))
  c <- c %>% filter(!!response_column == ADR) %>% pull(Frequency)
  
  d <- df %>% filter(!!independent_column != Drug & !!response_column == ADR)
  d <- d %>% count(!!response_column, name = "Frequency") %>% arrange(desc(Frequency))
  d <- d %>% filter(!!response_column == ADR) %>% pull(Frequency)
  
  BCPNN_value <- BCPNN(a, b, c, d, alpha = CI)
  #print(BCPNN_value)
  
  #return (glue("The Reporting Odds Ratio for {Drug}-{ADR} is: {ROR_value}"))
  return(BCPNN_value)
  
}

Running_BCPNN <- function(df, data_column = c("Drug_ADR", "year"), updateProgress = NULL) {
  # Extract unique rows (Drug_ADR) and columns (years)
  rows <- unique(df %>% select(data_column[1]) %>% pull())
  rows <- as.character(rows)
  
  columns <- unique(df %>% select(data_column[2]) %>% pull())
  columns <- sort(as.character(columns))
  
  # Initialize an empty data frame with ADRs as rows and years as columns
  BCPNN_table <- data.frame(matrix(NA, ncol = length(columns), nrow = length(rows)))
  rownames(BCPNN_table) <- rows
  colnames(BCPNN_table) <- columns
  
  # Loop through each Drug_ADR and year
  for (row in rows) {
    if (is.function(updateProgress)) {
      text <- paste0("Adding row: ", row)
      updateProgress(detail = text)
    }
    for (column in columns) {
      # Attempt to calculate BCPNN with error handling
      BCPNN <- tryCatch({
        result <- BCPNN_func(row, column, df, column = data_column)
        
        # Check if the result is numeric and has non-missing values
        if (is.null(result) || is.na(result)) {
          0  # Return 0 for missing or invalid values
        } else {
          result
        }
      }, error = function(e) {
        0  # Handle errors by returning 0
      })
      
      # Assign the BCPNN value to the table
      BCPNN_table[row, column] <- BCPNN
    }
  }
  
  # Convert to a data frame and return the results
  BCPNN_table <- as.data.frame(BCPNN_table)
  
  return(list(BCPNN_table = BCPNN_table))
}


#---------------------------Pie chart function---------------------------------#
################################################################################
pie_func <- function(table, condition, type=c("ROR", "PRR", "BCPNN")) {
  
  #if the type is not ROR or PRR, return an error
  if (!(type %in% c("ROR", "PRR", "BCPNN"))) {
    return("Invalid type. Please select 'ROR' or 'PRR' or 'BCPNN'.")
  }
  
  #check if condition is a accessible row or not
  if (condition %in% rownames(table)){
    # Select the row matching the condition
    selected_row <- table[condition, , drop = FALSE]
  }else{
    showModal(modalDialog(
      title = "Selection Error",
      glue("No existing rows for {condition}, please choose another condition."),
      easyClose = TRUE
    ))
    return()
    #return(glue("No existing rows for {condition}, please choose another condition."))
  }
  
  #Check if the selected row is all NA
  if (all(is.na(selected_row))) {
    showModal(modalDialog(
      title = "Selection Error",
      glue("No {type} values for {condition}"),
      easyClose = TRUE
    ))
    return()
    #return(glue("No {type} values for {condition}"))
  }
  if (nrow(selected_row) == 0) {
    showModal(modalDialog(
      title = "Selection Error",
      glue("No {type} values for {condition}"),
      easyClose = TRUE
    ))
    return()
    #return(glue("No {type} values for {condition}"))
  }
  
  #If a value is 0, replace with NA so it won't show in the pie chart
  # Remove values that are exactly 0
  selected_row <- selected_row[, selected_row != 0, drop = FALSE]
  
  # Check again if there are any values to plot
  if (ncol(selected_row) == 0) {
    showModal(modalDialog(
      title = "Selection Error",
      glue("No {type} values for {condition} to plot"),
      easyClose = TRUE
    ))
    return()
    #stop(glue("No {type} values for {condition} to plot"))
  }
  
  # Convert to a data frame for ggplot
  plot_data <- data.frame(
    Drug = colnames(selected_row),
    Value = as.numeric(selected_row)
  )
  
  #round the value in the plot_data
  plot_data$Value <- round(plot_data$Value, 2)
  
  # Create the barplot
  p <- ggplot(plot_data, aes(x = "", y = Value, fill=Drug)) +
    geom_col(color = "black") +
    geom_label(aes(label = Value),
               position = position_stack(vjust = 0.5),
               show.legend = FALSE, color = "black") + 
    labs(title = glue("{type} value for {condition}")) +
    coord_polar(theta = "y") +   
    scale_fill_brewer() +
    theme_void() + theme(
      plot.title = element_text(hjust = 0.5, size = 20),
      legend.title = element_text(size = 15),
      legend.text = element_text(size = 12)
    )
  
  return(p)
}


################################################################################
############################Line-plot function##################################

lineplot_func <- function(table, conditions, type=c("ROR", "PRR", "BCPNN")) {
  
  #if the type is not ROR or PRR, return an error
  if (!(type %in% c("ROR", "PRR", "BCPNN"))) {
    return("Invalid type. Please select 'ROR' or 'PRR'.")
  }
  
  #check if type is ROR and table is PRR_table, return an error
  if (type == "ROR" && identical(table, t(PRR_pair_years))) {
    return("Invalid table. Please select the ROR table.")
  }
  if (type == "PRR" && identical(table, t(ROR_pair_years))) {
    return("Invalid table. Please select the PRR table.")
  }
  if (type == "BCPNN" && identical(table, t(BCPNN_pair_years))) {
    return("Invalid table. Please select the BCPNN table.")
  }
  
  # Select the row matching the condition
  selected_rows <- table[conditions, , drop = FALSE]
  
  #Check if the selected row is all NA
  if (all(is.na(selected_rows))) {
    showModal(modalDialog(
      title = "Selection Error",
      glue("No {type} values for {condition}"),
      easyClose = TRUE
    ))
    return()
    #return(glue("No {type} values for {condition}"))
  }
  if (nrow(selected_rows) == 0) {
    showModal(modalDialog(
      title = "Selection Error",
      glue("No {type} values for {condition}"),
      easyClose = TRUE
    ))
    return()
    #return(glue("No {type} values for {condition}"))
  }
  
  plot_data <- as.data.frame(t(selected_rows))
  #print(plot_data)
  plot_data$year <- rownames(plot_data)
  data_long <- melt(plot_data, id="year")
  
  # Create the lineplot
  p <- ggplot(data_long, aes(x = year, y = value, color=variable, group=variable)) +
    geom_line() +
    geom_point(color = "black") +
    labs(title = glue("{type} value for Drug-ADR(s) over time"),
         x = "Year",
         y = "Value") +
    theme_minimal() + theme(
      plot.title = element_text(hjust = 0.5, size = 20),  # Centers the plot title
      axis.text.x = element_text(angle = 45, size = 15),
      axis.text.y = element_text(angle = 0, size = 15),
      axis.title.x = element_text(size = 18),
      axis.title.y = element_text(size = 18),
      legend.title = element_text(size = 15),
      legend.text = element_text(size = 12)
    )
  
  return(p)
}

################################################################################
#############################Mann Kendall Test##################################


# find the max value of each drug-adr pair in ROR_pair_years
mann_kendall_test <- function(df) {
  trend_results <- data.frame(Drug_ADR = rownames(df), P_Value = NA, Score = NA)
  
  for (drug_adr in rownames(df)) {
    ror_values <- as.numeric(df[drug_adr, ])
    
    # Perform Mann-Kendall test
    test <- mk.test(ror_values, alternative = "two.sided")
    trend_results[trend_results$Drug_ADR == drug_adr, "Score"] <- test$estimates[1]
    trend_results[trend_results$Drug_ADR == drug_adr, "P_Value"] <- test$p.value
  }
  
  return(trend_results)
}

#---------------------Data Preprocessing for trend test -----------------------#

columns <- c("ps_drugname", "reaction_pt")
df_reaction_pair_year <- df_reactions %>%
  group_by(across(all_of(columns)), years) %>%  # Group by the columns in the list and 'year'
  summarise(Count = n(), .groups = 'drop')

#in df_reaction_no_singulair, combine ps_drugname and reaction_pt to create a new column
df_reaction_pair_year$Drug_ADR <- paste(df_reaction_pair_year$ps_drugname,df_reaction_pair_year$reaction_pt, sep = "-")

tables <- Running_ROR_PRR(df_reaction_pair_year, c("Drug_ADR", "years"))

#ROR drug adr trend analysis
ROR_pair_years <- tables["ROR_table"]$ROR_table
trend_test_ROR <- mann_kendall_test(ROR_pair_years)
#filter trend test to only include values less than 0.05 and score greater than 0, indicating a positive trend
trend_test_ROR <- trend_test_ROR %>% filter(P_Value < 0.05 & Score > 0)
trend_test_ROR <- trend_test_ROR %>% arrange(P_Value)
drug_adr_trend_ROR <- trend_test_ROR$Drug_ADR

#PRR drug-adr trend analysis
PRR_pair_years <- tables["PRR_table"]$PRR_table
trend_test_PRR <- mann_kendall_test(PRR_pair_years)
trend_test_PRR <- trend_test_ROR %>% filter(P_Value < 0.05 & Score > 0)
trend_test_PRR <- trend_test_ROR %>% arrange(P_Value)
drug_adr_trend_PRR <- trend_test_PRR$Drug_ADR

BCPNN_table <- Running_BCPNN(df_reaction_pair_year, c("Drug_ADR", "years"))
BCPNN_pair_years <- BCPNN_table$BCPNN_table
trend_test_BCPNN <- mann_kendall_test(BCPNN_pair_years)
drug_adr_trend_BCPNN <- trend_test_BCPNN$Drug_ADR
#--------------------UI----------------------------#
####################################################

ui <- page_navbar(
  title = "Pharmocoviligance Application",
  theme = bs_theme(bootswatch = "minty"),
  #useShinyjs(),
  
  tags$head(
    tags$style(HTML("
    .title-page {
      background-image: url('https://cdn.pixabay.com/photo/2023/12/24/20/46/ai-generated-8467687_1280.jpg'); 
      background-size: cover; /* Ensures the image covers the entire container */
      background-attachment: fixed; /* Keeps the background fixed */
      background-position: center; /* Centers the image */
      padding: 0px; /* Adds space around the content */
      /*color: white; Ensures text is visible on the background */
      text-align: center; /* Aligns the text to the right */
      min-height: 3000px; /* Ensures it takes up full viewport height */
    }
    .content-a {
      margin-top: 1000px;
    }
    .content-b {
      margin-top: 1700px;
    }
    .content-c {
      margin-top: 2400px;
    }
    #text-box-a {
      box-sizing: content-box;  
      width: 300px;
      height: 250px;
      padding: 0px;  
      background-color: rgba(255, 0, 0, 0);
      color: white;
      font-size: 14px;
      float: left;
      text-align: left;
      margin-left: 20px;
      
    }
    
    #text-box-b {
      box-sizing: content-box;  
      width: 300px;
      height: 250px;
      padding: 0px;  
      background-color: rgba(255, 0, 0, 0);
      color: white;
      font-size: 14px;
      float: right; /* Move the entire box to the right */
      text-align: right; /* Align text inside the box to the right */
      margin-right: 20px; /* Optional spacing from the edge */
    }
    
    #text-box-c {
      box-sizing: content-box;  
      width: 300px;
      height: 250px;
      padding: 0px;  
      background-color: rgba(255, 0, 0, 0);
      color: white;
      font-size: 14px;
      float: left;
      text-align: left;
      margin-left: 20px;
    }

  "))
  ),
  
  # Title page
  nav_panel(
    title = "Front Page", icon = icon("house"),
    div(class = "title-page",
        h1("Welcome to this experimental pharmacovigilance website", style = "color: white;"),
        h2("This app is designed to help you analyze the FAERS dataset.", style = "color: white;"),
        div(class = "content-a", id="text-box-a",
            h3("Medications have always been about helping people get healthier and increase longevity, but what happens when this doesn't occur?", style = "color: white;"),
        ),
        div(class = "content-b", id = "text-box-b",
            h3("You may feel sick, dizzy or have a change in mood, whether it be physical or mental, this can be caused by medications unintentional effects", style = "color: white;")
        ),
        div(class = "content-c", id = "text-box-c",
            h3("These effects are very difficult to find and to find them we have to find patterns in our data to use this, now looking at the FDA Adverse Event Reporting System (FAERS),
               try and achieve this.", style = "color: white;")
        )
    )
  ),
  
  # Tab 1: Tables
  nav_panel(
    title = "Tables", icon = icon("table"),
    sidebarLayout(
      sidebarPanel(
        selectInput("table_choice", "Choose a table:", 
                    choices = c("Full Dataframe", "Target Drugs", "Psychiatric Conditions", "Description Table")),
        conditionalPanel(
          condition = "input.table_choice == 'Description Table'",
          h4("Description Table Definitions"),
          dataTableOutput("definition_table")
        ),
        conditionalPanel(
          condition = "input.table_choice == 'Full Dataframe'",
          h4("Full Dataframe Variable Definitions"),
          dataTableOutput("df_faers_data_dictionary")
        )
      ), 
      fluid = TRUE,
      mainPanel(
        dataTableOutput("selected_table")
      )
    )
  ),
  
  # Tab 2: Plots
  nav_panel(
    title = "Exploratory Data Analysis", icon = icon("magnifying-glass"),
    layout_sidebar(fluid = TRUE, position = "left", tags$style(HTML("
        .control-label { white-space: normal; }  /* Ensure labels wrap properly */
        .sidebar { padding: 10px; margin: 0; }  /* Adjust sidebar padding and margin */
      ")),
                   sidebar = sidebarPanel(width = 12,
                                          sliderInput(
                                            "slider_bins",
                                            label = "Number of bins:",
                                            min = 10,
                                            max = 50,
                                            value = 25
                                          ),
                                          sliderInput(
                                            "slider_age_bracket",
                                            label = "Select Age Bracket:",
                                            min = 0,
                                            max = 110,
                                            step = 1,
                                            value = c(0, 110)
                                          ),
                                          sliderInput(
                                            "year",
                                            label = "Select Interval:",
                                            min = min(as.numeric(df$years)),
                                            max = max(as.numeric(df$years)),
                                            step = 1,
                                            value = c(min(as.numeric(df$years)), max(as.numeric(df$years))),
                                            sep = ""
                                          ),
                                          sliderInput(
                                            "lag_bins",
                                            label = "Select Number of bins for lag histogram:",
                                            min = 1,
                                            max = 100,
                                            step = 1,
                                            value = 50
                                          ),
                                          selectInput(
                                            "selected_drug",
                                            label = "Select a Drug:",
                                            choices = unique(df$ps_drugname),
                                            selected = unique(df$ps_drugname)[1]
                                          ),
                                          selectInput("dataframe_columns", 
                                                      label = "Select 2 columns to analyze:", 
                                                      choices = colnames(df_outc_cod_limit_null), 
                                                      multiple = TRUE
                                          ),
                                          conditionalPanel(
                                            condition = "input.has_choices == TRUE", 
                                            selectInput("length_greater_15",
                                                        label = "Select as many options as you would like to compare (wouldn't recommend too many!)", 
                                                        choices = "", 
                                                        multiple = TRUE
                                            )
                                          ),
                                          selectInput(
                                            "unique_values", 
                                            label = "Select 1 unique variable corresponding to your 1st input in the menu above",
                                            choices = "",
                                            multiple = TRUE
                                          )
                   ),
                   mainPanel(
                     div(plotOutput("plot_histogram", width = "1600px", height = "400px"), style="margin-bottom:100px;"),
                     div(plotOutput("plot_drug_age_bracket", width = "1600px", height = "400px"), style="margin-bottom:100px;"),
                     div(plotOutput("plot_total_cases", width = "1600px", height = "400px"), style="margin-bottom:100px;"),
                     div(plotOutput("plot_lag_drug", width = "1600px", height = "400px"), style="margin-bottom:100px;"),
                     div(plotOutput("plot_patient_outcome", width = "1600px", height = "400px"), style="margin-bottom:100px;")
                   )
    )
  ),
  
  
  navbarMenu(
    title = "Statistical Tests and Analyses", icon = icon("chart-column"),
    
    # ROR Analysis
    nav_panel(
      title = "ROR Analysis", icon = icon("pills"),     
      fluidPage(
        div(style = "text-align:center;", 
          h2("Reporting Odds Ratio (ROR)"),
          p("The Reporting Odds Ratio (ROR) is calculated as:"),
          tags$div(style = "text-align: center;",
                   withMathJax("$$ROR = \\frac{a / b}{c / d} = \\frac{a \\cdot d}{b \\cdot c}$$")
          ),
          p("Where:"),
          p("a: Number of cases where the drug is reported with the specific ADR."),
          p("b: Number of cases where the drug is reported without the specific ADR."),
          p("c: Number of cases where other drugs are reported with the specific ADR."),
          p("d: Number of cases where other drugs are reported without the specific ADR."),
          h3("Interpretation"),
          p("ROR > 1: Suggests a positive association between the drug and the ADR."),
          p("ROR = 1: No association."),
          p("ROR < 1: Suggests a negative association.")
          
        )
      ),
      
      layout_sidebar(fluid = TRUE, position = "left", tags$style(HTML("
        .control-label { white-space: normal; }  /* Ensure labels wrap properly */
        .sidebar { padding: 10px; margin: 0; }  /* Adjust sidebar padding and margin */
      ")),
        sidebar = sidebarPanel(width = 12,
          checkboxGroupInput(inputId = "Gender_ROR",
                             label = "Select Gender(s):",
                             choices = c("Male" = "M", "Female" = "F", "Unknown" = "UNK"),
                             selected = "M"),
          
          checkboxGroupInput(inputId = "Patient_Outcome_ROR",
                             label = "Select Patient Outcome(s):",
                             choices = c("Required Intervention" = "RI", 
                                         "Congenital Anomaly" = "CA", 
                                         "Disability" = "DS", 
                                         "Hospitalization" = "HO", 
                                         "Life Threatening" = "LT", 
                                         "Death" = "DE"),
                             selected = c("DE", "LT", "DS", "CA")),
          
          selectInput("psych_dropdown_ROR",
                      label = "Select a psychiatric condition to analyze:",
                      choices = sort(unique(df_reactions$reaction_pt)),
                      multiple = FALSE,
                      selected = "fear"),
          
          selectInput("Drug_ADR_ROR",
                      label = "Pick a Drug-ADR to analyze:",
                      choices = sort(unique(drug_adr_trend_ROR)),
                      multiple = TRUE,
                      selected = "")
        ),
        mainPanel(
          div(plotOutput("ROR_plot", width = "1600px", height = "400px"), style = "margin-bottom:100px;"),
          div(uiOutput("caveatPieTextROR"), style = "margin-bottom:100px; text-align:center;"),
          div(DT::dataTableOutput("ROR_table"), style = "width: 100%; margin-bottom:100px; text-align:center;"),
          div(plotOutput("ROR_lineplot", width = "1600px", height = "400px"), style = "margin-bottom:100px;"),
          div(uiOutput("caveatTextROR"), style = "margin-bottom:100px; text-align:center;")
        )
      )
    ),
    
    # PRR Analysis
    nav_panel(
      title = "PRR Analysis",
      icon = icon("chart-area"),       
      fluidPage(
        div(style="text-align:center;",
          h2("Proportional Reporting Ratio (PRR)"),
          p("The Proportional Reporting Ratio (PRR) is calculated as:"),
          tags$div(style = "text-align: center;",
                   withMathJax("$$PRR = \\frac{\\text{Proportion of ADR for the drug}}{\\text{Proportion of ADR for all other drugs}}$$"),
                   withMathJax("$$PRR = \\frac{a / (a + b)}{c / (c + d)}$$")
          ),
          p("Where:"),
          p("a: Number of cases where the drug is reported with the specific ADR."),
          p("b: Number of cases where the drug is reported without the specific ADR."),
          p("c: Number of cases where other drugs are reported with the specific ADR."),
          p("d: Number of cases where other drugs are reported without the specific ADR."),
          h3("Interpretation"),
          p("PRR > 1: Indicates a higher frequency of the ADR for the drug."),
          p("PRR = 1: Indicates the same frequency."),
          p("PRR < 1: Indicates a lower frequency of the ADR for the drug.")
          
        )
      ),
      
      layout_sidebar(fluid = TRUE, position = "left", tags$style(HTML("
        .control-label { white-space: normal; }  /* Ensure labels wrap properly */
        .sidebar { padding: 10px; margin: 0; }  /* Adjust sidebar padding and margin */
      ")),
        sidebar = sidebarPanel(width = 12,
          checkboxGroupInput(inputId = "Gender_PRR",
                             label = "Select Gender(s):",
                             choices = c("Male" = "M", "Female" = "F", "Unknown" = "UNK"),
                             selected = "M"),
          
          checkboxGroupInput(inputId = "Patient_Outcome_PRR",
                             label = "Select Patient Outcome(s):",
                             choices = c("Required Intervention" = "RI", 
                                         "Congenital Anomaly" = "CA", 
                                         "Disability" = "DS", 
                                         "Hospitalization" = "HO", 
                                         "Life Threatening" = "LT", 
                                         "Death" = "DE"),
                             selected = "DE"),
          
          selectInput("psych_dropdown_PRR",
                      label = "Select a psychiatric condition to analyze:",
                      choices = sort(unique(df_reactions$reaction_pt)),
                      multiple = FALSE,
                      selected = "anger"),
          
          selectInput("Drug_ADR_PRR",
                      label = "Pick a Drug-ADR to analyze:",
                      choices = sort(unique(drug_adr_trend_PRR)),
                      multiple = TRUE,
                      selected = "")
        ),
        mainPanel(
          div(plotOutput("PRR_plot", width = "1600px", height = "400px"), style = "margin-bottom:100px;"),
          div(uiOutput("caveatPieTextPRR"), style = "margin-bottom:100px; text-align:center;"),
          div(DT::dataTableOutput("PRR_table"), style = "margin-bottom:100px; text-align:center;"),
          div(plotOutput("PRR_lineplot", width = "1600px", height = "400px"), style = "margin-bottom:100px;"),
          div(uiOutput("caveatTextPRR"), style = "margin-bottom:100px; text-align:center;")
        )
      )
    ),
    
    # BCPNN Analysis
    nav_panel(
      title = "BCPNN Analysis",
      icon = icon("chart-pie"),
      fluidPage(
        div(style = "text-align:center;",
          h2("Bayesian Confidence Propagation Neural Network (BCPNN)"),
          p("BCPNN uses Bayesian statistics to calculate the Information Component (IC) as:"),
          tags$div(style = "text-align: center;",
                   withMathJax("$$IC = \\log_2 \\frac{P(D, A)}{P(D) \\cdot P(A)}$$")
          ),
          p("Where:"),
          p("P(D, A): Joint probability of the drug (D) and ADR (A) being reported together."),
          p("P(D): Marginal probability of the drug (D) being reported."),
          p("P(A): Marginal probability of the ADR (A) being reported."),
          h3("Interpretation"),
          p("IC > 0: Indicates a positive association between the drug and the ADR."),
          p("IC = 0: Indicates no association."),
          p("IC < 0: Indicates a negative association.")
        )
      ),
      
      layout_sidebar(
        sidebar = sidebarPanel(width = 12,
          checkboxGroupInput(inputId = "Gender_BCPNN",
                             label = "Select Gender(s):",
                             choices = c("Male" = "M", "Female" = "F", "Unknown" = "UNK"),
                             selected = "M"),
          
          checkboxGroupInput(inputId = "Patient_Outcome_BCPNN",
                             label = "Select Patient Outcome(s):",
                             choices = c("Required Intervention" = "RI", 
                                         "Congenital Anomaly" = "CA", 
                                         "Disability" = "DS", 
                                         "Hospitalization" = "HO", 
                                         "Life Threatening" = "LT", 
                                         "Death" = "DE"),
                             selected = "DE"),
          
          selectInput("psych_dropdown_BCPNN",
                      label = "Select a psychiatric condition to analyze:",
                      choices = sort(unique(df_reactions$reaction_pt)),
                      multiple = FALSE,
                      selected = "completed suicide"),
          
          selectInput("Drug_ADR_BCPNN",
                      label = "Pick a Drug-ADR to analyze:",
                      choices = sort(unique(drug_adr_trend_BCPNN)),
                      multiple = TRUE,
                      selected = "")
        ),
        mainPanel(
          div(plotOutput("BCPNN_plot", width = "1600px", height = "400px"), style = "margin-bottom:100px;"),
          div(uiOutput("caveatPieTextBCPNN"), style = "margin-bottom:100px; text-align:center;"),
          div(DT::dataTableOutput("BCPNN_table"), style = "margin-bottom:100px; text-align:center;"),
          div(plotOutput("BCPNN_lineplot", width = "1600px", height = "400px"), style = "margin-bottom:100px;"),
          div(uiOutput("caveatTextBCPNN"), style = "margin-bottom:100px; text-align:center;")
        )
      )
    )
  ),
  
  nav_spacer(),
  nav_menu(
    title = "Links",
    align = "right",
    nav_item(tags$a("Github", href = "https://github.com/gabriel21-vt")),
    nav_item(tags$a("Linkedin" , href = "https://www.linkedin.com/in/gabriel-dell-2b5841222/")),
    nav_item(tags$a("Pharmocoviligance Summary", href = "https://pubmed.ncbi.nlm.nih.gov/30126707/"))
  )
)

# Server
server <- function(input, output, session) {
  
  has_choices <- reactiveVal(FALSE)
  
  # Render Selected Table
  output$selected_table <- renderDataTable({
    switch(input$table_choice,
           "Full Dataframe" = df,
           "Target Drugs" = df_target_drugs,
           "Psychiatric Conditions" = df_psychiatric,
           "Description Table" = description_table)
  }, options=list(paging = TRUE,    ## paginate the output
                  pageLength = 15,  ## number of rows to output for each page
                  scrollX = TRUE,   ## enable scrolling on X axis
                  scrollY = TRUE,   ## enable scrolling on Y axis
                  autoWidth = FALSE ## use smart column width handling
  ))
  
  # Render Definition Tables
  output$definition_table <- renderDataTable({
    req(input$table_choice == "Description Table")
    definition_table
  })
  
  output$df_faers_data_dictionary <- renderDataTable({
    req(input$table_choice == "Full Dataframe")
    df_faers_data_dictionary
  })
  
  # Render Histogram Plot
  output$plot_histogram <- renderPlot({
    ggplot(df, aes(x = age)) +
      labs(x = "Age (in Years)") +
      geom_histogram(bins = input$slider_bins) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(angle = 0, size = 15),
        axis.text.y = element_text(angle = 0, size = 15),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18), 
      )
  })
  
  output$plot_drug_age_bracket <- renderPlot({
    df_interval <- df_copy %>% filter(age_start >= input$slider_age_bracket[1], age_end <= input$slider_age_bracket[2])
    df_interval <- df_interval %>% group_by(ps_drugname, age_bracket) %>% summarise(Count = n(), .groups = 'drop')
    df_interval
    ggplot(df_interval, aes(x=ps_drugname, y=Count, fill=age_bracket)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Drug Counts by Age Bracket",
           x = "Drug",
           y = "Count",
           fill = "Age Bracket") +
      theme_minimal() + theme(
        plot.title = element_text(hjust = 0.5, size= 20),  # Centers the plot title
        axis.text.x = element_text(angle = 0, size = 15),
        axis.text.y = element_text(angle = 0, size = 15),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12)
      )
  })
  
  output$plot_total_cases <- renderPlot({
    years_df = df %>% group_by(years, ps_drugname) %>% summarise(Count = n(), .groups = 'drop')
    years_df$years = as.numeric(years_df$years)
    years_df = years_df %>% filter(years >= input$year[1] & years <= input$year[2])
    ggplot(years_df, aes(x=years, y=Count, fill=ps_drugname)) + geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Number of reports per year for each drug",
           x = "Year",
           y = "Frequency") +
      theme_minimal() + theme(
        plot.title = element_text(hjust = 0.5, size = 20),  # Centers the plot title
        axis.text.x = element_text(angle = 0, size=15),
        axis.text.y = element_text(angle = 0, size = 15),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12)
      )
  })
  
  output$plot_lag_drug <- renderPlot({
    req(input$selected_drug)  # Ensure a drug is selected
    
    # Filter the data for the selected drug
    lag_df <- df %>%
      filter(ps_drugname == input$selected_drug) %>%
      group_by(ps_drugname, lag) %>%
      summarise(Count = n(), .groups = 'drop')
    
    # Create a histogram
    ggplot(lag_df, aes(x = lag)) +
      labs(x = "Time difference between when FDA got case report and when ADR occurred (in days).",
           title = paste("Lag Histogram for", input$selected_drug)) +
      geom_histogram(bins = input$lag_bins, fill = "blue", color = "white") +
      theme_minimal() + theme(plot.title = element_text(hjust = 0.5, size = 20),
                              axis.text.x = element_text(angle=0, size=15),
                              axis.text.y = element_text(angle = 0, size = 15),
                              axis.title.x = element_text(size = 18),
                              axis.title.y = element_text(size = 18)
      )
  })
  
  observe({
    #Request input columns from the selectInput
    req(input$dataframe_columns)  
    
    #Check if the length is greater than 2, if so, return selection error
    if (length(input$dataframe_columns) > 2) {
      showModal(modalDialog(
        title = "Selection Error",
        "Please select no more than 2 columns.",
        easyClose = TRUE
      ))
      return()
    }
    
    #If length is 2, get data, split by ; for both columns and get present in
    #dropdown menus
    if (length(input$dataframe_columns) == 2) {
      
      # Select the two columns
      column_choice1 <- df_outc_cod %>%
        select(all_of(input$dataframe_columns[1]))
      
      #get second column values
      column_choice2 <- df_outc_cod %>% 
        select(input$dataframe_columns[2])
      
      # Check if either column has delimiters and split accordingly
      column_choice1 <- lapply(column_choice1, function(col) {
        if (any(grepl(";", col, fixed = TRUE))) {
          # Split by delimiter and flatten into a unique list
          unique(unlist(strsplit(tolower(str_trim(col)), ";")))
        } else {
          # Just return unique values if no delimiter
          unique(col)
        }
      })
      
      #split by ;
      column_choice2 <- column_choice2 %>% separate_rows(input$dataframe_columns[2], sep = ';')
      
      #Sort the second column options by alphabetical order
      num_choices2 <- sort(unique(column_choice2[[input$dataframe_columns[2]]]))
      if (length(num_choices2) > 0) {
        has_choices(TRUE)
        shinyjs::runjs("Shiny.setInputValue
                       ('has_choices', TRUE);")
        updateSelectInput(
          session,
          "length_greater_15",
          choices = num_choices2,
          selected = NULL
        )
        
      }
      
      
      # Update the select input with the processed unique values
      updateSelectInput(
        session,
        "unique_values",
        choices = column_choice1,
        selected = NULL
      )
    }
    
  })
  
  
  
  
  output$plot_patient_outcome <- renderPlot({
    
    req(input$dataframe_columns, input$unique_values, input$length_greater_15)
    
    # Validate that exactly 2 columns are selected
    if (length(input$dataframe_columns) > 2) {
      showModal(modalDialog(
        title = "Selection Error",
        "HI! You must select exactly 2 column.",
        easyClose = TRUE
      ))
      return()
    }
    
    # Validate that up to 2 unique values are selected
    if (length(input$unique_values) > 1) {
      showModal(modalDialog(
        title = "Selection Error",
        "HEY, you can only select 1 variable to analyze.",
        easyClose = TRUE
      ))
      return()
    }
    
    # Extract selected columns and unique values
    column1 <- input$dataframe_columns[1]
    column2 <- input$dataframe_columns[2]
    selected_value <- input$unique_values
    
    #get values from table of length_greater_15
    possible_overflow_options <- input$length_greater_15
    
    #explode df_outc_cod for column1 and column2
    df_outc_cod <- df_outc_cod %>% separate_rows(column1, sep = ";")
    #trim by whitespace and lower case
    df_outc_cod[ ,1] <- tolower(str_trim(df_outc_cod[ ,1]))
    df_outc_cod <- df_outc_cod %>% separate_rows(column2, sep = ";")
    df_outc_cod[ ,2] <- tolower(str_trim(df_outc_cod[ ,2]))
    
    
    
    df_outc_cod <- df_outc_cod %>% filter(!!sym(column1) == selected_value)
    
    #If the number of unique values is greater than 1
    if(length(possible_overflow_options) > 0){
      #filter by the values in possible_overflow_options
      df_outc_cod <- df_outc_cod %>% filter(!!sym(column2) %in% possible_overflow_options)
    }
    else{
      df_outc_cod <- df_outc_cod
    }
    
    #group by year and outc_cod
    df_outc_cod_year <- df_outc_cod %>% group_by(years, !!sym(column2)) %>% summarise(Count = n(), .groups = 'drop')
    
    #drop OT from outc_cod
    df_outc_cod_year <- df_outc_cod_year %>% mutate(years = as.numeric(years))
    
    #make a lineplot of df_outc_cod_year
    ggplot(df_outc_cod_year, aes(x=years, y=Count, color = !!sym(column2), group= !!sym(column2))) + geom_line() + geom_point(color='black') +
      labs(title = glue("Number of reports per year based on {column2} with respect to {selected_value[1]}"),
           x = "Year",
           y = "Frequency") +
      theme_minimal() + theme(
        plot.title = element_text(hjust = 0.5, size = 20),  # Centers the plot title
        axis.text.x = element_text(angle = 0, size = 15),
        axis.text.y = element_text(angle = 0, size = 15),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12)
      )
  })
  
  
  #################################################
  ###########ROR-table-plot########################
  
  # Create a reactive expression to compute ROR_data
  ROR_data <- reactive({
    # Ensure the required inputs are provided
    req(input$psych_dropdown_ROR)
    req(input$Gender_ROR)
    req(input$Patient_Outcome_ROR)
    
    # Filter df_reactions based on gender and patient outcome
    filtered_data <- df_reactions %>%
      filter(sex %in% input$Gender_ROR) %>%
      filter(outc_cod %in% input$Patient_Outcome_ROR)
    
    #Add a progress function
    progress <- shiny::Progress$new()
    progress$set(message = "Computing ROR", value = 0)
    # Close the progress when this reactive exits (even if there's an error)
    on.exit(progress$close())
    
    updateProgress <- function(value = NULL, detail = NULL) {
      if (is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value) / 5
      }
      progress$set(value = value, detail = detail)
    }
    
    # Compute ROR and PRR tables
    tables <- Running_ROR(filtered_data, c("ps_drugname", "reaction_pt"), updateProgress)
    #print(tables[["ROR_table"]])
    
    # Return the ROR table
    tables[["ROR_table"]]
  })
  
  # Render the pie chart using ROR_data
  output$ROR_plot <- renderPlot({
    req(ROR_data())  # Ensure ROR_data is available
    
    # Generate the pie chart
    pie_func(t(ROR_data()), input$psych_dropdown_ROR, "ROR")
  })
  
  # Render the table using the same ROR_data
  output$ROR_table <- DT::renderDataTable({
    req(ROR_data())
    table_ROR <- ROR_data() 
    table_ROR <- format(table_ROR, digits=2)
    table_ROR
  }, options = list(
    paging = TRUE,
    pageLength = 15,
    scrollX = TRUE,
    scrollY = TRUE,
    autoWidth = TRUE
  ))
  
  #################################################
  ###########PRR-table-plot########################
  
  # Create a reactive expression to compute ROR_data
  PRR_data <- reactive({
    
    # Ensure the required inputs are provided
    req(input$psych_dropdown_PRR)
    req(input$Gender_PRR)
    req(input$Patient_Outcome_PRR)
    
    # Filter df_reactions based on gender and patient outcome
    filtered_data <- df_reactions %>%
      filter(sex %in% input$Gender_PRR) %>%
      filter(outc_cod %in% input$Patient_Outcome_PRR)
    
    #Add a progress function
    progress <- shiny::Progress$new()
    progress$set(message = "Computing PRR", value = 0)
    # Close the progress when this reactive exits (even if there's an error)
    on.exit(progress$close())
    
    updateProgress <- function(value = NULL, detail = NULL) {
      if (is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value) / 5
      }
      progress$set(value = value, detail = detail)
    }
    
    # Compute ROR and PRR tables
    tables <- Running_PRR(filtered_data, c("ps_drugname", "reaction_pt"), updateProgress)
    
    # Return the ROR table
    tables[["PRR_table"]]
    
    
  })
  
  output$PRR_plot <- renderPlot({
    req(PRR_data)
    
    #convert data to data frame
    PRR_data_df <- as.data.frame(PRR_data())
    pie_func(t(PRR_data_df), input$psych_dropdown_PRR, "PRR")
  })
  
  output$PRR_table <- DT::renderDataTable({
    req(PRR_data)
    table_PRR <- PRR_data() 
    table_PRR <- format(table_ROR, digits=2)
    table_PRR
  }, options = list(
    paging = TRUE,
    pageLength = 15,
    scrollX = TRUE,
    scrollY = TRUE,
    autoWidth = TRUE
  ))
  
  ###########################BCPNN table plots #################################
  BCPNN_data <- reactive({
    
    # Ensure the required inputs are provided
    req(input$psych_dropdown_BCPNN)
    req(input$Gender_BCPNN)
    req(input$Patient_Outcome_BCPNN)
    
    # Filter df_reactions based on gender and patient outcome
    filtered_data <- df_reactions %>%
      filter(sex %in% input$Gender_BCPNN) %>%
      filter(outc_cod %in% input$Patient_Outcome_BCPNN)
    
    #Add a progress function
    progress <- shiny::Progress$new()
    progress$set(message = "Computing BCPNN", value = 0)
    # Close the progress when this reactive exits (even if there's an error)
    on.exit(progress$close())
    
    updateProgress <- function(value = NULL, detail = NULL) {
      if (is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value) / 5
      }
      progress$set(value = value, detail = detail)
    }
    
    # Compute ROR and PRR tables
    tables <- Running_BCPNN(filtered_data, c("ps_drugname", "reaction_pt"), updateProgress)
    
    # Return the ROR table
    tables[["BCPNN_table"]]
    
    
  })
  
  output$BCPNN_plot <- renderPlot({
    req(BCPNN_data)
    
    #convert data to data frame
    BCPNN_data_df <- as.data.frame(BCPNN_data())
    pie_func(t(BCPNN_data_df), input$psych_dropdown_BCPNN, "BCPNN")
  })
  
  output$BCPNN_table <- DT::renderDataTable({
    req(BCPNN_data)
    table_BCPNN <- BCPNN_data() 
    table_BCPNN <- format(table_BCPNN, digits=2)
    table_BCPNN
  }, options = list(
    paging = TRUE,
    pageLength = 15,
    scrollX = TRUE,
    scrollY = TRUE,
    autoWidth = TRUE
  ))
  

  
  #--------------ROR lineplot function and Mann Kendall Test-------------------#
  output$ROR_lineplot <- renderPlot({
    req(input$Drug_ADR_ROR)
    lineplot_func(ROR_pair_years,  input$Drug_ADR_ROR, "ROR")
  })
  
  #--------------PRR lineplot function and Mann Kendall Test-------------------#
  output$PRR_lineplot <- renderPlot({
    req(input$Drug_ADR_PRR)
    lineplot_func(PRR_pair_years, input$Drug_ADR_PRR, "PRR")
  })
  
  #--------------BCPNN lineplot function and Mann Kendall Test-------------------#
  output$BCPNN_lineplot <- renderPlot({
    req(input$Drug_ADR_BCPNN)
    lineplot_func(BCPNN_pair_years, input$Drug_ADR_BCPNN, "BCPNN")
  })

  #---------------------------Caveat for Pie Chart------------------------------#
  output$caveatPieTextROR <- renderUI({
    tags$div(
      "Caveat: Some drugs may not show up in the pie chart if their value is 0.",
      style = "text-align: center;"
    )
  })
  output$caveatPieTextPRR <- renderUI({
    tags$div(
      "Caveat: Some drugs may not show up in the pie chart if their value is 0.",
      style = "text-align: center;"
    )
  })
  output$caveatPieTextBCPNN <- renderUI({
    tags$div(
      "Caveat: Some drugs may not show up in the pie chart if their value is 0.",
      style = "text-align: center;"
    )
  })
  
  #-------------------------Caveat for beneath Lineplot------------------------#
  
  output$caveatTextROR <- renderUI({
    tags$div(
      "Caveat: These renderings are based on a sample dataset and may not represent real-world data accurately.",
      style = "text-align: center;"
    )
  })
  
  output$caveatTextPRR <- renderUI({
    tags$div(
      "Caveat: These renderings are based on a sample dataset and may not represent real-world data accurately.",
      style = "text-align: center;"
    )
  })
  
  output$caveatTextBCPNN <- renderUI({
    tags$div(
      "Caveat: These renderings are based on a sample dataset and may not represent real-world data accurately.",
      style = "text-align: center;"
    )
  })
  
  
}

shinyApp(ui = ui, server = server, options = list(height = 1300))