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
library(doParallel)
library(foreach)
library(zoo)
library(tidyquant)
library(tidyr)

start_time <- proc.time()

# Load and preprocess data
#setwd("C:/Users/gabel/allergy-allies/App-Dashboard")
df <- read.csv("./Data/test_cleaning_file.csv", header = TRUE)

#Read in the original unedited dataframe
df_unedited <- read.csv("./Data/target_drugs_sample.csv")

#Data dictionary
data_dictionary <- "./Data/faers_analysis_dataset_dictionary.xlsx"
df_faers_data_dictionary <- read.xlsx(data_dictionary, sheet = 1)
df_faers_data_dictionary$Note <- NULL

#Rename columns of df_faers_data_dictionary
df_faers_data_dictionary <- df_faers_data_dictionary %>% `colnames<-`(c("Variable Name", "Description", "Type of Variable"))
#Drop Type of Variable column
df_faers_data_dictionary <- df_faers_data_dictionary %>% select(-`Type of Variable`)

df_target_drugs <- read.xlsx(data_dictionary, sheet = 2) %>%
  mutate(
    Brand.Name = str_replace(Brand.Name, "Singulaira", "Singulair"),
    Brand.Name = str_to_lower(Brand.Name),
    Generic.Name = str_to_lower(Generic.Name)
  ) %>%
  filter(!is.na(Brand.Name)) %>%
  select(Generic.Name, Brand.Name)

# Create a mapping vector (dictionary)
drug_map <- setNames(df_target_drugs$Brand.Name, df_target_drugs$Generic.Name)

# Ensure ps_drugname is lowercase and character type
df <- df %>% mutate(ps_drugname = str_to_lower(as.character(ps_drugname)))

# Map generic names to brand names while keeping unmatched values the same
df <- df %>% mutate(ps_drugname = coalesce(recode(ps_drugname, !!!drug_map), ps_drugname))

df_psychiatric <- read.xlsx(data_dictionary, sheet = 3)
#Select the only relevant column
df_psychiatric <- df_psychiatric %>% select(Reaction)

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

#Use df_unedited
df_copy <- df %>%
  filter(ps_drugname %in% unique(df_target_drugs$Brand.Name)) %>%
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
 filter(ps_drugname %in% unique(df_target_drugs$Brand.Name))

# #Need to convert things to datetime
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

#Format such that most recent year in data is removed based on case report(s) because the year is not finished and therefore could miss represent the data
current_year <- as.numeric(format(Sys.Date(), "%Y")) - 1
#remove dates that are greater or equal to the value of current year
df <- df[as.numeric(df$years) < current_year, ]


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
#Keep only certain columns that have more than 1 unique value but less than 200 unique values 
#Doing manually for now, will figure out a better method later!
df_outc_cod_limit_null <- df_outc_cod_limit_null %>% select(ps_drugname, age, sex, occr_country, reaction_pt)

#################################################

#Seperate df_outc_cod by outc_cod and use delimiter semicolon
df_outc_cod <- df_outc_cod %>% separate_rows(outc_cod, sep = ";")



#Recollapse df_outc_cod back into delimited data for accurate visualizations used by multiple selectInput(s)
df_outc_cod <- df_outc_cod %>%
  group_by(caseid) %>%
  mutate(outc_cod = paste(outc_cod, collapse = ";")) %>%  # Collapse reaction_pt
  ungroup() %>%  # Ungroup after mutating
  distinct()  # Remove duplicate rows if needed

#Use in ROR-PRR function
df_reactions <- df_outc_cod %>% separate_rows(reaction_pt, sep = ";")

#Do a stratified sampling of df_reactions to try and account for reporting bias and reduce computational time
#Performing a proportional stratified sample so smaller groups are more represented.
df_reactions <- df_reactions %>% group_by(ps_drugname) %>% slice_sample(prop = .40) %>% ungroup()

print(head(df_reactions))

########################ROR, PRR, BCPNN FUNCTION###############################
ROR_PRR_BCPNN_func <- function(Drug, ADR, df, CI=0.95, column = c("Drug_ADR", "year"), type = c("ROR")){
  
  #check if column is of length 2
  if (length(column) != 2) {
    return ("Invalid column. Please provide a column with two elements.")
  }
  
  # #check if columns exist in df
  if (!(column[1] %in% colnames(df)) || !(column[2] %in% colnames(df))) {
    return ("Invalid column. Please provide valid column names.")
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
  
  
  results <- list()
  
  # Calculate ROR
  if ("ROR" %in% type) {
    value <- ROR(a,b,c,d)
    results$ROR <- value
  }
  
  # Calculate PRR
  if ("PRR" %in% type) {
    value1 <- PRR(a, b, c, d)
    results$PRR <- value1
  }
  
  # Calculate BCPNN
  if ("BCPNN" %in% type) {
    results$BCPNN <- BCPNN(a, b, c, d)
  }
  

  
  return (results)
  
}

#Running Function
Running_ROR_PRR_BCPNN <- function(df, data_column = c("Drug_ADR", "years"), type = c("ROR"), updateProgress=NULL) {
  
  rows <- unique(df %>% select(data_column[1]) %>% pull())
  rows <- as.character(rows)
  
  # Extract unique years and sort them
  columns <- unique(df %>% select(data_column[2]) %>% pull())
  columns <- sort(as.character(columns))
  
  # Initialize a list to hold empty tables for each type
  tables <- list()
  for (t in type) {
    tables[[t]] <- data.frame(matrix(NA, ncol = length(columns), nrow = length(rows)))
    rownames(tables[[t]]) <- rows
    colnames(tables[[t]]) <- columns
  }
  
  foreach (column = columns) %do% {
    foreach (row = rows) %do% {
      
      # Attempt to calculate ROR, PRR, or BCPNN with error handling
      ROR_PRR <- tryCatch({
        result <- ROR_PRR_BCPNN_func(row, column, df, column = data_column, type = type, updateProgress)
        
        # Check if the result is numeric and has non-missing values
        if (is.null(result) || length(result) == 0 || any(is.na(result))) {
          result <- list()  # Return an empty list for missing values
        } else {
          result
        }
      }, error = function(e) {
        list()
      })
      
      # Populate the appropriate tables based on the `type` results
      for (t in type) {
        tables[[t]][row, column] <- ifelse(!is.null(names(ROR_PRR)) && t %in% names(ROR_PRR), ROR_PRR[[t]], 0)
        if (is.function(updateProgress)) {
          text <- paste0("Adding column: ", column)
          updateProgress(detail = text)
        }
      }
    }
  }
  
  # Convert each table in the list to a data frame for output
  tables <- lapply(tables, as.data.frame)
  return(tables)
}



############################Bar chart function##################################
bar_func <- function(table, condition, type=c("ROR", "PRR", "BCPNN")) {
  
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
  }
  
  #Check if the selected row is all NA
  if (all(is.na(selected_row))) {
    showModal(modalDialog(
      title = "Selection Error",
      glue("No {type} values for {condition}"),
      easyClose = TRUE
    ))
    return()
  }
  if (nrow(selected_row) == 0) {
    showModal(modalDialog(
      title = "Selection Error",
      glue("No {type} values for {condition}"),
      easyClose = TRUE
    ))
    return()
  }
  
  #If a value is 0, replace with NA so it won't show in the chart
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
  }
  
  # Convert to a data frame for ggplot
  plot_data <- data.frame(
    Drug = colnames(selected_row),
    Value = as.numeric(selected_row)
  )
  
  #round the value in the plot_data
  plot_data$Value <- round(plot_data$Value, 2)
  
  # Create the barplot
  p <- ggplot(plot_data, aes(x = Drug, y = Value, fill = Drug)) +
    geom_bar(stat = "identity", color = "black") +  # Use 'identity' to plot actual values
    geom_text(aes(label = Value), vjust = -0.5, size = 5) +  # Add labels on top of bars
    labs(title = "ROR Values for Allergic Rhinitis Drugs given Psychartic Condition", x = "Drug", y = "ROR Value") +
    theme_minimal()+
    theme(
      plot.title = element_text(hjust = 0.5, size = 20),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title.x = element_text(size = 15),
      axis.title.y = element_text(size = 15),
      legend.position = "none"  # Remove the legend
      
    )
  
  return(p)
}


############################Line-plot function##################################

lineplot_func <- function(table, conditions, type=c("ROR", "PRR", "BCPNN")) {
  
  #if the type is not ROR or PRR, return an error
  if (!(type %in% c("ROR", "PRR", "BCPNN"))) {
    return("Invalid type. Please select 'ROR' or 'PRR'.")
  }
  
  # Select the row matching the condition
  selected_rows <- table %>% filter(Drug_ADR %in% conditions) %>% group_by(Drug_ADR) %>% arrange(years)

  #Check if the selected row is all NA
  if (all(is.na(selected_rows))) {
    showModal(modalDialog(
      title = "Selection Error",
      glue("No {type} values for {conditions}"),
      easyClose = TRUE
    ))
    return()
  }
  if (nrow(selected_rows) == 0) {
    showModal(modalDialog(
      title = "Selection Error",
      glue("No {type} values for {conditions}"),
      easyClose = TRUE
    ))
    return()
  }
  

  # Create the lineplot
  p <- ggplot(selected_rows, aes(x = years, y = .data[[type]], color = Drug_ADR, group = Drug_ADR)) +
    geom_point(size = 3) +  # Plot ROR values as points
    geom_line() +  # Connect points with a line
    geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), width = 0.2) +  # Confidence intervals
    scale_y_log10() +  # Log scale for better visualization
    labs(
      title = glue("{type} Over Time with log scale 95% Confidence Interval"),
      x = "Year",
      y = glue("{type} values"),
      color = "Drug-ADR"
    ) +
    theme_minimal() + theme(
      plot.title = element_text(hjust = 0.5, size = 20),  # Centers the plot title
      axis.text.x = element_text(angle = 0, size = 15),
      axis.text.y = element_text(angle = 0, size = 15),
      axis.title.x = element_text(size = 18),
      axis.title.y = element_text(size = 18),
      legend.title = element_text(size = 15),
      legend.text = element_text(size = 12)
    )
  
  return(p)
}


#############################Mann Kendall Test##################################


# find the max value of each drug-adr pair in ROR_pair_years
mann_kendall_test <- function(df, type=c("ROR", "PRR", "BCPNN")) {
  trend_results <- data.frame(Drug_ADR = character(), P_Value = numeric(), Score = numeric(), stringsAsFactors = FALSE)
  
  # Loop over unique Drug-ADR combinations
  for (drug_adr in unique(df$Drug_ADR)) {
    # Extract ROR values for the given Drug-ADR over time
    values <- df %>%
      filter(Drug_ADR == drug_adr) %>%
      arrange(years) %>%
      pull(type)  # Extract ROR values as a numeric vector
    
    # Ensure at least 2 unique time points exist for the test
    if (length(values) > 2 && length(unique(values)) > 1) {
      test <- mk.test(values, alternative = "two.sided")  # Mann-Kendall Test
      trend_results <- rbind(trend_results, data.frame(Drug_ADR = drug_adr, Score = test$estimates[1], P_Value = test$p.value))
    }
  }
  
  return(trend_results)
}

#Checking timings of computations
end_time <- proc.time()
elasped_time = end_time - start_time
print(elasped_time)

################################UI##############################################
ui <- fluidPage(
  title = "Pharmocoviligance Application",
  theme = bs_theme(bootswatch = "minty"),

  tags$head(
    tags$style(HTML("
    .title-page {
      background-image: url('https://cdn.pixabay.com/photo/2023/12/24/20/46/ai-generated-8467687_1280.jpg'); 
      background-size: cover; /* Ensures the image covers the entire container */
      background-attachment: fixed; /* Keeps the background fixed */
      background-position: center; /* Centers the image */
      padding: 0; /* No extra padding around the content */
      text-align: center; /* Aligns the text */
      min-height: 100vh; /* Takes up the full viewport height */
      display: grid; /* Use grid for layout */
      grid-template-rows: 1fr 2fr 3fr; /* Define grid row heights */
      grid-template-columns: 1fr 1fr; /* Define two columns */
      gap: 5vh; /* Space between rows */
    }

    #text-box-a {
      grid-row: 1; /* Place in the first row */
      grid-column: 1; /* Place in the first column */
      box-sizing: content-box;  
      width: 60%; /* Relative width */
      padding: 1em;  
      background-color: rgba(255, 0, 0, 0); /* Semi-transparent background */
      color: white;
      font-size: 1rem; /* Responsive font size */
      text-align: left; /* Align text inside the box to the left */
      margin: 0 auto; /* Center horizontally */
    }

    #text-box-b {
      grid-row: 2; /* Place in the second row */
      grid-column: 2; /* Place in the second column */
      box-sizing: content-box;  
      width: 50%; /* Relative width */
      padding: 1em;  
      background-color: rgba(255, 0, 0, 0); /* Semi-transparent background */
      color: white;
      font-size: 1rem; /* Responsive font size */
      text-align: right; /* Align text inside the box to the right */
      margin: 0 auto; /* Center horizontally */
    }

    #text-box-c {
      grid-row: 3; /* Place in the third row */
      grid-column: 1; /* Place in the first column */
      box-sizing: content-box;  
      width: 50%; /* Relative width */
      padding: 1em;  
      background-color: rgba(255, 0, 0, 0); /* Semi-transparent background */
      color: white;
      font-size: 1rem; /* Responsive font size */
      text-align: left; /* Align text inside the box to the left */
      margin: 0 auto; /* Center horizontally */
    }

    /* Media query for smaller screens */
    @media (max-width: 768px) {
      .title-page {
        grid-template-rows: auto; /* Adjust rows for stacking */
        grid-template-columns: 1fr; /* Single column layout */
      }

      #text-box-a, #text-box-b, #text-box-c {
        grid-column: 1; /* All boxes in the same column */
        width: 90%; /* Adjust width for smaller screens */
        text-align: center; /* Center text inside boxes */
        margin: 1em auto; /* Center-align boxes */
      }
    }
  "))
  ),
  
  navset_card_tab(
    height = "120%",
    full_screen = FALSE,
    wrapper = NULL,
    
    # Title page
    nav_panel(
      title = "Front Page", icon = icon("house"),
      div(class = "title-page",
          h1("Welcome to this experimental pharmacovigilance website", style = "text-align:center, color: white;"),
          div(class = "content-a", id="text-box-a",
              h3("Medications have always been about helping people get healthier and increase longevity, but what happens when this doesn't occur?", style = "color: grey;"),
          ),
          div(class = "content-b", id = "text-box-b",
              h3("You may feel sick, dizzy or have a change in mood, whether it be physical or mental, this can be caused by medications unintentional effects", style = "color: grey;")
          ),
          div(class = "content-c", id = "text-box-c",
              h3("These effects are very difficult to find and to find them we have to find patterns in our data to use this, now looking at the FDA Adverse Event Reporting System (FAERS),
                 we can try and achieve this.", style = "color: grey;")
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
      
      card(
        height = "100%",
        full_screen = TRUE,
        layout_sidebar(
          sidebar = sidebarPanel(width = 12,
                                 sliderInput(
                                   "slider_bins",
                                   label = "Number of bins:",
                                   min = 10,
                                   max = 50,
                                   value = 25
                                 )
          ),
          div(plotOutput("plot_histogram"), style="margin-bottom:100px;")
        )
      ),
      
      card(
        height = "100%",
        full_screen = TRUE,
        layout_sidebar(
          sidebar = sidebarPanel(
            width = 12,
            sliderInput(
              "slider_age_bracket",
              label = "Select Age Bracket:",
              min = 0,
              max = 110,
              step = 1,
              value = c(0, 110)
            )
          ),
          div(plotOutput("plot_drug_age_bracket"), style="margin-bottom:100px;")
        )
      ),
      
      card(
        height = "100%",
        full_screen = TRUE,
        layout_sidebar(
          sidebar = sidebarPanel(
            width = 12,
            sliderInput(
              "year",
              label = "Select Interval:",
              min = min(as.numeric(df$years)),
              max = max(as.numeric(df$years)),
              step = 1,
              value = c(min(as.numeric(df$years)), max(as.numeric(df$years))),
              sep = ""
            )
          ),
          div(plotOutput("plot_total_cases"), style="margin-bottom:100px;")
        )
      ),
      
      card(
        height = "100%",
        full_screen = TRUE,
        layout_sidebar(
          sidebar = sidebarPanel(
            width = 12,
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
            )
          ),
          div(plotOutput("plot_lag_drug"), style="margin-bottom:100px;")
        )
      ),
      
      card(
        height = "100%",
        full_screen = TRUE,
        layout_sidebar(
          sidebar = sidebarPanel(
            width = 12,
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
          plotOutput("plot_patient_outcome")
        )
      )
    ),
  
    navbarMenu(
      title = "Statistical Tests and Analyses", icon = icon("chart-column"),
      # ROR Analysis
      nav_panel(
        title = "ROR Analysis", 
        icon = icon("pills"),     
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
              
          ), 
        layout_sidebar(
             fillable = TRUE,
             sidebar = sidebarPanel(width = 10,
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
                                                selected = "completed suicide"),
                                    
                                    actionButton(inputId = "submit_ROR", label = "Compute ROR"),
                                    
                                    selectInput("Drug_ADR_ROR",
                                                label = "Pick a Drug-ADR to analyze:",
                                                choices = "",
                                                multiple = TRUE,
                                                selected = "")
             ),
             div(
               plotOutput("ROR_plot"),
               style = "margin-bottom: 30px;" # Add space after the plot
             ),
             div(
               DT::dataTableOutput("ROR_table"),
               style = "margin-bottom: 30px;" # Add space after the table
             ),
             div(
               plotOutput("ROR_lineplot"),
               style = "margin-bottom: 30px;" # Add space after the second plot
             )
          ),
        card_footer("Caveat: Some drugs may not show up in the pie chart if their value is 0."),
        card_footer("Caveat: These renderings are based on a sample dataset and may not represent real-world data accurately.")
      ),
    
      # PRR Analysis
      nav_panel(
        title = "PRR Analysis",
        icon = icon("chart-area"),       
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
            
        ), 
        layout_sidebar(
          fillable = TRUE,
           sidebar = sidebarPanel(
             width = 10,
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
                          selected = "completed suicide"),
             
             actionButton(inputId = "submit_PRR", label = "Compute PRR"),
              
              selectInput("Drug_ADR_PRR",
                          label = "Pick a Drug-ADR to analyze:",
                          choices = "",
                          multiple = TRUE,
                          selected = "")
           ),
          
          div(
            plotOutput("PRR_plot"),
            style = "margin-bottom: 30px;" # Add space after the plot
          ),
          div(
            DT::dataTableOutput("PRR_table"),
            style = "margin-bottom: 30px;" # Add space after the table
          ),
          div(
            plotOutput("PRR_lineplot"),
            style = "margin-bottom: 30px;" # Add space after the second plot
          )
        
      ),
      card_footer("Caveat: Some drugs may not show up in the pie chart if their value is 0."),
      card_footer("Caveat: These renderings are based on a sample dataset and may not represent real-world data accurately.")
    ),
    
    # BCPNN Analysis
    nav_panel(
      title = "BCPNN Analysis",
      icon = icon("chart-pie"),
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
      ), 
      layout_sidebar(
        sidebar = sidebarPanel(
          width = 10,
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
         
         actionButton(inputId = "submit_BCPNN", label = "Compute BCPNN"),
         
         selectInput("Drug_ADR_BCPNN",
                     label = "Pick a Drug-ADR to analyze:",
                     choices = "",
                     multiple = TRUE,
                     selected = "")
        ),
        
        div(
          plotOutput("BCPNN_plot"),
          style = "margin-bottom: 30px;" # Add space after the plot
        ),
        div(
          DT::dataTableOutput("BCPNN_table"),
          style = "margin-bottom: 30px;" # Add space after the table
        ),
        div(
          plotOutput("BCPNN_lineplot"),
          style = "margin-bottom: 30px;" # Add space after the second plot
        )
      ),
      card_footer("Caveat: Some drugs may not show up in the pie chart if their value is 0."),
      card_footer("Caveat: These renderings are based on a sample dataset and may not represent real-world data accurately.")
    )
  ),
  
  nav_spacer(),
  nav_menu(
    title = "External Links",
    align = "right",
    nav_item(tags$a("Github", href = "https://github.com/gabriel21-vt")),
    nav_item(tags$a("Linkedin" , href = "https://www.linkedin.com/in/gabriel-dell-2b5841222/")),
    nav_item(tags$a("Pharmocoviligance Summary", href = "https://pubmed.ncbi.nlm.nih.gov/30126707/"))
  )
  )
)

###############################Server###########################################
server <- function(input, output, session) {
  
  has_choices <- reactiveVal(FALSE)
  
  # Render Selected Table
  output$selected_table <- renderDataTable({
    switch(input$table_choice,
           "Full Dataframe" = df_unedited,
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
  
  

####################ROR-table-plot##############################################
  # Create a reactive expression to compute ROR_data
  ROR_data <- eventReactive(input$submit_ROR, {
    
    # Ensure the required inputs are provided
    #req(input$psych_dropdown_ROR)
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
        value <- value + (progress$getMax() - value) / length(unique(filtered_data$reaction_pt))
      }
      progress$set(value = value, detail = detail)
    }
    
    # Compute ROR and PRR tables
    table <- Running_ROR_PRR_BCPNN(filtered_data, c("ps_drugname", "reaction_pt"), type = c("ROR"), updateProgress)
    
    #Fill NA with 0's
    table$ROR[is.na(table$ROR)] <- 0

    

    # Return the ROR table
    return(list(ROR = table$ROR))
  })
  
  
  # Render the bar chart using ROR_data
  output$ROR_plot <- renderPlot({
    req(ROR_data())  # Ensure ROR_data is available
    
    # Generate the pie chart
    bar_func(t(ROR_data()$ROR), input$psych_dropdown_ROR, "ROR")
  })
  
  # Render the table using the same ROR_data
  output$ROR_table <- DT::renderDataTable({
    req(ROR_data())
    table_ROR <- ROR_data()$ROR
    table_ROR <- format(table_ROR, digits=2)
    table_ROR <- t(table_ROR)
    table_ROR
  }, options = list(
    paging = TRUE,
    pageLength = 10,
    scrollX = TRUE,
    scrollY = TRUE,
    autoWidth = FALSE
  ))

  
  ###########PRR-table-plot########################
  
  # Create a reactive expression to compute ROR_data
  PRR_data <- eventReactive(input$submit_PRR, {
    
    # Ensure the required inputs are provided
    #req(input$psych_dropdown_PRR)
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
        value <- value + (progress$getMax() - value) / length(unique(filtered_data$reaction_pt))
      }
      progress$set(value = value, detail = detail)
    }
    
    # Compute ROR and PRR tables
    table <- Running_ROR_PRR_BCPNN(filtered_data, c("ps_drugname", "reaction_pt"), type=c("PRR"), updateProgress)
    
    table$PRR[is.na(table$PRR)] <- 0

    
    # Return the ROR table
    return(list(PRR = table$PRR))
  })
  
  output$PRR_plot <- renderPlot({
    req(PRR_data)
    
    #convert data to data frame
    PRR_data_df <- as.data.frame(PRR_data()$PRR)
    bar_func(t(PRR_data_df), input$psych_dropdown_PRR, "PRR")
  })
  
  output$PRR_table <- DT::renderDataTable({
    req(PRR_data)
    table_PRR <- PRR_data()$PRR 
    table_PRR <- format(table_PRR, digits=2)
    table_PRR <- t(table_PRR)
    table_PRR
  }, options = list(
    paging = TRUE,
    pageLength = 10,
    scrollX = TRUE,
    scrollY = TRUE,
    autoWidth = FALSE
  ))
  

  
  ###########################BCPNN table plots #################################
  BCPNN_data <- eventReactive(input$submit_BCPNN, {
    
    # Ensure the required inputs are provided
    #req(input$psych_dropdown_BCPNN)
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
        value <- value + (progress$getMax() - value) / length(unique(filtered_data$reaction_pt))
      }
      progress$set(value = value, detail = detail)
    }
    
    # Compute ROR and PRR tables
    table <- Running_ROR_PRR_BCPNN(filtered_data, c("ps_drugname", "reaction_pt"), type=c("BCPNN"), updateProgress)
    
    table$BCPNN[is.na(table$BCPNN)] <- 0
    # Return the ROR table
    return(list(BCPNN = table$BCPNN))
  })
  
  output$BCPNN_plot <- renderPlot({
    req(BCPNN_data)
    
    #convert data to data frame
    BCPNN_data_df <- as.data.frame(BCPNN_data())
    bar_func(t(BCPNN_data_df), input$psych_dropdown_BCPNN, "BCPNN")
  })
  
  output$BCPNN_table <- DT::renderDataTable({
    req(BCPNN_data)
    table_BCPNN <- BCPNN_data()$BCPNN
    table_BCPNN <- format(table_BCPNN, digits=2)
    table_BCPNN <- t(table_BCPNN)
    table_BCPNN
  }, options = list(
    paging = TRUE,
    pageLength = 10,
    scrollX = TRUE,
    scrollY = TRUE,
    autoWidth = FALSE
  ))

  
  ROR_lineplot_data <- reactiveVal(NULL)
  PRR_lineplot_data <- reactiveVal(NULL)
  BCPNN_lineplot_data <- reactiveVal(NULL)
  
  #Create an observe method that runs the ror, prr and bcpnn plot for trend tests
  #Should move the computations to the server side, meaning the overall elapsed time to load the program should decrease significantly, like 95%.
  #Should only run once, to update the select input(s) for ROR, PRR and BCPNN tabs
  #Should have a progress bar for this one, may be a while to do (like 40-50 seconds)
  observe({
    columns <- c("ps_drugname", "reaction_pt")
    df_reaction_pair_year <- df_reactions %>%
      group_by(across(all_of(columns)), years) %>%  # Group by the columns in the list and 'year'
      summarise(Count = n(), .groups = 'drop')


    #in df_reaction_no_singulair, combine ps_drugname and reaction_pt to create a new column
    df_reaction_pair_year$Drug_ADR <- paste(df_reaction_pair_year$ps_drugname,df_reaction_pair_year$reaction_pt, sep = "-")

    dimensions <- dim(df_reaction_pair_year)


    #############################ROR############################################
    compute_ror <- function(df, year) {
      # Filter for the specific year
      df_year <- df %>% filter(years == year)
      
      # Compute a, b, c, d
      a_b_c_d <- df_year %>%
        group_by(ps_drugname, reaction_pt) %>%
        summarise(
          a = sum(Count), # Cases for the specific Drug-ADR pair
          b = sum(df_year$Count[df_year$ps_drugname == first(ps_drugname)]) - a, # Same drug, other ADRs
          c = sum(df_year$Count[df_year$reaction_pt == first(reaction_pt)]) - a, # Other drugs, same ADR
          d = sum(df_year$Count) - (a + b + c), # Other drugs, other ADRs
          .groups = "drop"
        ) 
      
      # Compute ROR with confidence intervals
      a_b_c_d <- a_b_c_d %>%
        mutate(
          ROR = ROR(a,b,c,d),
          lower_CI = exp(log(ROR) - 1.96 * sqrt(1/a + 1/b + 1/c + 1/d)),
          upper_CI = exp(log(ROR) + 1.96 * sqrt(1/a + 1/b + 1/c + 1/d)),
          years = year # Ensure 'years' is included as a column
        ) %>% mutate(
          ROR = ifelse(is.infinite(ROR) | is.nan(ROR), 0, ROR),
          lower_CI = ifelse(is.infinite(lower_CI) | is.nan(lower_CI), 0, lower_CI),
          upper_CI = ifelse(is.infinite(upper_CI) | is.nan(upper_CI), 0, upper_CI)
        ) %>%
        select(years, ps_drugname, reaction_pt, ROR, lower_CI, upper_CI)
      
      return(a_b_c_d)
    }
    
    # Apply function for each unique year
    df_ror <- df_reaction_pair_year %>%
      distinct(years) %>%
      pull(years) %>%
      lapply(function(y) compute_ror(df_reaction_pair_year, y)) %>%
      bind_rows()
    
    df_ror$Drug_ADR <- paste(df_ror$ps_drugname,df_ror$reaction_pt, sep = "-")


    #ROR drug adr trend analysis
    trend_test_ROR <- mann_kendall_test(df_ror, "ROR")
    #print(trend_test_ROR)
    
    # Filter to only include statistically significant (P < 0.05) and positive trends (Score > 0)
    trend_test_ROR <- trend_test_ROR %>%
      filter(Score > 0) %>%
      arrange(P_Value)  # Arrange by significance level
    
    # Extract top trending Drug-ADR pairs
    drug_adr_trend_ROR <- trend_test_ROR$Drug_ADR
    

    ###############################PRR##########################################
    
    compute_prr <- function(df, year) {
      # Filter for the specific year
      df_year <- df %>% filter(years == year)
      
      # Compute a, b, c, d
      a_b_c_d <- df_year %>%
        group_by(ps_drugname, reaction_pt) %>%
        summarise(
          a = sum(Count), # Cases for the specific Drug-ADR pair
          b = sum(df_year$Count[df_year$ps_drugname == first(ps_drugname)]) - a, # Same drug, other ADRs
          c = sum(df_year$Count[df_year$reaction_pt == first(reaction_pt)]) - a, # Other drugs, same ADR
          d = sum(df_year$Count) - (a + b + c), # Other drugs, other ADRs
          .groups = "drop"
        ) 
      
      # Compute ROR with confidence intervals
      a_b_c_d <- a_b_c_d %>%
        mutate(
          #PRR = (a / (a+b)) / (c / (c+d)),
          PRR = PRR(a,b,c,d),
          lower_CI = exp(log(PRR) - 1.96 * sqrt(1/a + 1/b + 1/c + 1/d)),
          upper_CI = exp(log(PRR) + 1.96 * sqrt(1/a + 1/b + 1/c + 1/d)),
          years = year # Ensure 'years' is included as a column
        ) %>% mutate(
          PRR = ifelse(is.infinite(PRR) | is.nan(PRR), 0, PRR),
          lower_CI = ifelse(is.infinite(lower_CI) | is.nan(lower_CI), 0, lower_CI),
          upper_CI = ifelse(is.infinite(upper_CI) | is.nan(upper_CI), 0, upper_CI)
        ) %>%
        select(years, ps_drugname, reaction_pt, PRR, lower_CI, upper_CI)
      
      return(a_b_c_d)
    }
    
    # Apply function for each unique year
    df_prr <- df_reaction_pair_year %>%
      distinct(years) %>%
      pull(years) %>%
      lapply(function(y) compute_prr(df_reaction_pair_year, y)) %>%
      bind_rows()
    
    df_prr$Drug_ADR <- paste(df_prr$ps_drugname,df_prr$reaction_pt, sep = "-")

    #PRR drug-adr trend analysis
    trend_test_PRR <- mann_kendall_test(df_prr, "PRR")
    
    # Filter to only include statistically significant (P < 0.05) and positive trends (Score > 0)
    trend_test_PRR <- trend_test_PRR %>%
      filter(Score > 0) %>%
      arrange(P_Value)  # Arrange by significance level
    
    drug_adr_trend_PRR <- trend_test_PRR$Drug_ADR
    
    
    ############################################################################
    ##############################BCPNN#########################################
    
    compute_bcpnn <- function(df, year) {
      # Filter for the specific year
      df_year <- df %>% filter(years == year)

      # Compute a, b, c, d
      a_b_c_d <- df_year %>%
        group_by(ps_drugname, reaction_pt) %>%
        summarise(
          a = sum(Count), # Cases for the specific Drug-ADR pair
          b = sum(df_year$Count[df_year$ps_drugname == first(ps_drugname)]) - a, # Same drug, other ADRs
          c = sum(df_year$Count[df_year$reaction_pt == first(reaction_pt)]) - a, # Other drugs, same ADR
          d = sum(df_year$Count) - (a + b + c), # Other drugs, other ADRs
          .groups = "drop"
        )

      # Compute ROR with confidence intervals
      a_b_c_d <- a_b_c_d %>%
        mutate(
          BCPNN = BCPNN(a, b, c, d),
          lower_CI = exp(log(BCPNN) - 1.96 * sqrt(1/a + 1/b + 1/c + 1/d)),
          upper_CI = exp(log(BCPNN) + 1.96 * sqrt(1/a + 1/b + 1/c + 1/d)),
          years = year # Ensure 'years' is included as a column
        ) %>% mutate(
          BCPNN = ifelse(is.infinite(BCPNN) | is.nan(BCPNN), 0, BCPNN),
          lower_CI = ifelse(is.infinite(lower_CI) | is.nan(lower_CI), 0, lower_CI),
          upper_CI = ifelse(is.infinite(upper_CI) | is.nan(upper_CI), 0, upper_CI)
        ) %>%
        select(years, ps_drugname, reaction_pt, BCPNN, lower_CI, upper_CI)

      return(a_b_c_d)
    }

    # Apply function for each unique year
    df_bcpnn <- df_reaction_pair_year %>%
      distinct(years) %>%
      pull(years) %>%
      lapply(function(y) compute_bcpnn(df_reaction_pair_year, y)) %>%
      bind_rows()

    df_bcpnn$Drug_ADR <- paste(df_bcpnn$ps_drugname,df_bcpnn$reaction_pt, sep = "-")

    #BCPNN drug-adr trend analysis
    trend_test_BCPNN <- mann_kendall_test(df_bcpnn, "BCPNN")
    
    # Filter to only include statistically significant (P < 0.05) and positive trends (Score > 0)
    trend_test_BCPNN <- trend_test_BCPNN %>%
      filter(Score > 0) %>%
      arrange(P_Value)  # Arrange by significance level
    
    drug_adr_trend_BCPNN <- trend_test_BCPNN$Drug_ADR
    
    ########################lineplot data storage###############################
    ROR_lineplot_data(df_ror)
    PRR_lineplot_data(df_prr)
    BCPNN_lineplot_data(df_bcpnn)

    updateSelectInput(
      session,
      "Drug_ADR_ROR",
      choices = sort(unique(drug_adr_trend_ROR)),
      selected = NULL
    )

    updateSelectInput(
      session,
      "Drug_ADR_PRR",
      choices = sort(unique(drug_adr_trend_PRR)),
      selected = NULL
    )

    updateSelectInput(
      session,
      "Drug_ADR_BCPNN",
      choices = sort(unique(drug_adr_trend_BCPNN)),
      selected = NULL
    )

  })
  
  
  
  #--------------ROR lineplot function and Mann Kendall Test-------------------#
  output$ROR_lineplot <- renderPlot({
    req(input$Drug_ADR_ROR, ROR_lineplot_data())
    lineplot_func(ROR_lineplot_data(),  input$Drug_ADR_ROR, "ROR")
  })
  
  #--------------PRR lineplot function and Mann Kendall Test-------------------#
  output$PRR_lineplot <- renderPlot({
    req(input$Drug_ADR_PRR, PRR_lineplot_data())
    lineplot_func(PRR_lineplot_data(), input$Drug_ADR_PRR, "PRR")
  })
  
  #--------------BCPNN lineplot function and Mann Kendall Test-------------------#
  output$BCPNN_lineplot <- renderPlot({
    req(input$Drug_ADR_BCPNN, BCPNN_lineplot_data())
    lineplot_func(BCPNN_lineplot_data(), input$Drug_ADR_BCPNN, "BCPNN")
  })
  
}

shinyApp(ui = ui, server = server)