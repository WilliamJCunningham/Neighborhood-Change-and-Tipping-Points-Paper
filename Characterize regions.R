#libraries
library(haven)
library(dplyr)
#load in data
#state coeffs
state_coeffs <- read.csv("state_coeffs.csv")
#gss data
GSS2010 <- read_dta("GSS2021.dta")

# Dictionary for mapping state abbreviations to regions
state_to_region <- c(
  'CT' = 'New England', 'ME' = 'New England', 'MA' = 'New England', 'NH' = 'New England', 'RI' = 'New England', 'VT' = 'New England',
  'NJ' = 'Middle Atlantic', 'NY' = 'Middle Atlantic', 'PA' = 'Middle Atlantic',
  'IL' = 'East North Central', 'IN' = 'East North Central', 'MI' = 'East North Central', 'OH' = 'East North Central', 'WI' = 'East North Central',
  'IA' = 'West North Central', 'KS' = 'West North Central', 'MN' = 'West North Central', 'MO' = 'West North Central', 
  'NE' = 'West North Central', 'ND' = 'West North Central', 'SD' = 'West North Central',
  'DE' = 'South Atlantic', 'FL' = 'South Atlantic', 'GA' = 'South Atlantic', 'MD' = 'South Atlantic',
  'NC' = 'South Atlantic', 'SC' = 'South Atlantic', 'VA' = 'South Atlantic', 'WV' = 'South Atlantic', 'DC' = 'South Atlantic',
  'AL' = 'East South Central', 'KY' = 'East South Central', 'MS' = 'East South Central', 'TN' = 'East South Central',
  'AR' = 'West South Central', 'LA' = 'West South Central', 'OK' = 'West South Central', 'TX' = 'West South Central',
  'AZ' = 'Mountain', 'CO' = 'Mountain', 'ID' = 'Mountain', 'MT' = 'Mountain', 'NV' = 'Mountain', 'NM' = 'Mountain', 'UT' = 'Mountain', 'WY' = 'Mountain',
  'AK' = 'Pacific', 'CA' = 'Pacific', 'HI' = 'Pacific', 'OR' = 'Pacific', 'WA' = 'Pacific'
)

# Add a new column to the data frame with the region based on the state abbreviation
state_coeffs$region <- state_to_region[state_coeffs$state]
#add population for weighted average of your betas
state_pops <- read.csv("state_pops.csv")
fips_to_state <- c(
  '1' = 'AL', '2' = 'AK', '4' = 'AZ', '5' = 'AR', '6' = 'CA', '8' = 'CO', '9' = 'CT',
  '10' = 'DE', '11' = 'DC', '12' = 'FL', '13' = 'GA', '15' = 'HI', '16' = 'ID', '17' = 'IL',
  '18' = 'IN', '19' = 'IA', '20' = 'KS', '21' = 'KY', '22' = 'LA', '23' = 'ME', '24' = 'MD',
  '25' = 'MA', '26' = 'MI', '27' = 'MN', '28' = 'MS', '29' = 'MO', '30' = 'MT', '31' = 'NE',
  '32' = 'NV', '33' = 'NH', '34' = 'NJ', '35' = 'NM', '36' = 'NY', '37' = 'NC', '38' = 'ND',
  '39' = 'OH', '40' = 'OK', '41' = 'OR', '42' = 'PA', '44' = 'RI', '45' = 'SC', '46' = 'SD',
  '47' = 'TN', '48' = 'TX', '49' = 'UT', '50' = 'VT', '51' = 'VA', '53' = 'WA', '54' = 'WV',
  '55' = 'WI', '56' = 'WY'
)


# Add a new column with state abbreviations using the FIPS codes
state_pops$state <- fips_to_state[as.character(state_pops$ANSI.Code)]
df <- merge(state_coeffs, state_pops, by = "state")
df$Population <- gsub(",", "", df$Population)
df$Population <- as.numeric(df$Population)
#collapse
weighted_average <- function(x, w) {
  sum(x * w) / sum(w)
}

result <- df %>%
  group_by(region) %>%
  summarize(across(3:10, 
                   ~ weighted_average(., df$Population)))
result$region<-tolower(result$region)
region_labels <- c("new england", "middle atlantic", "east north central", 
                   "west north central", "south atlantic", "east south atlantic",
                   "west south central", "mountain", "pacific")


result$region <- match(result$region, region_labels) 
for_analysis <- merge(result,GSS2010, by = "region")
#"raceacs1", "raclive", "natrace","racwork", "racdif1",  "racdif2", "racdif3", "racdif4", "closeblk", "closewht"
#dependent_vars <- c("raclive", "racwork", "closeblk" , "closewht")
for_analysis$White<-  ifelse(for_analysis$raceacs1 == 1, 1, 0)
for_analysis$AnyOppositeRaceInNeighborhood <-  ifelse(for_analysis$raclive == 1, 0, 1)
for_analysis$AnyOppositeRaceAtWork <-  ifelse(for_analysis$racwork == 1, 0, 1)
for_analysis$RaceDiffDiscrimination<-  ifelse(for_analysis$racdif1 == 1, 1, 0)
for_analysis$RaceDiffLearningAbility<-  ifelse(for_analysis$racdif2 == 1, 1, 0)
for_analysis$RaceDiffEducation<-  ifelse(for_analysis$racdif3 == 1, 1, 0)
for_analysis$RaceDiffEffort<-  ifelse(for_analysis$racdif4 == 1, 1, 0)

dependent_vars <- c("White", "AnyOppositeRaceInNeighborhood", "AnyOppositeRaceAtWork" , "RaceDiffDiscrimination", "RaceDiffLearningAbility", "RaceDiffEducation", "RaceDiffEffort")
#Corex
################################################################################
# Install devtools if not already installed
if (!require("devtools")) install.packages("devtools")
library(devtools)

# Install rcorex from GitHub
if (!require("rcorex")) install_github("jpkrooney/rcorex")
library(rcorex)

# Load other required packages
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("reshape2")) install.packages("reshape2")
library(ggplot2)
library(reshape2)

# Define variables
dependent_vars <- c("White", "AnyOppositeRaceInNeighborhood", "AnyOppositeRaceAtWork",
                    "RaceDiffDiscrimination", "RaceDiffLearningAbility", "RaceDiffEducation",
                    "RaceDiffEffort")

# Extract relevant columns and convert to matrix
data_subset <- as.matrix(for_analysis[, dependent_vars])

# Run CorEx analysis
# We'll try with 2 latent factors initially
corex_result <- biocorex(data_subset, 
                      n_hidden = 1,           # number of hidden factors
                      marginal_description = "discrete",  # for bindary data
)

# Extract and print total correlation explained
summary(corex_result)
corex_result$tcs
for_analysis$labels<-corex_result$labels

################################################################################
indep_vars <- colnames(df)[3:10]
dependent_vars = "labels"
library(fixest)
run_regression <- function(dep_var) {
  formula <- as.formula(paste(dep_var, "~", paste(indep_vars, collapse = " + "), "+ 0"))
  model <- feols(formula, data = for_analysis)
  return(summary(model))
}

# Run regressions for all dependent variables
regression_results <- lapply(dependent_vars, run_regression)

# Name the list elements with dependent variable names
names(regression_results) <- dependent_vars

# Print summary of results
for (i in seq_along(regression_results)) {
  cat("\nRegression results for dependent variable:", names(regression_results)[i], "\n")
  print(regression_results[[i]])
  cat("\n")
}
################################################################################
#now with google trends data
library(gtrendsR)
testthis<-  gtrends(keyword = c("nigger","illegal immigrants", "affirmative action"),time = "2010-01-01 2020-01-01", geo = "US", compared_breakdown = FALSE)$interest_by_region
#pivot
pivoted_test <- pivot_wider(testthis,id_cols = "location", names_from = "keyword", values_from = "hits")
#map names
library(tidyr)
library(dplyr)

pivot_keywords <- function(pivoted_test) {
  # Transform tall dataframe to wide format
  result <- pivoted_test %>%
    pivot_wider(
      names_from = keyword,
      values_from = value,
      id_cols = location
    )
  
  return(result)
}

convert_state_names <- function(pivoted_test, location_col = "location") {
  # State name to abbreviation mapping
  state_mapping <- c(
    "Alabama" = "AL", "Alaska" = "AK", "Arizona" = "AZ", "Arkansas" = "AR",
    "California" = "CA", "Colorado" = "CO", "Connecticut" = "CT", "Delaware" = "DE",
    "Florida" = "FL", "Georgia" = "GA", "Hawaii" = "HI", "Idaho" = "ID",
    "Illinois" = "IL", "Indiana" = "IN", "Iowa" = "IA", "Kansas" = "KS",
    "Kentucky" = "KY", "Louisiana" = "LA", "Maine" = "ME", "Maryland" = "MD",
    "Massachusetts" = "MA", "Michigan" = "MI", "Minnesota" = "MN", "Mississippi" = "MS",
    "Missouri" = "MO", "Montana" = "MT", "Nebraska" = "NE", "Nevada" = "NV",
    "New Hampshire" = "NH", "New Jersey" = "NJ", "New Mexico" = "NM", "New York" = "NY",
    "North Carolina" = "NC", "North Dakota" = "ND", "Ohio" = "OH", "Oklahoma" = "OK",
    "Oregon" = "OR", "Pennsylvania" = "PA", "Rhode Island" = "RI", "South Carolina" = "SC",
    "South Dakota" = "SD", "Tennessee" = "TN", "Texas" = "TX", "Utah" = "UT",
    "Vermont" = "VT", "Virginia" = "VA", "Washington" = "WA", "West Virginia" = "WV",
    "Wisconsin" = "WI", "Wyoming" = "WY", "District of Columbia" = "DC"
  )
  
  # Function to standardize state names for matching
  standardize_name <- function(x) {
    x <- tolower(trimws(x))
    x <- gsub("\\s+", " ", x)  # standardize spaces
    return(x)
  }
  
  # Create a standardized version of the mapping
  names(state_mapping) <- standardize_name(names(state_mapping))
  
  # Convert state names to abbreviations
  pivoted_test[["location"]] <- sapply(pivoted_test[["location"]], function(x) {
    std_name <- standardize_name(x)
    if (std_name %in% names(state_mapping)) {
      return(state_mapping[std_name])
    } else {
      return(x)  # Return original if no match found
    }
  })
  
  return(pivoted_test)
}
result <- convert_state_names(pivoted_test)
result$state <- result$location
result <- result[-19,]
trends <- merge(state_coeffs, result, by ="state")
indep_vars <- colnames(df)[3:10]
trends$slur1<-as.numeric(trends$`nigger`)
trends$illegal_immigrants<-as.numeric(trends$`illegal immigrants`)
trends$affirmative_action<-as.numeric(trends$`affirmative action`)
dependent_vars = c("slur1","illegal_immigrants" ,"affirmative_action")
library(fixest)
run_regression <- function(dep_var) {
  formula <- as.formula(paste(dep_var, "~", paste(indep_vars, collapse = " + "), "+ 0"))
  model <- feols(formula, data = trends)
  return(summary(model))
}

# Run regressions for all dependent variables
regression_results <- lapply(dependent_vars, run_regression)

# Name the list elements with dependent variable names
names(regression_results) <- dependent_vars

# Print summary of results
for (i in seq_along(regression_results)) {
  cat("\nRegression results for dependent variable:", names(regression_results)[i], "\n")
  print(regression_results[[i]])
  cat("\n")
}
