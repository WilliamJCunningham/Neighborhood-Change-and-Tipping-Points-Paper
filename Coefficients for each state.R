install.packages("tidycensus")
install.packages("tidyverse")
install.packages("reshape2")
install.packages("dplyr")
install.packages("AER")
install.packages("stringr")
library("dplyr")
library(tidyr)
state_fips_data <- data.frame(
  state_abbr = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA",
                 "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
                 "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ",
                 "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC",
                 "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"),
  fips_code = c("01", "02", "04", "05", "06", "08", "09", "10", "12", "13",
                "15", "16", "17", "18", "19", "20", "21", "22", "23", "24",
                "25", "26", "27", "28", "29", "30", "31", "32", "33", "34",
                "35", "36", "37", "38", "39", "40", "41", "42", "44", "45",
                "46", "47", "48", "49", "50", "51", "53", "54", "55", "56")
)

# Function to convert state abbreviation to FIPS code
state_abbr_to_fips <- function(state_abbr) {
  fips_code <- state_fips_data$fips_code[match(state_abbr, state_fips_data$state_abbr)]
  return(fips_code)
}


  states <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA",
              "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
              "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ",
              "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC",
              "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")

# Loop through each state
#for (iteration in states) {
  #Pull Data
iteration = "AL"
fips = state_abbr_to_fips(iteration)
  ################################################################################
  df_2020_1 <- read.csv(paste0('apidata//2020//2020_1',iteration,'.csv'))
  
  
  #drop "name column"
  df_2020_2 <- df_2020_1[ , -c(2)]
  
  # grepl(x=df_2020_1)
  #reshape
  df_2020_3 <- df_2020_2 %>% pivot_wider(names_from = variable, values_from = value)
  drop(x=df_2020_2)
  #create 10 yr cohorts (11+12,13+14,15+16,17+18+19)
  df_2020_4 <-data.frame(matrix(NA,ncol=17,nrow=nrow(df_2020_3)))
  colnames(df_2020_4) <- c("GEOID","All_25_34_2020","All_35_44_2020","All_45_54_2020","All_55_64_2020",
                           "Hisp_25_34_2020","Hisp_35_44_2020","Hisp_45_54_2020","Hisp_55_64_2020",
                           "White_25_34_2020","White_35_44_2020","White_45_54_2020","White_55_64_2020",
                           "Black_25_34_2020","Black_35_44_2020","Black_45_54_2020","Black_55_64_2020")
  
  df_2020_4[,1]<- df_2020_3[,1]
  df_2020_4[,2] <- df_2020_3[,2] + df_2020_3[,3]
  df_2020_4[,3] <- df_2020_3[,4] + df_2020_3[,5]
  df_2020_4[,4] <- df_2020_3[,6] + df_2020_3[,7]
  df_2020_4[,5] <- df_2020_3[,8] + df_2020_3[,9] + df_2020_3[,10]
  df_2020_4[,6] <- df_2020_3[,11] + df_2020_3[,12]
  df_2020_4[,7] <- df_2020_3[,13] + df_2020_3[,14]
  df_2020_4[,8] <- df_2020_3[,15] + df_2020_3[,16]
  df_2020_4[,9] <- df_2020_3[,17] + df_2020_3[,18] + df_2020_3[,19]
  df_2020_4[,10] <- df_2020_3[,20] + df_2020_3[,21]
  df_2020_4[,11] <- df_2020_3[,22] + df_2020_3[,23]
  df_2020_4[,12] <- df_2020_3[,24] + df_2020_3[,25]
  df_2020_4[,13] <- df_2020_3[,26] + df_2020_3[,27] + df_2020_3[,28]
  df_2020_4[,14] <- df_2020_3[,29] + df_2020_3[,30]
  df_2020_4[,15] <- df_2020_3[,31] + df_2020_3[,32]
  df_2020_4[,16] <- df_2020_3[,33] + df_2020_3[,34]
  df_2020_4[,17] <- df_2020_3[,35] + df_2020_3[,36] + df_2020_3[,37]
  
  ################################################################################
  #Repeat for 2010
  ################################################################################
  #All
  #P012011N...becomes P012T011 FOR INSTANCE
  
  #begin data call
  df_2010_1 <- read.csv(paste0('apidata//2010//2010_1',iteration,'.csv'))
  
  
  #drop "name colum"
  df_2010_2 <- df_2010_1[ , -c(2)]
  drop
  
  #reshape
  df_2010_3 <- df_2010_2 %>% pivot_wider(names_from = variable, values_from = value)
  
  
  #create 10 yr cohorts (11+12,13+14,15+16,17+18+19)
  df_2010_4 <-data.frame(matrix(NA,ncol=17,nrow=nrow(df_2010_3)))
  colnames(df_2010_4) <- c("GEOID","All_25_34_2010","All_35_44_2010","All_45_54_2010","All_55_64_2010",
                           "Hisp_25_34_2010","Hisp_35_44_2010","Hisp_45_54_2010","Hisp_55_64_2010",
                           "White_25_34_2010","White_35_44_2010","White_45_54_2010","White_55_64_2010",
                           "Black_25_34_2010","Black_35_44_2010","Black_45_54_2010","Black_55_64_2010")
  df_2010_4[,1]<- df_2010_3[,1]
  df_2010_4[,2] <- df_2010_3[,2] + df_2010_3[,3]
  df_2010_4[,3] <- df_2010_3[,4] + df_2010_3[,5]
  df_2010_4[,4] <- df_2010_3[,6] + df_2010_3[,7]
  df_2010_4[,5] <- df_2010_3[,8] + df_2010_3[,9] + df_2010_3[,10]
  df_2010_4[,6] <- df_2010_3[,11] + df_2010_3[,12]
  df_2010_4[,7] <- df_2010_3[,13] + df_2010_3[,14]
  df_2010_4[,8] <- df_2010_3[,15] + df_2010_3[,16]
  df_2010_4[,9] <- df_2010_3[,17] + df_2010_3[,18] + df_2010_3[,19]
  df_2010_4[,10] <- df_2010_3[,20] + df_2010_3[,21]
  df_2010_4[,11] <- df_2010_3[,22] + df_2010_3[,23]
  df_2010_4[,12] <- df_2010_3[,24] + df_2010_3[,25]
  df_2010_4[,13] <- df_2010_3[,26] + df_2010_3[,27] + df_2010_3[,28]
  df_2010_4[,14] <- df_2010_3[,29] + df_2010_3[,30]
  df_2010_4[,15] <- df_2010_3[,31] + df_2010_3[,32]
  df_2010_4[,16] <- df_2010_3[,33] + df_2010_3[,34]
  df_2010_4[,17] <- df_2010_3[,35] + df_2010_3[,36] + df_2010_3[,37]
  
  ################################################################################
  #Repeat for 2000
  ################################################################################
  #All
  #P012011N...becomes P012T011 FOR INSTANCE
  
  #begin data call
  df_2000_1 <- read.csv(paste0('apidata//2000//2000_1',iteration,'.csv'))
  
  
  #drop "name colum"
  df_2000_2 <- df_2000_1[ , -c(2)]
  
  
  #reshape
  df_2000_3 <- df_2000_2 %>% pivot_wider(names_from = variable, values_from = value)
  
  
  #create 10 yr cohorts (11+12,13+14,15+16,17+18+19)
  df_2000_4 <-data.frame(matrix(NA,ncol=17,nrow=nrow(df_2000_3)))
  colnames(df_2000_4) <- c("GEOID","All_25_34_2000","All_35_44_2000","All_45_54_2000","All_55_64_2000",
                           "Hisp_25_34_2000","Hisp_35_44_2000","Hisp_45_54_2000","Hisp_55_64_2000",
                           "White_25_34_2000","White_35_44_2000","White_45_54_2000","White_55_64_2000",
                           "Black_25_34_2000","Black_35_44_2000","Black_45_54_2000","Black_55_64_2000")
  df_2000_4[,1]<- df_2000_3[,1]
  df_2000_4[,2] <- df_2000_3[,2] + df_2000_3[,3]
  df_2000_4[,3] <- df_2000_3[,4] + df_2000_3[,5]
  df_2000_4[,4] <- df_2000_3[,6] + df_2000_3[,7]
  df_2000_4[,5] <- df_2000_3[,8] + df_2000_3[,9] + df_2000_3[,10]
  df_2000_4[,6] <- df_2000_3[,11] + df_2000_3[,12]
  df_2000_4[,7] <- df_2000_3[,13] + df_2000_3[,14]
  df_2000_4[,8] <- df_2000_3[,15] + df_2000_3[,16]
  df_2000_4[,9] <- df_2000_3[,17] + df_2000_3[,18] + df_2000_3[,19]
  df_2000_4[,10] <- df_2000_3[,20] + df_2000_3[,21]
  df_2000_4[,11] <- df_2000_3[,22] + df_2000_3[,23]
  df_2000_4[,12] <- df_2000_3[,24] + df_2000_3[,25]
  df_2000_4[,13] <- df_2000_3[,26] + df_2000_3[,27] + df_2000_3[,28]
  df_2000_4[,14] <- df_2000_3[,29] + df_2000_3[,30]
  df_2000_4[,15] <- df_2000_3[,31] + df_2000_3[,32]
  df_2000_4[,16] <- df_2000_3[,33] + df_2000_3[,34]
  df_2000_4[,17] <- df_2000_3[,35] + df_2000_3[,36] + df_2000_3[,37]
  ################################################################################
  #crosswalks
  ################################################################################
  #for 2000 to 2010, use brown.edu's crosswalk weights if at a larger geography, otherwise use nhgis
  #weightfiles
  xwalk_2000_2010 <- read.csv(paste0('Crosswalks//2000_2010//nhgis_blk2000_blk2010_',fips,'.csv'))
  xwalk_2010_2020 <- read.csv(paste0('Crosswalks//2010_2020//nhgis_blk2010_blk2020_',fips,'.csv'))
  #2000 to 2010
  weights_2000_2010 <- xwalk_2000_2010
  weights_2000_2010<- weights_2000_2010[,-1]
  colnames(weights_2000_2010)[1]<-"GEOID"
  #colnames(weights_2000_2010)[2]<-"weight"
  
  # Function to keep only the last 16 characters of a string
  #keep_last_16_characters <- function(text) {
  #  substr(text, nchar(text)-16, nchar(text))
  #}
  
  # Applying the function to the first two columns
  #weights_2000_2010$GEOID <- sapply(weights_2000_2010$GEOID, keep_last_16_characters)
  #weights_2000_2010$GJOIN2010 <- sapply(weights_2000_2010$blk2010ge, keep_last_16_characters)
  
  
  
  
  
  
  # Function to remove the 3th and 7th characters from a string
  #remove_3th_and_7th_characters <- function(text) {
  #  return(paste0(substr(text, 2), substr(text, 4, 6),substr(text, 8, nchar(text))))
  #}
  
  # Apply the function to the entire column
  #weights_2000_2010$GEOID <- sapply(weights_2000_2010$GEOID, remove_3th_and_7th_characters)
  #weights_2000_2010$GJOIN2010 <- sapply(weights_2000_2010$GJOIN2010, remove_3th_and_7th_characters)
  
  
  
  df_2000_2010_1 <- merge(x=df_2000_4, y=weights_2000_2010, BY="GEOID")
  # Step 1: Multiply columns 2 to 17 by the weight in column 19
  df_2000_2010_1$weight <- as.numeric(df_2000_2010_1$weight)
  df_2000_2010_2 <- df_2000_2010_1 %>%
    mutate(across(2:17, ~. * df_2000_2010_1$weight))
  
  # Step 2: Collapse the results by summing using column 18
  df_2000_2010_3 <- df_2000_2010_2 %>%
    group_by(blk2010ge) %>%
    summarise(across(2:17, sum))
  colnames(df_2000_2010_3)[1]<-"GEOID"
  
  #2010s(2000 and 2010) to 
  weights_2010_2020 <- xwalk_2010_2020
  weights_2010_2020 <- weights_2010_2020[-1]
  colnames(weights_2010_2020)[1]<-"GEOID"
  #colnames(weights_2010_2020)[2]<-"weight"
  # Function to keep only the last 16 characters of a string
  
  
  # Applying the function to the first two columns
  #weights_2010_2020$GEOID <- sapply(weights_2010_2020$GEOID, keep_last_16_characters)
  #weights_2010_2020$GEOID20 <- sapply(weights_2010_2020$GEOID20, keep_last_16_characters)
  
  
  
  
  
  # # Function to remove the 3th and 7th characters from a string
  # remove_3th_and_7th_characters <- function(text) {
  #   return(paste0(substr(text, 1, 2), substr(text, 4, 6),substr(text, 8, nchar(text))))
  # }
  # 
  # # Apply the function to the entire column
  # weights_2010_2020$GEOID <- sapply(weights_2010_2020$GEOID, remove_3th_and_7th_characters)
  # weights_2010_2020$GJOIN2010 <- sapply(weights_2010_2020$GJOIN2010, remove_6th_and_7th_characters)
  
  # # Function to remove the 6th and 7th characters from a string
  # remove_6th_and_7th_characters <- function(text) {
  #   return(paste0(substr(text, 1, 5), substr(text, 8, nchar(text))))
  # }
  # 
  # # Apply the function to the entire column
  # weights_2010_2020$GEOID <- sapply(weights_2010_2020$GEOID, remove_6th_and_7th_characters)
  # weights_2010_2020$GEOID20 <- sapply(weights_2010_2020$GEOID20, remove_6th_and_7th_characters)
  
  #2000_2020
  
  df_2000_2010_2020_1 <- merge(x=df_2000_2010_3, y=weights_2010_2020, BY="GEOID")
  # Step 1: Multiply columns 2 to 17 by the weight in column 19
  df_2000_2010_2020_1$weight <- as.numeric(  df_2000_2010_2020_1$weight)
  df_2000_2010_2020_2 <- df_2000_2010_2020_1 %>%
    mutate(across(2:17, ~. * df_2000_2010_2020_1$weight))
  
  # Step 2: Collapse the results by summing using column 18
  df_2000_2010_2020_3 <- df_2000_2010_2020_2 %>%
    group_by(GEOID) %>%
    summarise(across(2:17, sum))
  colnames(df_2000_2010_2020_3)[1]<-"GEOID"
  
  #2010_2020
  
  df_2010_2020_1 <- merge(x=df_2010_4, y=weights_2010_2020, BY="GEOID")
  # Step 1: Multiply columns 2 to 17 by the weight in column 19
  df_2010_2020_1$weight <- as.numeric(df_2010_2020_1$weight)
  df_2010_2020_2 <- df_2010_2020_1 %>%
    mutate(across(3:18, ~. * df_2010_2020_1$weight))
  
  # Step 2: Collapse the results by summing using column 18
  df_2010_2020_3 <- df_2010_2020_2 %>%
    group_by(GEOID) %>%
    summarise(across(3:18, sum))
  colnames(df_2010_2020_3)[1]<-"GEOID"
  
  # Function to remove characters from specific positions
  # remove_chars <- function(x) {
  #   x_str <- as.character(x)
  #   modified_str <- paste0(substr(x_str, 1, 7), substr(x_str, 9, 9), substr(x_str, 12, nchar(x_str)))
  #   as.numeric(modified_str)
  # }
  
  # Apply the function to the Number column
  # df_2000_2010_2020_3$GEOID <- remove_chars(df_2000_2010_2020_3$GEOID)
  # df_2010_2020_3$GEOID <- remove_chars(df_2010_2020_3$GEOID)
  #everything is in 2020 geographies now
  #combine dataframes
  merged_df <- merge(df_2000_2010_2020_3,df_2020_4, by="GEOID")
  
  merged_df <- merge(df_2010_2020_3,merged_df, by="GEOID")
  #export
  write.csv(merged_df,paste0("merged_df", iteration, ".csv"))
#}