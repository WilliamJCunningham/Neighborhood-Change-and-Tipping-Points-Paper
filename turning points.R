#install packages
install.packages("tidycensus")
install.packages("tidyverse")
install.packages("reshape2")
install.packages("dplyr")
install.packages("AER")
install.packages("stringr")
final_data <- data.frame()
final_beta_coeffs <- data.frame()

# Vector of all 50 U.S. state abbreviations
states <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA",
            "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
            "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ",
            "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC",
            "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")

# Loop through each state
for (iteration in states) {
  
  ################################################################################
  #load required packages
  library(tidycensus)
  library(tidyverse)
  library(reshape2)
  library(dplyr)
  library(AER)
  library(stringr)
  ################################################################################
  #Set options
  ################################################################################
  options(scipen=9999999999999999999999999999)
  #set census api key
  census_api_key("d32248c42f637caede00ae72803d920d129705e4", overwrite=TRUE, install = TRUE)
  ################################################################################
  #Pull Data
  ################################################################################
  df_2020_1 <- get_decennial(geography= "block",
                             variables=c(All_25_29 = "P12_011N", All_30_34 = "P12_012N",
                                         All_35_39 = "P12_013N", All_40_44 = "P12_014N",
                                         All_45_49 = "P12_015N", All_50_54 = "P12_016N",
                                         All_55_59 = "P12_017N", All_60_61 = "P12_018N",
                                         All_62_65 = "P12_019N",
                                         Hisp_25_29 = "P12H_011N", Hisp_30_34 = "P12H_012N",
                                         Hisp_35_39 = "P12H_013N", Hisp_40_44 = "P12H_014N",
                                         Hisp_45_49 = "P12H_015N", Hisp_50_54 = "P12H_016N",
                                         Hisp_55_59 = "P12H_017N", Hisp_60_61 = "P12H_018N",
                                         Hisp_62_65 = "P12H_019N",
                                         White_25_29 = "P12I_011N", White_30_34 = "P12I_012N",
                                         White_35_39 = "P12I_013N", White_40_44 = "P12I_014N",
                                         White_45_49 = "P12I_015N", White_50_54 = "P12I_016N",
                                         White_55_59 = "P12I_017N", White_60_61 = "P12I_018N",
                                         White_62_65 = "P12I_019N",
                                         Black_25_29 = "P12B_011N", Black_30_34 = "P12B_012N",
                                         Black_35_39 = "P12B_013N", Black_40_44 = "P12B_014N",
                                         Black_45_49 = "P12B_015N", Black_50_54 = "P12B_016N",
                                         Black_55_59 = "P12B_017N", Black_60_61 = "P12B_018N",
                                         Black_62_65 = "P12B_019N"
                             ),
                             year= 2020,
                             sumfile="dhc",
                             state=iteration)
  
  
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
  vars<- load_variables(2010,"sf1")
  #All
  #P012011N...becomes P012T011 FOR INSTANCE
  
  #begin data call
  df_2010_1 <- get_decennial(geography= "block",
                             variables=c(All_25_29 = "P012011", All_30_34 = "P012012",
                                         All_35_39 = "P012013", All_40_44 = "P012014",
                                         All_45_49 = "P012015", All_50_54 = "P012016",
                                         All_55_59 = "P012017", All_60_61 = "P012018",
                                         All_62_65 = "P012019",
                                         Hisp_25_29 = "P012H011", Hisp_30_34 = "P012H012",
                                         Hisp_35_39 = "P012H013", Hisp_40_44 = "P012H014",
                                         Hisp_45_49 = "P012H015", Hisp_50_54 = "P012H016",
                                         Hisp_55_59 = "P012H017", Hisp_60_61 = "P012H018",
                                         Hisp_62_65 = "P012H019",
                                         White_25_29 = "P012I011", White_30_34 = "P012I012",
                                         White_35_39 = "P012I013", White_40_44 = "P012I014",
                                         White_45_49 = "P012I015", White_50_54 = "P012I016",
                                         White_55_59 = "P012I017", White_60_61 = "P012I018",
                                         White_62_65 = "P012I019",
                                         Black_25_29 = "P012B011", Black_30_34 = "P012B012",
                                         Black_35_39 = "P012B013", Black_40_44 = "P012B014",
                                         Black_45_49 = "P012B015", Black_50_54 = "P012B016",
                                         Black_55_59 = "P012B017", Black_60_61 = "P012B018",
                                         Black_62_65 = "P012B019"
                             ),
                             year= 2010,
                             sumfile="sf1",
                             state=iteration)
  
  
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
  vars<- load_variables(2000,"sf1")
  #All
  #P012011N...becomes P012T011 FOR INSTANCE
  
  #begin data call
  df_2000_1 <- get_decennial(geography= "block",
                             variables=c(All_25_29 = "P012011", All_30_34 = "P012012",
                                         All_35_39 = "P012013", All_40_44 = "P012014",
                                         All_45_49 = "P012015", All_50_54 = "P012016",
                                         All_55_59 = "P012017", All_60_61 = "P012018",
                                         All_62_65 = "P012019",
                                         Hisp_25_29 = "P012H011", Hisp_30_34 = "P012H012",
                                         Hisp_35_39 = "P012H013", Hisp_40_44 = "P012H014",
                                         Hisp_45_49 = "P012H015", Hisp_50_54 = "P012H016",
                                         Hisp_55_59 = "P012H017", Hisp_60_61 = "P012H018",
                                         Hisp_62_65 = "P012H019",
                                         White_25_29 = "P012I011", White_30_34 = "P012I012",
                                         White_35_39 = "P012I013", White_40_44 = "P012I014",
                                         White_45_49 = "P012I015", White_50_54 = "P012I016",
                                         White_55_59 = "P012I017", White_60_61 = "P012I018",
                                         White_62_65 = "P012I019",
                                         Black_25_29 = "P012B011", Black_30_34 = "P012B012",
                                         Black_35_39 = "P012B013", Black_40_44 = "P012B014",
                                         Black_45_49 = "P012B015", Black_50_54 = "P012B016",
                                         Black_55_59 = "P012B017", Black_60_61 = "P012B018",
                                         Black_62_65 = "P012B019"
                             ),
                             year= 2000,
                             sumfile="sf1",
                             state=iteration)
  
  
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
  xwalk_2000_2010 <- read.csv("nhgis_blk2000_blk2010.csv")
  xwalk_2010_2020 <- read.csv("nhgis_blk2000_blk2010.csv")
  #2000 to 2010
  weights_2000_2010 <- xwalk_2000_2010
  colnames(weights_2000_2010)[1]<-"GEOID"
  colnames(weights_2000_2010)[3]<-"weight"
  
  # Function to keep only the last 16 characters of a string
  keep_last_16_characters <- function(text) {
    substr(text, nchar(text)-16, nchar(text))
  }
  
  # Applying the function to the first two columns
  weights_2000_2010$GEOID <- sapply(weights_2000_2010$GEOID, keep_last_16_characters)
  weights_2000_2010$GJOIN2010 <- sapply(weights_2000_2010$blk2010ge, keep_last_16_characters)
  
  
  
  
  
  
  # Function to remove the 3th and 7th characters from a string
  remove_3th_and_7th_characters <- function(text) {
    return(paste0(substr(text, 1, 2), substr(text, 4, 6),substr(text, 8, nchar(text))))
  }
  
  # Apply the function to the entire column
  weights_2000_2010$GEOID <- sapply(weights_2000_2010$GEOID, remove_3th_and_7th_characters)
  weights_2000_2010$GJOIN2010 <- sapply(weights_2000_2010$GJOIN2010, remove_3th_and_7th_characters)
  
  
  
  df_2000_2010_1 <- merge(x=df_2000_4, y=weights_2000_2010, BY="GEOID")
  # Step 1: Multiply columns 2 to 17 by the weight in column 19
  df_2000_2010_1$weight <- as.numeric(df_2000_2010_1$weight)
  df_2000_2010_2 <- df_2000_2010_1 %>%
    mutate(across(3:18, ~. * df_2000_2010_1$weight))
  
  # Step 2: Collapse the results by summing using column 18
  df_2000_2010_3 <- df_2000_2010_2 %>%
    group_by(GJOIN2010) %>%
    summarise(across(3:18, sum))
  colnames(df_2000_2010_3)[1]<-"GEOID"
  
  #2010s(2000 and 2010) to 
  weights_2010_2020 <- xwalk_2010_2020
  colnames(weights_2010_2020)[1]<-"GEOID"
  colnames(weights_2010_2020)[3]<-"weight"
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
  write.csv(merged_df,paste0("C:\\Users\\willi\\Desktop\\Research\\Year_Four\\Intermediate_Data\\merged_df", iteration, ".csv"))
  
  
  
  ################################################################################
  #Construct Variables of Interest
  ################################################################################
  #Construct "shares"
  merged_df <-read.csv(paste0("C:\\Users\\willi\\Desktop\\Research\\Year_Four\\Intermediate_Data\\merged_df", iteration, ".csv"))
  merged_df[is.na(merged_df)] <- 0
  merged_df$All_2020 <- merged_df$All_25_34_2020+merged_df$All_35_44_2020+merged_df$All_45_54_2020+merged_df$All_55_64_2020
  merged_df$Hisp_2020 <- merged_df$Hisp_25_34_2020+merged_df$Hisp_35_44_2020+merged_df$Hisp_45_54_2020+merged_df$Hisp_55_64_2020
  merged_df$Hisp_Share_2020 <- merged_df$Hisp_2020 / merged_df$All_2020
  merged_df$All_2010 <- merged_df$All_25_34_2010+merged_df$All_35_44_2010+merged_df$All_45_54_2010+merged_df$All_55_64_2010
  merged_df$Hisp_2010 <- merged_df$Hisp_25_34_2010+merged_df$Hisp_35_44_2010+merged_df$Hisp_45_54_2010+merged_df$Hisp_55_64_2010
  merged_df$Hisp_Share_2010 <- merged_df$Hisp_2010 / merged_df$All_2010
  merged_df$All_2000 <- merged_df$All_25_34_2000+merged_df$All_35_44_2000+merged_df$All_45_54_2000+merged_df$All_55_64_2000
  merged_df$Hisp_2000 <- merged_df$Hisp_25_34_2000+merged_df$Hisp_35_44_2000+merged_df$Hisp_45_54_2000+merged_df$Hisp_55_64_2000
  merged_df$Hisp_Share_2000 <- merged_df$Hisp_2000 / merged_df$All_2000
  merged_df[is.na(merged_df)] <- 0
  merged_df$All_2020 <- merged_df$All_25_34_2020+merged_df$All_35_44_2020+merged_df$All_45_54_2020+merged_df$All_55_64_2020
  merged_df$Black_2020 <- merged_df$Black_25_34_2020+merged_df$Black_35_44_2020+merged_df$Black_45_54_2020+merged_df$Black_55_64_2020
  merged_df$Black_Share_2020 <- merged_df$Black_2020 / merged_df$All_2020
  merged_df$All_2010 <- merged_df$All_25_34_2010+merged_df$All_35_44_2010+merged_df$All_45_54_2010+merged_df$All_55_64_2010
  merged_df$Black_2010 <- merged_df$Black_25_34_2010+merged_df$Black_35_44_2010+merged_df$Black_45_54_2010+merged_df$Black_55_64_2010
  merged_df$Black_Share_2010 <- merged_df$Black_2010 / merged_df$All_2010
  merged_df$All_2000 <- merged_df$All_25_34_2000+merged_df$All_35_44_2000+merged_df$All_45_54_2000+merged_df$All_55_64_2000
  merged_df$Black_2000 <- merged_df$Black_25_34_2000+merged_df$Black_35_44_2000+merged_df$Black_45_54_2000+merged_df$Black_55_64_2000
  merged_df$Black_Share_2000 <- merged_df$Black_2000 / merged_df$All_2000
  merged_df[is.na(merged_df)] <- 0
  merged_df$All_2020 <- merged_df$All_25_34_2020+merged_df$All_35_44_2020+merged_df$All_45_54_2020+merged_df$All_55_64_2020
  merged_df$White_2020 <- merged_df$White_25_34_2020+merged_df$White_35_44_2020+merged_df$White_45_54_2020+merged_df$White_55_64_2020
  merged_df$White_Share_2020 <- merged_df$White_2020 / merged_df$All_2020
  merged_df$All_2010 <- merged_df$All_25_34_2010+merged_df$All_35_44_2010+merged_df$All_45_54_2010+merged_df$All_55_64_2010
  merged_df$White_2010 <- merged_df$White_25_34_2010+merged_df$White_35_44_2010+merged_df$White_45_54_2010+merged_df$White_55_64_2010
  merged_df$White_Share_2010 <- merged_df$White_2010 / merged_df$All_2010
  merged_df$All_2000 <- merged_df$All_25_34_2000+merged_df$All_35_44_2000+merged_df$All_45_54_2000+merged_df$All_55_64_2000
  merged_df$White_2000 <- merged_df$White_25_34_2000+merged_df$White_35_44_2000+merged_df$White_45_54_2000+merged_df$White_55_64_2000
  merged_df$White_Share_2000 <- merged_df$White_2000 / merged_df$All_2000
  merged_df[is.na(merged_df)] <- 0
  #make natural logs
  
  columns_to_transform <- 2:71
  #try to transform 0 to .0000000001
  merged_df$block_group <- substr(merged_df$GEOID, 1, nchar(merged_df$GEOID) - 4)
  merged_df$total <-merged_df$All_25_34_2020 + merged_df$All_35_44_2020 + merged_df$All_45_54_2020 + merged_df$All_55_64_2020 
  #drop if total is zero
  merged_df <- merged_df %>%
    filter(total != 0)
  merged_df <- merged_df[, -1]
  #
  # Selecting columns 2 to 71 (adjust column indices accordingly)
  
  # Calculating the weighted average for each roW
  library(dplyr)
  library(purrr)
  
  # Assuming merged_df is your dataframe
  
  merged_df_i <- merged_df %>%
    mutate(across(2:71, ~. * merged_df$total))
  merged_df_i <- merged_df_i[, c(1:71, 73, 72)]
  # Step 2: Collapse the results by summing using column 18
  merged_df_tract <- merged_df_i %>%
    group_by(block_group) %>%
    summarise(across(c(2:72), sum))
  merged_df_tract <- merged_df_tract %>%
    mutate(across(2:71, ~. / merged_df_tract$total))
  na_counts <- colSums(is.na(merged_df_tract))
  
  # Display the results
  print(na_counts)
  
  zero_counts <- colSums(merged_df_tract == 0, na.rm = TRUE)
  
  # Display the results
  print(zero_counts)
  #getting maybe 4000 out of 18548
  merged_df <- merged_df_tract
  merged_df[is.na(merged_df)] <- 0
  # Loop through the columns and create new columns with the natural logarithms
  
  for (col in columns_to_transform) {
    new_col_name <- paste0("log_", colnames(merged_df[col]))  # Create a new column name
    #Replace zeros with .0000000001 and then calculate the natural logarithm
    merged_df[[new_col_name]] <- log(ifelse(merged_df[[col]] == 0, .000000000000001, merged_df[[col]]))
  }
  
  
  
  ##############################################################################
  #handle NA'S
  
  merged_df$Hisp_Share_45_54_2000 = merged_df$Hisp_45_54_2000/merged_df$All_45_54_2000
  merged_df$Black_Share_45_54_2000 = merged_df$Black_45_54_2000/merged_df$All_45_54_2000
  merged_df$White_Share_45_54_2000 = merged_df$White_45_54_2000/merged_df$All_45_54_2000
  merged_df[is.na(merged_df) | merged_df=="Inf"] = NA
  merged_df[is.na(merged_df) | merged_df=="-Inf"] = NA
  # #############################################################################optional controls
  
  
  #Outputs:
  #Summary Stats
  Summary_Stats <- summary(merged_df)
  Summary_Stats
  NA_Count <- colSums(is.na(merged_df))
  NA_Count
  
  
  #simple
  #construct minority variables
  merged_df$Minority_25_34_2000 <- merged_df$Black_25_34_2000+merged_df$Hisp_25_34_2000
  merged_df$Minority_35_44_2000 <- merged_df$Black_35_44_2000+merged_df$Hisp_35_44_2000
  merged_df$Minority_45_54_2000 <- merged_df$Black_45_54_2000+merged_df$Hisp_45_54_2000
  merged_df$Minority_55_64_2000 <- merged_df$Black_55_64_2000+merged_df$Hisp_55_64_2000
  
  merged_df$Minority_25_34_2010 <- merged_df$Black_25_34_2010+merged_df$Hisp_25_34_2010
  merged_df$Minority_35_44_2010 <- merged_df$Black_35_44_2010+merged_df$Hisp_35_44_2010
  merged_df$Minority_45_54_2010 <- merged_df$Black_45_54_2010+merged_df$Hisp_45_54_2010
  merged_df$Minority_55_64_2010 <- merged_df$Black_55_64_2010+merged_df$Hisp_55_64_2010
  
  merged_df$Minority_25_34_2020 <- merged_df$Black_25_34_2020+merged_df$Hisp_25_34_2020
  merged_df$Minority_35_44_2020 <- merged_df$Black_35_44_2020+merged_df$Hisp_35_44_2020
  merged_df$Minority_45_54_2020 <- merged_df$Black_45_54_2020+merged_df$Hisp_45_54_2020
  merged_df$Minority_55_64_2020 <- merged_df$Black_55_64_2020+merged_df$Hisp_55_64_2020
  
  merged_df$Minority_Share_2010 <- 1 - merged_df$White_Share_2010
  merged_df$Minority_Share_45_54_2000 <- 1 - merged_df$White_Share_45_54_2000
  columns_to_transform<- 146:157
  
  for (col in columns_to_transform) {
    new_col_name <- paste0("log_", colnames(merged_df[col]))  # Create a new column name
    #Replace zeros with .0000000001 and then calculate the natural logarithm
    merged_df[[new_col_name]] <- log(ifelse(merged_df[[col]] == 0, .000000000000001, merged_df[[col]]))
  }
  #Analysis
  beta_coeffs <- list()
  
  # Age groups
  age_groups <- c("25_34", "35_44", "45_54", "55_64")
  
  for (age in age_groups) {
    # White regression
    white_formula <- as.formula(paste0("log_White_", age, "_2020 ~ Minority_Share_2010 + log_Minority_25_34_2010 + log_Minority_35_44_2010 + log_Minority_45_54_2010 + log_White_25_34_2010 + log_White_35_44_2010 + log_White_45_54_2010 | Minority_Share_45_54_2000 + log_Minority_25_34_2010 + log_Minority_35_44_2010 + log_Minority_45_54_2010 + log_White_25_34_2010 + log_White_35_44_2010 + log_White_45_54_2010"))
    
    white_reg <- ivreg(white_formula, data = state_data)
    beta_coeffs[[paste0("beta_white_", age)]] <- coef(white_reg)['Minority_Share_2010']
    
    # Minority regression
    minority_formula <- as.formula(paste0("log_Minority_", age, "_2020 ~ Minority_Share_2010 + log_Minority_25_34_2010 + log_Minority_35_44_2010 + log_Minority_45_54_2010 + log_White_25_34_2010 + log_White_35_44_2010 + log_White_45_54_2010 | Minority_Share_45_54_2000 + log_Minority_25_34_2010 + log_Minority_35_44_2010 + log_Minority_45_54_2010 + log_White_25_34_2010 + log_White_35_44_2010 + log_White_45_54_2010"))
    
    minority_reg <- ivreg(minority_formula, data = state_data)
    beta_coeffs[[paste0("beta_minority_", age)]] <- coef(minority_reg)['Minority_Share_2010']
  }
  beta_coeffs$state = iteration
  final_beta_coeffs <- rbind(final_beta_coeffs, beta_coeffs)
  
  #turning point
  #Counterfactual demand
  increment <- 0.001
  
  # Initialize an empty data frame to store results
  for (i in (1:nrow(merged_df))) {
    simulation_results <- data.frame()
    for_sim <- merged_df[i,]
    #for_sim <- merged_df[3791,]
    # Perform Monte Carlo simulation with a loop for each division
    s_old=for_sim$Minority_Share_2010
    for (s in seq(0.001, 1, increment)) {
      
      
      n_white_25_34 = exp(for_sim$log_White_25_34_2010+beta_white_25_34*(s-for_sim$Minority_Share_2010))
      n_white_35_44 = exp(for_sim$log_White_35_44_2010+beta_white_35_44*(s-for_sim$Minority_Share_2010))
      n_white_45_54 = exp(for_sim$log_White_45_54_2010+beta_white_45_54*(s-for_sim$Minority_Share_2010))
      n_white_55_64 = exp(for_sim$log_White_55_64_2010+beta_white_55_64*(s-for_sim$Minority_Share_2010))
      n_minority_25_34 = exp(for_sim$log_Minority_25_34_2010+beta_minority_25_34*(s-for_sim$Minority_Share_2010))
      n_minority_35_44 = exp(for_sim$log_Minority_35_44_2010+beta_minority_35_44*(s-for_sim$Minority_Share_2010))
      n_minority_45_54 = exp(for_sim$log_Minority_45_54_2010+beta_minority_45_54*(s-for_sim$Minority_Share_2010))
      n_minority_55_64 = exp(for_sim$log_Minority_55_64_2010+beta_minority_55_64*(s-for_sim$Minority_Share_2010))
      
      #Rescale to ensure constant pop? would have to do stage above to every row for denominator :/
      
      #implied minority share
      s_new = (n_minority_55_64+n_minority_45_54+n_minority_35_44+n_minority_35_44+n_minority_25_34)/(n_minority_55_64+n_minority_45_54+n_minority_35_44+n_minority_35_44+n_minority_25_34+n_white_55_64+n_white_45_54+n_white_35_44+n_white_25_34)
      
      # For illustration, let's simulate a random variable and store the result
      simulated_result <- s_new # Replace this with your actual simulation code
      
      # Store the result in the data frame
      simulation_results <- rbind(simulation_results, data.frame(Division = s, Result = simulated_result))
    }
    
    simulation_results$difference <- simulation_results$Division-simulation_results$Result
    # Initialize variables
    merged_df$crossings[i] = sum(diff(sign(simulation_results$difference)) != 0)
    simulation_results$switch[2:1000] = diff(sign(simulation_results$difference)) !=0
    
    row_index_1 <- which(simulation_results$switch == TRUE)[2]
    merged_df$turning_point[i] = simulation_results$Result[row_index_1]
    row_index_2 <- which(simulation_results$switch == TRUE)[1]
    merged_df$stable_1[i] = simulation_results$Result[row_index_2]
    row_index_3 <- which(simulation_results$switch == TRUE)[3]
    merged_df$stable_2[i] = simulation_results$Result[row_index_3]
    print(i)
  }
  
  
  
  final_data <- rbind(final_data, merged_df)
  
  
  
  
  
  ####################################################################
  
  
  # Clear the cache
  rm(list = setdiff(ls(), c("final_data", "final_beta_coeffs")))
  
  gc()
  print(iteration)
}

write.csv(final_data, "final_data.csv")
write.csv(final_beta_coeffs, "final_beta_coeffs.csv")



