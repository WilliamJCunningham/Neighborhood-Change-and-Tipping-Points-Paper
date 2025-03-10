#Betas for every state
state_coeffs_df <- data.frame()
options(scipen=9999999999999999999999999999)
states <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA",
            "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
            "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ",
            "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC",
            "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")

# Loop through each state
for (iteration in states) {

merged_df <-read.csv(paste0("merged_df", iteration, ".csv"))
merged_df$GEOID <- as.character(merged_df$GEOID)
merged_df$GEOID <- stringr::str_pad(merged_df$GEOID, width = 15, side = "left", pad = "0")
#merged_df <- merged_df[-1]
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

columns_to_transform <- 2:70
#try to transform 0 to .0000000001
merged_df$block_group <- substr(merged_df$GEOID, 1, nchar(merged_df$GEOID) - 4)
merged_df$total <-merged_df$All_25_34_2020 + merged_df$All_35_44_2020 + merged_df$All_45_54_2020 + merged_df$All_55_64_2020 
#drop if total is zero
library(dplyr)
merged_df <- merged_df %>% 
  filter(total != 0)
#merged_df <- merged_df[, -1]
#
# Selecting columns 2 to 71 (adjust column indices accordingly)

# Calculating the weighted average for each roW
library(purrr)

# Assuming merged_df is your dataframe





#################TRACT ANALYSIS############################################
merged_df_i <- merged_df %>%
  mutate(across(2:70, ~. * merged_df$total))
#merged_df_i <- merged_df_i[, c(1:71, 73, 72)]
# Step 2: Collapse the results by summing using column 18
merged_df_tract <- merged_df_i %>%
  group_by(block_group) %>%
  summarise(across(c(2:71 ), sum))
merged_df_tract <- merged_df_tract %>%
  mutate(across(2:70, ~. / merged_df_tract$total))
na_counts <- colSums(is.na(merged_df_tract))

# Display the results
print(na_counts)

zero_counts <- colSums(merged_df_tract == 0, na.rm = TRUE)

# Display the results
print(zero_counts)
merged_df <- merged_df_tract
colnames(merged_df)[1] <- 'GEOID'
merged_df[is.na(merged_df)] <- 0
# Loop through the columns and create new columns with the natural logarithms

for (col in columns_to_transform) {
  new_col_name <- paste0("log_", colnames(merged_df[col]))  # Create a new column name
  #Replace zeros with .0000000001 and then calculate the natural logarithm
  merged_df[[new_col_name]] <- log(ifelse(merged_df[[col]] == 0, .000000000000001, merged_df[[col]]))
}
# for (col in columns_to_transform) {
#   new_col_name <- paste0("log_", colnames(merged_df[col]))  # Create a new column name
#   # Replace zeros with .0000000001 and then calculate the natural logarithm
#   merged_df[[new_col_name]] <- log(merged_df[[col]])}

########

##############################################################################
#handle NA'S

merged_df$Hisp_Share_45_54_2000 = merged_df$Hisp_45_54_2000/merged_df$All_45_54_2000
merged_df$Black_Share_45_54_2000 = merged_df$Black_45_54_2000/merged_df$All_45_54_2000
merged_df$White_Share_45_54_2000 = merged_df$White_45_54_2000/merged_df$All_45_54_2000
merged_df[is.na(merged_df) | merged_df=="Inf"] = NA
merged_df[is.na(merged_df) | merged_df=="-Inf"] = NA
##############################################################################
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

################################################################################
#Geocode
library(tigris)
library(sf)
library(dplyr)

# Step 1: Download census tract data for a specific state
tx_tracts <- tracts(state = iteration, cb = TRUE, year = 2020)

# Step 2: Calculate centroids for each tract
tx_tracts_centroids <- st_centroid(tx_tracts)

# Step 3: Extract coordinates
tx_tracts_coords <- tx_tracts_centroids %>%
  mutate(
    longitude = st_coordinates(.)[,1],
    latitude = st_coordinates(.)[,2]
  ) %>%
  st_drop_geometry()

# Step 4: Merge coordinates with original data
tx_tracts_geocoded <- tx_tracts %>%
  left_join(tx_tracts_coords,by = 'GEOID')

#Merge with merged_df
merged_df$GEOID <- sprintf("%011s", merged_df$GEOID)
merged_df_geocoded <- left_join(merged_df, tx_tracts_coords, by = 'GEOID')

##########################################################################
#2sls with conley
#################################################################################
library(fixest)
library(AER)
library(dplyr)
#Repeat regressions with spatially correlated errors
run_regression_conley <- function(dependent_var, data_i) {
  formula <- as.formula(paste(dependent_var, "~ 
               log_Minority_25_34_2010 + log_Minority_35_44_2010 + log_Minority_45_54_2010 + 
               log_White_25_34_2010 + log_White_35_44_2010 + log_White_45_54_2010  | Minority_Share_2010 ~
               Minority_Share_45_54_2000 "))
  
  model_i <- feols(formula,  data=data_i, vcov = vcov_conley(lat='latitude',lon='longitude', cutoff = 160.934))
  summary_conley <- summary(model_i, vcov = vcov_conley(lat='latitude',lon='longitude', cutoff = 160.934))
  coeff_i <- coef(model_i,vcov = vcov_conley(lat='latitude',lon='longitude', cutoff = 160.934))
  beta <- coeff_i['fit_Minority_Share_2010']
  vcovconley_i <- vcov_conley(model_i)
  
  return(list(model = model_i, beta = beta, vcovconley = vcovconley_i, coeff= coeff_i))
}

# Run regressions for each group and store results
groups <- c("White_25_34", "Minority_25_34", "White_35_44", "Minority_35_44",
            "White_45_54", "Minority_45_54", "White_55_64", "Minority_55_64")

results <- lapply(groups, function(group) {
  dependent_var <- paste0("log_", group, "_2020")
  run_regression_conley(dependent_var, merged_df_geocoded)
})

# Assign results to variables
for (i in seq_along(groups)) {
  assign(paste0(groups[i], "_model","_conley"), results[[i]]['model'])
  assign(paste0("beta_", tolower(groups[i]),"_conley"), results[[i]]['beta'],)
  vcov_temp <- as.matrix(do.call(rbind, results[[i]]['vcovconley']))
  assign(paste0("vcov_", tolower(groups[i]), "_conley"), vcov_temp)
  coef_temp <- unlist(results[[i]]['coeff'])
  assign(paste0("coef_", tolower(groups[i]), "_conley"), coef_temp)
}
groups <- c("White_25_34", "Minority_25_34", "White_35_44", "Minority_35_44",
            "White_45_54", "Minority_45_54", "White_55_64", "Minority_55_64")
dependent_var <- paste0("log_", groups, "_2020")
new_row <- data.frame(
  state = iteration,
log_White_25_34_2020    <- results[[1]]$coeff['fit_Minority_Share_2010'],
log_Minority_25_34_2020    <- results[[2]]$coeff['fit_Minority_Share_2010'],
log_White_35_44_2020       <- results[[3]]$coeff['fit_Minority_Share_2010'], 
log_Minority_35_44_2020    <- results[[4]]$coeff['fit_Minority_Share_2010'],
log_White_45_54_2020    <- results[[5]]$coeff['fit_Minority_Share_2010'],    
log_Minority_45_54_2020    <- results[[6]]$coeff['fit_Minority_Share_2010'],
log_White_55_64_2020    <- results[[7]]$coeff['fit_Minority_Share_2010'],    
log_Minority_55_64_2020    <- results[[8]]$coeff['fit_Minority_Share_2010']
)
colnames(new_row)[2:9] <- dependent_var
state_coeffs_df <- rbind(state_coeffs_df, new_row)
}
write.csv(state_coeffs_df, "state_coeffs.csv")