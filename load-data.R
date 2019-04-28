library(tidyverse)
library(caret)
library(corrplot)
library(Hmisc)

rm(list = ls())
train_values <- read_csv('train_values.csv')
test <- read_csv('test_values.csv')

train_values$applicant_income <- with(train_values, impute(applicant_income, median))
train_values$population <- with(train_values, impute(population, median))



colnames(train_values)[colnames(train_values) == 'number_of_owner-occupied_units'] = 'number_of_owner_occupied_units'
colnames(test)[colnames(test) == 'number_of_owner-occupied_units'] = 'number_of_owner_occupied_units'
## Change column names in train and test which don't parse well

#drops = c('number_of_owner_occupied_units', 'number_of_1_to_4_family_units')
#train_values <- train_values[, !names(train_values) %in% drops]
#test <- test[, !names(test) %in% drops]
## Identify list of columns to drop and then drops them
### These were selected either because they were collinear (state and county code to msa_md)
### or because they had NA and were generally unrelated, according to some visualizations


## Find acceptance rate for lender
df <- train_values %>% group_by(lender) %>%  summarise(lender_acceptance_rate = mean(accepted), count = n())
train_values <- merge(train_values,df, by = 'lender')
test <- merge(test,df, by = 'lender', all.x = TRUE)
train_values$lender_acceptance_rate <- impute(train_values$lender_acceptance_rate, median)
test$lender_acceptance_rate <- impute(test$lender_acceptance_rate, median)

## Combine the labels and the values of the training set

factors <- c('msa_md','lender','loan_type','property_type',
             'loan_purpose','occupancy', 'preapproval', 'applicant_ethnicity',
             'applicant_race','applicant_sex','row_id', 'state_code','county_code')


## Create a list of factor variables
test[,factors] <- lapply(test[,factors], factor)
train_values[,factors] <- lapply(train_values[, factors], factor)
train_values$accepted <- as.factor(train_values$accepted)
train_values$co_applicant <- as.factor(as.numeric(train_values$co_applicant))
test$co_applicant <- as.factor(as.numeric(test$co_applicant))

train_values$ffiecmedian_family_income <- impute(train_values$ffiecmedian_family_income,median)
test$ffiecmedian_family_income <- impute(test$ffiecmedian_family_income,median)
## Turn all factor variables into factors and convert accepted into bool
train_values <- train_values %>% mutate(percent_of_income = applicant_income/ffiecmedian_family_income)
train_values <- train_values %>% mutate(loan_percent = loan_amount/applicant_income)
test <- test %>% mutate(loan_percent = loan_amount/applicant_income)
test <- test %>% mutate(percent_of_income = applicant_income/ffiecmedian_family_income)
test$percent_of_income <- impute(test$percent_of_income, median)
test$loan_percent <- impute(test$loan_percent, median)
train_values$number_of_1_to_4_family_units <- impute(train_values$number_of_1_to_4_family_units, median)
train_values$number_of_owner_occupied_units <- impute(train_values$number_of_owner_occupied_units, median)
test$number_of_1_to_4_family_units <- impute(test$number_of_1_to_4_family_units, median)
test$number_of_owner_occupied_units <- impute(test$number_of_owner_occupied_units, median)
test$minority_population_pct <- impute(test$minority_population_pct, median)
test$tract_to_msa_md_income_pct <- impute(test$tract_to_msa_md_income_pct, median)
train_values$minority_population_pct <- impute(train_values$minority_population_pct, median)
train_values$tract_to_msa_md_income_pct <- impute(train_values$tract_to_msa_md_income_pct, median)

## Find acceptance rate for msa_md
df <- train_values %>% group_by(msa_md,state_code,county_code) %>%  summarise(msa_md_acceptance_rate = mean(as.numeric(accepted))-1, count = n()) 
train_values <- merge(train_values,df, by = c('msa_md','state_code','county_code'))
test <- merge(test,df, by = c('msa_md','state_code','county_code'), all.x = TRUE)
train_values$msa_md_acceptance_rate <- impute(train_values$msa_md_acceptance_rate, median)
test$msa_md_acceptance_rate <- impute(test$msa_md_acceptance_rate, median)

## Find acceptance rate for race
df <- train_values %>% group_by(applicant_race,applicant_ethnicity) %>%  summarise(race_acceptance_rate = mean(as.numeric(accepted))-1) 
train_values <- merge(train_values,df, by = c('applicant_race','applicant_ethnicity'))
test <- merge(test,df, by = c('applicant_race','applicant_ethnicity'), all.x = TRUE)
train_values$race_acceptance_rate <- impute(train_values$race_acceptance_rate, median)
test$race_acceptance_rate <- impute(test$race_acceptance_rate, median)

df  <- train_values %>% group_by(occupancy,property_type) %>% summarise(occ_prop_rate = mean(as.numeric(as.character(accepted))))
train_values <- merge(train_values,df, by = c('occupancy','property_type'))
test <- merge(test,df, by = c('occupancy','property_type'), all.x = TRUE)
train_values$occ_prop_rate <- impute(train_values$occ_prop_rate, median)
test$occ_prop_rate <- impute(test$occ_prop_rate, median)

df  <- train_values %>% group_by(preapproval,loan_type) %>% summarise(pre_type_rate = mean(as.numeric(as.character(accepted))))
train_values <- merge(train_values,df, by = c('preapproval','loan_type'))
test <- merge(test,df, by = c('preapproval','loan_type'), all.x = TRUE)
train_values$pre_type_rate <- impute(train_values$pre_type_rate, median)
test$pre_type_rate <- impute(test$pre_type_rate, median)

training <- train_values %>% mutate_if(is.numeric,scale)
test <- test %>% mutate_if(is.numeric,scale)
test <- test[order(test$row_id),]
# Scale the numeric variables


set.seed(1)
# Set seed for reproducibility
test_index <- createDataPartition(y = training$accepted, times = 1, p = 0.1, list = FALSE)
split_train <- training[-test_index,]
split_test <- training[test_index,]

results <- read_csv('results.csv')
