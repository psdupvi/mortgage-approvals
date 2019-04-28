df <- train_values %>% group_by(lender) %>%  summarise(lender_acceptance_rate = mean(accepted))
df <- train_values %>% group_by(msa_md,state_code,county_code) %>%  summarise(msa_md_acceptance_rate = mean(as.numeric(accepted))-1) 
df <- train_values %>% group_by(applicant_race,applicant_ethnicity) %>%  summarise(race_acceptance_rate = mean(as.numeric(accepted))-1) 
df  <- train_values %>% group_by(occupancy,property_type,preapproval,loan_purpose,loan_type) %>% summarise(occ_prop_rate = mean(as.numeric(as.character(accepted))),number = n())




df  <- train_values %>% group_by(lender, applicant_sex) %>% summarise(avg = mean(as.numeric(as.character(accepted))))



train_values %>% group_by(loan_type, loan_purpose, property_type) %>% summarise(avg = mean(as.numeric(as.character(accepted))), number = n())






train_values %>% ggplot(aes(y = tract_to_msa_md_income_pct, group = accepted)) + geom_boxplot()


df  <- train_values %>% group_by(occupancy,property_type) %>% summarise(avg = mean(as.numeric(as.character(accepted))))

