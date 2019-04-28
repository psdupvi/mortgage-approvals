# Visualizing Data
## TO DOS:
### Visualize Income of Applicants - could color code by accepted
### Visualize Race of Applicants - could color code by accepted
### Visualize loan amount vs. income, color coded by accepted
### Visualize loan_type
### Visualize property type
### Visualize loan_purpose
### Visaulize preapproval rates
### Visualize Occupancy
### Visaulize sex (and maybe sex vs. race, prop table of accepted?)

train_values %>% ggplot(aes(x = applicant_income, fill = accepted)) + geom_histogram() + facet_wrap( ~ accepted) + 
  xlim(0,2500)

train_values %>% group_by(lender) %>%  summarise(lender_rate = mean(as.numeric(as.character(accepted))), 
                                                 count = n()) %>%
  filter(count >=5) %>% 
  ggplot(aes(x = lender_rate)) + geom_point(aes(size = count))

train_values %>% group_by(lender) %>%  summarise(lender_rate = mean(as.numeric(as.character(accepted))), 
                                                 count = n()) %>%
  filter(count <100) %>% ggplot(aes(x = lender_rate)) + geom_histogram()

train_values %>% group_by(msa_md,state_code,county_code) %>%  summarise(msa_md_rate = mean(as.numeric(as.character(accepted))), 
                                                 count = n()) %>%
    ggplot(aes(x = msa_md_rate)) + geom_histogram(binwidth = 0.05) + labs(title = 'Histogram of MSA/MD Acceptance Rate',
                                                                        x = ' Acceptance Rate (binwidth = 0.05',
                                                                        y = 'Count')

train_values %>% ggplot(aes(y = applicant_income)) + geom_boxplot() + facet_wrap( ~ accepted) + ylim(0,300)

train_values %>% ggplot(aes(y = loan_percent)) + geom_boxplot() + facet_wrap( ~ accepted) + ylim(0,7.5) +
  theme(axis.text.x = element_blank(), axis.ticks = element_blank())

train_values %>% ggplot(aes()) + geom_freqpoly(aes(x = loan_percent,color = accepted, stat(density))) + xlim(0,20) 

train_values %>% ggplot(aes(y = percent_of_income*1000, x = accepted)) + geom_boxplot()  + ylim(0,3) + 
  theme( axis.ticks = element_blank()) + labs(title = 'Applicant % of FFIEC Income by Acceptance', 
                                              x = 'Accepted',
                                              y = 'Applicant Income/FFIEC Median Income')

df <- train_values %>% group_by(lender) %>%  summarise(lender_acceptance_rate = mean(as.numeric(as.character(accepted))), count = n())
df <- df %>% mutate(lender_acceptance_rate = ifelse(count < 5,median(lender_acceptance_rate),lender_acceptance_rate))

train_values %>% ggplot(aes(y = loan_percent, group = accepted)) + geom_boxplot(aes(color = accepted)) + ylim(0,1) 


train_values %>% filter(applicant_sex != 4) %>% ggplot(aes(x = applicant_sex, fill = accepted)) + geom_bar(position = "dodge") + labs(title = 'Applicant Sex by Acceptance',
                                                                                                       subtitle =  'Only males were accepted at more than 50%',
                                                                                                       x = 'Applicant Sex',
                                                                                                       y = 'Count')
train_values %>% ggplot(aes(x = co_applicant, fill = accepted)) + 
  geom_bar(position = "dodge") + 
  labs(x = 'Co-Applicant',
       y = 'Count',
       title = )
train_values %>% ggplot(aes(x = loan_purpose, fill = accepted)) + 
  geom_bar(position = "dodge") + 
  labs(x = 'Loan Purpose',
       y = 'Count',
       title = 'Loan Purpose by Acceptance',
       subtitle = 'Home purchase was typically approved, but refinancing and home improvement were not.')
train_values %>% ggplot(aes(x = occupancy, fill = accepted)) + geom_bar(position = "dodge") + labs(x = 'Occupancy',
                                                                                                   y = 'Count')
train_values %>% group_by(loan_purpose) %>% summarise(Acceptance_Rate = mean(as.numeric(as.character(accepted))))

train_values %>% ggplot(aes(x = preapproval, fill = accepted)) + geom_bar(position = "dodge") + labs(x = 'Applicant Ethnicity',
                                                                                                       y = 'Count')

train_values %>% mutate(ethn_race = paste(applicant_ethnicity,applicant_race)) %>% group_by(ethn_race) %>% 
  mutate(count = n()) %>% filter(count >= 20000) %>% ungroup() %>% 
  ggplot(aes(x = ethn_race, fill = accepted)) +
  geom_bar(position = 'dodge', stat = 'count') +
  labs(title = 'Large Counts (n > 25000)',
       x = 'Ethnicity and Race',
       y = 'Count', subtitle = 'White and Asian non-Hispanics are the only large count where the majority are accepted')

train_values %>% mutate(ethn_race = paste(applicant_ethnicity,applicant_race)) %>% group_by(ethn_race) %>% 
  mutate(count = n()) %>% filter(count <= 20000 && count >= 1000) %>% ungroup() %>% 
  ggplot(aes(x = ethn_race, fill = accepted)) +
  geom_bar(position = 'dodge', stat = 'count') +
  labs(title = 'Medium Counts (n > 1000)',
       x = 'Ethnicity and Race',
       y = 'Count', subtitle = 'Corporations are the only medium count where the majority are accepted')

train_values %>% mutate(ethn_race = paste(applicant_ethnicity,applicant_race)) %>% group_by(ethn_race) %>% 
  mutate(count = n()) %>% filter(count <= 1000) %>% ungroup() %>% 
  ggplot(aes(x = ethn_race, fill = accepted)) +
  geom_bar(position = 'dodge', stat = 'count') +
  labs(title = 'Small Counts (n < 1000)',
       x = 'Ethnicity and Race',
       y = 'Count', subtitle = 'Black Hispanics are denied at almost double the rate they are accepted')

ethnc_race_large_count <- train_values %>% mutate(ethn_race = paste(applicant_ethnicity,applicant_race)) %>% group_by(ethn_race) %>% 
                               mutate(count = n()) %>% filter(count >= 20000) %>% ungroup() %>% 
                               ggplot(aes(x = ethn_race, fill = accepted)) +
                               geom_bar(position = 'dodge', stat = 'count')

ethnc_race_med_count <- train_values %>% mutate(ethn_race = paste(applicant_ethnicity,applicant_race)) %>% group_by(ethn_race) %>% 
  mutate(count = n()) %>% filter(count <= 20000 && count >= 1000) %>% ungroup() %>% 
  ggplot(aes(x = ethn_race, fill = accepted)) +
  geom_bar(position = 'dodge', stat = 'count') +
  labs(title = 'Medium Counts (n > 1000)',
       x = 'Ethnicity and Race',
       y = 'Count', subtitle = 'Corporations are the only medium count where the majority are accepted')

print(ethnc_race_med_count)

ethnc_race_small_count <- train_values %>% mutate(ethn_race = paste(applicant_ethnicity,applicant_race)) %>% group_by(ethn_race) %>% 
  mutate(count = n()) %>% filter(count <= 1000) %>% ungroup() %>% 
  ggplot(aes(x = ethn_race, fill = accepted)) +
  geom_bar(position = 'dodge', stat = 'count') +
  labs(title = 'Small Counts (n < 1000)',
       x = 'Ethnicity and Race',
       y = 'Count', subtitle = 'Black Hispanics are denied at almost double the rate they are accepted')
print(ethnc_race_small_count)

occ_prop_large_count <- train_values %>% mutate(occ_prop = paste(occupancy,property_type)) %>% group_by(occ_prop) %>% 
  mutate(count = n()) %>% filter(count >= 100000) %>% ungroup() %>% 
  ggplot(aes(x = occ_prop, fill = accepted)) +
  geom_bar(position = 'dodge', stat = 'count')

occ_prop_large_count + labs(title = 'Large Counts of Occupancy and Property Type merger',
                            subtitle = 'Single Family homes were nearly a 50/50 toss up',
                            x = 'Occupancy and Property Type Merger',
                            y = 'Count')
print(occ_prop_large_count)

occ_prop_med_count <- train_values %>% mutate(occ_prop = paste(occupancy,property_type)) %>% group_by(occ_prop) %>% 
  mutate(count = n()) %>% filter(count <= 100000 && count >= 10000) %>% ungroup() %>% 
  ggplot(aes(x = occ_prop, fill = accepted)) +
  geom_bar(position = 'dodge', stat = 'count')
occ_prop_med_count + labs(title = 'Medium Counts of Occupancy and Property Type merger',
                          subtitle = 'Owner-occupied manufactured Housing was typically denied',
                          x = 'Occupancy and Property Type Merger',
                          y = 'Count')

occ_prop_small_count <- train_values %>% mutate(occ_prop = paste(occupancy,property_type)) %>% group_by(occ_prop) %>% 
  mutate(count = n()) %>% filter(count <= 10000) %>% ungroup() %>% 
  ggplot(aes(x = occ_prop, fill = accepted)) +
  geom_bar(position = 'dodge', stat = 'count')

occ_prop_small_count + labs(title = 'Small Counts of Occupancy and Property Type merger',
                          subtitle = 'Significant variation occurs in small counts',
                          x = 'Occupancy and Property Type Merger',
                          y = 'Count')
print(occ_prop_small_count)
 ## pre_type
pre_type_large <- train_values %>% mutate(pre_type = paste(preapproval,loan_type)) %>% group_by(pre_type) %>% 
  mutate(count = n()) %>% filter(count >= 100000) %>% ungroup() %>% 
  ggplot(aes(x = pre_type, fill = accepted)) +
  geom_bar(position = 'dodge', stat = 'count')
pre_type_large + labs(title = 'Large Counts of Pre-Approval and Loan Type merger',
                      subtitle = 'Conventional loans were accepted slightly less than half the time when pre-approval was not applicable',
                      x = 'Pre-Approval and Loan Type Merger',
                      y = 'Count')

print(pre_type_large)
pre_type_med <- train_values %>% mutate(pre_type = paste(preapproval,loan_type)) %>% group_by(pre_type) %>% 
  mutate(count = n()) %>% filter(count <= 100000 && count > 10000) %>% ungroup() %>% 
  ggplot(aes(x = pre_type, fill = accepted)) +
  geom_bar(position = 'dodge', stat = 'count')
pre_type_med + labs(title = 'Medium Counts of Pre-Approval and Loan Type merger',
                      subtitle = 'People who applied for pre-approval were accepted far less',
                      x = 'Pre-Approval and Loan Type Merger',
                      y = 'Count')

print(pre_type_med)
pre_type_small <- train_values %>% mutate(pre_type = paste(preapproval,loan_type)) %>% group_by(pre_type) %>% 
  mutate(count = n()) %>% filter(count <= 10000) %>% ungroup() %>% 
  ggplot(aes(x = pre_type, fill = accepted)) +
  geom_bar(position = 'dodge', stat = 'count')
pre_type_small + labs(title = 'Small Counts of Pre-Approval and Loan Type merger',
                    subtitle = 'People who applied for pre-approval were accepted far less ',
                    x = 'Pre-Approval and Loan Type Merger',
                    y = 'Count')

print(pre_type_small)
x <-sample_n(train_values,10000)

x %>% ggplot(aes(y = msa_md_acceptance_rate, x = ffiecmedian_family_income)) + geom_point(alpha = 1/10) 


