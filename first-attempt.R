
# Testing!


install.packages("xgboost")
library(xgboost)
set.seed(2)

train_control <- trainControl(method = 'cv',number = 5)

start_time <- Sys.time()
model_glm <- train(accepted ~ loan_percent + lender_acceptance_rate + percent_of_income + 
                     msa_md_acceptance_rate + race_acceptance_rate + co_applicant + 
                     applicant_sex + occ_prop_rate + pre_type_rate + loan_purpose + 
                     number_of_1_to_4_family_units + number_of_owner_occupied_units + 
                     tract_to_msa_md_income_pct + minority_population_pct + population, 
                   data = split_train,
                   trControl = train_control, method = 'glm')

pred_glm <- predict(model_glm,split_test,type = 'raw')
glm_results <- round(mean(pred_glm == split_test$accepted),4)
run_time_glm <- Sys.time() - start_time
results <- bind_rows(results, 
                     data.frame(Method = "GLM - Pre-Type (Not Purpose) with #", Accuracy = glm_results, Runtime = run_time_glm))
results %>% knitr::kable()

start_time <- Sys.time()
model_lda <- train(accepted ~ loan_percent + lender_acceptance_rate + percent_of_income + 
                     msa_md + state_code + county_code + race_acceptance_rate + co_applicant + 
                     applicant_sex + occ_prop_rate + pre_type_rate + loan_purpose + 
                     number_of_1_to_4_family_units + number_of_owner_occupied_units,
                   data = split_train,
                   trControl = train_control, method = 'lda')

pred_lda <- predict(model_lda,split_test, type = 'raw')
lda_results <- round(mean(pred_lda == split_test$accepted),4)
run_time_lda <- Sys.time() - start_time
results <- bind_rows(results,
                     data_frame(Method = "LDA - Pre-Type (Not Purpose) with #", 
                                Accuracy = lda_results, 
                                Runtime = run_time_lda))
results %>% knitr::kable()

start_time <- Sys.time()

model_gbm <- train(accepted ~ loan_percent + lender_acceptance_rate + percent_of_income + 
                     msa_md_acceptance_rate + race_acceptance_rate + co_applicant + 
                     applicant_sex + occ_prop_rate + pre_type_rate + loan_purpose + 
                     number_of_1_to_4_family_units + number_of_owner_occupied_units, 
                   data = split_train,
                   trControl = train_control, method = 'gbm')

pred_gbm <- predict(model_gbm,split_test, type = 'raw')
gbm_results <- round(mean(pred_gbm == split_test$accepted),4)
run_time_gbm <- Sys.time() - start_time
results <- bind_rows(results,
                     data_frame(Method = "GBM - Pre-Type (Not Purpose) with Some #", 
                                Accuracy = gbm_results, 
                                Runtime = run_time_gbm))
results %>% knitr::kable()

start_time <- Sys.time()
model_rf <- train(accepted ~ loan_percent + lender_acceptance_rate + percent_of_income + 
                    msa_md_acceptance_rate + race_acceptance_rate + co_applicant + 
                    applicant_sex + occ_prop_rate + pre_type_rate + loan_purpose,
                  data = split_train,
                  trControl = train_control, method = 'ranger')

pred_rf <- predict(model_rf,split_test, type = 'raw')
rf_results <- round(mean(pred_rf == split_test$accepted),4)
run_time_rf <- Sys.time() - start_time
results <- bind_rows(results,
                     data_frame(Method = "Ranger - Pre-Type (Not Purpose)", 
                                Accuracy = rf_results, 
                                Runtime = run_time_rf))
results %>% knitr::kable()


start_time <- Sys.time()
model_ada <- train(accepted ~ loan_percent + lender_acceptance_rate + percent_of_income + 
                    msa_md_acceptance_rate + race_acceptance_rate + co_applicant + 
                    applicant_sex + occ_prop_rate + pre_type_rate + loan_purpose,
                  data = split_train,
                  trControl = train_control, method = 'xgbTree')

pred_ada <- predict(model_ada,split_test, type = 'raw')
ada_results <- round(mean(pred_ada == split_test$accepted),4)
run_time_ada <- Sys.time() - start_time
results <- bind_rows(results,
                     data_frame(Method = "XGBoost - Pre-Type (Not Purpose)", 
                                Accuracy = ada_results, 
                                Runtime = run_time_ada))
results %>% knitr::kable()



test_pred_lda <- as.data.frame(predict(model_lda,test, type = 'raw'))
test_pred_glm <- as.data.frame(predict(model_glm,test, type = 'raw'))
test_pred_gbm <- as.data.frame(predict(model_gbm,test, type = 'raw'))
test_pred_rf <- as.data.frame(predict(model_rf,test, type = 'raw'))
test_pred_xgb <- as.data.frame(predict(model_ada,test, type = 'raw'))

write.csv(test_pred_rf, 'test_pred_rf.csv')
write.csv(test_pred_glm, 'test_pred_glm.csv')
write.csv(test_pred_gbm, 'test_pred_gbm.csv')
write.csv(test_pred_lda, 'test_pred_lda.csv')
write.csv(test_pred_xgb, 'test_pred_xgb.csv')

write.csv(pred_rf,'pred_rf.csv', row.names = FALSE)

write.csv(results, 'results.csv', row.names = FALSE)