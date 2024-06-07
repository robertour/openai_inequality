
source("5.0_evaluation_utilities.r")

# set an output folder in data/output/Usage
output_folder <- "data/output/Usage"

# create the output folder if it does not exist
dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)

# create a variable that contains the folder with the selected features for the current model
binomial_folder <- "data/LASSOs/chatgpt_usage-flat-zeroes-binomial"

# load the selected features for the binomial
features_non_imputed <- get_features(paste0(binomial_folder, "/selected_features.csv"), base_features, "non_imputed", exclude_features = c(""))
features_imputed <- get_features(paste0(binomial_folder, "/selected_features.csv"), base_features, "imputed", exclude_features = c(""))

# load the dataframes
df_non_imputed <- load_dataframe("non_imputed", features_non_imputed, NULL, response_renames = response_renames)
df_imputed <- load_dataframe("imputed", features_imputed, NULL, response_renames = response_renames)

# fit the binomial model on non-imputed data
non_imputed_binomial_model <- fit_binomial(df_non_imputed, features_non_imputed, "Usage")

# fit the binomial model without NAs
non_imputed_binomial_model_no_nas <- fit_binomial(df_non_imputed, features_non_imputed, "Usage", with_na = FALSE)

# call the stepwise function using AIC without NAs
non_imputed_stepwise_model_no_nas <- stepAIC(non_imputed_binomial_model_no_nas, direction = "both", trace = TRUE)

# fit the binomial model on imputed data
imputed_binomial_model <- fit_binomial(df_imputed, features_imputed, "Usage")

# call the stepwise function using AIC with imputed data
imputed_stepwise_model <- stepAIC(imputed_binomial_model, direction = "both", trace = TRUE)


# table of the main main model
save_tab_models(non_imputed_binomial_model, labels = "Non Imputed", output_folder = output_folder, filename="tab_model_non_imputed", show.aic = TRUE, show.aicc = TRUE, show.ci = FALSE)

# table of the models
save_tab_models(list(non_imputed_binomial_model, non_imputed_binomial_model_no_nas, non_imputed_stepwise_model_no_nas, imputed_binomial_model, imputed_stepwise_model),
    labels = c("Non Imputed", "No NAs", "No NAs (Stepwise AIC)", "Imputed", "Imputed (Stepwise AIC)"),
    output_folder = output_folder, filename="tab_models_all", show.aic = TRUE, show.aicc = TRUE, show.ci = FALSE) 

# store the variance inflation factors
write.csv(vif(non_imputed_binomial_model), file = paste0(output_folder, "/vif_values.csv"), row.names = TRUE)

# store the selected features with AIC on the non-imputed data
write.csv(non_imputed_stepwise_model_no_nas$coefficients, file = paste0(output_folder, "/stepwise_aic_features.csv"), row.names = TRUE)

# # perform dominance analysis
# da_mc <- start_domir_binomial_capture(non_imputed_stepwise_model_no_nas, na.omit(df_non_imputed), stat="r2_mcfadden", force_init = FALSE, output_folder = output_folder)
# da_ng <- start_domir_binomial_capture(non_imputed_stepwise_model_no_nas, na.omit(df_non_imputed), stat="r2_nagelkerke", force_init = FALSE, output_folder = output_folder)
# da_tj <- start_domir_binomial_capture(non_imputed_stepwise_model_no_nas, na.omit(df_non_imputed), stat="r2_tjur", force_init = FALSE, output_folder = output_folder)

# # save dominance matrix
# save_dominance_matrix(paste0(output_folder, "/dominance_matrix_mc.png"), da_mc$Complete_Dominance)
# save_dominance_matrix(paste0(output_folder, "/dominance_matrix_ng.png"), da_ng$Complete_Dominance)
# save_dominance_matrix(paste0(output_folder, "/dominance_matrix_tj.png"), da_tj$Complete_Dominance)

# # save the correlation matrix of all the features
# save_correlation_matrix(df_non_imputed, features_non_imputed, filepath = file.path(output_folder, "corrplot_non_imputed.png"))
# save_correlation_matrix(df_imputed, features_imputed, filepath = file.path(output_folder, "corrplot_imputed.png"))
