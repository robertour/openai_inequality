source("5.0_evaluation_utilities.r")

# Select the response variable
response_var <- "Sessions"

# get the folder with the lasso outputs
counts_folder <- get_lasso_folder("nb", "chatgpt_sessions")

# create output folder based on the concatenation of the response variable and the output folder
output_folder <- paste0("data/output/", response_var)

# create the output folder if it does not exist
dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)

# create a variable that contains the folder with the selected features for the current model
binomial_folder <- "data/LASSOs/chatgpt_usage-flat-zeroes-binomial"

# load the selected features for the binomial
features_binomial_non_imputed <- get_features(paste0(binomial_folder, "/selected_features.csv"), base_features, "non_imputed", exclude_features = c(""))
features_binomial_imputed <- get_features(paste0(binomial_folder, "/selected_features.csv"), base_features, "imputed", exclude_features = c(""))

# Optional (USE AIC selected, results are similar)
# binomial_folder <- "data/output/Usage"
# features_binomial_non_imputed <- get_features(paste0(binomial_folder, "/stepwise_aic_features.csv"), base_features, "non_imputed", exclude_features = c(""))
# features_binomial_imputed <- get_features(paste0(binomial_folder, "/stepwise_aic_features.csv"), base_features, "imputed", exclude_features = c(""))

# load the selected features for the counts
features_count_non_imputed_wo_excl <-get_features(paste0(counts_folder, "/selected_features.csv"), base_features, "non_imputed", exclude_features = c(""))
features_count_imputed_wo_excl <- get_features(paste0(counts_folder, "/selected_features.csv"), base_features, "imputed", exclude_features = c(""))
features_count_non_imputed <-get_features(paste0(counts_folder, "/selected_features.csv"), base_features, "non_imputed", exclude_features = c("web engagement"))
features_count_imputed <- get_features(paste0(counts_folder, "/selected_features.csv"), base_features, "imputed", exclude_features = c("web engagement"))

# load the dataframe
df_non_imputed <- load_dataframe("non_imputed", features_binomial_non_imputed, features_count_non_imputed, response_renames = response_renames)
df_imputed <- load_dataframe("imputed", features_binomial_imputed, features_count_imputed, response_renames = response_renames)


#1.1 NON-IMPUTED ZERO-INFLATED MODELS###

# fit the zero-inflated models
hurdle_nb_non_imputed_model <- fit_zinf_non_imputed_model(df_non_imputed, features_binomial_non_imputed, features_count_non_imputed, response_var, use_hurdle = TRUE, dist = "negbin")
zinb_non_imputed_model <- fit_zinf_non_imputed_model(df_non_imputed, features_binomial_non_imputed, features_count_non_imputed, response_var, use_hurdle = FALSE, dist = "negbin") 
hurdle_poisson_non_imputed_model <- fit_zinf_non_imputed_model(df_non_imputed, features_binomial_non_imputed, features_count_non_imputed, response_var, use_hurdle = TRUE, dist = "poisson")
poisson_non_imputed_model <- fit_zinf_non_imputed_model(df_non_imputed, features_binomial_non_imputed, features_count_non_imputed, response_var, use_hurdle = FALSE, dist = "poisson")


# table of the models
save_tab_models(list(hurdle_nb_non_imputed_model, zinb_non_imputed_model, hurdle_poisson_non_imputed_model, poisson_non_imputed_model),
    labels = c("Hurdle (NB)", "ZINB", "Hurdle (Poisson)", "Poisson"),
    output_folder = output_folder, filename="tab-models_zero-inflation_non-imputed", show.aic = TRUE, show.aicc = TRUE, show.ci = FALSE) 

# table of the models
save_tab_models(list(hurdle_nb_non_imputed_model, zinb_non_imputed_model),
    labels = c("Hurdle (NB)", "ZINB"),
    output_folder = output_folder, filename="tab-models_zero-inflation_non-imputed_only-nb", show.aic = TRUE, show.aicc = TRUE) 


# store the variance inflation factors
write.csv(vif(hurdle_nb_non_imputed_model), file = paste0(output_folder, "/vif_values_zero-inflation_non-imputed_hurdle-nb.csv"), row.names = TRUE)



#1.2 STEP AIC OF NON-IMPUTED NO-NAS DATA###

# hurdle equivalent without NAs
hurdle_nb_non_imputed_model_no_nas <- fit_zinf_non_imputed_model(df_non_imputed, features_binomial_non_imputed, features_count_non_imputed, response_var, use_hurdle = TRUE, dist = "negbin", with_na = FALSE)

# call the stepwise function using AIC
step_aic_non_imputed_aic <- stepAIC(hurdle_nb_non_imputed_model_no_nas, direction = "both", trace = TRUE)

# table of the models
save_tab_models(list(hurdle_nb_non_imputed_model, hurdle_nb_non_imputed_model_no_nas, step_aic_non_imputed_aic),
    labels = c("Non Imputed", "Non Imputed (NO NAs)", "Step AIC"),
    output_folder = output_folder, filename="tab-models_zero-inflation_non-imputed_step-AIC"
    , show.ci=F, show.aic=T, show.aicc=T) 




#2.1 IMPUTED ZERO-INFLATED MODELS###

# fit the zero-inflated models
hurdle_nb_imputed_model <- fit_zinf_imputed_model(df_imputed, features_binomial_imputed, features_count_imputed, response_var, use_hurdle = TRUE, dist = "negbin")
zinb_imputed_model <- fit_zinf_imputed_model(df_imputed, features_binomial_imputed, features_count_imputed, response_var, use_hurdle = FALSE, dist = "negbin") 
hurdle_poisson_imputed_model <- fit_zinf_imputed_model(df_imputed, features_binomial_imputed, features_count_imputed, response_var, use_hurdle = TRUE, dist = "poisson")
poisson_imputed_model <- fit_zinf_imputed_model(df_imputed, features_binomial_imputed, features_count_imputed, response_var, use_hurdle = FALSE, dist = "poisson")

# table of the models
save_tab_models(list(hurdle_nb_imputed_model, zinb_imputed_model, hurdle_poisson_imputed_model, poisson_imputed_model),
    labels = c("Hurdle (NB)", "ZINB", "Hurdle (Poisson)", "Poisson"),
    output_folder = output_folder, filename="tab-models_zero-inflation_imputed", show.aic = TRUE, show.aicc = TRUE, show.ci = FALSE) 

# table of the models
save_tab_models(list(hurdle_nb_imputed_model, zinb_imputed_model),
    labels = c("Hurdle (NB)", "ZINB"),
    output_folder = output_folder, filename="tab-models_zero-inflation_imputed_only-nb", show.aic = TRUE, show.aicc = TRUE) 

# store the variance inflation factors
write.csv(vif(hurdle_nb_imputed_model), file = paste0(output_folder, "/vif_values_zero-inflation_imputed_hurdle-nb.csv"), row.names = TRUE)

#2.2 STEP AIC OF IMPUTED DATA###

# call the stepwise function using AIC
step_aic_imputed_aic <- stepAIC(hurdle_nb_imputed_model, direction = "both", trace = TRUE)

# table of the models
save_tab_models(list(hurdle_nb_imputed_model, step_aic_imputed_aic),
    labels = c("Imputed", "Step AIC"),
    output_folder = output_folder, filename="tab-models_zero-inflation_imputed_step-AIC"
    , show.ci=F, show.aic=T, show.aicc=T) 


#3.1 IMPUTED NO-ZEROS MODELS###

# remove zeroes
df_imputed_no_zeroes <- df_imputed[df_imputed$Usage != 0, ]

# fit the models
nb_imputed_no_zeroes_model <- fit_no_zeroes_model(df_imputed_no_zeroes, features_count_imputed, response_var, dist = "negbin")
gaussian_imputed_no_zeroes_model <- fit_no_zeroes_model(df_imputed_no_zeroes, features_count_imputed, response_var, dist = "gaussian") 
poisson_imputed_no_zeroes_model <- fit_no_zeroes_model(df_imputed_no_zeroes, features_count_imputed, response_var, dist = "poisson")

# table of the models
save_tab_models(list(nb_imputed_no_zeroes_model, gaussian_imputed_no_zeroes_model, poisson_imputed_no_zeroes_model),
    labels = c("Negative Binomial", "Gaussian", "Poisson"),
    output_folder = output_folder, filename="tab-models_no-zeros_imputed", show.aic = TRUE, show.aicc = TRUE, show.ci = FALSE) 

# store the variance inflation factors
write.csv(vif(nb_imputed_no_zeroes_model), file = paste0(output_folder, "/vif_values_no-zeros_imputed_nb.csv"), row.names = TRUE)


#3.2 STEP AIC OF IMPUTED NO-ZEROS NO-NAS DATA###

# call the stepwise function using AIC
step_aic_no_zeroes_imputed_aic <- stepAIC(nb_imputed_no_zeroes_model, direction = "both", trace = TRUE)

# table of the models
save_tab_models(list(nb_imputed_no_zeroes_model, step_aic_no_zeroes_imputed_aic),
    labels = c("Imputed",  "Step AIC"),
    output_folder = output_folder, filename="tab-models_no-zeroes_imputed_step-AIC"
    , show.ci=F, show.aic=T, show.aicc=T) 




#4.1 NON-IMPUTED NO-ZEROS MODELS###

# remove zeroes
df_non_imputed_no_zeroes <- df_non_imputed[df_non_imputed$Usage != 0, ]

# fit the models
nb_non_imputed_no_zeroes_model <- fit_no_zeroes_model(df_non_imputed_no_zeroes, features_count_non_imputed, response_var, dist = "negbin")
gaussian_non_imputed_no_zeroes_model <- fit_no_zeroes_model(df_non_imputed_no_zeroes, features_count_non_imputed, response_var, dist = "gaussian") 
poisson_non_imputed_no_zeroes_model <- fit_no_zeroes_model(df_non_imputed_no_zeroes, features_count_non_imputed, response_var, dist = "poisson")

# table of the models
save_tab_models(list(nb_non_imputed_no_zeroes_model, gaussian_non_imputed_no_zeroes_model, poisson_non_imputed_no_zeroes_model),
    labels = c("Negative Binomial", "Gaussian", "Poisson"),
    output_folder = output_folder, filename="tab-models_no-zeros_non-imputed", show.aic = TRUE, show.aicc = TRUE, show.ci = FALSE) 


# store the variance inflation factors
write.csv(vif(nb_non_imputed_no_zeroes_model), file = paste0(output_folder, "/vif_values_no-zeros_non-imputed_nb.csv"), row.names = TRUE)


#4.2 STEP AIC OF NON-IMPUTED NO-ZEROS DATA###

# hurdle equivalent
nb_non_imputed_no_zeroes_model_no_nas <- fit_no_zeroes_model(df_non_imputed_no_zeroes, features_count_non_imputed, response_var, dist = "negbin", with_na = FALSE)

# call the stepwise function using AIC
step_aic_non_zeroes_non_imputed_aic <- stepAIC(nb_non_imputed_no_zeroes_model_no_nas, direction = "both", trace = TRUE)

# table of the models
save_tab_models(list(nb_non_imputed_no_zeroes_model, nb_non_imputed_no_zeroes_model_no_nas, step_aic_non_zeroes_non_imputed_aic),
    labels = c("Non Imputed", "Non Imputed (NO NAs)", "Step AIC"),
    output_folder = output_folder, filename="tab-models_no-zeroes_non_imputed_step-AIC"
    , show.ci=F, show.aic=T, show.aicc=T) 


# store the variance inflation factors
write.csv(vif(nb_non_imputed_no_zeroes_model), file = paste0(output_folder, "/vif_values_no-zeroes_non-imputed.csv"), row.names = TRUE)

# save the correlation matrix of all the features
save_correlation_matrix(df_non_imputed, features_count_non_imputed_wo_excl, filepath = file.path(output_folder, "corrplot_non_imputed.png"))
save_correlation_matrix(df_imputed, features_count_imputed_wo_excl, filepath = file.path(output_folder, "corrplot_imputed.png"))


# # perform dominance analysis
# da_mc <- start_domir_nb_no_zeroes_capture(nb_non_imputed_no_zeroes_model, na.omit(df_non_imputed), stat="r2_mcfadden", force_init = FALSE, output_folder = output_folder)
# da_ng <- start_domir_nb_no_zeroes_capture(nb_non_imputed_no_zeroes_model, na.omit(df_non_imputed), stat="r2_nagelkerke", force_init = FALSE, output_folder = output_folder)

# # save dominance matrix
# save_dominance_matrix(paste0(output_folder, "/dominance_matrix_mc.png"), da_mc$Complete_Dominance)
# save_dominance_matrix(paste0(output_folder, "/dominance_matrix_ng.png"), da_ng$Complete_Dominance)


# # merge da_ng$General_Dominance, da_ng$Standardized, and da_ng$Ranks into a dataframe
# da_summ <- data.frame( da_ng$Ranks,  da_ng$General_Dominance,  da_ng$Standardized)

# # save the summary
# write.csv(da_summ[order(da_summ$da_ng.Ranks),], file = paste0(output_folder, "/dominance_summary.csv"), row.names = FALSE)

