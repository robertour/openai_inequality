source("5.0_evaluation_utilities.r")


# get the folder with the lasso outputs
counts_adoption_folder <- get_lasso_folder("nb", "chatgpt_weekly_usages")
counts_sessions_folder <- get_lasso_folder("nb", "chatgpt_sessions")

# variable to indicate if the run is for imputed or non imputed
imputed_option <- "non_imputed"

# create output folder based on the concatenation of the response variable and the output folder
outputs_folder <- "data/output"
output_folder <- paste0(outputs_folder, "/All")

# create the output folder if it does not exist
dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)

# create a variable that contains the folder with the selected features for the current model
binomial_folder <- "data/LASSOs/chatgpt_usage-flat-zeroes-binomial"

# create a variable that contains the folder with the selected features for the current model
usage_output_folder <- paste0(outputs_folder, "/Usage")
sessions_output_folder <- paste0(outputs_folder, "/Sessions")
adoption_output_folder <- paste0(outputs_folder, "/Adoption")


# load the selected features for the binomial
features_binomial <- get_features(paste0(binomial_folder, "/selected_features.csv"), base_features, imputed_option, exclude_features = c(""))

# load the selected features for the binomial. Options:
# 1. features_zeros_component <- features_binomial
# 2. features_zeros_component <- get_features(paste0(usage_output_folder, "/stepwise_aic_features.csv"), base_features, imputed_option, exclude_features = c(""))
# 3. features_zeros_component <- c("") # NOT IMPLEMENTED
features_zeros_component <- features_binomial 

# load the selected features for the counts
features_count_adoption <- get_features(paste0(counts_adoption_folder, "/selected_features.csv"), base_features, imputed_option, exclude_features = c(""))
features_count_sessions <- get_features(paste0(counts_sessions_folder, "/selected_features.csv"), base_features, imputed_option, exclude_features = c("web engagement"))

# merge all the count features
features_count_all <- unique(c(features_count_adoption, features_count_sessions))

# load the non imputed dataframe
df <- load_dataframe(imputed_option, features_binomial, features_count_all, response_renames = response_renames)


# fit the binomial model
binomial_model <- fit_binomial(df, features_binomial, "Usage")

#1. NON-IMPUTED ZERO-INFLATED MODELS###

# fit the zero-inflated models for adoption, sessions and duration
hurdle_nb_adoption_model <- fit_zinf_non_imputed_model(df, features_zeros_component, features_count_adoption, "Adoption", use_hurdle = TRUE, dist = "negbin")
hurdle_nb_sessions_model <- fit_zinf_non_imputed_model(df, features_zeros_component, features_count_sessions, "Sessions", use_hurdle = TRUE, dist = "negbin")  

# table of the models
save_tab_models(list(hurdle_nb_sessions_model, hurdle_nb_adoption_model),
    labels = c("Visits", "Adoption"),
    output_folder = output_folder, filename=paste0("tab-models_hurdle-nb_zero-inflation_",imputed_option), 
    show.aic = TRUE, show.aicc = TRUE, show.zeroinf = TRUE)

# plot the model coefficients
g0 <- format_ggplot_no_zeroes(binomial_model, "Usage")
g1 <- format_ggplot_zeroinfl(hurdle_nb_sessions_model, "Visits")
g2 <- format_ggplot_zeroinfl(hurdle_nb_adoption_model, "Adoption")

# the plot to a file
ggsave(filename = paste0(output_folder, "/plot_models_hurdle-nb_zero-inflation_", imputed_option, ".png"), plot = g0 + g1 + g2,  width = 14, height = 8, dpi = 300, units='cm')


#2. NON-IMPUTED NO-ZEROS MODELS###

# remove zeroes
df_no_zeroes <- df[df$Usage != 0, ]

# fit the models
nb_no_zeroes_adoption_model <- fit_no_zeroes_model(df_no_zeroes, features_count_adoption, "Adoption", dist = "negbin")
nb_no_zeroes_sessions_model <- fit_no_zeroes_model(df_no_zeroes, features_count_sessions, "Sessions", dist = "negbin")

# table of the models
save_tab_models(list(binomial_model, nb_no_zeroes_sessions_model, nb_no_zeroes_adoption_model),
    labels = c("Usage", "Visits", "Adoption"),
    output_folder = output_folder, filename=paste0("tab-models_nb_no-zeros_", imputed_option), 
    show.aic = TRUE, show.aicc = TRUE)

# plot the coefficients
g0 <- format_ggplot_no_zeroes(binomial_model, "Usage")
g1 <- format_ggplot_no_zeroes(nb_no_zeroes_sessions_model, "Visits")
g2 <- format_ggplot_no_zeroes(nb_no_zeroes_adoption_model, "Adoption")

# the plot to a file
ggsave(filename = paste0(output_folder, "/plot_models_nb_no-zeros_", imputed_option, ".png"), plot = g0 + g1 + g2,  width = 14, height = 8, dpi = 300, units='cm')

#3. Summary table with VIF

# open imputed vif tables from the output folders
vif_usage <- read.csv(paste0(usage_output_folder, "/vif_values.csv"))
vif_sessions <- read.csv(paste0(sessions_output_folder, "/vif_values_no-zeros_", gsub("_", "-", imputed_option) ,"_nb.csv"))
vif_adoption <- read.csv(paste0(adoption_output_folder, "/vif_values_no-zeros_", gsub("_", "-", imputed_option) ,"_nb.csv"))

# a summary table of the VIF values by merging the three dataframes
vif_summary <- merge(merge(vif_usage, vif_sessions, by = "X", all = TRUE), vif_adoption, by = "X", all = TRUE)

# rename the columns X to feature, x.x to Usage, x.y to Sessions, and x to Adoption
colnames(vif_summary) <- c("feature", "Usage", "Visits", "Adoption")

# sort the summary table by the Usage, Sessions, and Adoption columns
vif_summary <- vif_summary[order(vif_summary$Usage, vif_summary$Visits, vif_summary$Adoption, decreasing = TRUE), ]

# round values to 2 decimal places
vif_summary[, 2:4] <- round(vif_summary[, 2:4], 3)

# replace NAs with '-'
vif_summary[is.na(vif_summary)] <- "-"

# save the summary table to a file
write.csv(vif_summary, paste0(output_folder, "/vif_summary_",imputed_option,".csv"), row.names = FALSE)
