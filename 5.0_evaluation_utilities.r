library(pscl)
library(MASS)
library(webshot2) 
library(magick)
library(sjPlot)
library(performance)
library(domir)
library(car)
library(corrplot)
library(psych)
library(ggplot2)
library(patchwork)



# base features
base_features <- c(
  # pre-selected features
  "gender" , "age" , "education" , "income" , "residence rural", 
  # structural
  "income disclosure"
)

# create a list with the response renames
response_renames <- list(
  list(old = "chatgpt_usage", new = "Usage"),
  list(old = "chatgpt_weekly_usages", new = "Adoption"),
  list(old = "chatgpt_sessions", new = "Sessions")
)

# function to replace the labels
replace_labels <- function(labels) {
    labels <- gsub("web engagement", "weekly web duration", labels)
    labels <- gsub("web activity", "weekly web visits", labels)
    return(labels)
}


get_features <- function(features_file, base_features, imputed_option, exclude_features = c()) {
  # load a csv with the selected features for the binomial by concatenating the folder
  selected_features <- read.csv(features_file)

  # create a list with the column X for the selected features dataframe
  selected_features <- selected_features$X[selected_features$X != "(Intercept)"]

  # remove character ` from the selected features
  selected_features <- gsub("`", "", selected_features)

  # add base features to the selected features removing duplicates
  selected_features <- unique(c(base_features, selected_features))

  # if non imputed data, drop income disclosure
  if (imputed_option == "non_imputed") {

    # remove income disclusure from the binomial features
    selected_features <- selected_features[selected_features != "income disclosure"]
  }

  # remove the excluded features
  selected_features <- selected_features[!selected_features %in% exclude_features]

  # replace the feature names with the replace_labels function
  selected_features <- replace_labels(selected_features)

  return(selected_features)
}

get_lasso_folder <- function(lasso_distribution, response_var) {
  if (lasso_distribution == "negbin") {
    lasso_distribution <- "nb"
  }

  if (response_var == "chatgpt_duration") {
    transform <- "log"
  } else {
    transform <- "flat"
  }

  lasso_folder <- paste0("data/LASSOs/", response_var, "-", transform, "-no_zeroes-", lasso_distribution)

  return(lasso_folder)
}


load_dataframe <- function(imputed_option, selected_features_binomial, selected_features_counts, response_renames) {
  if (imputed_option == "imputed") {
    df <- read.csv("data/input/imputed.csv", check.names = FALSE)
  } else {
    df <- read.csv("data/input/non_imputed.csv", check.names = FALSE)
  }
  # replace the column names with the replace_labels function
  colnames(df) <- replace_labels(colnames(df))

  # iterate over the response_renames and rename the dependent variables
  for (response_rename in response_renames) {
    df[,response_rename$new] <- df[,response_rename$old]
  }

  # merge the binomial and count features without duplicates
  all_features <- unique(c(selected_features_binomial, selected_features_counts))

  # Center and scale the columns
  df[, all_features] <- scale(df[, all_features], center = TRUE, scale = TRUE)

  return(df)

}

concatenate_features <- function(features) {
  return(paste0("`", features, "`", collapse = " + "))
}

get_simple_formula <- function(selected_features, dependent_var) {
  # Generate the formula dynamically
  formula <- paste(dependent_var, "~", concatenate_features(selected_features))
  form <- as.formula(formula)

  return(form)
}



##########
# SAVING
##########

save_tab_models <- function(models, labels, output_folder, filename, ...) {
  # set the path for the html and png files
  html_file <- paste0(output_folder, "/", filename, ".html")
  png_file <- paste0(output_folder, "/", filename, ".png")

  # create an HTML table summary of the mixed model the print is necessary to save the file
  print(tab_model(models, dv.labels = labels, file = html_file, ...))

  # Save the table to a file
  webshot(html_file, png_file)

  # trim the png file
  image_write(image_trim(image_read(png_file)), png_file)
}


save_dominance_matrix <- function(filepath, da_matrix){

  # Set the dimensions of the plot
  png(filepath, width = 800, height = 800)


  #Plot the correlation matrix
  corrplot(da_matrix, 
   #method="number",
   addCoef.col = 'black', 
   is.corr = FALSE,
   col.lim = c(0, 1),
   col = COL2('PiYG')
  )


  # Close the device and save the file
  dev.off()

}
save_correlation_matrix <- function(df, variables, filepath) {
  # select the columns to use
  df <- df[variables]

  # Create a correlation matrix
  cor_matrix <- cor(df, method="pearson", use="pairwise.complete.obs")

  # Calculate p-values with Bonferroni correction
  p_values <- corr.test(df, adjust = "bonferroni", use="pairwise")$p

  # Set the dimensions of the plot
  png(filepath, width = 800, height = 800)


  #plot the correlation matrix
  corrplot(cor_matrix, method="pie",  
          p.mat = p_values, sig.level = 0.05, insig = "blank")


  # Close the device and save the file
  dev.off()

}

format_ggplot_zeroinfl <- function(model, title){

  g <- plot_model(model, type = "est",  dot.size=.75, wrap.labels=2, max.width = 2, sort.est=TRUE, show.zeroinf = FALSE) + 
    ggtitle(title) + 
    theme_bw() + 
    theme(plot.margin = margin(0, 0, 0, 0)) + 
    theme(text = element_text(size = 7, family = "sans"))  +
    # set the scale of the x axis between 0.1 and 10.0 but log scale
    scale_y_log10(limits = c(0.2, 5.0) ) +
    theme(axis.line = element_line(colour = "black"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.background = element_blank())

  return(g)
}


format_ggplot_no_zeroes <- function(model, title){

  g <- plot_model(model, type = "est",  dot.size=.75, wrap.labels=2, max.width = 2, sort.est=TRUE) + 
    ggtitle(title) + 
    theme_bw() + 
    theme(plot.margin = margin(0, 0, 0, 0)) + 
    theme(text = element_text(size = 7, family = "sans"))  +
    # set the scale of the x axis between 0.1 and 10.0 but log scale
    scale_y_log10(limits = c(0.2, 5.0) ) +
    theme(axis.line = element_line(colour = "black"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.background = element_blank())

  return(g)
}

#############
# FIT MODELS      
#############

fit_no_zeroes_model <- function(df_no_zeroes, selected_features_counts, dependent_var, dist, with_na = TRUE) {

  # NOTE: the with_na conditioning is necessary, otherwise the na.omit will not be included
  # in the tab_model
  if (with_na){
    # keep a global variable of the df to be able to use it outside the function
    # for, e.g. the stepwise function
    df_global_no_zeroes <<- df_no_zeroes
  } else {
    print("lse")
    # get all features
    all_features <- unique(c(selected_features_counts, dependent_var))

    # remove the NAs
    df_global_no_zeroes <<- na.omit(df_no_zeroes[,all_features])
  }

  # building the formula
  form <- get_simple_formula(selected_features_counts, dependent_var)

  if (dist=="negbin") {
    model <- glm.nb(form, data = df_global_no_zeroes)
  } else {
    model <- glm(form, data = df_global_no_zeroes, family=dist)
  }

  return(model)
}

fit_binomial <- function(df_binomial, selected_features_binomial, dependent_var, with_na = TRUE) {

  # keep a global variable of the df to be able to use it outside the function
  # for, e.g. the stepwise function
  df_global_binomial <<- df_binomial

  # building the formula
  form <- get_simple_formula(selected_features_binomial, dependent_var)

  if (with_na) {
    # fit the model
    model <- glm(form, data = df_global_binomial, family=binomial)
  } else {
    # fit the model
    model <- glm(form, data = na.omit(df_global_binomial), family=binomial)
  }

  return(model)
}


fit_zinf_imputed_model <- function(df, selected_features_binomial, selected_features_counts, dependent_var, use_hurdle, dist) {

  # keep a global variable of the df to be able to use it outside the function
  # for, e.g. the stepwise function
  df_global_imputed_zinf <<- df

  # building the formula
  binomial_regressors <- concatenate_features(selected_features_binomial)
  count_regressors <- concatenate_features(selected_features_counts)

  # Generate the formula dynamically
  formula <- paste(dependent_var, "~", count_regressors, "|", binomial_regressors)
  form <- as.formula(formula)

  # if use_hurdle is TRUE
  if (use_hurdle) {
    model <- hurdle(form, data = df_global_imputed_zinf, dist = dist)
  } else {
    model <- zeroinfl(form, data = df_global_imputed_zinf, dist = dist)
  }

  return(model)
}



fit_zinf_non_imputed_model <- function(df_ni, selected_features_binomial, selected_features_counts, dependent_var, use_hurdle, dist, with_na = TRUE) {
  
  # NOTE: the with_na conditioning is necessary, otherwise the na.omit will not be included
  # in the tab_model
  if (with_na){
    # keep a global variable of the df to be able to use it outside the function
    # for, e.g. the stepwise function
    df_global_non_imputed_zinf <<- df_ni
  } else {
    print("lse")
    # get all features
    all_features <- unique(c(selected_features_binomial, selected_features_counts, dependent_var))

    # remove the NAs
    df_global_non_imputed_zinf <<- na.omit(df_ni[,all_features])
  }

  # building the formula
  binomial_regressors <- concatenate_features(selected_features_binomial)
  count_regressors <- concatenate_features(selected_features_counts)

  # Generate the formula dynamically
  forumla <- paste(dependent_var, "~", count_regressors, "|", binomial_regressors)
  form <- as.formula(forumla)

  # if use_hurdle is TRUE
  if (use_hurdle) {
    model <- hurdle(form, data = df_global_non_imputed_zinf, dist = dist)
  } else {
    model <- zeroinfl(form, data = df_global_non_imputed_zinf, dist = dist)
  }

  return(model)

}


#####################
# DOMINANCE ANALYSIS
#####################



glm_binomial_capture <- 
  # wrapper program that accepts formula, data, and ellipsis arguments
  function(formula, data, ...) { 

    # increment counter in enclosing environment
    count <<- count + 1
    # if count is divisible by 100, print count
    if (count %% 100 == 0) {
      print(count)
    }

    # save data in global environment so that r2_zeroinflated can access it
    data_glob <<- data

    # if the statistic is not yet in the DA_stats_df (i.e., DA_stats_df[count, statistic]), create it
    if (is.na(DA_stats_df[count, statistic])) {

      # estimate 'hurdle' model and save object
      bin_obj <<- glm(formula, data = data_glob, family = "binomial", ...)

      # record string version of formula passed in 'DA_stats_df' in enclosing environment
      DA_stats_df[count, "formula"] <<- paste0(deparse(formula), collapse = "")

      # record the r2 mcfadden
      DA_stats_df[count, "r2_mcfadden"] <<- r2_mcfadden(bin_obj)$R2_adjusted

      # record the r2 nagelkerke
      DA_stats_df[count, "r2_nagelkerke"] <<- r2_nagelkerke(bin_obj)

      # record the r2_tjur
      DA_stats_df[count, "r2_tjur"] <<- r2_tjur(bin_obj)

    }


    #return(AIC(bin_obj)) # return AIC
    return(DA_stats_df[count, statistic])

}


start_domir_binomial_capture <- function(model, data, stat, force_init, output_folder) {

  # load DA_stats_df from output_folder if the file exists
  filename <- paste0(output_folder, "/DA_stats_df.Rdata")
  if (file.exists(filename)) {
    load(filename)
    # make it global
    DA_stats_df <<- DA_stats_df
  }

  # get the formula of the model
  formula <- formula(model)

  # number of models
  n_models <- 2^(length(coef(model))-1)-1

  # print the number of possible combinations
  print(paste("Number of possible combinations: ", n_models))

  # initialize the count indicating the row in which the results will fill-in
  count <<- 0 

  # set the statistic
  statistic <<- stat


  # if DA_stats_df does not exist, create it
  if (!exists("DA_stats_df") | force_init) {
    # container data frame in which to record results
    DA_stats_df <<- 
      data.frame(formula = rep("", times = n_models), 
                  r2_mcfadden = rep(NA, times = n_models), 
                  r2_nagelkerke = rep(NA, times = n_models),
                  r2_tjur = rep(NA, times = n_models),
                  check.names = FALSE)
  }

  # apply the domir function
  result <- domir(formula,
        glm_binomial_capture,
        data = data)
  
  # save the DA_stats_df
  save(DA_stats_df, file = filename)

  return (result)

}





nb_no_zeroes_capture <- 
  # wrapper program that accepts formula, data, and ellipsis arguments
  function(formula, data, ...) { 

    # increment counter in enclosing environment
    count <<- count + 1
    # if count is divisible by 100, print count
    if (count %% 100 == 0) {
      print(count)
    }

    # save data in global environment so that r2_zeroinflated can access it
    data_glob <<- data

    # if the statistic is not yet in the DA_stats_df (i.e., DA_stats_df[count, statistic]), create it
    if (is.na(DA_stats_df[count, statistic])) {

      # capture the error if the model does not converge
      tryCatch({

        # estimate 'hurdle' model and save object
        bin_obj <<- glm.nb(formula, data = data_glob, ...)

        # record string version of formula passed in 'DA_stats_df' in enclosing environment
        DA_stats_df[count, "formula"] <<- paste0(deparse(formula), collapse = "")

        # record the r2 mcfadden
        # catch the error if the model does not converge
        tryCatch({
          DA_stats_df[count, "r2_mcfadden"] <<- r2_mcfadden(bin_obj)$R2_adjusted
        }, error = function(e) {
        })

        # record the r2 nagelkerke
        DA_stats_df[count, "r2_nagelkerke"] <<- r2_nagelkerke(bin_obj)

      }, error = function(e) {
        # raise an error
        print(formula)
        global_formula<<-formula
        
      })

    }


    #return(AIC(bin_obj)) # return AIC
    return(DA_stats_df[count, statistic])

}


start_domir_nb_no_zeroes_capture <- function(model, data, stat, force_init, output_folder) {

  # load DA_stats_df from output_folder if the file exists
  filename <- paste0(output_folder, "/DA_stats_df.Rdata")
  if (file.exists(filename)) {
    load(filename)
    # make it global
    DA_stats_df <<- DA_stats_df
  }

  # get the formula of the model
  formula <- formula(model)

  # number of models
  n_models <- 2^(length(coef(model))-1)-1

  # print the number of possible combinations
  print(paste("Number of possible combinations: ", n_models))

  # initialize the count indicating the row in which the results will fill-in
  count <<- 0 

  # set the statistic
  statistic <<- stat

  # if DA_stats_df does not exist, create it
  if (!exists("DA_stats_df") | force_init) {
    # container data frame in which to record results
    DA_stats_df <<- 
      data.frame(formula = rep("", times = n_models), 
                  r2_mcfadden = rep(NA, times = n_models), 
                  r2_nagelkerke = rep(NA, times = n_models),
                  r2_tjur = rep(NA, times = n_models),
                  check.names = FALSE)
  }

  # apply the domir function
  result <- domir(formula,
        nb_no_zeroes_capture,
        data = data)
  
  # save the DA_stats_df
  save(DA_stats_df, file = filename)

  return (result)

}