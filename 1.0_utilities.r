
library(glmnet)
library(doParallel)
library(pROC)
library(coefplot)
library(MASS)
library(mpath)

numCores <- detectCores()
registerDoParallel(cores = numCores)


perform_model_training <- function(
    df_train, df_test, family, folder, filename, 
    response, nfold = 10, plot=FALSE, theta=NULL) {

  # set the family
  family_str <- family

  # create the model matrix and response vector
  trainY <- as.matrix(df_train[response])
  testY <- as.matrix(df_test[response])

  # check if family is string and the string contains "binomial"
  if (is.character(family_str) && (family_str == "binomial")) {
    trainY <- as.factor(trainY)
    testY <- as.factor(testY)
  } else {
    trainY <- as.numeric(trainY)
    testY <- as.numeric(testY)
  }

  trainX <- as.matrix(df_train[setdiff(colnames(df_train), response)])
  testX <- as.matrix(df_test[setdiff(colnames(df_test), response)])

  # create a vector of fold IDs
  foldid <- sample(rep(seq(nfold), length.out = nrow(trainX)))

  # Create subfolders within the main folder
  cv_folder <- file.path(folder, "cv_plots")
  cv_raw_folder <- file.path(folder, "cv_raw_plots")
  coef_folder <- file.path(folder, "coef_plots")
  coef_raw_folder <- file.path(folder, "coef_raw_plots")
  roc_folder <- file.path(folder, "roc_plots")
  roc_raw_folder <- file.path(folder, "roc_raw_plots")

  # Create the subfolders if they don't exist
  dir.create(cv_folder, recursive = TRUE, showWarnings = FALSE)
  dir.create(cv_raw_folder, recursive = TRUE, showWarnings = FALSE)
  dir.create(coef_folder, recursive = TRUE, showWarnings = FALSE)
  dir.create(coef_raw_folder, recursive = TRUE, showWarnings = FALSE)
  dir.create(roc_folder, recursive = TRUE, showWarnings = FALSE)
  dir.create(roc_raw_folder, recursive = TRUE, showWarnings = FALSE)

  # check if family is string and the string contains "nb" and theta is null
  is_nb_wo_theta <- is.character(family) && (family == "nb") && is.null(theta)

  # check if family is string and the string contains "nb" and theta is null
  if (is_nb_wo_theta) {
  

    # Generate the formula dynamically
    formula <- paste(response, "~", paste(paste0("`", setdiff(colnames(df_train), response), "`"), collapse = " + "))

    formula <- as.formula(formula)
    cv <- cv.glmregNB(formula, data = df_train, nfold = nfold, foldid = foldid, 
                   alpha = 1, plot.it=FALSE, parallel = FALSE)

    # globalcvnb <<- cv

    # Find the best lambda and theta
    bestlam <- cv$lambda.optim
    theta <- cv$fit$theta[cv$lambda.which]

    # set the family
    family <- negative.binomial(theta = theta)

  } else {

    # if family is nb and theta is not null, create the distribution
    if (is.character(family_str) && (family_str == "nb") && !is.null(theta)) {
      family <- negative.binomial(theta = theta)
    }


    cv <- cv.glmnet(trainX, trainY, family = family, nfold = nfold, foldid = foldid, 
                    type.measure = "default", parallel = TRUE, alpha = 1)


    # globalcvg <<- cv

    # Find the best lambda
    bestlam <- cv$lambda.min
  }

  if (plot){

    # skip the cv plot if the family is negative binomial
    if (is_nb_wo_theta) {
      
      # Extract the cross-validated results
      cv_results <- data.frame(lambda = cv$lambda,
                              deviance = cv$cv,
                              cvsd = cv$cv.error,
                              nzero = cv$fit$df)
    } else {
  
      # Extract the cross-validated results
      cv_results <- data.frame(lambda = cv$lambda,
                              deviance = cv$cvm,
                              cvsd = cv$cvsd,
                              nzero = cv$glmnet.fit$df)
    }

    # Save the cv_results dtaframe object as an RData file
    save(cv_results, file = file.path(cv_raw_folder, paste0(filename, ".RData")))

    # Save cross-validation plot
    png(file.path(cv_folder, paste0(filename, ".png")), width = 480, height = 480)
    plot(cv)
    dev.off()
  }

  # Train the final model using the best lambda
  model <- glmnet(trainX, trainY, family = family, lambda = bestlam, alpha = 1)

  # add the theta to the model
  if (is_nb_wo_theta) {
    model$theta <- theta
  }

  globalmodl <<- model

  if (plot) {
    if (any(coef(model)[-1] != 0)) {
    
      # Save coefficient plot
      coef_plot <- coefplot(model, lambda = bestlam, sort = "magnitude", intercept=FALSE, pointSize=1,
        legend=FALSE, title="")

      save(coef_plot, file = file.path(coef_raw_folder, paste0(filename, ".RData")))

      # Save the coefplot as a PNG file
      ggsave(file.path(coef_folder, paste0(filename, ".png")), coef_plot)

    } else {

      # Create an empty ggplot object
      empty_plot <- ggplot() + theme_void()

      # Save the empty plot
      ggsave(file.path(coef_folder, paste0(filename, ".png")), empty_plot)

    }
  }

  # if the family_str is binomial
  if (is.character(family_str) && (family_str == "binomial")) {
    roc <- roc(testY, as.numeric(predict(model, testX, type = "response")))

    if (plot) {
      save(roc, file = file.path(roc_raw_folder, paste0(filename, ".RData")))

      # Save ROC curve plot
      png(file.path(roc_folder, paste0(filename, ".png")), width = 480, height = 480)
      rocplot <- plot(roc, type = "l")
      dev.off()


    }

    # add the roc to the model
    model$roc <- roc
  } 

  # return the model coefficients
  return(model)

}

perform_iteration <- function(i, df, sample_partition, dist_folder, dependant, family, nfold, n_plots_to_save, theta=NULL) {
  # count the numbers of rows in df
  n <- nrow(df)

  # split the data in train and test 
  sample <- sample(seq(n), size = n * sample_partition, replace = FALSE)
  
  # create the train and test dataframes
  df_train <- df[sample, ]
  df_test <- df[-sample, ]

  # train the model
  model = perform_model_training(df_train, df_test, family=family, 
      folder=dist_folder, filename=i, response = c(dependant), nfold = nfold, plot= i < n_plots_to_save, theta)

  coef_matrix <- as.matrix(coef(model))

  # Change the name of the first column
  colnames(coef_matrix)[1] <- paste0("i", i)

  # Create a dataframe with the coefficients
  df_coefs <- data.frame(coef_matrix)
  
  return(list(df_coefs, model))
}

filter_df <- function(df, independent_variables_path, include_zeroes, dependant) {
  # open the independent variables and set them in a list
  independent_variables <- read.table(independent_variables_path, header = FALSE)$V1

  # raise an exception if some of the independent variables are not in the dataframe
  stopifnot(all(independent_variables %in% colnames(df)))

  # add dependant to the list of independent variables
  variables <- c(independent_variables, dependant)

  # select the columns to use
  df <- df[variables]

  # if include zeroes is set to no_zeroes, remove the rows with zeroes
  if (include_zeroes == "no_zeroes") {
    df <- df[df[, dependant] != 0, ]
  }
  
  return(df)
}

perform_iterations <- function(df, seed, n_iterations, threshold, sample_partition, dist_folder, dependant, family, nfold, n_plots_to_sav, theta) {
  # set the seed for reproducibility
  set.seed(seed)

  # count the numbers of rows in df
  n <- nrow(df)

  #print the number of rows
  print(paste("Number of rows:", n))

  df_coefs <- NULL
  rocs <- c()
  tethas <- c()
  
  for (i in 1:n_iterations) {
    print(i)
    results <- perform_iteration(i, df, sample_partition, dist_folder, dependant, family, nfold, n_plots_to_save, theta)

    if (i == 1) {
        df_coefs <- results[[1]]
    } else {
        df_coefs <- cbind(df_coefs, results[[1]])
    }

    # get the model
    model <- results[[2]]
    
    # if the model conatins the roc, add it to the list
    roc <- model$roc
    if (!is.null(roc)) {
        rocs <- c(rocs, roc$auc)
    }

    # if the model conatins the theta, add it to the list
    estimated_theta <- model$theta
    if (!is.null(estimated_theta)) {
        tethas <- c(tethas, estimated_theta)
    }

  }

  # if the rocs are not empty, print the mean and store the rocs in a file
  if (length(rocs) > 0) {
    print(paste("Mean ROC:", mean(rocs)))
    write.csv(as.data.frame(rocs), file.path(dist_folder, "rocs.csv"), row.names = FALSE)
  }
 
  # if the tethas are not empty, print the mean and store the tethas in a file
  if (length(tethas) > 0) {
    print(paste("Mean Theta:", mean(tethas)))
    write.csv(as.data.frame(tethas), file.path(dist_folder, "tethas.csv"), row.names = FALSE)
  }

  # clculate the selected features and save files
  results <- calculate_and_save_results(df_coefs, threshold, dist_folder)
  selected_features <- results[[1]]
  df_coefs_summ <- results[[2]]

  return(list(df_coefs_summ, selected_features))

}


calculate_and_save_results <- function(df_coefs, threshold, dist_folder) {
  # calculate the averages and the number of coefficients that are greater than 0
  gt0 <- rowSums(ifelse(df_coefs < 0, -1, ifelse(df_coefs > 0, 1, 0)))
  meanrow <- rowMeans(df_coefs)

  # create dataframe with the statistics
  df_coefs_summ <- data.frame(gt0, meanrow)

  # plot the histogram
  png(file.path(dist_folder, "histogram.png"), width = 480, height = 480)
  hist(df_coefs_summ$gt0, breaks = 20)
  dev.off()

  # select the features
  selected_features <- subset(df_coefs_summ, gt0 > threshold | gt0 < -1 * threshold)

  # save the results in a csv file
  write.csv(selected_features, file.path(dist_folder, "selected_features.csv"))

  # save all the coefficients in a csv file
  write.csv(df_coefs, file.path(dist_folder, "all_coefficients.csv"))

  # save the summary in a csv file
  write.csv(df_coefs_summ, file.path(dist_folder, "summary.csv"))
  
  return(list(selected_features, df_coefs_summ))
}