
library(glmnet)
library(doParallel)
library(jsonlite)

source("1.0_utilities.r")


#0. LOAD THE DATA####

# with imputed data
df <- read.csv("data/input/imputed.csv", check.names = FALSE)


#1. SET PARAMETERS####

dependant <- "chatgpt_sessions"
transformation <- "flat"
family <- "nb"
theta <- NULL
include_zeroes <- "no_zeroes"
nfold <- 5
n_iterations <- 1000
sample_partition <- 0.8
seed <- 2023
n_plots_to_save <- 10

# convert threshold to integer
threshold <- as.integer(n_iterations / 2)

if (family == "nb") {
  if (is.null(theta)) {
    # Create the full path to the folder
    dist_folder <- file.path("data/LASSOs", paste0(dependant, "-", transformation, "-", include_zeroes, "-", family))
  } else {
    # Create the full path to the folder
    dist_folder <- file.path("data/LASSOs", paste0(dependant, "-", transformation, "-", theta, "-", include_zeroes, "-", family))
  }
} else {
  # Create the full path to the folder
  dist_folder <- file.path("data/LASSOs", paste0(dependant, "-", transformation, "-", include_zeroes, "-", family))
}

# create the folder if it does not exist
dir.create(dist_folder, showWarnings = FALSE)

# save the parameters in a text file as json using the jsonlite package
writeLines(toJSON(list(
  dependant = dependant, transformation = transformation, family = family, theta = theta, include_zeroes = include_zeroes, nfold = nfold, n_iterations = n_iterations, 
  sample_partition = sample_partition, seed = seed, n_plots_to_save = n_plots_to_save)), file.path(dist_folder, "parameters.json"))


#2 SUBSET THE DATAFRAME####

df <- filter_df(df, "data/raw/independent_variables.txt", include_zeroes, dependant)

# Print the ratio
cat("Variance-to-Mean Ratio:",  var(df[,dependant]) / mean(df[,dependant]), "\n")

if (transformation == "log") {
  # log transform the dependant variable
  df[,dependant] <- log(df[,dependant] )
  
  # Print the ratio
  cat("Variance-to-Mean Ratio (after logged):",  var(df[,dependant]) / mean(df[,dependant]), "\n")

}

#3.LASSO ITERATIONS ####

# perform the iterations
results <- perform_iterations(df, seed, n_iterations, threshold, sample_partition, dist_folder, dependant, family, nfold, n_plots_to_save, theta)
df_coefs_summ<- results[[1]]
selected_features<- results[[2]]

print(selected_features)
