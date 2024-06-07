options(width=300)
library(tidyverse)
library(ggplot2)
library(tidyr)
library(forcats)
library(patchwork)

#1. LOAD DATA####

# Specify the directory path
input_directory <- "data/LASSOs"
output_directory <- "data/output"

# Get a list of subdirectories
subdirectories <- list.dirs(input_directory, recursive = FALSE)

# Initialize an empty data frame to store the concatenated data frames
df_combined <- data.frame()


# Iterate over each subdirectory
for (subdir in subdirectories) {
  # Get the file path of the summary.csv file
  file_path <- file.path(subdir, "all_coefficients.csv")

  # Check if the file exists in the subdirectory
  if (file.exists(file_path)) {
    # Read the data from the file
    file_data <- read.csv(file_path)

    # Set the "name" column as the index
    rownames(file_data) <- file_data$X

    # Remove the "X" column
    file_data$X <- NULL

    # calculate the averages and the number of coefficients that are greater than 0
    counts <- rowSums(ifelse(file_data < 0, -1, ifelse(file_data > 0, 1, 0)))
    meanrow <- rowMeans(file_data)

    # create dataframe with the statistics
    file_data <- data.frame(counts, meanrow)

    # add a column with the rownames
    file_data <- file_data %>% rownames_to_column(var = "X")

    # Add a new column to the data frame with the subdirectory name
    file_data$fit <- basename(subdir)

    # # Append the data frame to df_combined
    df_combined <- rbind(df_combined, file_data)

  } else {
    # this correspond to lass iteratons that failed, visits != sessions
    print(paste("File 'summary.csv' not found in", subdir))
  }

}

#2. PREPARE THEDATA####

# Extract string after the first underscore in the column
df_combined$fit <- sub("^[^_]*_(.*)$", "\\1", df_combined$fit)

# factorize the column fit
df_combined$fit <- fct_inorder(df_combined$fit, ordered=T)

# remove the intercept in the X column from the dataframe
df_combined <- subset(df_combined, X != "(Intercept)")

# select the counts
dfcounts <- df_combined[,c("X", "counts", "fit")]

# select the counts
dfmeans <- df_combined[,c("X", "meanrow", "fit")]

# transform to long format
df_wider_counts <- dfcounts %>% pivot_wider(names_from = fit, values_from = counts)

# transform to long format
df_wider_means <- dfmeans %>% pivot_wider(names_from = fit, values_from = meanrow)


#3. PLOT NON SUM MODELS ####


# Convert dfcounts$fit to a character vector
dfcounts$fit <- as.character(dfcounts$fit)


# Define the mapping from IDs to labels
id_to_label <- c(
  "usage-flat-zeroes-binomial" = "Usage",

  "sessions-flat-no_zeroes-nb" = "Visits",
  "sessions-flat-1.5-no_zeroes-nb" = "IGNORE", #"Visits - NB(1.5)",
  "sessions-flat-no_zeroes-poisson" = "IGNORE", #"Visits (Flat) - Poisson",
  "sessions-log-no_zeroes-gaussian" = "IGNORE", #"Visits (Log) - Gaussian",

  "weekly_usages-flat-no_zeroes-nb" = "Adoption",
  "weekly_usages-flat-1.5-no_zeroes-nb" = "IGNORE", #"Adoption - NB(1.5)",
  "weekly_usages-flat-no_zeroes-poisson" = "IGNORE", #"Adoption (Flat) - Poisson",
  "weekly_usages-log-no_zeroes-gaussian" = "IGNORE", #"Adoption (Log) - Gaussian",

  "median_duration-flat-no_zeroes-nb" = "IGNORE", #"Median Duration",
  
  "duration-log-no_zeroes-nb" = "IGNORE", #"Duration (log)",
  "duration-flat-1.5-no_zeroes-nb" = "IGNORE", #"Duration - NB(1.5)",
  "duration-flat-no_zeroes-poisson" = "IGNORE", #"Duration (Flat) - Poisson",
  "duration-log-no_zeroes-gaussian" = "IGNORE", #"Duration (Log) - Gaussian",

  "duration-flat-0.5-no_zeroes-nb" = "IGNORE", #"Duration - NB(0.5)",
  "duration-flat-5-no_zeroes-nb" = "IGNORE", #"Duration - NB(5)",
  "duration-flat-no_zeroes-gaussian" = "IGNORE", #"Duration (Flat) - Gaussian",
  "duration-log-no_zeroes-poisson" = "IGNORE", #"Duration (Log) - Poisson",
  "usage-flat-no_web_activity-binomial" = "IGNORE", #"Usage (Flat) - No Web Activity - Binomial",
  "sessions-flat-0.5-no_zeroes-nb" = "IGNORE", #"Visits - NB(0.5)",
  "sessions-flat-5-no_zeroes-nb" = "IGNORE", #"Visits - NB(5)",
  "sessions-flat-no_zeroes-gaussian" = "IGNORE", #"Visits (Flat) - Gaussian",
  "sessions-log-no_zeroes-poisson" = "IGNORE", #"Visits (Log) - Poisson",
  "weekly_usages-flat-0.5-no_zeroes-nb" = "IGNORE", #"Adoption - NB(0.5)",
  "weekly_usages-flat-5-no_zeroes-nb" = "IGNORE", #"Adoption - NB(5)",
  "weekly_usages-flat-no_zeroes-gaussian" = "IGNORE", #"Adoption (Flat) - Gaussian",
  "weekly_usages-log-no_zeroes-poisson" = "IGNORE" #"Adoption (Log) - Poisson"
)

# Change labels that exist in id_to_label, keep others the same
dfcounts$fit <- ifelse(dfcounts$fit %in% names(id_to_label), id_to_label[dfcounts$fit], dfcounts$fit)

# IGNORE the rows with "IGNORE" in the fit column
dfcounts <- dfcounts %>% filter(fit != "IGNORE")

# convert factor to string
dfcounts$fit <- as.character(dfcounts$fit)

# IGNORE items that are mapped to "IGNORE"
new_labels <- unname(id_to_label[id_to_label != "IGNORE"])

# order the factor
distribution_order <- sorted_labels <- new_labels


# filter the dfcounts to include only the rows that are in Usage, Visits, Visits (Poisson), Visits (N.B.), and Duration
dfmains <- dfcounts %>% filter(fit %in% distribution_order)

# order the factor
dfmains$fit <- factor(dfmains$fit, levels = distribution_order)

# drop duplicates values in column X
label_order <- rev(dfmains[!duplicated(dfmains$X), 'X'])

# plot the heatmap
g1 <- ggplot(dfmains, aes(x = fit, y = `X`, fill = counts)) +
  ggtitle("(A) All Features") +
  geom_tile() +
  labs(x = "", y = "", fill = "") +
  scale_fill_gradient2(low = "#8B0000", mid = "white", high = "#00008B", midpoint = 0)+
  scale_y_discrete(
    limits = label_order,
    labels = function(x) {
      x <- ifelse(x == "web engagement", "weekly web duration", x)
      x <- ifelse(x == "web activity", "weekly web visits", x)

      return(x)
    }) + 
  theme(text = element_text(size = 7, family = "sans"), plot.margin = margin(0, 0, 0, 0),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# create a new column with the counts of the selected features
dfmains$selected_counts <- dfmains$counts
is_binomial <- dfmains$fit == "Usage"
is_else <- !is_binomial


# select the counts above 500 and below -500 for "Usage"
dfmains$selected_counts[is_binomial & dfmains$selected_counts < 500 & dfmains$selected_counts > -500] <- 0

# select the counts above 500 and below -500
dfmains$selected_counts[is_else & dfmains$selected_counts < 500 & dfmains$selected_counts > -500] <- 0




# heatmap with the selected features
g2 <- ggplot(dfmains, aes(x = fit, y = `X`, fill = selected_counts)) +
  ggtitle("(B) Only Selected Features") +
  geom_tile() +
  labs(x = "", y = "", fill = "") +
  scale_fill_gradient2(low = "#8B0000", mid = "white", high = "#00008B", midpoint = 0) +
  scale_y_discrete(
    limits = label_order,
    labels = function(x) {
      x <- ifelse(x == "web engagement", "weekly web duration", x)
      x <- ifelse(x == "web activity", "weekly web visits", x)

      return(x)
    }) + 
  theme(text = element_text(size = 7, family = "sans"), plot.margin = margin(0, 0, 0, 0),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


g <- g1 + g2  + plot_layout(guides = "collect") +
    theme(plot.margin = margin(0, 0, 0, 0))

# save the plot
ggsave(filename = paste0(output_directory, "/heatmap_selected_features_binomial_nb.png"), 
        plot = g, width = 14, height = 16,
        dpi = 300, units='cm')
