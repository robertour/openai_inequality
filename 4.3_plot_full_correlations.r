
library(corrplot)
library(psych)
library(ggplot2)

#1 LOAD DATA####

# Use this to compare with the imputed data
# df <- read.csv("data/input/imputed.csv", check.names = FALSE)
# df <- df_full


df <- read.csv("data/input/non_imputed.csv", check.names = FALSE)
output_directory <- "data/output"
str(df)


#2. SETUP THE DATAFRAME ####

# independent variables path
independent_variables_path <- "data/input/independent_variables.txt"

# open the independent variables and set them in a list
independent_variables <- read.table(independent_variables_path, header = FALSE)$V1

# raise an exception if some of the independent variables are not in the dataframe
stopifnot(all(independent_variables %in% colnames(df)))

# list the the dependant variables
dependant <- c("chatgpt_usage", "chatgpt_weekly_usages", "chatgpt_sessions")

# add dependant to the list of independent variables
variables <- c(independent_variables, dependant)

# select the columns to use
df <- df[variables]
str(df)


#4. Calculate correlations and p-values

# function to replace the labels (backwards compatibility with previous dataset)
replace_labels <- function(labels){
    labels <- gsub("web engagement", "weekly web duration", labels)
    labels <- gsub("web activity", "weekly web visits", labels)
    return(labels)
}


# Create a correlation matrix
cor_matrix <- cor(df, method="pearson", use="pairwise.complete.obs")

# change the label web engagement to weekly web duration in the correlation matrix using the function replace_labels
rownames(cor_matrix) <- replace_labels(rownames(cor_matrix))
colnames(cor_matrix) <- replace_labels(colnames(cor_matrix))


# Calculate p-values with Bonferroni correction
p_values <- corr.test(df, adjust = "bonferroni", use="pairwise")$p

# change the label web engagement to weekly web duration in the p-values using the function replace_labels
rownames(p_values) <- replace_labels(rownames(p_values))
colnames(p_values) <- replace_labels(colnames(p_values))

# a variable with the full path to the file
file_path <- file.path(output_directory, "full_corrplot.png")
# Set the dimensions of the plot
png(file_path, width = 1000, height = 1000)


#5. Plot the correlation matrix
corrplot(cor_matrix, method="pie",  
         p.mat = p_values, sig.level = 0.05, insig = "blank")
# Close the device and save the file
dev.off()

# correlation test for the weekly web visits and the weekly web duration`
cor.test(df$`weekly web visits`, df$`weekly web duration`, method = "pearson", use = "pairwise.complete.obs")
