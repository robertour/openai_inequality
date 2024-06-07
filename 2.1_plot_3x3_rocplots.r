
library(ggplot2)
library(patchwork)
library(pROC)

plot_roc_curves <- function(folder) {
  # Create the roc folder path
  roc_folder <- paste0(folder, "/roc_raw_plots/")

  # Open all RData files in the roc folder
  roc_plots <- list.files(roc_folder, pattern = "*.RData", full.names = TRUE)

  # Create a list to store the plots
  plots <- list()

  # Loop over the list of roc plots
  for (i in 1:length(roc_plots)) {
    # Load the plot
    load(roc_plots[i])

    # Add the plot to the list
    plots[[i]] <- ggroc(roc, legacy.axes = TRUE) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
      theme(text = element_text(size = 7, family = "sans"))
  }

  # Combine the plots using patchwork
  combined_plot <- wrap_plots(plots, ncol = 3)

  # Set the same width and height for the combined plot
  combined_plot <- combined_plot +
    plot_layout(widths = rep(1/3, 3), heights = rep(1/3, 3))

  # Save the plot
  ggsave(filename = paste0(folder, "/roc_plots.png"),
         plot = combined_plot, width = 18, height = 18,
         dpi = 300, units = 'cm')
}

plot_roc_curves("data/LASSOs/chatgpt_usage-flat-zeroes-binomial")
plot_roc_curves("data/LASSOs/chatgpt_usage-flat-no_web_activity-binomial")


# open all rocs values
df <- read.csv("data/LASSOs/chatgpt_usage-flat-zeroes-binomial/rocs.csv", header = TRUE)
df_no_web <- read.csv("data/LASSOs/chatgpt_usage-flat-no_web_activity-binomial/rocs.csv", header = TRUE)

# print the mean AUC value
cat("Mean AUC value: ", mean(df$rocs), "\n")
cat("Mean AUC value (no web activity): ", mean(df_no_web$rocs), "\n")

# print the standard deviation of the AUC values
cat("Standard deviation of the AUC values: ", sd(df$rocs), "\n")
cat("Standard deviation of the AUC values (no web activity): ", sd(df_no_web$rocs), "\n")