library(ggplot2)
library(patchwork)


plot_cv_results <- function(cv_folder) {

  cv_raw_plots <- paste0(cv_folder, "cv_raw_plots/")
  # Open all RData files in the cv_raw_plots
  cv_plots <- list.files(cv_raw_plots, pattern = "*.RData", full.names = TRUE)

  # Create a list to store the plots
  plots <- list()

  # Loop over the list of roc plots
  for (i in 1:length(cv_plots)) {
    # Load the plot
    load(cv_plots[i])

    # Create the plot using ggplot2
    plots[[i]] <- ggplot(cv_results, aes(x = log(lambda), y = deviance)) +
      geom_point(size = 0.1, color = "red") +
      labs(y = NULL) +
      geom_errorbar(aes(ymin = deviance - cvsd, ymax = deviance + cvsd),
                    width = 0.1, alpha = 0.5, linewidth = 0.1) +
      theme(plot.margin = margin(0, 0, 0, 0),
            text = element_text(size = 7, family = "sans"))
  }

  # Combine the plots using patchwork
  combined_plot <- wrap_plots(plots, ncol = 3)

  # Set the same width and height for the combined plot
  combined_plot <- combined_plot +
    plot_layout(widths = rep(1/3, 3), heights = rep(1/3, 3))

  # Save the plot
  ggsave(filename = paste0(cv_folder, "/cv_plots.png"),
         plot = combined_plot, width = 18, height = 18,
         dpi = 300, units = 'cm')
}


# create a variable that contains the folder with the selected features for the current model
plot_cv_results("data/LASSOs/chatgpt_usage-flat-zeroes-binomial/")
plot_cv_results("data/LASSOs/chatgpt_weekly_usages-flat-no_zeroes-nb/")
plot_cv_results("data/LASSOs/chatgpt_sessions-flat-no_zeroes-nb/")
