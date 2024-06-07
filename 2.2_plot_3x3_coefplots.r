library(ggplot2)
library(patchwork)

create_coef_plots <- function(coef_folder) {
  # Open all RData files in the coef folder
  coef_plots <- list.files(paste0(coef_folder, "coef_raw_plots"), pattern = "*.RData", full.names = TRUE)

  # Create a list to store the plots
  plots <- list()

  # Loop over the list of coef plots
  for (i in 1:length(coef_plots)) {
    # Load the plot
    load(coef_plots[i])

    # Add the plot to the list
    plots[[i]] <- coef_plot +
      theme(text = element_text(size = 7, family = "sans")) +
      labs(x = NULL, y = NULL) +
      ggtitle("") + 
      theme(plot.margin = margin(0, 0, 0, 0))
  }

  # Combine the plots using patchwork
  combined_plot <- wrap_plots(plots, ncol = 3)

  # Set the same width and height for the combined plot
  combined_plot <- combined_plot +
    plot_layout(widths = rep(1/3, 3), heights = rep(1/3, 3)) +
    theme(plot.margin = margin(0, 0, 0, 0))


  # Save the plot
  ggsave(filename = paste0(coef_folder, "coef_plots.png"),
         plot = combined_plot, width = 18, height = 18,
         dpi = 300, units='cm')

}

# create a variable that contains the folder with the selected features for the current model
create_coef_plots("data/LASSOs/chatgpt_usage-flat-zeroes-binomial/")
create_coef_plots("data/LASSOs/chatgpt_weekly_usages-flat-no_zeroes-nb/")
create_coef_plots("data/LASSOs/chatgpt_sessions-flat-no_zeroes-nb/")

