#0 Load libraries####

options(scipen=999)
library(dplyr)
library(ggplot2)

#1 LOAD DATA####

# load the data from the csv file
df <- read.csv("data/input/timeseries.csv", check.names = FALSE)

# create directory if it does not exist
dir.create("data/output", showWarnings = FALSE)


#2 PLOT THE NUMBER OF SESSIONS PER PARTICIPANT####

#calculate percentage of views_openai_sum in pageviews_sum
df$sessions_perc <- df$chatgpt_sessions / (df$sessions + df$chatgpt_sessions)
df$visits_perc <- df$chatgpt_visits / (df$visits + df$chatgpt_sessions)


##df that has the mean of views_openai and pageviews and ai_perc grouped on week
dfm <- df %>% 
  group_by(week) %>% 
  summarise(
    mean_sessions_perc = mean(sessions_perc),
    mean_visits_perc = mean(visits_perc)
    )


# Add a new column to dfm that combines "W" with the week number
dfm$week_label <- paste0("W", dfm$week)

g <- ggplot(dfm, aes(x = week, y = mean_sessions_perc)) +
  geom_line() +
  geom_point(size = 1) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Week (Period from Dec. 1st, 2022 to Oct. 31st., 2023)", y = "Percentage of visits corresponding to ChatGPT", title = "") +
  scale_y_continuous(n.breaks = 7) +
  scale_x_continuous(breaks = unique(dfm$week), labels = paste0("W", unique(dfm$week))) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5)) +
  theme(text = element_text(size = 7, family = "sans"))  +
  theme(axis.line = element_line(colour = "black"),
  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_blank(),
  # panel.border = element_blank(),
  panel.background = element_blank())
# Save the plot
ggsave(filename = "data/output/timeseries_visits.png",
       plot = g, width = 14, height = 10,
       dpi = 300, units = 'cm')

# Reshape the data from wide to long format
dfm_long <- tidyr::pivot_longer(dfm, cols = starts_with("mean"), names_to = "Mean", values_to = "Value")

# replace values of column Line
dfm_long$Mean[dfm_long$Mean == "mean_sessions_perc"] <- "Domain visits"
dfm_long$Mean[dfm_long$Mean == "mean_visits_perc"]   <- "URL visits"


g <- ggplot(dfm_long, aes(x = week, y = Value, color = Mean, linetype = Mean)) +
  geom_line() +
  geom_point(size=1) +
  labs(x = "Week (Period from Dec. 1st, 2022 to Oct. 31st., 2023)", y = "Percentage of visits corresponding to ChatGPT", title = "", color="", linetype="") +
  scale_color_manual(values = c("blue", "darkgray")) +
  scale_x_continuous(breaks = unique(dfm$week), labels = paste0("W", unique(dfm$week))) + 
  scale_y_continuous(n.breaks = 7) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5),
    text = element_text(size = 7, family = "sans"),
    axis.line = element_line(colour = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.background = element_blank(),
    legend.position = c(1, 1),
    legend.justification = c(1.1, 1.1),
    legend.margin = margin(-10, 0, 0, 0)
  )

# Save the plot
ggsave(filename = "data/output/timeseries_sessions_visits.png",
       plot = g, width = 14, height = 10,
       dpi = 300, units = 'cm')
