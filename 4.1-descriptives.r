#0 Load libraries####

options(scipen=999)
library(dplyr)
library(ggplot2)
library(patchwork)

#1 LOAD DATA####

df <- read.csv("data/input/non_imputed.csv", check.names = FALSE)
str(df)

#2 SAMPLE SIZE, CHATGPT AND WEB-TRACKING STATS####

# print a message with the number of participants 
cat("The number of participants is: ", nrow(df), "\n")

# print a meassage with the number of participants that have used ChatGPT
cat("The number of participants that have used ChatGPT is: ", sum(df$chatgpt_usage > 0), "\n")

# print the percentage of participants that have used ChatGPT
cat("The percentage of participants that have used ChatGPT is: ", sum(df$chatgpt_usage > 0) / nrow(df) * 100, "%\n")

# print the number of participants that have not used ChatGPT
cat("The number of participants that have not used ChatGPT is: ", sum(df$chatgpt_usage == 0), "\n")


# print the median number of visits website
cat("The median number of visits is: ", median(df$sessions), "\n")

# print the mean number of visits website
cat("The mean number of visits is: ", mean(df$sessions), "\n")

# print the standard deviation of the number of visits website
cat("The standard deviation of the number of visits is: ", sd(df$sessions), "\n")

# most active participant
cat("The most active participant visited the website ", max(df$sessions), " times\n")

# least active participant
cat("The least active participant visited the website ", min(df$sessions), " times\n")

# filter the data to exclude the participants that have not used ChatGPT
df_used <- df %>% filter(df$chatgpt_usage > 0)

# print the median number of visits to the ChatGPT website
cat("The median number of visits to the ChatGPT website is (for those who visted it): ", median(df_used$chatgpt_sessions), "\n")

# print the mean number of visits to the ChatGPT website
cat("The mean number of visits to the ChatGPT website is (for those who visted it): ", mean(df_used$chatgpt_sessions), "\n")

# print the standard deviation of the number of visits to the ChatGPT website
cat("The standard deviation of the number of visits to the ChatGPT website is (for those who visted it): ", sd(df_used$chatgpt_sessions), "\n")

# print the visits to the ChatGPT website of the most active participant
cat("The most active participant visited the ChatGPT website ", max(df_used$chatgpt_sessions), " times\n")

# print the visits to the ChatGPT website of the least active participant
cat("The least active participant visited the ChatGPT website ", min(df_used$chatgpt_sessions), " times\n")

# print the median number of weeks with visits to the ChatGPT website
cat("The median number of weeks with visits to the ChatGPT website is (for those who visted it): ", median(df_used$chatgpt_weekly_usages), "\n")

# print the mean number of weeks with visits to the ChatGPT website
cat("The mean number of weeks with visits to the ChatGPT website is (for those who visted it): ", mean(df_used$chatgpt_weekly_usages), "\n")

# print the standard deviation of the number of weeks with visits to the ChatGPT website
cat("The standard deviation of the number of weeks with visits to the ChatGPT website is (for those who visted it): ", sd(df_used$chatgpt_weekly_usages), "\n")

# print the number of weeks with visits to the ChatGPT website of the most active participant
cat("The most active participant visited the ChatGPT website for ", max(df_used$chatgpt_weekly_usages), " weeks\n")

# print the number of weeks with visits to the ChatGPT website of the least active participant
cat("The least active participant visited the ChatGPT website for ", min(df_used$chatgpt_weekly_usages), " weeks\n")



str(df_used)


#3 DEMOGRAPHICS####

# Women and men

# print the number of women in the sample using the column gender
cat("Number of participants self-identified as women", sum(df$gender == 0), "\n")

# print the percentage of  women and men in the sample
cat("Percentage of participants self-identified as women", sum(df$gender == 0) / nrow(df) * 100, "%\n")
cat("Percentage of participants self-identified as men", sum(df$gender == 1) / nrow(df) * 100, "%\n")

# Age

# calculate the median age of the participants
cat("The median age of the participants is: ", median(df$age), "\n")

# calculate the mean age of the participants
cat("The mean age of the participants is: ", mean(df$age), "\n")

# calculate the standard deviation of the age of the participants
cat("The standard deviation of the age of the participants is: ", sd(df$age), "\n")

# calculate the min and max age of the participants
cat("The minimum age of the participants is: ", min(df$age), " and the maximum age is: ", max(df$age), "\n")

# number of participants above 65
cat("Number of participants above 65 years old: ", sum(df$age > 65), "\n")

# Education

# print the table with absolute frequencies of the education level
table(df$edu_s, useNA = "ifany")

# print the table with relative frequencies of the education level
table(df$edu_s, useNA = "ifany") / nrow(df) * 100

# East / West Germany

# print the table with absolute frequencies of the east germany variable (named east german)
table(df$`east german`, useNA = "ifany")

# print the table with relative frequencies of the east germany variable (named east german)
table(df$`east german`, useNA = "ifany") / nrow(df) * 100

# Income

# # map values to income ranges
# df$income_str <- NA
# df$income_str[df$income == 1] <- "0-1000"
# df$income_str[df$income == 2] <- "1000-2000"
# df$income_str[df$income == 3] <- "2000-3000"
# df$income_str[df$income == 4] <- "3000-4000"
# df$income_str[df$income == 5] <- "4000-5000"
# df$income_str[df$income == 6] <- "5000-7500"
# df$income_str[df$income == 7] <- "7500-over"

# Median net income
cat("The median net income of the participants is: ", median(df$income, na.rm = TRUE), "\n")

# household earning less than 1000
cat("Number of participants with a household income less than 1000: ", sum(df$income == 1, na.rm = TRUE), "\n")

# household earning more than 5000
cat("Number of participants with a household income more than 5000: ", sum(df$income >= 6, na.rm = TRUE) + sum(df$income == 7, na.rm = TRUE), "\n")

# number of participants with missing income data
cat("Number of participants with missing income data: ", sum(is.na(df$income)), "\n")

# Employment

# print the table with absolute frequencies of the employment status
table(df$employment, useNA = "ifany")

# print the table with relative frequencies of the employment status
table(df$employment, useNA = "ifany") / nrow(df) * 100



#4 PLOTS####

# recode age into 18-29, 30-39, 40-49, 50-59, 60-69, 70+
df$age_group <- NA
df$age_group[df$age >= 18 & df$age <= 29] <- "18-29"
df$age_group[df$age >= 30 & df$age <= 39] <- "30-39"
df$age_group[df$age >= 40 & df$age <= 49] <- "40-49"
df$age_group[df$age >= 50 & df$age <= 59] <- "50-59"
df$age_group[df$age >= 60 & df$age <= 69] <- "60-69"
df$age_group[df$age >= 70] <- "70+"

# recode women and men using the gender variable (women =0, men = 1)
df$gender_str <- NA
df$gender_str[df$gender == 0] <- "women"
df$gender_str[df$gender == 1] <- "men"

# Plot the distribution of participants (rows) of a dataframe according to age group (x-axis), gender_str (legend) and edu_s (columns)


# Create the plot and assign it to a variable
p <- ggplot(df, aes(x = age_group, fill = gender_str)) +
  geom_bar(position = "dodge") +
  facet_grid(~edu_s) +
  labs(x = "Age Group", y = "Number of Participants", fill = "Gender") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("data/output/demographics.png", plot = p, dpi = 300, width = 10, height = 6, units = "in")

# Plot of the weekly visits to the ChatGPT website ignoring zeroes

# filter the data to exclude the participants that have not used ChatGPT
df_used <- df %>% filter(df$chatgpt_usage > 0)

p1 <- ggplot(df_used, aes(x = chatgpt_sessions)) +
  geom_histogram(binwidth = 1, fill = "white", color = "black") +
  labs(x = "ChatGPT Visits", y = "Number of Participants") +
  theme_bw()

# create a histogram of the weekly visits (chatgpt_weekly_usages)
p2 <- ggplot(df_used, aes(x = chatgpt_weekly_usages)) +
  geom_histogram(binwidth = 1, fill = "white", color = "black") +
  labs(x = "Number of weeks with ChatGPT Visits", y = "Number of Participants") +
  theme_bw()

# save the plot to a file
ggsave("data/output/adoption_distribution.png", plot = p1 + p2, dpi = 300, width = 8, height = 4, units = "in")
