## Load the libraries
library(dplyr)
library(ggplot2)
library(tidyverse)
library(plotly)

## Reading the dataset
data <- read.csv('./Data/Training Data.csv')

## Get the dimension of the data set
paste("The number of rows in the dataset is",dim(data)[1])
paste("The number of attributes in the dataset is",dim(data)[2])

## Summary of the data set
summary(data)

## Find out how many null values in the data set and which variable has null values
sum(is.na(data))
names(which(colSums(is.na(data))>0))

## Determine the variable types
str(data)

#Income attribute
paste("The number of unique Income levels are",length(unique(data$Income)))

# Age attribute
sort(unique(data$Age))
paste("The number of unique Age levels are",length(unique(data$Age)))

# Experience attribute
sort(unique(data$Experience))
paste("The number of unique Experience levels are",length(unique(data$Experience)))

# Married.Single attribute
paste("The number of unique Marital Status levels are",length(unique(data$Married.Single)))

# House_Ownership attribute
paste("The number of unique House Ownership levels are",length(unique(data$House_Ownership)))

# Car_Ownership attribute
paste("The number of unique Car Ownership levels are",length(unique(data$Car_Ownership)))

# Profession attribute
paste("The number of unique Profession levels are",length(unique(data$Profession)))

# CITY attribute
paste("The number of unique Cities are",length(unique(data$CITY)))

# STATE attribute
paste("The number of unique States are",length(unique(data$STATE)))

# CURRENT_JOB_YRS attribute
sort(unique(data$CURRENT_JOB_YRS))
paste("The number of unique Current Job Year levels are",length(unique(data$CURRENT_JOB_YRS)))

# CURRENT_HOUSE_YRS attribute
sort(unique(data$CURRENT_HOUSE_YRS))
paste("The number of unique Current House Years levels are",length(unique(data$CURRENT_HOUSE_YRS)))


## Visualisation

# Marital status of defaulters and non-defaulters together
data %>% group_by(Risk_Flag) %>% 
  summarise("Married" = sum(Married.Single == "married"),
            "Single" = sum(Married.Single == "single")) %>%
  gather(key = "Status", value = "Status Count", c(-Risk_Flag)) %>%
  ggplot(aes(fill = Status, y = log(`Status Count`), x = as.factor(Risk_Flag))) + 
  geom_bar(position = "dodge", stat = "identity") + 
  ggtitle("Comparing Marital Status of Defaulters & Non-defaulters") + 
  xlab("Risk Flag") + 
  theme(plot.title = element_text(hjust = 0.5))

# House of ownership of defaulters and non-defaulters together
  # Bar Chart
data %>% group_by(Risk_Flag) %>% 
  summarise("Rented" = sum(House_Ownership == "rented"),
            "Owned" = sum(House_Ownership == "owned")) %>% 
  gather(key = "Type", value = "Count", c(-Risk_Flag)) %>%
  ggplot(aes(fill = Type, y = log(Count), x = as.factor(Risk_Flag))) + 
  geom_bar(position = "dodge", stat = "identity") + 
  ggtitle("Comparing House Ownership of Defaulters & Non-defaulters") + 
  xlab("Risk Flag") + 
  theme(plot.title = element_text(hjust = 0.5)) 

  # Pie Chart
data %>% filter(Risk_Flag == 1) %>% 
  group_by(House_Ownership) %>%
  count() %>% 
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc)) %>%
  ggplot(aes(x = "", y = perc, fill = as.factor(House_Ownership))) + 
  geom_col() +
  labs(x = "", y = "", title = "Comparison of the House Ownership of Defaulters",
       fill = "Ownership Types") + 
  geom_text(aes(label = labels), cex = 3,
            position = position_stack(vjust = 0.7)) +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(hjust = 0.5))

# Car Ownership of defaulters and non-defaulters together
data %>% group_by(Risk_Flag) %>% 
  summarise("Yes" = sum(Car_Ownership == "yes"),
            "No" = sum(Car_Ownership == "no")) %>% 
  gather(key = "Type", value = "Count", c(-Risk_Flag)) %>%
  ggplot(aes(fill = Type, y = Count, x = as.factor(Risk_Flag))) + 
  geom_bar(position = "dodge", stat = "identity") + 
  ggtitle("Comparing Car Ownership of Defaulters & Non-defaulters") + 
  xlab("Risk Flag") + 
  theme(plot.title = element_text(hjust = 0.5))

# Comparing the defaulters' professions
data %>% filter(Risk_Flag == 1) %>% group_by(Profession) %>% summarise(count = n()) %>%
  arrange(desc(count)) %>% slice(1:5) %>%
  ggplot(., aes(x=Profession, y=count)) +
  geom_bar(stat='identity', fill = "seagreen") + 
  ggtitle("Plot of the top 5 professions of Defaulters") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_text(aes(label=count), vjust=2)

# Analysing the age distribution of defaulters
age_plot <- data %>% filter(Risk_Flag == 1) %>% group_by(Age) %>% summarise(count = n()) %>%
  arrange(desc(count)) %>% 
  ggplot(., aes(x=Age, y=count)) +
  geom_bar(stat='identity', fill = "hotpink3") +
  ggtitle("Age distribution of Defaulters") +
  theme(plot.title = element_text(hjust = 0.5)) 

ggplotly(age_plot)

# Analysing the distribution of work experience of defaulters
exp_plot <- data %>% filter(Risk_Flag == 1) %>% group_by(Experience) %>% summarise(count = n()) %>%
  arrange(desc(count)) %>% 
  ggplot(., aes(x=Experience, y=count)) +
  geom_bar(stat='identity', fill = "skyblue") +
  ggtitle("Distribution of Work Experience of Defaulters") +
  theme(plot.title = element_text(hjust = 0.5)) 

ggplotly(exp_plot)

# Comparing the cities where defaulters are found
data %>% filter(Risk_Flag == 1) %>% group_by(CITY) %>% summarise(count = n()) %>%
  arrange(desc(count)) %>% slice(1:5) %>%
  ggplot(., aes(x=CITY, y=count)) +
  geom_bar(stat='identity', fill = "salmon2") + 
  ggtitle("Plot of the top 5 cities where Defaulters are found") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_text(aes(label=count), vjust=2)

# Comparing the states where defaulters are found
data %>% filter(Risk_Flag == 1) %>% group_by(STATE) %>% summarise(count = n()) %>%
  arrange(desc(count)) %>% slice(1:5) %>%
  ggplot(., aes(x=STATE, y=count)) +
  geom_bar(stat='identity', fill = "mediumpurple") + 
  ggtitle("Plot of the top 5 States where Defaulters are found") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_text(aes(label=count), vjust=2)

# Comparing the current house years of defaulters
#https://r-charts.com/part-whole/pie-chart-percentages-ggplot2/
data %>% filter(Risk_Flag == 1) %>% 
  group_by(CURRENT_HOUSE_YRS) %>%
  count() %>% 
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc)) %>%
  ggplot(aes(x = "", y = perc, fill = as.factor(CURRENT_HOUSE_YRS))) +
  geom_col() + 
  labs(x = "", y = "", title = "Comparison of the Current House Years of Defaulters",
       fill = "Current House Years") + 
  geom_text(aes(label = labels), cex = 3,
            position = position_stack(vjust = 0.7)) +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(hjust = 0.5))

# Comparing the Current job years of defaulters
data %>% filter(Risk_Flag == 1) %>% 
  group_by(CURRENT_JOB_YRS) %>%
  count() %>% 
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc)) %>%
  ggplot(aes(x = "", y = perc, fill = as.factor(CURRENT_JOB_YRS))) +
  geom_col() +
  labs(x = "", y = "", title = "Comparison of the Current Job Years of Defaulters",
       fill = "Current Job Years") + 
  geom_text(aes(label = labels), cex = 3,
            position = position_stack(vjust = 0.7)) +
  theme(plot.title = element_text(hjust = 0.5))






