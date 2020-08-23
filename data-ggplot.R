setwd("C:/Users/ishanichakraborty/Documents/visualstudio-projects/data-visualization/titanic")
install.packages("ggplot2")
install.packages("dplyr")
library(dplyr)
library(ggplot2)

# Load dataset for analysis
titanic <- read.csv("titanic.csv", stringsAsFactors = FALSE)
View(titanic)


# Set factors
titanic$Pclass <- as.factor(titanic$Pclass)
titanic$Survived <- as.factor(titanic$Survived)
titanic$Sex <- as.factor(titanic$Sex)
titanic$Embarked <- as.factor(titanic$Embarked)

# survival rate - factor | dependent, categorical variable  

ggplot(titanic, aes(x = Survived)) + 
  geom_bar()

# Calculates another factor - percentage survived
prop.table(table(titanic$Survived))

# set up the structures and axises of your bar graph.
ggplot(titanic, aes(x = Survived)) + 
  theme_bw() +
  geom_bar() +
  labs(y = "Passenger Count",
       title = "Titanic Survival Rates")


# To bring more dimension and visual analysis - take gender
# color used to compare two aspects (i.e., dimensions) of data simultaneouly
# dynamic data manipulation using a static dataset
ggplot(titanic, aes(x = Sex, fill = Survived)) + 
  theme_bw() +
  geom_bar() +
  labs(y = "Passenger Count",
       title = "Titanic Survival Rates by Sex")


# survival rate by ticket
ggplot(titanic, aes(x = Pclass, fill = Survived)) + 
  theme_bw() +
  geom_bar() +
  labs(y = "Passenger Count",
       title = "Titanic Survival Rates by Pclass")

# Further leverage facets of multiple variables 
# in high order functions to find the Survival
# rate by class of ticket and gender
ggplot(titanic, aes(x = Sex, fill = Survived)) + 
  theme_bw() +
  facet_wrap(~ Pclass) +
  geom_bar() +
  labs(y = "Passenger Count",
       title = "Titanic Survival Rates by Pclass and Sex")

# transistion to understanding distribution of data using stats concepts
# helps with prediciton of survival based on existing variables
ggplot(titanic, aes(x = Age)) +
  theme_bw() +
  geom_histogram(binwidth = 5) +
  labs(y = "Passenger Count",
       x = "Age (binwidth = 5)",
       title = "Titanic Age Distribtion")

# survival rates by age?
ggplot(titanic, aes(x = Age, fill = Survived)) +
  theme_bw() +
  geom_histogram(binwidth = 5) +
  labs(y = "Passenger Count",
       x = "Age (binwidth = 5)",
       title = "Titanic Survival Rates by Age")

# so far we have a bar graph, histrogram, and 
# another good respresentation of this dataset
# to understand distribution is a box-and-whisker plot
ggplot(titanic, aes(x = Survived, y = Age)) +
  theme_bw() +
  geom_boxplot() +
  labs(y = "Age",
       x = "Survived",
       title = "Titanic Survival Rates by Age")


# survival rates by age when segmented by gender and class of ticket?
# form a similar graph with as a histrogram => density plot
ggplot(titanic, aes(x = Age, fill = Survived)) +
  theme_bw() +
  facet_wrap(Sex ~ Pclass) +
  geom_density(alpha = 0.5) +
  labs(y = "Age",
       x = "Survived",
       title = "Titanic Survival Rates by Age, Pclass and Sex")

ggplot(titanic, aes(x = Age, fill = Survived)) +
  theme_bw() +
  facet_wrap(Sex ~ Pclass) +
  geom_histogram(binwidth = 5) +
  labs(y = "Age",
       x = "Survived",
       title = "Titanic Survival Rates by Age, Pclass and Sex")


# Load other dataset for comparative analysis
h1b <- read.csv("H-1B_FY2018.csv", stringsAsFactors = FALSE, encoding = 'UTF-8')
View(h1b)

# Set factors.
h1b$EMPLOYER_NAME <- as.factor(h1b$EMPLOYER_NAME)
h1b$EMPLOYER_CITY <- as.factor(h1b$EMPLOYER_CITY)
h1b$EMPLOYER_STATE <- as.factor(h1b$EMPLOYER_STATE)
h1b$SOC_NAME <- as.factor(h1b$SOC_NAME)
h1b$WORKSITE_CITY <- as.factor(h1b$WORKSITE_CITY)
h1b$WORKSITE_STATE <- as.factor(h1b$WORKSITE_STATE)
h1b$CASE_STATUS <- as.factor(h1b$CASE_STATUS)
h1b$PW_WAGE_LEVEL<- as.factor(h1b$PW_WAGE_LEVEL)
h1b$JOB_TITLE<- as.factor(h1b$JOB_TITLE)
h1b$PREVAILING_WAGE <- as.numeric(h1b$PREVAILING_WAGE)
h1b$PREVAILING_WAGE[is.na(h1b$PREVAILING_WAGE)] <- round(mean(h1b$PREVAILING_WAGE, na.rm = TRUE))
head(h1b)


ggplot(h1b, aes(x = EMPLOYER_STATE, fill = CASE_STATUS)) + 
  theme_bw() +
  geom_bar() + 
  labs(y = "No. of Applications", x = "Employer State",  
       title = "Distribution by Employer State")

# Subset the data to keep only verified/valid cases
certified_h1b <- h1b %>%
  filter(CASE_STATUS == "CERTIFIED")

# return max number of workers looking at the factors and dataset
top_N_employers <- function(num_emp) {
  certified_h1b %>%
    group_by(EMPLOYER_NAME) %>%
    summarise(num_apps = n()) %>%
    arrange(desc(num_apps)) %>%
    slice(1:num_emp)
}

# determines eligibility amongest 30 highest H1B candidates
ggplot(top_N_employers(30), 
       aes(x = reorder(EMPLOYER_NAME, num_apps), y = num_apps)) +
  geom_bar(stat = "identity", alpha = 0.9, fill = "purple", width = 0.7) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 11000), breaks = seq(0, 11000)) +
  geom_text(aes(label = num_apps), hjust = -0.2, size = 2) +
  ggtitle("Top 30 Employers with most applications") +
  theme_bw() +
  labs(x = "Employer Name", y = "No. of Applications")

# determines the pattern occupational distribution
# amongst top H1B candidates
top_N_SOC <- function(num) {
  certified_h1b %>%
    filter(!is.na(certified_h1b$SOC_NAME)) %>%
    group_by(SOC_NAME) %>%
    summarise(num_apps = n()) %>%
    arrange(desc(num_apps)) %>%
    slice(1:num)
}

# Bar plot to show the top 30 H1B occupations 
ggplot(top_N_SOC(30), 
       aes(x = reorder(SOC_NAME, num_apps), y = num_apps)) +
  geom_bar(stat = "identity", alpha = 0.9, fill = "pink", width = 0.7) +
  coord_flip() +
  scale_y_continuous() +
  geom_text(aes(label = num_apps), hjust = -0.2, size = 2) +
  ggtitle("Top 30 occupations with most H1B petitions") +
  theme(plot.title = element_text(size = rel(1)),
        axis.text.y = element_text(size = rel(0.8))) +
  labs(x = "SOC Name", y = "No. of Applications") 

