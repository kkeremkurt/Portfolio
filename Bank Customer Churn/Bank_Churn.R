#Importing packages
library("tidyverse")
library("dplyr")
install.packages("janitor")
library("janitor")
library("lubridate")
library("ggplot2")
install.packages("scales")
library("scales")
install.packages("ggtext")
library("ggtext")
library("corrplot")

#Importing the data set

mydata = read.csv("C:\\Users\\kkere\\Desktop\\BIA 5303 - Big Data 2 for BIA\\Project\\Churn_Modelling.csv")

#Checking if the data is tidy
mydata
##Each variable has its own column, 
##Each observation forms a row, 
##Each cell is a single measurement.
##The data is tidy.

###DATA CLEANING###

##Checking data types
glimpse(mydata)
##Data type is accurate and consistent.


##Reformatting column names for easy use
mydata <-clean_names(mydata)


##Checking if there are duplicate rows
sum(duplicated(mydata))
##There are no duplicates.


##Checking for missing values
sapply(mydata, function(x) sum(is.na(x)))
##There are no missing values.
mydata


##Checking for negative values
sapply(mydata, function(x) sum(x < 0))
##There are no negative values.


##Checking for uniqueness of categorical variables
glimpse(mydata)
##
# RowNumber -> Ordinal
# CustomerId -> Nominal
# Surname -> Nominal
# CreditScore -> Numerical
# Geography -> Nominal
# Gender -> Nominal
# Age -> Numerical
# Tenure -> Numerical
# Balance -> Numerical
# NumOfProducts -> Numerical
# HasCrCard -> Nominal(Binary)
# IsActiveMember -> Nominal(Binary)
# EstimatedSalary -> Numerical
# Exited -> Nominal(Binary)
##
#geography, gender, has_cr_card, is_active_member, exited
unique(mydata$geography)
unique(mydata$gender)
unique(mydata$has_cr_card)
unique(mydata$is_active_member)
unique(mydata$exited)
##All categorical variables are unique


##Checking for outliers
summary(mydata)
#Salary has possible outliers.
ggplot(mydata) + aes(x = "", y = estimated_salary) + geom_boxplot(fill = "#0c4c8a") + theme_minimal()
##Outliers are not detected based on the result of the boxplot.



###ANALYSIS###


#Overall look for the number of exited customers 


ggplot(mydata) + aes(exited) +geom_bar(fill = "#0099f9", colour="black") + theme_minimal() +
  labs(title = "Number of <span style = 'color: red;'>exited</span> customers", x = "Status", y = "Count") + 
  theme(axis.text.x = element_text(vjust = 6, size = 16, face = "bold"), plot.title = element_markdown(hjust = 0.5, face = "bold",color = "black", size = 25), 
        axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_text(stat = 'count', fontface= "bold", aes(label = ..count..), vjust = 2, size = 7, color = "#ffffff") + 
  scale_x_continuous(breaks = c(0,1), labels=c("Active", "Exited"))


#Percentage of exited customers by Gender

mydata %>% count(exited, gender) %>% group_by(exited) %>% mutate(pct= prop.table(n) * 100) %>% ggplot() + aes(exited, pct, fill=gender) + geom_bar(stat="identity", colour = "Black")+
  geom_text(fontface = "bold", size = 7, aes(label=paste0(sprintf("%1.1f", pct),"%")), position=position_stack(vjust=0.5)) + theme_minimal() +
  labs(title = "Exited Customers by <span style = 'color: red;'>Gender</span>", x = "Status", y = "Count", fill = "Gender") + 
  theme(axis.text.x = element_text(vjust = 6, size = 16, face = "bold"), plot.title = element_markdown(hjust = 0.5, face = "bold",color = "black", size = 25), 
        axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank(), legend.title = element_text(size=13, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_continuous(breaks = c(0,1), labels=c("Active", "Exited"))


#Percentage of exited customers by Geography

mydata %>% count(exited, geography) %>% group_by(exited) %>% mutate(pct= prop.table(n) * 100) %>% ggplot() + aes(exited, pct, fill=geography) + geom_bar(stat="identity", colour = "Black")+
  geom_text(fontface = "bold", size = 7, aes(label=paste0(sprintf("%1.1f", pct),"%")), position=position_stack(vjust=0.5)) + theme_minimal() +
  labs(title = "Exited Customers by <span style = 'color: red;'>Geography</span>", x = "Status", y = "Count", fill = "Geography") + 
  theme(axis.text.x = element_text(vjust = 6, size = 16, face = "bold"), plot.title = element_markdown(hjust = 0.5, face = "bold",color = "black", size = 25), 
        axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank(), legend.title = element_text(size=13, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_continuous(breaks = c(0,1), labels=c("Active", "Exited"))

#Percentage of exited customers by Credit Card

mydata %>% count(exited, has_cr_card) %>% group_by(exited) %>% mutate(pct= prop.table(n) * 100) %>% ggplot() + aes(exited, pct, fill=factor(has_cr_card)) + geom_bar(stat="identity", colour = "Black")+
  geom_text(fontface = "bold", size = 7, aes(label=paste0(sprintf("%1.1f", pct),"%")), position=position_stack(vjust=0.5)) + theme_minimal() +
  labs(title = "Exited Customers by <span style = 'color: red;'>Credit Card</span>", x = "Status", y = "Count", fill = "Credit Card") + 
  theme(axis.text.x = element_text(vjust = 6, size = 16, face = "bold"), plot.title = element_markdown(hjust = 0.5, face = "bold",color = "black", size = 25), 
        axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank(), legend.title = element_text(size=13, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_continuous(breaks = c(0,1), labels=c("Active", "Exited")) + scale_fill_discrete(breaks = c(0,1), labels = c("1" = "Yes","0" = "No")) + guides(fill = guide_legend(reverse = TRUE))
      
#Percentage of exited customers by Activity

mydata %>% count(exited, is_active_member) %>% group_by(exited) %>% mutate(pct= prop.table(n) * 100) %>% ggplot() + aes(exited, pct, fill=factor(is_active_member)) + geom_bar(stat="identity", colour = "Black")+
  geom_text(fontface = "bold", size = 7, aes(label=paste0(sprintf("%1.1f", pct),"%")), position=position_stack(vjust=0.5)) + theme_minimal() +
  labs(title = "Exited Customers by <span style = 'color: red;'>Activity</span>", x = "Status", y = "Count", fill = "Activity") + 
  theme(axis.text.x = element_text(vjust = 6, size = 16, face = "bold"), plot.title = element_markdown(hjust = 0.5, face = "bold",color = "black", size = 25), 
        axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank(), legend.title = element_text(size=13, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_continuous(breaks = c(0,1), labels=c("Active", "Exited")) + scale_fill_discrete(breaks = c(0,1), labels = c("1" = "Active","0" = "Passive")) + guides(fill = guide_legend(reverse = TRUE))

#Exited customers by Credit Score
  
ggplot(mydata, aes(credit_score, fill = factor(exited))) + geom_histogram(aes(y = stat(count / sum(count))), colour = "Black") + scale_y_continuous(labels = scales::percent_format()) + theme_minimal() +
  labs(title = "Exited Customers by <span style = 'color: red;'>Credit Score</span>", x = "Credit Score", y = "Count", fill = "Exited") +
  theme(axis.text.x = element_text(vjust = 6, size = 16, face = "bold"), plot.title = element_markdown(hjust = 0.5, face = "bold",color = "black", size = 25), 
        axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank(), legend.title = element_text(size=13, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_manual(values=c("#999999", "#E69F00"), breaks = c(0,1), labels = c("1" = "Yes","0" = "No")) + guides(fill = guide_legend(reverse = TRUE))

#Exited customers by Age

ggplot(mydata, aes(age, fill = factor(exited))) + geom_histogram(aes(y = stat(count / sum(count))), colour = "Black") + scale_y_continuous(labels = scales::percent_format()) + theme_minimal() +
  labs(title = "Exited Customers by <span style = 'color: red;'>Age</span>", x = "Age", y = "Count", fill = "Exited") +
  theme(axis.text.x = element_text(vjust = 6, size = 16, face = "bold"), plot.title = element_markdown(hjust = 0.5, face = "bold",color = "black", size = 25), 
        axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank(), legend.title = element_text(size=13, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_manual(values=c("#999999", "#E69F00"), breaks = c(0,1), labels = c("1" = "Yes","0" = "No")) + guides(fill = guide_legend(reverse = TRUE))

#Exited customers by Tenure

ggplot(mydata, aes(tenure, fill = factor(exited))) + geom_bar(aes(y = stat(count / sum(count))), colour = "Black") + scale_y_continuous(labels = scales::percent_format()) + theme_minimal() +
  labs(title = "Exited Customers by <span style = 'color: red;'>Tenure</span>", x = "Tenure", y = "Count", fill = "Exited") +
  theme(axis.text.x = element_text(vjust = 6, size = 16, face = "bold"), plot.title = element_markdown(hjust = 0.5, face = "bold",color = "black", size = 25), 
        axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank(), legend.title = element_text(size=13, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_manual(values=c("#999999", "#E69F00"), breaks = c(0,1), labels = c("1" = "Yes","0" = "No")) + guides(fill = guide_legend(reverse = TRUE))

#Exited customers by Number of Products

ggplot(mydata, aes(num_of_products, fill = factor(exited))) + geom_bar(aes(y = stat(count / sum(count))), colour = "Black") + scale_y_continuous(labels = scales::percent_format()) + theme_minimal() +
  labs(title = "Exited Customers by <span style = 'color: red;'># of Products</span>", x = "Number of Products", y = "Count", fill = "Exited") +
  theme(axis.text.x = element_text(vjust = 6, size = 16, face = "bold"), plot.title = element_markdown(hjust = 0.5, face = "bold",color = "black", size = 25), 
        axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank(), legend.title = element_text(size=13, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_manual(values=c("#999999", "#E69F00"), breaks = c(0,1), labels = c("1" = "Yes","0" = "No")) + guides(fill = guide_legend(reverse = TRUE))

#Exited customers by Balance

ggplot(mydata, aes(balance, fill = factor(exited))) + geom_histogram(colour = "Black") + scale_y_continuous(labels = scales::percent_format()) + theme_minimal() +
  labs(title = "Exited Customers by <span style = 'color: red;'>Balance</span>", x = "Balance", y = "Count", fill = "Exited") +
  theme(axis.text.x = element_text(vjust = 6, size = 16, face = "bold"), plot.title = element_markdown(hjust = 0.5, face = "bold",color = "black", size = 25), 
        axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank(), legend.title = element_text(size=13, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_fill_manual(values=c("#999999", "#E69F00"), breaks = c(0,1), labels = c("1" = "Yes","0" = "No")) + guides(fill = guide_legend(reverse = TRUE)) +
  scale_x_continuous(labels=comma) +
  stat_bin(bins=15, geom="text", colour="black", size=6,
           aes(label=ifelse(..x..==0,..count..,""), vjust=0.5, hjust = -0.5))

#Exited customers by Estimated Salary

ggplot(mydata, aes(estimated_salary, fill = factor(exited))) + geom_histogram(aes(y = stat(count / sum(count))), colour = "Black") + scale_y_continuous(labels = scales::percent_format()) + theme_minimal() +
  labs(title = "Exited Customers by <span style = 'color: red;'>Est. Salary</span>", x = "Estimated Salary", y = "Count", fill = "Exited") +
  theme(axis.text.x = element_text(vjust = 6, size = 16, face = "bold"), plot.title = element_markdown(hjust = 0.5, face = "bold",color = "black", size = 25), 
        axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank(), legend.title = element_text(size=13, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_manual(values=c("#999999", "#E69F00"), breaks = c(0,1), labels = c("1" = "Yes","0" = "No")) + guides(fill = guide_legend(reverse = TRUE)) 


#Correlation Matrix
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggcorrplot")


Num_churn_df <- mydata[, c("credit_score", "age", "tenure","balance","estimated_salary","exited")] 
numm <- sapply(Num_churn_df, is.numeric)
corr.matrix <- cor(Num_churn_df[,numm])
corrplot(corr.matrix, method="color")
title("Correlation Matix", line=3)

Num_churn_df <- mydata[, c("credit_score", "age", "tenure","balance","estimatedSalary","exited")] 
numm <- sapply(Num_churn_df, is.numeric)
corr.matrix <- cor(Num_churn_df[,numm])
corrplot(corr.matrix, method="number")
title("Correlation Matix", line=3)


#Box plots by Gender
ggplot(mydata) + geom_boxplot(aes(x = factor(exited), y = age)) + facet_wrap(~gender)

ggplot(mydata) + geom_boxplot(aes(x = factor(exited), y = tenure))  + facet_wrap(~gender)

ggplot(mydata) + geom_boxplot(aes(x = factor(exited), y = balance))  + facet_wrap(~gender)

ggplot(mydata) + geom_boxplot(aes(x = factor(exited), y = num_of_products))  + facet_wrap(~gender)

ggplot(mydata) + geom_boxplot(aes(x = factor(exited), y = estimated_salary))  + facet_wrap(~gender)

ggplot(mydata) + geom_boxplot(aes(x = factor(exited), y = credit_score))  + facet_wrap(~gender)

#Box plots by Geography
ggplot(mydata) + geom_boxplot(aes(x = factor(exited), y = age)) + facet_wrap(~geography)

ggplot(mydata) + geom_boxplot(aes(x = factor(exited), y = tenure))  + facet_wrap(~geography)

ggplot(mydata) + geom_boxplot(aes(x = factor(exited), y = balance))  + facet_wrap(~geography)

ggplot(mydata) + geom_boxplot(aes(x = factor(exited), y = num_of_products))  + facet_wrap(~geography)

ggplot(mydata) + geom_boxplot(aes(x = factor(exited), y = estimated_salary))  + facet_wrap(~geography)

ggplot(mydata) + geom_boxplot(aes(x = factor(exited), y = credit_score))  + facet_wrap(~geography)

#Box plots by Credit Card
ggplot(mydata) + geom_boxplot(aes(x = factor(exited), y = age)) + facet_wrap(~has_cr_card)

ggplot(mydata) + geom_boxplot(aes(x = factor(exited), y = tenure))  + facet_wrap(~has_cr_card)

ggplot(mydata) + geom_boxplot(aes(x = factor(exited), y = balance))  + facet_wrap(~has_cr_card)

ggplot(mydata) + geom_boxplot(aes(x = factor(exited), y = num_of_products))  + facet_wrap(~has_cr_card)

ggplot(mydata) + geom_boxplot(aes(x = factor(exited), y = estimated_salary))  + facet_wrap(~has_cr_card)

ggplot(mydata) + geom_boxplot(aes(x = factor(exited), y = credit_score))  + facet_wrap(~has_cr_card)


#Box plots by Gender
ggplot(mydata) + geom_boxplot(aes(x = factor(exited), y = age)) + facet_wrap(~is_active_member)

ggplot(mydata) + geom_boxplot(aes(x = factor(exited), y = tenure))  + facet_wrap(~is_active_member)

ggplot(mydata) + geom_boxplot(aes(x = factor(exited), y = balance))  + facet_wrap(~is_active_member)

ggplot(mydata) + geom_boxplot(aes(x = factor(exited), y = num_of_products))  + facet_wrap(~is_active_member)

ggplot(mydata) + geom_boxplot(aes(x = factor(exited), y = estimated_salary))  + facet_wrap(~is_active_member)

ggplot(mydata) + geom_boxplot(aes(x = factor(exited), y = credit_score))  + facet_wrap(~is_active_member)
