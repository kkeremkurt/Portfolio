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
library(magrittr)
library("sqldf")

#Importing the data set

customer = read.csv("C:\\Users\\kkere\\Desktop\\BIA 5303 - Big Data 2 for BIA\\Final Project\\PrimeAnalytics_CustomerDetail_cleaned.csv")
customer

points = read.csv("C:\\Users\\kkere\\Desktop\\BIA 5303 - Big Data 2 for BIA\\Final Project\\PrimeAnalytics_Points_cleaned.csv")
points

#################################################################################
###DATA CLEANING###

##Checking if there are duplicate rows
sum(duplicated(customer))

sum(duplicated(points))
##There are no duplicates.

##Checking for missing values
sapply(customer, function(x) sum(is.na(x)))

sapply(points, function(x) sum(is.na(x)))

#Replacing missing values with 9999 in "ex_sourceid"
points$ex_sourceid[is.na(points$ex_sourceid)] = 9999

#Replacing missing values with 0 in "TransAmount"
points$TransAmount[is.na(points$TransAmount)] = 0


#Formatting Date column
points$Date <- format(as.POSIXct(points$Date,format='%y/%m/%d %H:%M'),format='%m/%d/%Y')
summary(points)

points %<>% mutate(points, Date= as.Date(Date, format= "%m/%d/%Y"))

#Removing outliers from Date column
summary(points)

#Filtering dates after December 2022(2022/12/01)
points_cleaned <- points %>% filter(Date < '2022-12-01')
summary(points_cleaned)

#################################################################################


#Merging dataframes
merged_df <- merge(customer, points_cleaned, by = "Unique_member_identifier")

merged_df


############################################################
##Customer Demographics

#Number of Customers by Province
count(customer, StateProv)

ggplot(customer, aes(StateProv)) + geom_bar() + geom_text(stat='count', aes(label=..count..), vjust=-1)

#Number of Customers by LanguagePreference
count(customer, LanguagePreference)

ggplot(customer, aes(LanguagePreference)) + geom_bar() + geom_text(stat='count', aes(label=..count..), vjust=-1)

#Number of Customers by gender
count(customer, gender)

ggplot(customer, aes(gender)) + geom_bar() + geom_text(stat='count', aes(label=..count..), vjust=-1)

#Number of Customers by age_class
count(customer, age_class)

ggplot(customer, aes(age_class)) + geom_bar() + geom_text(stat='count', aes(label=..count..), vjust=-1)
############################################################


##############################
#Total points by Demographics

#by State
p <- sqldf("SELECT AVG(PointsTotal) as avg , StateProv FROM customer WHERE PointsTotal >= 0 GROUP BY StateProv ORDER BY PointsTotal")
p
ggplot(p) + geom_bar(aes(x = StateProv, y= avg), stat = "identity") + geom_text(stat = "identity", aes(x = StateProv, y= avg, label = avg), vjust = -1)

#by Gender
sqldf("SELECT AVG(PointsTotal) as avg , gender FROM customer WHERE PointsTotal >= 0 GROUP BY gender ORDER BY PointsTotal")

#by Age
sqldf("SELECT AVG(PointsTotal) as avg , age_class FROM customer WHERE PointsTotal >= 0 GROUP BY age_class ORDER BY PointsTotal")
##############################


############################################################
#Average transactions by Demographics

#Count of total customers
sqldf("SELECT count(DISTINCT Unique_member_identifier) FROM merged_df")

#Overall average of transactions
sqldf("SELECT (count(Unique_member_identifier))/12278 as avg FROM merged_df")


#By State
p1 <- sqldf("SELECT count(Unique_member_identifier) as avgtransactions, StateProv  FROM merged_df GROUP BY StateProv")
p2 <- sqldf("SELECT count(DISTINCT Unique_member_identifier) as count, StateProv FROM merged_df GROUP BY StateProv")

p3 <- merge(p1, p2, by = "StateProv")

sqldf("SELECT (avgtransactions/count) as avgtransactions, StateProv from p3 ORDER BY avgtransactions DESC")

#By Gender
p4 <- sqldf("SELECT count(Unique_member_identifier) as avgtransactions, gender  FROM merged_df GROUP BY gender")
p5 <- sqldf("SELECT count(DISTINCT Unique_member_identifier) as count, gender FROM merged_df GROUP BY gender")

p6 <- merge(p4,p5, by = "gender")

sqldf("SELECT (avgtransactions/count) as avgtransactions, gender from p6 ORDER BY avgtransactions DESC")

#By Age
p7 <- sqldf("SELECT count(Unique_member_identifier) as avgtransactions, age_class  FROM merged_df GROUP BY age_class")
p8 <- sqldf("SELECT count(DISTINCT Unique_member_identifier) as count, age_class FROM merged_df GROUP BY age_class")

p9 <- merge(p7,p8, by = "age_class")

sqldf("SELECT (avgtransactions/count) as avgtransactions, age_class from p9 ORDER BY avgtransactions DESC")
############################################################

#Getting year information as a separate column
merged_df$Year <- format(as.Date(merged_df$Date), "%Y")

############################################################


###########REPEAT PURCHASE RATE for StateProv###########

#Count of Repeat customers by StateProv for Each Year

state_repeat <- sqldf("SELECT count(*) as 'Repeat Cust Count', StateProv, Year FROM
    (SELECT Unique_member_identifier, count(*) as count, StateProv, Year FROM merged_df GROUP BY Unique_member_identifier, Year HAVING COUNT(*) > 2)
    GROUP BY StateProv, Year")

#Count of customers by StateProv for Each Year

state_total <- sqldf("SELECT count(DISTINCT Unique_member_identifier) as 'Total Cust Count', StateProv, Year FROM merged_df GROUP BY StateProv, Year")
state_total
state_repeat

#Left joining total and repeat dfs
state_merged <- left_join(state_total, state_repeat, by = c("StateProv", "Year"))

#Replacing NA values with 0
state_merged[is.na(state_merged)] <- 0

#Exporting to csv
write.csv(state_merged, "C:\\Users\\kkere\\Desktop\\BIA 5303 - Big Data 2 for BIA\\Final Project\\State_Repeat.csv", row.names=FALSE)


###########REPEAT PURCHASE RATE for Gender###########
#Count of Repeat customers by Gender for Each Year

gender_repeat <- sqldf("SELECT count(*) as 'Repeat Cust Count', gender, Year FROM
    (SELECT Unique_member_identifier, count(*) as count, gender, Year FROM merged_df GROUP BY Unique_member_identifier, Year HAVING COUNT(*) > 2)
    GROUP BY gender, Year")

#Count of customers by Gender for Each Year

gender_total <- sqldf("SELECT count(DISTINCT Unique_member_identifier) as 'Total Cust Count', gender, Year FROM merged_df GROUP BY gender, Year")

#Left joining total and repeat dfs
gender_merged <- left_join(gender_total, gender_repeat, by = c("gender", "Year"))

#Exporting to csv
write.csv(gender_merged, "C:\\Users\\kkere\\Desktop\\BIA 5303 - Big Data 2 for BIA\\Final Project\\Gender_Repeat.csv", row.names=FALSE)


###########REPEAT PURCHASE RATE for Age###########
#Count of Repeat customers by Age for Each Year
age_repeat <- sqldf("SELECT count(*) as 'Repeat Cust Count', age_class, Year FROM
    (SELECT Unique_member_identifier, count(*) as count, age_class, Year FROM merged_df GROUP BY Unique_member_identifier, Year HAVING COUNT(*) > 2)
    GROUP BY age_class, Year")

#Count of customers by Age for Each Year

age_total <- sqldf("SELECT count(DISTINCT Unique_member_identifier) as 'Total Cust Count', age_class, Year FROM merged_df GROUP BY age_class, Year")

#Left joining total and repeat dfs
age_merged <- left_join(age_total, age_repeat, by = c("age_class", "Year"))

#Exporting to csv
write.csv(age_merged, "C:\\Users\\kkere\\Desktop\\BIA 5303 - Big Data 2 for BIA\\Final Project\\Age_Repeat.csv", row.names=FALSE)


