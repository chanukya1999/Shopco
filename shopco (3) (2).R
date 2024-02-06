################################################################################
#                            CLASS PROJECT
################################################################################

# This is the class project for the MGT585 course (15 points, individual assignment)

# Please write your name here- Chanukya Bolli

# This script is based on the Shopco data- Shopco is a mall operator
# Use the Shopco caselet and data (consumer.csv,  purchase.csv) 
# on D2L to answer questions below
# Think about the managerial implications as you go along
# You will use the knowledge from MGT585 that you have learned so far

################################################################################
#                                  MGT585
################################################################################

# We have covered following:

# Basic functions in R
# DPLYR
# Summary stats
# Viz. using GGPLOT
# Regression
# Classification
# Clustering (Session 8)

################################################################################
#                                    Data
################################################################################

library(dplyr)
library(ggplot2)

# Q1) Read the data in: consumer and purchase tables

# set your working directory and use read.csv() to read files

setwd("C:/Users/Srinivas Bolli/Documents/Final")

consumer <- read.csv(file = "consumer.csv")

purchase <- read.csv(file = "purchase.csv")

View(consumer)
View(purchase)

################################################################################

# Q2) Let's first start with exploring various tables

# explore consumer table using 5 functions
# hint: loyalty status 1 is low and 2 is high
# age is in years

dim(consumer)

str(consumer)

colnames(consumer)

head(consumer)

tail(consumer)

# explore purchase table using 5 functions
# Hint: real time message 1 is received and 0 is not received
# Sales are in $ dollars

dim(purchase)

str(purchase)

colnames(purchase)

head(purchase)

tail(purchase)

################################################################################
# Q3) correct the data types in consumer table #

# correct gender as a factor

table(consumer$gender)
consumer$gender<- as.factor(consumer$gender)

# correct loyalty_status as a factor

table(consumer$loyalty_status)
consumer$loyalty_status<- as.factor(consumer$loyalty_status)

# correct the data types in purchase table #

# correct realtime_message as a factor

table(purchase$realtime_message)
purchase$realtime_message<- as.factor(purchase$realtime_message)

################################################################################
#                         Descriptive Analytics
################################################################################

# Q4) let's look at one table at a time: consumer table #

# continuous variables #
# Show what is the average age of consumer by gender
# Hint: you will use group_by()

consumer %>% group_by(gender) %>% summarise(mean=mean(age))

# categorical variables #

# How many consumers are female and male

table(consumer$gender)

# How many consumers are there in each loyalty_status

table(consumer$loyalty_status)

# what can you conclude about consumers of Shopco (write in one line): 
# Ans) There are more female consumers than males in the Shopco data and the number of customers with high
#      loyalty status is also more compared to those with low loyalty status.    
################################################################################

# Q5) Now, let's look at the second table : purchase  #

# continuous variables #

# How much, on an average, customers spend on second trip onwards
# Hint: summary() on from_second_store_sales

purchase %>% summarise(Average_customer_spent_on_second_store_onwards=mean(from_second_store_sales))

# As per hint given
summary(purchase$from_second_store_sales)

# categorical variables #
# How many customers received realtime_message
# Hint: you will use table()

table(purchase$realtime_message)

# What can you say about the real time mobile experiment in the Shopco mall (write in one line):
# Ans) 4516 customers received real time message and on an average, customers spent 
#      about 34.65362$ from second store onwards. 

################################################################################
# Next, let's see what impacts sales

# Q6) before that, we will combine consumer and purchase data using inner join
# Using inner_join() on consumer_id create a new data frame called experiment

experiment <- consumer %>% inner_join(purchase, by="consumer_id")
dim(experiment)
View(experiment)

# how many number of rows are there in the experiment table:
# Ans) 9032 rows
################################################################################
#                                  Predictive analytics
################################################################################
#                                    Regression
################################################################################

# Now, let's run regression to see what impacts sales from second store onwards

# Q7) what is the impact of age on sales from second store onwards

# visualize using scatter plot

ggplot(experiment, mapping = aes(x=age, y=from_second_store_sales)) + geom_point() + geom_smooth(method = "lm",se = FALSE, colour = "pink1") + ggtitle("Impact of age on sales from second store onwards") + xlab("Age") +ylab("Sales from second store onwards")

# run regression using lm()
reg1 <- lm(from_second_store_sales ~ age, data = experiment)
summary(reg1)

# p-value: a: p< 0.05; b: p< 0.05
# coeffiecients: a = 11.9675, b= 0.7307
# R square: 0.5% of second store sales is explained by age
# F stats: 50.35>1

# dummy variable regression

# Q8) what is the impact of realtime mobile message on sales from second store onwards

# visualize using bar chart

ggplot(experiment, mapping = aes(x=realtime_message, y=from_second_store_sales, fill=realtime_message)) + geom_bar(position="dodge", stat = "identity") + ggtitle("impact of realtime mobile message on sales from second store onwards") + xlab("realtime mobile messages") +ylab("sales from second store onwards")

# run regression using lm()

reg2<- lm(from_second_store_sales ~ realtime_message, data = experiment)
summary(reg2)

# p-value: a: p< 0.05; b: p< 0.05
# coeffiecients: a = 32.578, b= 4.151
# R square: 0.04% of second store sales is explained by realtime message
# F stats: 4.629>1

# managerial implication (write in one line):
# Ans)  The sales from second store onwards is significantly higher for consumers who received the realtime
#       message, hence the managers must make sure that the realtime messages are also sent to other consumers
#       in order to increase their sales.
################################################################################
#                                    Clustering
################################################################################

# Q9) Last, let's run clustering to find clusters of different customers

# use kmeans() to run cluster analysis on age and sales on second store onwards
# Hint: this will be your first parameter: experiment[, c("age","from_second_store_sales")], which is the second and fifth column
# we know from experience that there are three clusters

set.seed(1234)
cluster_1 <- kmeans(experiment[, c("age","from_second_store_sales")],3, nstart = 20)

cluster_1
# visualize using ggplot()

cluster_1$cluster <- as.factor(cluster_1$cluster)

cluster_1$withinss

cluster_1$tot.withinss

# visualize clusters


table(cluster_1$cluster)

cluster_1$centers

g1 <- ggplot(experiment, aes(x=age,y=from_second_store_sales, color = cluster_1$cluster)) + 
geom_point() +
ggtitle("Clusters of Consumers intention to keep shopping based on age of the customer and sales from second store onwards") +
xlab("Age") + ylab("From second store sales") 
labs(colour = "Clusters")

g1

library(plotly)
ggplotly(g1)

# managerial implication (write in one line):
# Ans) People in their thirties are the once who shop the most, hence the managers must focus on this age
#      group of people while planning relevant strategies to attract consumers from other age groups.
################################################################################
