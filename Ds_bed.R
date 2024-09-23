library(tidyverse)
library(dslabs)
library("dplyr")
data("murders")
data("movielens")

#murders %>%
#  ggplot(aes(population, total, label=abb,color=region)) + geom_label()


# We may create vectors of class numeric or character with the concatenate function

codes <- c(380,124,818)
country <- c("italy","canada","egypt")

# We can also name the elements of a numeric vector
# Note that the two lines of code below have the same result

codes <- c(italy = 380, canada =124, egypt =818)
codes
codes <- c("italy" =380, "canada" = 124, "egypt" = 818)
codes
# Using square brackets is useful for subsetting to access specific elements of a vector
codes[2]
codes[c(1,3)]
codes[1:3]

# If the entries of a vector are named, they may be accessed by referring to their name
codes["Canada"]
#codes[c("egypt","italy")]

x <- c(1,"canada",3)
x

as.numeric(x)

y <- c(1,3,5)
y
as.character(y)
y

z <- c(31,4,15,92,65)
z
sort(z) #puts elements in order

index <- order(z) #returns index that will put z in order
z[index] #rearranging by this index puts elemennts in order
order(z)

murders$state[1:10]
murders$abb[1:10]

index <- order(murders$total)
murders$abb[index] #order abbreviations by total murders

max(murders$total) #highest number of totals
i_max <- which.max(murders$total) #index with highest number of muders
murders$state[i_max]#state name with highest number of totals

p <- c(31,4,15,92,65)
p
rank(p) #returns ranks (smallest to largest)

structure(murders)

#the name of the state with the maximu population is found by 
murders$state[which.max(murders$population)]

#how to obtain the murder rate
murder_rate <- murders$total / murders$population * 10000

#order the states by murder rate, in a decreasing orde
murders$state[order(murder_rate, decreasing = TRUE)]

t <- c(2, 43, 27, 96, 18)

sort(t)

order(t)

rank(t)

rank(-t)

min(t)

which.min(t)

max(t)

which.max(t)

# Given data
name <- c("Mandi", "Amy", "Nicole", "Olivia")
distance <- c(0.8, 3.1, 2.8, 4.0)
time <- c(10, 30, 40, 50)

# Convert time to hours
time_in_hours <- time / 60

# Calculate speed in miles per hour
speed <- distance / time_in_hours

# Print the results
speed  # This will show the speed for each runner
time_in_hours #time for all runners in hours
time_in_hours[4]  # This will show the time Olivia ran in hours by selecting the 4th elemennt which olivia


# defining murder rate as before
murder_rate <- murders$total / murders$population * 100000

#creating a logical vector that specifies if the murder rate in that state is less than or equal to 0.71
index <- murder_rate <= 0.71

# determining which states have murder rates less than or equal to 0.71
murders$state[index]
# calculating how many states have a murder rate less than or equal to 0.71
sum(index)

# creating the two logical vectors representing our conditions
west <- murders$region == "West"
safe <- murder_rate <= 1
safe

# defining an index and identifying states with both conditions true
index <- safe & west
murders$state[index]

x <- c(FALSE,TRUE,FALSE,TRUE,TRUE,FALSE)
which(x) #returns indices that are TRUE

#to determine the murder rate in Massachusetts we may do the following
index <- which(murders$state == "Massachusetts")
index
murder_rate[index]

#to obtain the indices and subsequent murder rates of New York, Florida, Texas, we do:
ind <- c("New York", "Florida", "Texas")
index <- match(ind, murders$state)

index
murders$state[index]
murder_rate[index]

x <- c("a","b","c","d","e")
y <- c("a","d","f")

!y %in% x

#to see if Boston, Dakota, and Washington are states
c("Boston","Dakota","Washington") %in% murders$state

#adding a column with mutate
library(dslabs)
library(dplyr)
data("murders")
murders <- mutate(murders, rate = total / population * 100000)
murders
#subsetting with filter
filter(murders, rate <= 0.71)

#creating a data frame with stringASFactors = FALSE , however this is nolonger required in R 4.0 
grades <- data.frame(names = c("john", "juan", "jean","Yao"),
                     exam_1 = c(95,80,90,85),
                     exam_2 = c(90,85,89,97),
                     stringsAsFactors = FALSE)

grades <- data.frame(names = c("john", "juan", "jean","Yao"),
                     exam_1 = c(95,80,90,85),
                     exam_2 = c(90,85,89,97))

grades
class(grades$names)

library(dplyr)
library(dslabs)
data("murders")

#a simple scatterplot of total murders versus population
x <- murders$population / 10^6
y <- murders$total

plot(x,y)

#a histogram of murder rates
murders <- mutate(murders, rate = total / population * 100000)
hist(murders$rate)

#boxplotts of murder rates by region
boxplot(rate~region, data = murders)

library(tidyverse)
library(dplyr)
library(dslabs)
data(murders)
murders <- mutate(murders, rate = total / population * 10^5)

#order the states by population size
murders %>% arrange(population) %>% head()

#order the states by murder rate - the default is ascending order
murders %>% arrange(rate) %>% head()

#order the states by murder rate in descending order
murders %>% arrange(desc(rate)) %>% head()

#order the states by region and then by murder rate with region
murders %>% arrange(region,rate) %>% head()

#return the top 10 states by murder rate
murders %>% top_n(10,rate)

#return the top 10 states ranked by murder rate, sorted by murder rate
murders %>% arrange(desc(rate)) %>% top_n(10)


#group by region
murders %>% group_by(region)

#sumarize after grouping 
murders %>%
  group_by(region) %>%
  summarise(median = median(rate))


# minimum, median, and maximum murder rate for the states in the West region
s <- murders %>% 
  filter(region == "West") %>%
  summarize(minimum = min(rate), 
            median = median(rate), 
            maximum = max(rate))
s

#accessinng the components with the accessor $
s$median
s$maximum
#average rate unadjusted by population size
mean(murders$rate)

# average rate adjusted by population size
us_murder_rate <- murders %>% 
  summarize(rate = sum(total) / sum(population) * 10^5)
us_murder_rate

# us_murder_rate is stored as a data frame
class(us_murder_rate)

# the pull function can return it as a numeric value
us_murder_rate %>% pull(rate)

# using pull to save the number directly
us_murder_rate <- murders %>% summarise(rate = sum(total) / sum(population) * 10^5) %>% pull(rate)

us_murder_rate

# us_murder_rate is stored as a data frame
class(us_murder_rate)

# using the dot to access the rate
us_murder_rate <- murders %>% summarise(rate = sum(total) / sum(population) * 10^5) %>% .$rate

us_murder_rate

class(us_murder_rate)

# minimum, median, and maximum murder rate for the states in the West region using quantile
# note that this returns a vector
murders %>% filter(region == "west") %>% summarise(range = quantile(rate, c(0,0.5,1)))

# returning minimum, median, and maximum as a data frame
my_q <- function(x){
  r <- quantile(x, c(0,0.5,1))
  data.frame(minimum = r[1], median =r[2], maximum = r[3])
}

murders %>%filter(region == "West") %>% summarise(my_q(rate))



library(dplyr)
library(NHANES)
data(NHANES)
top_n(NHANES, 20)

ref <- NHANES %>%
  filter(AgeDecade == " 20-29" & Gender == "female") %>%
  summarize(average = mean(BPSysAve, na.rm = TRUE), 
            standard_deviation = sd(BPSysAve, na.rm = TRUE))
ref

#Now we will repeat the exercise and generate only the 
#average blood pressure for 20-29 year old females. 
#For this exercise, you should review how to 
#use the place holder . in dplyr or the pull function.
#Modify the line of sample code to assign 
#the average to a numeric variable called 
#ref_avg using the . or pull.

library(dplyr)
library(NHANES)
data(NHANES)
ref_avg <- NHANES %>%
  filter(AgeDecade == " 20-29" & Gender == "female") %>%
  summarize(average = mean(BPSysAve, na.rm = TRUE)) %>%
  .$average

#Now let's practice using the group_by function.

#What we are about to do is a very common operation in data science: you will split a data table into groups and then compute summary statistics for each group.

#We will compute the average and standard deviation of systolic blood pressure for females for each age group separately. Remember that the age groups are contained in AgeDecade

#Use the functions filter, group_by, summarize, and the pipe %>% to compute the average and standard deviation of systolic blood pressure for females for each age group separately.
#Within summarize, save the average and standard deviation of systolic blood pressure (BPSysAve) as average and standard_deviation.
#Note: ignore warnings about implicit NAs. This warning will not prevent your code from running or being graded correctly.

library(dplyr)
library(NHANES)
data(NHANES)
NHANES %>%
  filter(Gender == "female") %>%
  group_by(AgeDecade) %>%
  summarize(average = mean(BPSysAve, na.rm = TRUE), 
            standard_deviation = sd(BPSysAve, na.rm=TRUE))

NHANES %>%
  group_by(AgeDecade, Gender) %>%
  summarize(average = mean(BPSysAve, na.rm = TRUE), 
            standard_deviation = sd(BPSysAve, na.rm=TRUE))

#Compute the average and standard deviation for each value of Race1 for males in the age decade 40-49.
#Order the resulting table from lowest to highest average systolic blood pressure.
#Use the functions filter, group_by, summarize, arrange, and the pipe %>% to do this in one line of code.
#Within summarize, save the average and standard deviation of systolic blood pressure as average and standard_deviation.

library(dplyr)
library(NHANES)
data(NHANES)
NHANES %>%
  filter(Gender == "male" & AgeDecade==" 40-49") %>%
  group_by(Race1) %>%
  summarize(average = mean(BPSysAve, na.rm = TRUE), 
            standard_deviation = sd(BPSysAve, na.rm=TRUE)) %>%
  arrange(average)


library(data.table)

#load other packages and datasets
library(tidyverse)
library(dplyr)
library(dslabs)
data(murders)

#convert the data into a data.table object
murders <- setDT(murders)

#selecting in dplyr
select(murders, state, region)

#slecting in data.table - 2mothds
murders[, c("state","region")] |> head()
murders[, .(state, region)] |> head()

#adding or changig a column in dplyr
murders <- mutate(murders, rate = total / population * 10^5)

#adding or changing a column in data.table
murders[, rate := total / population * 100000]
head(murders)

murders[, ":="(rate = total / population * 100000, rank = rank(population))]
head(murders)

#y is reffering to x and := changes by refrence

x <- data.table(a=1)
y <- x

x[,a :=2]
y
y[, a := 2]
x

#use copy to make an actual copy 
x <- data.table(a = 1)
y <- copy(x)
x[,a :=2]
y

library(tidyverse)
library(dplyr)
library(dslabs)
data(murders)
library(data.table)
murders <- setDT(murders)
murders <- mutate(murders, rate = total / population * 10^5)
murders[, rate := total / population * 100000]

#subettinng in dplyr
filter(murders, rate <= 0.7)

#subsettig in data.table
murders[rate <= 0.7]

#combig filter and selectt in data.table
murders[rate <= 0.7, .(state, region, rate)]

#combing filter and select in dplyr
murders %>% filter(rate <= 0.7) %>% select(state,rate)

# load packages and prepare the data - heights dataset
library(tidyverse)
library(dplyr)
library(dslabs)
data(heights)
heights <- setDT(heights)

#summarizing in dplyr
s <- heights %>%
  summarize(average = mean(height), standard_dev = sd(height))
s
#summarizing in data.table
s <- heights[, .(average = mean(height), standard_deviattion = sd(height))]
s

#subsettinng and summarizinng in dplyr
s <- heights %>%
  filter(sex == "Female") %>%
  summarise(average = mean(height), standard_dev = sd(height))

s
#subset and summarize. in data table 
s <- heights[sex == "Female", .(average = mean(height), stddv = sd(height))]
s

#previously defined function
median_min_max <- function(x){
  qs <- quantile(x,c(0.5,0,1))
  data.frame(median = qs[1], minimum = qs[2], maximum = qs[3])
}

#multiple summaries in datable would be
heights[, .(median_min_max(height))]

#group then sumarize in data.table
heights[, .(average = mean(height), sdd = sd(height)), by = sex]

#order by population 
murders[order(population)] |> head()
#order by population in descending order
murders[order(population, decreasing = TRUE)] |> head()
#order by region and then murer rate
murders[order(region, rate)] |> head()

#View the dataset
murders %>% group_by(region)

#see the class
murders %>% group_by(region) %>% class()

#comparison , regular df from a tibble
class(murders[,1])
class(as_tibble(murders)[,1])

#access a column vector not as a tibble using $
class(as_tibble(murders)$state)

## compare what happens when accessing a column that doesn't exist 
#in a regular data frame to in a tibble
murders$State
as_tibble(murders$State)

#tocreate a tibble
#tibble(id = c(1,2,3), func = c(mean, median, sd)

# First, determine the average height in this dataset. Then create a logical vector ind with the indices for those individuals who are above average height.
# 
# How many individuals in the dataset are above average height?

library(dslabs)
data(heights)
options(digits = 3)    # report 3 significant digits for all answers

averageH <- mean(heights$height)
averageH

ind <- heights %>% filter(height > averageH)
ind
count(ind)

#How many individuals in the dataset are above 
#average height and are female?

female <- heights %>% filter(sex == "Female") %>% filter(height > averageH)

count(female)
#What proportion of individuals in the dataset are female?

prop.table(table(heights$sex))

#Determine the minimum height in the heights dataset.

MH <- min(heights$height)
MH
#Use the match() function to determine the index of the first individual with the minimum height.
indMH <- match(MH , heights$height)
indMH

#Subset the sex column of the dataset by 
#the index in 4b to determine the individual’s sex.

IndSex <- heights$sex[indMH]
IndSex

#Determine the maximum height.
maxH <- max(heights$height)
maxH

# Which integer values are between the maximum and minimum heights? For example, 
# if the minimum height is 10.2 and the maximum height is 20.8, your answer should be x <- 11:20 
# to capture the integers in between those values. (If either the maximum or minimum height are integers,
#                                                   include those values too.)
# 
# Write code to create a vector x that includes the integers between the minimum and maximum 
# heights in this dataset (as numbers).

x <- seq(MH,maxH,1)

# How many of the integers in x are NOT heights in the dataset?
#   Use the sum() and %in% functions in addition to the ! operator.

sum(!x %in% heights$height)

# Using the heights dataset, create a new column of heights in centimeters named ht_cm. Recall 
# that 1 inch = 2.54 centimeters. Save the resulting dataset as heights2.
#What is the height in centimeters of the 18th individual (index 18)?

heights2 <- heights %>% mutate(ht_cm = heights$height * 2.54) 
head(heights2)

h18 <- heights2$ht_cm[18]
h18

#What is the mean height in centimeters?
MHC <- mean(heights2$ht_cm)
MHC

#Create a data frame females by filtering the heights2 data to contain only female individuals.
#How many females are in the heights2 dataset?
females <- heights2 %>% filter(heights2$sex == "Female")

count(females)

mHF <- mean(females$ht_cm)
mHF

library(dslabs)
data(olive)
head(olive)

#Plot the percent palmitic acid versus palmitoleic acid in a scatterplot. What relationship do you see?
#Ans:there is a postive liner relationship between palmitic and palmitoleic
x <- olive$palmitic

y <- olive$palmitoleic

plot(x,y)

#Create a histogram of the percentage of eicosenoic acid in olive.

hist(olive$eicosenoic)

#Make a boxplot of palmitic acid percentage in olive with separate distributions for each region.

boxplot(olive$palmitic ~ region, data = olive)

#general structure of if-else
a <- 0
if (a! = 0)
{
  print(1/a)
}else{
  print("No reciprocal for 0.")
}

#an example that tells us which states, if any, have a murder rate less than 0.5
library(dslabs)
data(murders)
murder_rate <- murders$total / murders$population * 100000
ind <- which.min(murder_rate)

if(murder_rate[ind] < 0.5)
{
  print(murders$state[ind])
}else {
  print ("no state has murder rate low ")
}

# changing the condition to < 0.25 changes the result
if(murder_rate[ind] < 0.25){
  print(murders$state[ind]) 
} else{
  print("No state has a murder rate that low.")
}
# the ifelse() function works similarly to an if-else conditional
a <- 0
ifelse(a > 0, 1/a, NA)

# the ifelse() function is particularly useful on vectors
a <- c(0,1,2,-4,5)
result <- ifelse(a>0, 1/a, NA)

# the ifelse() function is also helpful for replacing missing values

data(na_example)
no_nas <- ifesle(is.na(na_example), 0, na_example)
sum(is.na(no_nas))

# the any() and all() functions evaluate logical vectors
z <- c(TRUE, TRUE, FALSE)
any(z)
all(z)

# example of defining a function to compute the average of a vector x
avg <- function(x){
  s <- sum(x)
  n <- length(x)
  s/n
}

# we see that the above function and the pre-built R mean() function are identical
x <- 1:100
identical(mean(x), avg(x))

# functions can have multiple arguments as well as default values
avg <- function(x, arithmetic = TRUE){
  n <- length(x)
  ifelse(arithmetic, sum(x)/n, prod(x)^(1/n))
}

## creating a function that computes the sum of integers 1 through n
compute_s_n <- function(n){
  x <- 1:n
  sum(x)
}


#a very simple for-looop
for (i in 1:5)
{
  print(i)
}

#a for loop for our summation
m <- 25
s_n <- vector(length = m) #creating an empty vector
for (n in 1:m)
{
  s_n[n] <- compute_s_n(n)
}

#creating a plot for our summation function
n <- 1:m
plot(n, s_n)

#a table of values comparing our function to the summtion formula
head(data.frame(s_n = s_n, formula = n*(n+1)/2))

#overlaying our function with the summation formula

plot(n, s_n)
lines(n, n* (n+1)/2)

#last value in the vector 
test <- vector(length = 5)
for (i in 1:5){
  test[i] <- i^2
}
i
test


x <- c(1,2,-3,4)
if(all(x>0)){
  print("All Positives")
} else{
  print("Not All Positives")
}

#load the dataset
library(dslabs)
data(heights)


#make a table of category propotions
prop.table(table(heights$sex))

#define x as vector of male heights 
library(tidyverse)
library(dslabs)
data(heights)
index <- heights$sex == "Male"
x <- heights$height[index]

# calculate the mean and standard deviation manually
average <- sum(x)/length(x)
SD <- sqrt(sum((x - average)^2)/length(x))

# built-in mean and sd functions
average <- mean(x)
SD <- sd(x)
c(average = average, SD = SD)


# calculate standard units
z <- scale(x)

# calculate proportion of values within 2 SD of mean
mean(abs(z) < 2)

library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)
#We can estimate the probability that a male is taller than 70.5 inches with:

1 - pnorm(70.5, mean(x), sd(x))

table(x)

prop.table(table(x))
# plot distribution of exact heights in data
plot(prop.table(table(x)), xlab = "a = Height in inches", ylab = "Pr(x = a)")

# probabilities in actual data over length 1 ranges containing an integer
mean(x <= 68.5) - mean(x <= 67.5)
mean(x <= 69.5) - mean(x <= 68.5)
mean(x <= 70.5) - mean(x <= 69.5)

# probabilities in normal approximation match well
pnorm(68.5, mean(x), sd(x)) - pnorm(67.5, mean(x), sd(x))
pnorm(69.5, mean(x), sd(x)) - pnorm(68.5, mean(x), sd(x))
pnorm(70.5, mean(x), sd(x)) - pnorm(69.5, mean(x), sd(x))

# probabilities in actual data over other ranges don't match normal approx as well
mean(x <= 70.9) - mean(x <= 70.1)
pnorm(70.9, mean(x), sd(x)) - pnorm(70.1, mean(x), sd(x))


# What proportion of the data is between 69 and 72 inches (taller than 69 but shorter or equal to 72)? A proportion is between 0 and 1.
# Use the mean function in your code. Remember that you can use mean to compute the proportion of entries of a logical vector that are TRUE.

library(dslabs)
data(heights)
x <- heights$height[heights$sex == "Male"]

mean(x <= 72) - mean(x <= 69)

# Suppose you only have avg and stdev below, but no access to x, can you approximate the proportion of the data that is between 69 and 72 inches?
#   
#   Given a normal distribution with a mean mu and standard deviation sigma, you can calculate the proportion of observations less than or equal to a certain value with pnorm(value, mu, sigma). Notice that this is the CDF for the normal distribution. We will learn much more about pnorm later in the course series, but you can also learn more now with ?pnorm.
# Use the normal approximation to estimate the proportion the proportion of the data that is between 69 and 72 inches.
# Note that you can't use x in your code, only avg and stdev. Also note that R has a function that may prove very helpful here - check out the pnorm function (and remember that you can get help by using ?pnorm).

library(dslabs)
data(heights)
x <- heights$height[heights$sex=="Male"]
avg <- mean(x)
stdev <- sd(x)

pnorm(72, avg, stdev) - pnorm(69, avg, stdev)

# # Use normal approximation to estimate the proportion of heights between 79 and 81 
# inches and save it in an object called approx.
# # Report how many times bigger the actual proportion is compared to the approximation.

library(dslabs)
data(heights)
x <- heights$height[heights$sex == "Male"]
avg <- mean(x)
stdev <- sd(x)
exact <- mean(x>79 & x<=81)
approx <- pnorm(81, avg, stdev) - pnorm(79, avg, stdev)
exact/approx


# # Someone asks you what percent of seven footers are in the National Basketball Association (NBA).
# Can you provide an estimate? Let's try using the normal approximation to answer this question.
# # 
# # Given a normal distribution with a mean mu and standard deviation sigma, you can calculate 
# the proportion of observations less than or equal to a certain value with pnorm(value, mu, sigma).
# Notice that this is the CDF for the normal distribution. We will learn much more about pnorm later
# in the course series, but you can also learn more now with ?pnorm.
# # 
# # First, we will estimate the proportion of adult men that are taller than 7 feet.
# # 
# # Assume that the distribution of adult men in the world as normally distributed with an 
# average of 69 inches and a standard deviation of 3 inches.
# 
# # Using the normal approximation, estimate the proportion of adult men that are taller than 7 feet,
# referred to as seven footers. Remember that 1 foot equals 12 inches.
# # Use the pnorm function. Note that pnorm finds the proportion less than or equal to a given value,
# but you are asked to find the proportion greater than that value.
# # Print out your estimate; don't store it in an object.

1 - pnorm(7*12, 69, 3)


# Now we have an approximation for the proportion, call it p, of men that are 7 feet tall or taller.
# 
# We know that there are about 1 billion men between the ages of 18 and 40 in the world, the age range for the NBA.
# 
# Can we use the normal distribution to estimate how many of these 1 billion men are at least seven feet tall?

# # Use your answer to the previous exercise to estimate the proportion of men that are seven feet tall 
# or taller in the world and store that value as p.
# # Then multiply this value by 1 billion (10^9) round the number of 18-40 year old men who are seven f
# eet tall or taller to the nearest integer with round. (Do not store this value in an object.)

p <- 1 - pnorm(7*12, 69, 3)
round(p * 10^9)

# # There are about 10 National Basketball Association (NBA) players that are 7 feet tall or higher.
# # Use your answer to exercise 4 to estimate the proportion of men that are seven 
# feet tall or taller in the world and store that value as p.
# # Use your answer to the previous exercise (exercise 5) to round the number of 
# 18-40 year old men who are seven feet tall or taller to the nearest integer and store that value as N.
# # Then calculate the proportion of the world's 18 to 40 year old seven footers 
# that are in the NBA. (Do not store this value in an object.)

p <- 1 - pnorm(7*12, 69, 3)
N <- round(p * 10^9)
10/N


# In the previous exerceise we estimated the proportion of seven footers in the NBA using this simple code:
#   
#   p <- 1 - pnorm(7*12, 69, 3)
# N <- round(p * 10^9)
# 10/N
# Repeat the calculations performed in the previous question for Lebron James' height: 6 feet 8 inches. There are about 150 players, instead of 10, that are at least that tall in the NBA.
# 
# Instructions
# 0 XP
# Report the estimated proportion of people at least Lebron's height that are in the NBA.
p <- 1 - pnorm(6*12 + 8, 69, 3)
N <- round(p * 10^9)
150/N


library(dslabs)
data(heights)
#Use summary() on the heights$height variable to find the quartiles:
  
summary(heights$height)


#Find the percentiles of heights$height:
  
p <- seq(0.01, 0.99, 0.01)
p
percentiles <- quantile(heights$height, p)
percentiles
# Confirm that the 25th and 75th percentiles match the 1st and 3rd quartiles. Note that quantile() 
# returns a named vector. You can access the 25th and 75th percentiles like this (adapt the code for other percentile values):
  
percentiles[names(percentiles) == "25%"]
percentiles[names(percentiles) == "75%"]
percentiles[names(percentiles) == "85%"]

# Example test scores
test_scores <- c(60, 70, 75, 80, 85, 90, 92, 95, 98, 100)

summary(test_scores)

# define x and z
library(tidyverse)
library(dslabs)
data(heights)
index <- heights$sex=="Male"
x <- heights$height[index]
z <- scale(x)

# proportion of data below 69.5
mean(x <= 69.5)

# calculate observed and theoretical quantiles
p <- seq(0.05, 0.95, 0.05)

observed_quantiles <- quantile(x, p)
theoretical_quantiles <- qnorm(p, mean = mean(x), sd = sd(x))

# make QQ-plot
plot(theoretical_quantiles, observed_quantiles)
abline(0,1)

# make QQ-plot with scaled values
observed_quantiles <- quantile(z, p)
theoretical_quantiles <- qnorm(p)
plot(theoretical_quantiles, observed_quantiles)

abline(0,1)

# Create two five row vectors showing the 10th, 30th, 50th, 70th, and 90th percentiles for 
# the heights of each sex called these vectors female_percentiles and male_percentiles.
# Then create a data frame called df with these two vectors as columns. The column names
# should be female and male and should appear in 
# that order. As an example consider that if you want a data frame to have column names 
# names and grades, in that order, you do it like this:

# Assuming you have a data frame 'heights' with columns 'height' and 'sex'

# # Extract heights for females and males
# library(dslabs)
# data(heights)
# female_heights <- heights$height[heights$sex == "female"]
# male_heights <- heights$height[heights$sex == "male"]
# 
# # Define percentiles
# percentiles <- c(0.1, 0.3, 0.5, 0.7, 0.9)
# 
# # Calculate percentiles for females
# female_percentiles <- quantile(female_heights, percentiles)
# female_percentiles
# # Calculate percentiles for males
# male_percentiles <- quantile(male_heights, percentiles)
# 
# # Create a data frame
# df <- data.frame(female = female_percentiles, male = male_percentiles)
# 
# # Print the data frame
# print(df)

library(dslabs)
data(heights)
male <- heights$height[heights$sex=="Male"]
female <- heights$height[heights$sex=="Female"]

percentiles <- c(0.1,0.3,0.5,0.7,0.9)

female_percentiles <- quantile(female, percentiles)

male_percentiles <- quantile(male, percentiles)

df <- data.frame(female = female_percentiles, male = male_percentiles)

df


# Write a function called error_avg that takes a value k and returns the average 
# of the vector x after the first entry changed to k. Show the results for k=10000 and k=-10000.
x <- Galton$child

error_avg <- function(k){
  x[1] <- k
  mean(x)
}

error_avg(10000)
error_avg(-10000)

library(tidyverse)
library(dslabs)
data(murders)
# ggplot(data = murders)
# murders %>% ggplot()
# murders %>% ggplot() + geom_point(aes(x = population/10^6, y =total))
# 
# #add points layer to predefined ggplot object
# 
# p <- ggplot(data = murders)
# class(p)
# print(p) + geom_point(aes(population/10^6, total))

#add text layer to scatterplot
# p + geom_point(aes(population/10^6, total),size = 3) + 
#   geom_text(aes(population/10^6, total, label=abb), nudge_x = 1)
# change the size of the points
## move text labels slightly to the right
# p + geom_point(aes(population/10^6, total), size = 3) +
#   geom_text(aes(population/10^6), total, label=abb)

# simplify code by adding global aesthetic
# p <- murders %>% ggplot(aes(population/10^6, total, label = abb)) +
#   geom_point(size = 3) +
#   geom_text(nudge_x = 0.5) +
#   # log base 10 scale the x-axis and y-axis
#   scale_x_continuous(trans = "log10") +
#   scale_y_continuous(trans = "log10")

p <- murders %>% ggplot(aes(population/10^6, total, label = abb))

# log base 10 scale the x-axis and y-axis
p + geom_point(size = 3) +
  geom_text(nudge_x = 0.05) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10")

# efficient log scaling of the axes
p + geom_point(size = 3) +
  geom_text(nudge_x = 0.075) +
  scale_x_log10() +
  scale_y_log10()

#Add lables and titles 
p + geom_point(size = 3) +
  geom_text(nudge_x = 0.075) +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Population in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010")

#change color points and redfine p to be everything execpt the points layer 
p <- murders %>%
  ggplot(aes(population/10^6, total, label = abb)) +
  geom_text(nudge_x = 0.075) +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Population in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010")

#make all points blue 
p + geom_point(size = 3, color = "blue")

#color points by region 
p + geom_point(aes(col = region), size = 3)

#Add a line with average murder rate
#define average murder rate 
r <- murders %>%
  summarise(rate = sum(total) / sum(population) * 10^6) %>% pull(rate)

# basic line with average murder rate for the country
p <- p + geom_point(aes(col = region), size = 3) +
  geom_abline(intercept = log10(r))    # slope is default of 1

# change line to dashed and dark grey, line under points
p + 
  geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
  geom_point(aes(col = region), size = 3)

#Change legend title
p <- p + scale_color_discrete(name = "Region")    # capitalize legend title


# theme used for graphs in the textbook and course
library(dslabs)
ds_theme_set()

# themes from ggthemes
library(ggthemes)
p + theme_economist()    # style of the Economist magazine
p + theme_fivethirtyeight()    # style of the FiveThirtyEight website

# load libraries
library(tidyverse)
library(ggrepel)
library(ggthemes)
library(dslabs)
data(murders)

# define the intercept
r <- murders %>%
  summarize(rate = sum(total) / sum(population) * 10^6) %>%
  .$rate

# make the plot, combining all elements
murders %>%
  ggplot(aes(population/10^6, total, label = abb)) +
  geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
  geom_point(aes(col = region), size = 3) +
  geom_text_repel() +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Population in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010") +
  scale_color_discrete(name = "Region") +
  theme_economist()


#Histograms in ggplot2
# load heights data
library(tidyverse)
library(dslabs)
data(heights)

# define p
p <- heights %>%
  filter(sex == "Male") %>%
  ggplot(aes(x = height))

# dot plot showing the data
heights %>% ggplot(aes(sex, height)) + geom_point()

# jittered, alpha blended point plot
heights %>% ggplot(aes(sex, height)) + geom_jitter(width = 0.1, alpha = 0.2)

# basic histograms
p + geom_histogram()
p + geom_histogram(binwidth = 1)

# histogram with blue fill, black outline, labels and title
p + geom_histogram(binwidth = 1, fill = "blue", col = "black") +
  xlab("Male heights in inches") +
  ggtitle("Histogram")

#Smooth density plots in ggplot2
p + geom_density()
p + geom_density(fill = "blue")

#Quantile-quantile plots in ggplot2
# basic QQ-plot
p <- heights %>% filter(sex == "Male") %>%
  ggplot(aes(sample = height))
p + geom_qq()

# QQ-plot against a normal distribution with same mean/sd as data
params <- heights %>%
  filter(sex == "Male") %>%
  summarize(mean = mean(height), sd = sd(height))
p + geom_qq(dparams = params) +
  geom_abline()

# QQ-plot of scaled data against the standard normal distribution
heights %>%
  ggplot(aes(sample = scale(height))) +
  geom_qq() +
  geom_abline()

#Grids of plots with the gridExtra package
# define plots p1, p2, p3
p <- heights %>% filter(sex == "Male") %>% ggplot(aes(x = height))
p1 <- p + geom_histogram(binwidth = 1, fill = "blue", col = "black")
p2 <- p + geom_histogram(binwidth = 2, fill = "blue", col = "black")
p3 <- p + geom_histogram(binwidth = 3, fill = "blue", col = "black")

# arrange plots next to each other in 1 row, 3 columns
library(gridExtra)
grid.arrange(p1, p2, p3, ncol = 3)


# library(dplyr)
# library(ggplot2)
# library(dslabs)
# data(heights)
# data(murders)
# murders %>% ggplot(aes(population, total, label = abb)) +
#   geom_label()

# Create separate smooth density plots for males and females by
# defining group by sex. Use the existing aes function inside of 
# the ggplot function.
# heights %>% 
#   ggplot(aes(height, group = sex)) + 
#   geom_density()


# We can also assign groups through the color or fill argument. 
# For example, if you type color = sex ggplot knows you want a 
# different color for each sex. So two densities must be drawn. 
# You can therefore skip the group = sex mapping. Using color has 
# the added benefit that it uses color to distinguish the groups.
# heights %>% 
#   ggplot(aes(height, color = sex)) + geom_density()

# However, here the second density is drawn over the other.
# We can change this by using something called alpha blending.
# 
# heights %>% 
#   ggplot(aes(height, fill = sex)) + 
#   geom_density() 
# 
# Set the alpha parameter to 0.2 in 
# the geom_density function to make this change.
# 
# heights %>% 
#   ggplot(aes(height, fill = sex)) + 
#   geom_density(alpha = 0.2)

# load and inspect gapminder data
library(dslabs)
data(gapminder)
head(gapminder)

# compare infant mortality in Sri Lanka and Turkey
gapminder %>%
  filter(year == 2015 & country %in% c("Sri Lanka", "Turkey")) %>%
  select(country,infant_mortality)


# basic scatterplot of life expectancy versus fertility
ds_theme_set()    # set plot theme
filter(gapminder, year == 1962) %>%
  ggplot(aes(fertility, life_expectancy)) +
  geom_point()


# add color as continent
filter(gapminder, year == 1962) %>%
  ggplot(aes(fertility, life_expectancy, color = continent)) +
  geom_point()

# facet by continent and year
filter(gapminder, year %in% c(1962, 2012)) %>%
  ggplot(aes(fertility, life_expectancy, col = continent)) +
  geom_point() +
  facet_grid(continent ~ year)

# facet by year only
filter(gapminder, year %in% c(1962, 2012)) %>%
  ggplot(aes(fertility, life_expectancy, col = continent)) +
  geom_point() +
  facet_grid(. ~ year)

# facet by year, plots wrapped onto multiple rows
years <- c(1962, 1980, 1990, 2000, 2012)
continents <- c("Europe", "Asia")
gapminder %>%
  filter(year %in% years & continent %in% continents) %>%
  ggplot(aes(fertility, life_expectancy, col = continent)) +
  geom_point() +
  facet_wrap(~year)

# scatterplot of US fertility by year
gapminder %>%
  filter(country == "United States") %>%
  ggplot(aes(year, fertility)) +
  geom_point()

# line plot of US fertility by year
gapminder %>%
  filter(country == "United States") %>%
  ggplot(aes(year, fertility)) +
  geom_line()

# line plot fertility time series for two countries- only one line (incorrect)
countries <- c("South Korea", "Germany")
gapminder %>% filter(country %in% countries) %>%
  ggplot(aes(year, fertility)) +
  geom_line()

# line plot fertility time series for two countries - one line per country
gapminder %>% filter(country %in% countries) %>%
  ggplot(aes(year, fertility, group = country)) +
  geom_line()

# fertility time series for two countries - lines colored by country
gapminder %>% filter(country %in% countries) %>%
  ggplot(aes(year, fertility, col = country)) +
  geom_line()

# life expectancy time series - lines colored by country and labeled, no legend
labels <- data.frame(country = countries, x = c(1975, 1965), y = c(60, 72))
gapminder %>% filter(country %in% countries) %>%
  ggplot(aes(year, life_expectancy, col = country)) +
  geom_line() +
  geom_text(data = labels, aes(x, y, label = country), size = 5) +
  theme(legend.position = "none")

# add dollars per day variable
gapminder <- gapminder %>%
  mutate(dollars_per_day = gdp/population/365)

# histogram of dollars per day
past_year <- 1970
gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black")

# repeat histogram with log2 scaled data
gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(log2(dollars_per_day))) +
  geom_histogram(binwidth = 1, color = "black")

# repeat histogram with log2 scaled x-axis
gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2")

# add dollars per day variable
gapminder <- gapminder %>%
  mutate(dollars_per_day = gdp/population/365)

# number of regions
length(levels(gapminder$region))

# boxplot of GDP by region in 1970
past_year <- 1970
p <- gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(region, dollars_per_day))
p + geom_boxplot()

# rotate names on x-axis
p + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# reorder by median income and color by continent
p <- gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%    # reorder
  ggplot(aes(region, dollars_per_day, fill = continent)) +    # color by continent
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("")
p

# log2 scale y-axis
p + scale_y_continuous(trans = "log2")

# add data points
p + scale_y_continuous(trans = "log2") + geom_point(show.legend = FALSE)

library(tidyverse)
library(dplyr)
# add dollars per day variable and define past year
gapminder <- gapminder %>%
  mutate(dollars_per_day = gdp/population/365)
past_year <- 1970

# define Western countries
west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")

# facet by West vs devloping
gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2") +
  facet_grid(. ~ group)

# facet by West/developing and year
present_year <- 2010
gapminder %>%
  filter(year %in% c(past_year, present_year) & !is.na(gdp)) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2") +
  facet_grid(year ~ group)

# define countries that have data available in both years
country_list_1 <- gapminder %>%
  filter(year == past_year & !is.na(dollars_per_day)) %>% .$country
country_list_2 <- gapminder %>%
  filter(year == present_year & !is.na(dollars_per_day)) %>% .$country
country_list <- intersect(country_list_1, country_list_2)

# make histogram including only countries with data available in both years
gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%    # keep only selected countries
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2") +
  facet_grid(year ~ group)

p <- gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%
  ggplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") + scale_y_continuous(trans = "log2")

p + geom_boxplot(aes(region, dollars_per_day, fill = continent)) +
  facet_grid(year ~ .)

# arrange matching boxplots next to each other, colored by year
p + geom_boxplot(aes(region, dollars_per_day, fill = factor(year)))

# see the code below the previous video for variable definitions

# smooth density plots - area under each curve adds to 1
gapminder %>%
  filter(year == past_year & country %in% country_list) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>% group_by(group) %>%
  summarize(n = n()) %>% knitr::kable()

# smooth density plots - variable counts on y-axis
p <- gapminder %>%
  filter(year == past_year & country %in% country_list) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day, y = ..count.., fill = group)) +
  scale_x_continuous(trans = "log2")
p + geom_density(alpha = 0.2, bw = 0.75) + facet_grid(year ~ .)

# add group as a factor, grouping regions
gapminder <- gapminder %>%
  mutate(group = case_when(
    .$region %in% west ~ "West",
    .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    .$region %in% c("Caribbean", "Central America", "South America") ~ "Latin America",
    .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
    TRUE ~ "Others"))

# reorder factor levels
gapminder <- gapminder %>%
  mutate(group = factor(group, levels = c("Others", "Latin America", "East Asia", "Sub-Saharan Africa", "West")))
# note you must redefine p with the new gapminder object first
p <- gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  ggplot(aes(dollars_per_day, fill = group)) +
  scale_x_continuous(trans = "log2")

# stacked density plot
p + geom_density(alpha = 0.2, bw = 0.75, position = "stack") +
  facet_grid(year ~ .)

# weighted stacked density plot
gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  group_by(year) %>%
  mutate(weight = population/sum(population*2)) %>%
  ungroup() %>%
  ggplot(aes(dollars_per_day, fill = group, weight = weight)) +
  scale_x_continuous(trans = "log2") +
  geom_density(alpha = 0.2, bw = 0.75, position = "stack") + facet_grid(year ~ .)


# Using ggplot and the points layer, create a scatter plot of life 
# expectancy versus fertility for the African continent in 2012.
library(dplyr)
library(ggplot2)
library(dslabs)
data(gapminder)
## fill out the missing parts in filter and aes
gapminder %>% filter(year == 2012 &  continent %in% c("Africa") ) %>%
  ggplot(aes(fertility,life_expectancy,col = country)) +
  geom_point()

# Remake the plot from the previous exercises but this time use color to distinguish 
# the different regions of Africa to see if this explains the clusters. Remember that 
# you can explore the gapminder data to see how the regions of Africa are labeled in 
# the data frame!

gapminder %>% filter(year == 2012 &  continent %in% c("Africa") ) %>%
  ggplot(aes(fertility,life_expectancy , color = region )) +
  geom_point()

# Create a table showing the country and region for the African countries (use select) 
# that in 2012 had fertility rates of 3 or less and life expectancies of at least 70.
# Assign your result to a data frame called df.

df <- gapminder %>% filter(year == 2012 &  continent %in% c("Africa") & fertility <=3 & life_expectancy >= 70 ) %>%
  select("country","region")

# Use filter to create a table with data for the years from 1960 to 2010 in 
# Vietnam and the United States.
# Save the table in an object called tab.

years <- 1960:2010
countries <- c("United States", "Vietnam")
tab <- gapminder %>% filter(year %in% years & country %in% countries)

# Use geom_line to plot life expectancy vs year for Vietnam and the United States 
# and save the plot as p. The data table is stored in tab.
# Use color to distinguish the two countries.
# Print the object p.

p <- tab %>% ggplot(aes(x=year,y=life_expectancy,color=country)) + geom_line()
p

# p <- tab %>% ggplot(aes(year,life_expectancy,color=country)) + geom_line()
# p

# Use a single line of code to create a time series plot from 1960 to 2010 of 
# life expectancy vs year for Cambodia.
years <- 1960:2010
countries <- c("Cambodia")
gapminder %>% filter(year %in% years & country %in% countries)%>%
  ggplot(aes(year,life_expectancy)) + geom_line()

# Use mutate to create a dollars_per_day variable, which is defined as gdp/population/365.
# Create the dollars_per_day variable for African countries for the year 2010.
# Remove any NA values.
# Save the mutated dataset as daydollars.

library(dplyr)
library(dslabs)
data(gapminder)
daydollars <- gapminder %>% mutate(dollars_per_day = gdp/population/365) %>% 
  filter(continent == "Africa" & year == 2010 & !is.na(dollars_per_day)) 


# The dataset including the dollars_per_day variable is preloaded as daydollars.
# Create a smooth density plot of dollars per day from daydollars.
# Use scale_x_continuous to change the x-axis to a log (base 2) scale.
daydollars %>% ggplot(aes(dollars_per_day)) + 
  geom_density() + scale_x_continuous(trans = "log2")

# Create the dollars_per_day variable as in Exercise 7, but for African countries 
# in the years 1970 and 2010 this time.
# Make sure you remove any NA values.
# Create a smooth density plot of dollars per day for 1970 and 2010 using a 
# log (base 2) scale for the x axis.
# Use facet_grid to show a different density plot for 1970 and 2010.

gapminder %>% 
  mutate(dollars_per_day = gdp/population/365) %>%
  filter(continent == "Africa" & year %in% c(1970,2010) & !is.na(dollars_per_day)) %>%
  ggplot(aes(dollars_per_day)) + 
  geom_density() + 
  scale_x_continuous(trans = "log2") + 
  facet_grid(year ~ .)

# Much of the code will be the same as in Exercise 9:
#   Create the dollars_per_day variable as in Exercise 7, 
# but for African countries in the years 1970 and 2010 this time.
# Make sure you remove any NA values.
# Create a smooth density plot of dollars per day for 1970 and 2010
# using a log (base 2) scale for the x axis.
# Use facet_grid to show a different density plot for 1970 and 2010.
# Make sure the densities are smooth by using bw = 0.5.
# Use the fill and position arguments where appropriate to create 
# the stacked density plot of each region.


gapminder %>% 
  mutate(dollars_per_day = gdp/population/365) %>%
  filter(continent == "Africa" & year %in% c(1970,2010) & !is.na(dollars_per_day)) %>%  
  ggplot(aes(dollars_per_day, fill = region)) + 
  geom_density(bw=0.5, position = "stack") + 
  scale_x_continuous(trans = "log2") + 
  facet_grid(year ~ .)

# Generate dollars_per_day using mutate and filter for the year 2010 for African countries.
# Remember to remove NA values.
# Store the mutated dataset in gapminder_Africa_2010.
# Make a scatter plot of infant_mortality versus dollars_per_day 
# for countries in the African continent.
# Use color to denote the different regions of Africa.
#   

gapminder_Africa_2010 <- gapminder %>% 
  mutate(dollars_per_day = gdp/population/365) %>%
  filter(continent == "Africa" & year == 2010 & !is.na(dollars_per_day) & !is.na(infant_mortality))

gapminder_Africa_2010 %>%  ggplot(aes(dollars_per_day, infant_mortality, color = region)) +
  geom_point()

# The mutated dataset is preloaded as gapminder_Africa_2010.
# As in the previous exercise, make a scatter plot of infant_mortality versus 
# dollars_per_day for countries in the African continent.
# As in the previous exercise, use color to denote the different regions of Africa.
# Transform the x axis to be in the log (base 2) scale.

gapminder_Africa_2010 %>% 
  ggplot(aes(dollars_per_day, infant_mortality, color = region)) +
  geom_point() + 
  scale_x_continuous(trans = "log2")

# The mutated dataset is preloaded as gapminder_Africa_2010.
# As in the previous exercise, make a scatter plot of infant_mortality 
# versus dollars_per_day for countries in the African continent.
# As in the previous exercise, use color to denote the different regions of Africa.
# As in the previous exercise, transform the x axis to be in the log (base 2) scale.
# Add a geom_text layer to display country names in addition to of points.

gapminder_Africa_2010 %>% 
  ggplot(aes(dollars_per_day, infant_mortality, color = region, label = country)) +
  geom_text() + 
  scale_x_continuous(trans = "log2")

# Generate dollars_per_day using mutate and filter for the years 1970 and 2010 for 
# African countries.
# Remember to remove NA values.
# As in the previous exercise, make a scatter plot of infant_mortality versus 
# dollars_per_day for countries in the African continent.
# As in the previous exercise, use color to denote the different regions of Africa.
# As in the previous exercise, transform the x axis to be in the log (base 2) scale.
# As in the previous exercise, add a layer to display country names instead of points.
# Use facet_grid to show different plots for 1970 and 2010. Align the plots vertically.


gapminder %>% 
  mutate(dollars_per_day = gdp/population/365) %>%
  filter(continent == "Africa" & year %in% c(1970, 2010) & !is.na(dollars_per_day) 
         & !is.na(infant_mortality)) %>%
  ggplot(aes(dollars_per_day, infant_mortality, color = region, label = country)) +
  geom_text() + 
  scale_x_continuous(trans = "log2") +
  facet_grid(year~.)

#Slope chart
library(tidyverse)
library(dslabs)
data(gapminder)

west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")

dat <- gapminder %>%
  filter(year %in% c(2010, 2015) & region %in% west & !is.na(life_expectancy) & population > 10^7)

dat %>%
  mutate(location = ifelse(year == 2010, 1, 2),
         location = ifelse(year == 2015 & country %in% c("United Kingdom", "Portugal"),
                           location + 0.22, location),
         hjust = ifelse(year == 2010, 1, 0)) %>%
  mutate(year = as.factor(year)) %>%
  ggplot(aes(year, life_expectancy, group = country)) +
  geom_line(aes(color = country), show.legend = FALSE) +
  geom_text(aes(x = location, label = country, hjust = hjust), show.legend = FALSE) +
  xlab("") +
  ylab("Life Expectancy") 

#Bland-Altman plot
library(ggrepel)
dat %>%
  mutate(year = paste0("life_expectancy_", year)) %>%
  select(country, year, life_expectancy) %>% spread(year, life_expectancy) %>%
  mutate(average = (life_expectancy_2015 + life_expectancy_2010)/2,
         difference = life_expectancy_2015 - life_expectancy_2010) %>%
  ggplot(aes(average, difference, label = country)) +
  geom_point() +
  geom_text_repel() +
  geom_abline(lty = 2) +
  xlab("Average of 2010 and 2015") +
  ylab("Difference between 2015 and 2010")

#Tile plot of measles rate by year and state
# import data and inspect
library(tidyverse)
library(dslabs)
data(us_contagious_diseases)
str(us_contagious_diseases)

# assign dat to the per 10,000 rate of measles, removing Alaska and Hawaii 
#and adjusting for weeks reporting
the_disease <- "Measles"
dat <- us_contagious_diseases %>%
  filter(!state %in% c("Hawaii", "Alaska") & disease == the_disease) %>%
  mutate(rate = count / population * 10000 * 52/weeks_reporting) %>%
  mutate(state = reorder(state, rate))

# plot disease rates per year in California
dat %>% filter(state == "California" & !is.na(rate)) %>%
  ggplot(aes(year, rate)) +
  geom_line() +
  ylab("Cases per 10,000") +
  geom_vline(xintercept=1963, col = "blue")

# tile plot of disease rate by state and year
dat %>% ggplot(aes(year, state, fill=rate)) +
  geom_tile(color = "grey50") +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "Reds"), trans = "sqrt") +
  geom_vline(xintercept = 1963, col = "blue") +
  theme_minimal() + theme(panel.grid = element_blank()) +
  ggtitle(the_disease) +
  ylab("") +
  xlab("")

#Code: Line plot of measles rate by year and state
# compute US average measles rate by year
avg <- us_contagious_diseases %>%
  filter(disease == the_disease) %>% group_by(year) %>%
  summarize(us_rate = sum(count, na.rm = TRUE)/sum(population, na.rm = TRUE)*10000)

# make line plot of measles rate by year by state
dat %>%
  filter(!is.na(rate)) %>%
  ggplot() +
  geom_line(aes(year, rate, group = state), color = "grey50", 
            show.legend = FALSE, alpha = 0.2, size = 1) +
  geom_line(mapping = aes(year, us_rate), data = avg, size = 1, col = "black") +
  scale_y_continuous(trans = "sqrt", breaks = c(5, 25, 125, 300)) +
  ggtitle("Cases per 10,000 by state") +
  xlab("") +
  ylab("") +
  geom_text(data = data.frame(x = 1955, y = 50),
            mapping = aes(x, y, label = "US average"), color = "black") +
  geom_vline(xintercept = 1963, col = "blue")
# 
# Modify the tile plot to show the rate of smallpox cases instead of measles cases.
# Exclude years in which cases were reported in fewer than 10 weeks from the plot.

library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(dslabs)
data(us_contagious_diseases)

the_disease = "Smallpox"
dat <- us_contagious_diseases %>% 
  filter(!state%in%c("Hawaii","Alaska") & disease == the_disease & weeks_reporting>=10) %>% 
  mutate(rate = count / population * 10000) %>% 
  mutate(state = reorder(state, rate))

dat %>% ggplot(aes(year, state, fill = rate)) + 
  geom_tile(color = "grey50") + 
  scale_x_continuous(expand=c(0,0)) + 
  scale_fill_gradientn(colors = brewer.pal(9, "Reds"), trans = "sqrt") + 
  theme_minimal() + 
  theme(panel.grid = element_blank()) + 
  ggtitle(the_disease) + 
  ylab("") + 
  xlab("")

# Modify the sample code for the time series plot to plot data for smallpox instead of for measles.
# Once again, restrict the plot to years in which cases were reported in at least 10 weeks.

library(dplyr)
library(ggplot2)
library(dslabs)
library(RColorBrewer)
data(us_contagious_diseases)

the_disease = "Smallpox"
dat <- us_contagious_diseases %>%
  filter(!state%in%c("Hawaii","Alaska") & disease == the_disease & weeks_reporting>=10) %>% 
  mutate(rate = count / population * 10000) %>% 
  mutate(state = reorder(state, rate))

avg <- us_contagious_diseases %>%
  filter(disease==the_disease) %>% group_by(year) %>%
  summarize(us_rate = sum(count, na.rm=TRUE)/sum(population, na.rm=TRUE)*10000)

dat %>% ggplot() +
  geom_line(aes(year, rate, group = state),  color = "grey50", 
            show.legend = FALSE, alpha = 0.2, size = 1) +
  geom_line(mapping = aes(year, us_rate),  data = avg, size = 1, color = "black") +
  scale_y_continuous(trans = "sqrt", breaks = c(5,25,125,300)) + 
  ggtitle("Cases per 10,000 by state") + 
  xlab("") + 
  ylab("") +
  geom_text(data = data.frame(x=1955, y=50), mapping = aes(x, y, label="US average"), color="black") + 
  geom_vline(xintercept=1963, col = "blue")


# For the state of California, make a time series plot showing rates for all diseases.
# Include only years with 10 or more weeks reporting.
# Use a different color for each disease.
# Include your aes function inside of ggplot rather than inside your geom layer.

library(dplyr)
library(ggplot2)
library(dslabs)
library(RColorBrewer)
data(us_contagious_diseases)

us_contagious_diseases %>% filter(state=="California" & weeks_reporting>=10) %>% 
  group_by(year, disease) %>%
  summarize(rate = sum(count)/sum(population)*10000) %>%
  ggplot(aes(year, rate, color = disease)) + 
  geom_line()

# Compute the US rate by using summarize to sum over states. Call the variable rate.
# The US rate for each disease will be the total number of cases divided by the total population.
# Remember to convert to cases per 10,000.
# You will need to filter for !is.na(population) to get all the data.
# Plot each disease in a different color.

library(dplyr)
library(ggplot2)
library(dslabs)
library(RColorBrewer)
data(us_contagious_diseases)

us_contagious_diseases %>% filter(!is.na(population)) %>% 
  group_by(year, disease) %>%
  summarize(rate = sum(count)/sum(population)*10000) %>%
  ggplot(aes(year, rate, color = disease)) + 
  geom_line()

#Define the titanic dataset starting from the titanic library with the following code:
options(digits = 3)    # report 3 significant digits
library(tidyverse)
library(titanic)

titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))

?titanic_train

ggplot(titanic_train, aes(x = Age, fill = Sex)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~Sex, scales = "free_y") +  # Facet by Sex
  labs(title = "Density Plot of Age Grouped by Sex",
       x = "Age",
       y = "Density") +
  theme_minimal()

#correction
# A faceted plot is useful for comparing the distributions of males and females for A.
# Each sex has the same general shape with two modes at the same locations, though 
# proportions differ slightly across ages and there are more males than females.

titanic %>%
  ggplot(aes(Age, fill = Sex)) +
  geom_density(alpha = 0.2) +
  facet_grid(Sex ~ .)

# A stacked density plot with count on the y-axis is useful for answering B, C and D. 
# The main mode is around age 25 and a second smaller mode is around age 4-5. 
# There are more males than females as indicated by a higher total area and higher 
# counts at almost all ages. With count on the y-axis, it is clear that more males than 
# females are age 40.
titanic %>%
  ggplot(aes(Age, y = ..count.., fill = Sex)) +
  geom_density(alpha = 0.2, position = "stack")

# A plot filled by sex with alpha blending helps reveal the answers to E, F
# and G. There is a higher proportion of females than males below age 17, a 
# higher proportion of males than females for ages 18-35, approximately 
# the same proportion of males and females age 35-55, and a higher proportion
# of males over age 55. The oldest individuals are male.

titanic %>%
  ggplot(aes(Age, fill = Sex)) +
  geom_density(alpha = 0.2)

# Filter out NA values in the 'Age' column
filtered_titanic <- titanic[!is.na(titanic$Age), ]

# Create a QQ-plot with ggplot2
qq_plot <- ggplot(filtered_titanic, aes(sample = Age)) +
  geom_qq() +                   # QQ-plot
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +  # Identity line
  labs(title = "QQ-Plot of Passenger Age", x = "Theoretical Quantiles", y = "Sample Quantiles")

qq_plot



titanic %>%
  filter(!is.na(Age)) %>%
  ggplot(aes(sample = Age)) +
  geom_qq(dparams = params) +
  geom_abline()

# Filter out NA values in the 'Age' column
filtered_titanic <- titanic[!is.na(titanic$Age), ]

# Create a QQ-plot with ggplot2
qq_plot <- ggplot(filtered_titanic, aes(sample = Age)) +
  stat_qq() +                   # QQ-plot
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +  # Identity line
  labs(title = "QQ-Plot of Passenger Age", x = "Theoretical Quantiles", y = "Sample Quantiles")

# Display the plot
print(qq_plot)

# Bar plot for Survived variable
#plot 1 - survival filled by sex
titanic %>%
  ggplot(aes(Survived, fill = Sex)) +
  geom_bar()
# plot 2 - survival filled by sex with position_dodge
titanic %>%
  ggplot(aes(Survived, fill = Sex)) +
  geom_bar(position = position_dodge())
#plot 3 - sex filled by survival
titanic %>%
  ggplot(aes(Sex, fill = Survived)) +
  geom_bar()

###################Probability####################################
beads <- rep(c("red", "blue"), times = c(2,3))    # create an urn with 2 red, 3 blue
beads    # view beads object
sample(beads, 1)    # sample 1 bead at random

B <- 10000    # number of times to draw 1 bead
events <- replicate(B, sample(beads, 1))    # draw 1 bead, B times
tab <- table(events)    # make a table of outcome counts
tab    # view count table
prop.table(tab)    # view table of outcome proportions

#set.seed(1986)

# set.seed(1)
# set.seed(1, sample.kind="Rounding")    # will make R 3.6 generate a seed as in R 3.5

c <- 3
m <-5
y <- 7


ps <- sum(c,m,y)
ps
pmy <- 12/ps

pmy

# When drawing without replacement, the probabilities for each draw change because
# the total number of balls in the box decreases after the first draw.
# 
# Let's break it down:
# 
# Initially, you have 3 cyan balls, 5 magenta balls, and 7 yellow balls, making a total of 15 balls.
# 
# 1. **Probability of the first draw being cyan:**
#    \[ P(\text{cyan on the first draw}) = \frac{\text{Number of cyan balls}}{\text{Total number of balls}} = \frac{3}{15} \]
# 
# 2. **After the first draw, there are now 14 balls left in the box.**
#    - 2 cyan balls, 5 magenta balls, and 7 yellow balls.
# 
# 3. **Probability of the second draw not being cyan:**
#    \[ P(\text{not cyan on the second draw}) = \frac{\text{Number of balls that are not cyan}}{\text{Total number of remaining balls}} = \frac{12}{14} \]
# 
# Now, to find the probability of both events happening (cyan on the first draw and not cyan on the second draw), you multiply the individual probabilities:
# 
# \[ P(\text{cyan on the first draw and not cyan on the second draw}) = P(\text{cyan on the first draw}) \times P(\text{not cyan on the second draw}) \]
# 
# \[ P(\text{both events}) = \frac{3}{15} \times \frac{12}{14} \]
# 
# You can simplify this fraction to find the probability.

p1 <- 3/15

p2 <- 12/14

p3 <- 12/15

p <- p1 * p3
p

#Introducing paste() and expand.grid()

# joining strings with paste
number <- "Three"
suit <- "Hearts"
paste(number, suit)

# joining vectors element-wise with paste
paste(letters[1:5], as.character(1:5))

# generating combinations of 2 vectors with expand.grid
expand.grid(pants = c("blue", "black"), shirt = c("white", "grey", "plaid"))

#Generating a deck of cards
suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(number = numbers, suit = suits)
#deck
deck <- paste(deck$number, deck$suit)
#deck
# probability of drawing a king
kings <- paste("King", suits)
kings
mean(deck %in% kings)

#Permutations and combinations
library(gtools)
permutations(5,2)    # ways to choose 2 numbers in order from 1:5
all_phone_numbers <- permutations(10, 7, v = 0:9)
# all_phone_numbers
n <- nrow(all_phone_numbers)
index <- sample(n, 5)
all_phone_numbers[index,]

permutations(3,2)    # order matters
combinations(3,2)    # order does not matter

#Probability of drawing a second king given that one king is drawn
hands <- permutations(52,2, v = deck)
hands
first_card <- hands[,1]
first_card
second_card <- hands[,2]
second_card
sum(first_card %in% kings)

sum(first_card %in% kings & second_card %in% kings) / sum(first_card %in% kings)

#Probability of a natural 21 in blackjack
aces <- paste("Ace", suits)
aces
facecard <- c("King", "Queen", "Jack", "Ten")
facecard
facecard <- expand.grid(number = facecard, suit = suits)
facecard
facecard <- paste(facecard$number, facecard$suit)
facecard
hands <- combinations(52, 2, v=deck) # all possible hands

# probability of a natural 21 given that the ace is listed first in `combinations`
mean(hands[,1] %in% aces & hands[,2] %in% facecard)

# probability of a natural 21 checking for both ace first and ace second
mean((hands[,1] %in% aces & hands[,2] %in% facecard)|(hands[,2] %in% aces & hands[,1] %in% facecard))

#Monte Carlo simulation of natural 21 in blackjack
# code for one hand of blackjack
hand <- sample(deck, 2)
hand

# code for B=10,000 hands of blackjack
B <- 10000
results <- replicate(B, {
  hand <- sample(deck, 2)
  (hand[1] %in% aces & hand[2] %in% facecard) | (hand[2] %in% aces & hand[1] %in% facecard)
})
mean(results)

# checking for duplicated bdays in one 50 person group
n <- 50
bdays <- sample(1:365, n, replace = TRUE)    # generate n random birthdays
any(duplicated(bdays))    # check if any birthdays are duplicated

# Monte Carlo simulation with B=10000 replicates
B <- 10000
results <- replicate(B, {    # returns vector of B logical values
  bdays <- sample(1:365, n, replace = TRUE)
  any(duplicated(bdays))
})
mean(results)    # calculates proportion of groups with duplicated bdays
# function to calculate probability of shared bdays across n people
compute_prob <- function(n, B = 10000) {
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}

n <- seq(1, 60)
#Element-wise operation over vectors and sapply
x <- 1:10
sqrt(x)    # sqrt operates on each element of the vector

y <- 1:10
x*y    # * operates element-wise on both vectors

compute_prob(n)    # does not iterate over the vector n without sapply

x <- 1:10
sapply(x, sqrt)    # this is equivalent to sqrt(x)

prob <- sapply(n, compute_prob)    # element-wise application of compute_prob to n
plot(n, prob)

#Computing birthday problem probabilities with sapply
# function for computing exact probability of shared birthdays for any n
exact_prob <- function(n){
  prob_unique <- seq(365, 365-n+1)/365   # vector of fractions for mult. rule
  1 - prod(prob_unique)    # calculate prob of no shared birthdays and subtract from 1
}

# applying function element-wise to vector of n values
eprob <- sapply(n, exact_prob)

# plotting Monte Carlo results and exact probabilities on same graph
plot(n, prob)    # plot Monte Carlo results
lines(n, eprob, col = "red")    # add line for exact prob


# Assign the variable 'p_no6' as the probability of not seeing a 6 on a single roll.
p_no6 <- 5/6

# Calculate the probability of not seeing a 6 on six rolls using `p_no6`. Print your result to the console: do not assign it to a variable.

p_no6**6

# Two teams, say the Celtics and the Cavs, are playing a seven game series. 
# The Cavs are a better team and have a 60% chance of winning each game.
# 
# What is the probability that the Celtics win at least one game? 
# Remember that the Celtics must win one of the first four games, or the series will be over!
# 
# Calculate the probability that the Cavs will win the first four games of the series.
# Calculate the probability that the Celtics win at least one game in the first four games of the series.

# Assign the variable `p_cavs_win4` as the probability that the Cavs will win the first four games of the series.
p_cavs_win4 <- 0.6**4

# Using the variable `p_cavs_win4`, calculate the probability that the Celtics win at least one game in the first four games of the series.

1-p_cavs_win4


# Create a Monte Carlo simulation to confirm your answer to the previous problem by 
# estimating how frequently the Celtics win at least 1 of 4 games. Use B <- 10000 simulations.
# 
# The provided sample code simulates a single series of four random games, simulated_games.
# 
# Use the replicate function for B <- 10000 simulations of a four game series. 
# The results of replicate should be stored to a variable named celtic_wins.
# Within each simulation, replicate the sample code to simulate a four-game series
# named simulated_games. Then, use the any function to indicate whether the four-game 
# series contains at least one win for the Celtics. Perform these operations in two separate steps.
# Use the mean function on celtic_wins to find the proportion of simulations 
# that contain at least one win for the Celtics out of four games.
# This line of example code simulates four independent random games where the Celtics either lose or win. Copy this example code to use within the `replicate` function.
simulated_games <- sample(c("lose","win"), 4, replace = TRUE, prob = c(0.6, 0.4))

# The variable 'B' specifies the number of times we want the simulation to run. Let's run the Monte Carlo simulation 10,000 times.
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling.
set.seed(1)

# Create an object called `celtic_wins` that replicates two steps for B iterations: (1) generating a random four-game series `simulated_games` using the example code, then (2) determining whether the simulated series contains at least one win for the Celtics. Put these steps on separate lines.
celtic_wins <- replicate(B, {
  simulated_games <- sample(c("lose","win"), 4, replace = TRUE, prob = c(0.6, 0.4))
  any(simulated_games=="win")
})

# Calculate the frequency out of B iterations that the Celtics won at least one game. Print your answer to the console. 
mean(celtic_wins)

#Monte Carlo simulation of stick strategy
B <- 10000
stick <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car","goat","goat"))    # puts prizes in random order
  prize_door <- doors[prize == "car"]    # note which door has prize
  my_pick  <- sample(doors, 1)    # note which door is chosen
  show <- sample(doors[!doors %in% c(my_pick, prize_door)],1)    # open door with no prize that isn't chosen
  stick <- my_pick    # stick with original door
  stick == prize_door    # test whether the original door has the prize
})
mean(stick)    # probability of choosing prize door when sticking

#Monte Carlo simulation of switch strategy
switch <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car","goat","goat"))    # puts prizes in random order
  prize_door <- doors[prize == "car"]    # note which door has prize
  my_pick  <- sample(doors, 1)    # note which door is chosen first
  show <- sample(doors[!doors %in% c(my_pick, prize_door)], 1)    # open door with no prize that isn't chosen
  switch <- doors[!doors%in%c(my_pick, show)]    # switch to the door that wasn't chosen first or opened
  switch == prize_door    # test whether the switched door has the prize
})
mean(switch)    # probability of choosing prize door when switching


# Two teams, say the Cavs and the Warriors, are playing a seven game championship series. 
# The first to win four games wins the series. The teams are equally good, so they each
# have a 50-50 chance of winning each game.
# 
# If the Cavs lose the first game, what is the probability that they win the series?

# Assign a variable 'n' as the number of remaining games.
n <- 6

# Assign a variable `outcomes` as a vector of possible game outcomes, where 0 indicates a loss and 1 indicates a win for the Cavs.
outcomes <- c(0, 1)

outcomes

# Assign a variable `l` to a list of all possible outcomes in all remaining games. Use the `rep` function on `list(outcomes)` to create list of length `n`.
l <- rep(list(outcomes), n)

l

# Create a data frame named 'possibilities' that contains all combinations of possible outcomes for the remaining games.
possibilities <- data.frame(do.call(expand.grid, l))

possibilities

# Create a vector named 'results' that indicates whether each row in the data frame 'possibilities' contains enough wins for the Cavs to win the series.
results <- rowSums(possibilities) >= 4

# Calculate the proportion of 'results' in which the Cavs win the series. Print the outcome to the console.
prob_cavs_win_series <- mean(results)

prob_cavs_win_series

# Confirm the results of the previous question with a Monte Carlo simulation to 
# estimate the probability of the Cavs winning the series after losing the first game.

# The variable `B` specifies the number of times we want the simulation to run. Let's run the Monte Carlo simulation 10,000 times.
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling.
set.seed(1)

# Create an object called `results` that replicates for `B` iterations a simulated series and determines whether that series contains at least four wins for the Cavs.
results <- replicate(B, {
  cavs_wins <- sample(c(0,1), 6, replace = TRUE)
  sum(cavs_wins)>=4 
})

# Calculate the frequency out of `B` iterations that the Cavs won at least four games in the remainder of the series. Print your answer to the console. 
mean(results)

# Two teams, 
# and 
# , are playing a seven series game series. Team 
#  A is better than team B 
# and has a p > 0.5
# chance of winning each game

# Let's assign the variable 'p' as the vector of probabilities that team A will win.
p <- seq(0.5, 0.95, 0.025)

# Given a value 'p', the probability of winning the series for the underdog team B can be computed with the following function based on a Monte Carlo simulation:
prob_win <- function(p){
  B <- 10000
  result <- replicate(B, {
    b_win <- sample(c(1,0), 7, replace = TRUE, prob = c(1-p, p))
    sum(b_win)>=4
  })
  mean(result)
}

# Apply the 'prob_win' function across the vector of probabilities that team A will win to determine the probability that team B will win. Call this object 'Pr'.
Pr <- sapply(p, prob_win)

# Plot the probability 'p' on the x-axis and 'Pr' on the y-axis.
plot(p, Pr)

# Repeat the previous exercise, but now keep the probability that A team 
# wins fixed at p <- 0.75 and compute the probability for different series lengths. For example, 
# wins in best of 1 game, 3 games, 5 games, and so on through a series that lasts 25 games.


# Given a value 'p', the probability of winning the series for the underdog team $B$ can be computed with the following function based on a Monte Carlo simulation:
prob_win <- function(N, p=0.75){
  B <- 10000
  result <- replicate(B, {
    b_win <- sample(c(1,0), N, replace = TRUE, prob = c(1-p, p))
    sum(b_win)>=(N+1)/2
  })
  mean(result)
}

# Assign the variable 'N' as the vector of series lengths. Use only odd numbers ranging from 1 to 25 games.
N <- seq(1, 25, 2)

# Apply the 'prob_win' function across the vector of series lengths to determine the probability that team B will win. Call this object `Pr`.
Pr<- sapply(N, prob_win)

# Plot the number of games in the series 'N' on the x-axis and 'Pr' on the y-axis.
plot(N, Pr)

library(gtools)
library(tidyverse)
# In the 200m dash finals in the Olympics, 8 runners compete for 3 medals (order matters). 
# In the 2012 Olympics, 3 of the 8 runners were from Jamaica and the other 5 were from different countries. 
# The three medals were all won by Jamaica (Usain Bolt, Yohan Blake, and Warren Weir).
#How many different ways can the 3 medals be distributed across 8 runners?
medals <- permutations(8,3)
nrow(medals)
#How many different ways can the three medals be distributed among the 3 runners from Jamaica?
jamaica < permutations(3,3)
nrow(jamaica)

#What is the probability that all 3 medals are won by Jamaica?
nrow(jamaica)/nrow(medals)
#Run a Monte Carlo simulation on this vector representing the countries of the 8 runners in this race:

runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
# For each iteration of the Monte Carlo simulation, within a replicate() 
# loop, select 3 runners representing the 3 medalists and check whether they are 
# all from Jamaica. Repeat this simulation 10,000 times. Set the seed to 1 before running the loop.
# 
# Calculate the probability that all the runners are from Jamaica.
set.seed(1)
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
B <- 10000
all_jamaica <- replicate(B, {
  results <- sample(runners, 3)
  all(results == "Jamaica")
})
mean(all_jamaica)

installed.packages()

ls()
#######Rebounnd Data Sience#####################

murders$population


#The function length tells you how many entries are in the vector:
pop <- murders$population
length(pop)
#This makes it a numeric vector
class(pop)
as.integer(pop)
class(pop)
class(1L)

mat <- matrix(1:12, 4, 3)

mat

mat[1:2, 2:3]

murders$total

head(murders,100)

murder_rate <- murders$total / murders$population * 100000 

ind <- murder_rate <= 0.71

ind

murders$state[ind]

ind <- which(murders$state == "California")

ind
murder_rate[ind]

# What is the sum of the first 100 positive integers? The formula for the sum of integers 1 through n is n(n+1)/2
# . Define n = 100
# and then use R to compute the sum of 1 through 100 using the formula. What is the sum?

n <- 100
sum <- n * (n + 1) / 2

sum

n2 <- 1000

sum2 <- n2 * (n2 + 1) / 2
  
sum2


n <- 1000
x <- seq(1, n)
sum(x)

# In math and programming, we say that we evaluate a function when we replace the argument with a given value. 
# So if we type sqrt(4), we evaluate the sqrt function. In R, you can evaluate a function inside another function. 
# The evaluations happen from the inside out. Use one line of code to compute the log, in base 10, 
# of the square root of 100

log10(sqrt(100))
x<-3
log(10^x)
log10(x^10)
log(exp(x))
exp(log(x, base = 2))

head(murders)
str(murders)

a <- murders$abb

a

class(st)


b <- murders[, "abb"]

b

identical_ab <- identical(a, b)

identical_ab

class(murders$region)

length(levels(murders$region))

table(murders$region)

temp <- c(35, 88, 42, 84, 81, 30)

city <- c("Beijing", "Lagos", "Paris", "Rio de Janeiro", "San Juan", "Toronto")

names(temp) <- city

temp[1:3]

x <- c(1,2,-3,4)

if(all(x>0)){
  print("All Postives")
} else{
  print("Not all positives")
}

#This code selects the state abbreviations (murders$abb) where the corresponding state names 
#(murders$state) are longer than 8 characters.

new_names <- murders$abb[nchar(murders$state) > 8]


temp[c("Paris", "San Juan")]

odd_numbers <- seq(1, 99, by = 2)

odd_numbers

# Define the sequence
sequence <- seq(from = 6, to = 55, by = 4/7)

# Count the number of elements in the sequence
num_elements <- length(sequence)

# Print the sequence and the number of elements
print(sequence)
print(num_elements)

a <- seq(1, 10, 0.5)

class(a)


# Define the sum_n function
sum_n <- function(n) {
  sum <- 0
  for (i in 1:n) {
    sum <- sum + i
  }
  return(sum)
}

# Use the sum_n function to determine the sum of integers from 1 to 5000
result <- sum_n(5000)

# Print the result
print(result)


altman_plot <- function(x, y) {
  diff <- x - y
  sum <- x + y
  plot(sum, diff, main = "Altman Plot", xlab = "Sum", ylab = "Difference")
}

# Example usage:
x <- c(20, 2, 30, 40, 5)
y <- c(5, 4, 3, 2, 1)
altman_plot(x, y)

# Define the sum_n function
sum_n <- function(n) {
  sum <- 0
  for (i in 1:n) {
    sum <- sum + i^2
  }
  return(sum)
}

sum_n(2)

search()

??filter

a <- seq(1, 10)
class(a)

a<-1L

class(a)

x <- c("1", "3", "5")

as.integer(x)

class(as.integer(x))

pop <- murders$population

sort(pop)

which.min(pop)

pop[51]

# Load the US murders dataset
data(murders)

# Access the population size data using the $ operator and store it as pop
pop <- murders$population

# Sort the population size data
pop <- sort(pop)

# Report the smallest population size
smallest_population <- pop[1]
smallest_population


# Load the US murders dataset
data(murders)

# Access the population size data using the $ operator and store it as pop
pop <- murders$population

# Find the index of the entry with the smallest population size
smallest_index <- order(pop)[1]
smallest_index

# Define an empty numerical vector s_n of size 25
s_n <- vector("numeric", 25)

# Use a for-loop to generate values and store them in s_n
for (i in 1:25) {
  s_n[i] <- i
}

# Print the vector s_n
print(s_n)

n <- 1:25
s_n <- sapply(n, s_n)

# Use sapply to generate values and store them in a vector
s_n <- sapply(1:25, function(x) x)

# Print the vector s_n
print(s_n)

# Suppose 'murders' is your data frame containing the population sizes and state names
head(murders)

# Calculate the ranks of population sizes
ranks <- rank(murders$population)
ranks
# Create a data frame with state name and its rank
my_df <- data.frame(name = murders$state, rank = ranks)

head(my_df)

# Calculate the ranks of population sizes
ranks <- rank(murders$population)

# Get the indexes needed to order the population values
ind <- order(murders$population)

# Create a data frame with state name and its rank
my_df <- data.frame(name = murders$state[ind], rank = ranks[ind])

head(my_df)

str(na_example)
#>  int [1:1000] 2 1 3 2 1 3 1 4 3 2 ...

# Determine which entries are NA
ind <- is.na(na_example)
ind
# Count the number of NAs
num_nas <- sum(ind)

# Print the number of NAs
print(num_nas)


# Determine which entries are not NA
not_nas <- !is.na(na_example)

# Extract the non-NA values
non_na_values <- na_example[not_nas]

# Print the non-NA values
print(sum(non_na_values))

data(co2)
co2
str(co2)

data(ChickWeight)
head(ChickWeight)
str(ChickWeight)

data(BOD)
head(BOD)
str(BOD)

data(BJsales)
head(BJsales)

data(EuStockMarkets)
head(EuStockMarkets)

data(DNase)
head(DNase)

data(Formaldehyde)
head(Formaldehyde)

data(Orange)
head(Orange)

data(UCBAdmissions)
head(UCBAdmissions)
UCBAdmissions


BJsales

murders <- mutate(murders, rate = total/population*100000)
murders

filter(murders, rate <= 0.71)

new_dataframe <- select(murders, state, region, rate)
filter(new_dataframe, rate <= 0.71)

new_dataframe <- select(murders, where(is.numeric))
names(new_dataframe)
#> [1] "population" "total"      "rate"
#> 

new_dataframe <- select(murders, starts_with("r"))
names(new_dataframe)
#> [1] "region" "rate"
#> 
mutate(murders, across(where(is.character), tolower))


mutate(murders, across(where(is.numeric), log10))

plot(x=murders$rate,y=murders$population)

library(dplyr)

# Assuming murders is already loaded or defined
murders <- mutate(murders, rate = (total / population) * 100000)

# View the updated murders dataset
head(murders)

library(dplyr)

# Assuming murders is already loaded or defined
murders <- mutate(murders, rank = rank(-rate))

# View the updated murders dataset
head(murders)

library(dplyr)

# Assuming murders is already loaded or defined
murders <- mutate(murders, rank = rank(-rate, ties.method = "min"))

# View the updated murders dataset
head(murders)

library(dplyr)

# Assuming murders dataset is already loaded or defined

# Filter the top 5 states with the highest murder rates
top_5_states <- filter(murders, rank <= 5)

# View the top 5 states
top_5_states

# Create a new data frame called no_south that removes states from the South region
no_south <- filter(murders, region != "South")

# Count the number of states in the no_south category
num_states_no_south <- nrow(no_south)

# Print the number of states in the no_south category
print(num_states_no_south)


filter(murders, state %in% c("New York", "Texas"))

# Create a new data frame called murders_nw with states from the Northeast and West
murders_nw <- filter(murders, region %in% c("Northeast", "West"))

# Count the number of states in the murders_nw category
num_states_murders_nw <- nrow(murders_nw)

# Print the number of states in the murders_nw category
print(num_states_murders_nw)

# Create a table called my_states satisfying the conditions
my_states <- filter(murders, (region %in% c("Northeast", "West")) & (rate < 1))

# Select only the state name, rate, and rank columns
my_states <- select(my_states, state, rate, rank)

# Print the table
print(my_states)

library(dplyr)
library(dslabs)

heights

s <- heights |> 
  filter(sex == "Female") |>
  summarize(average = mean(height), standard_deviation = sd(height))
s
#>   average standard_deviation
#> 1    64.9               3.76
#> 
heights |> group_by(sex)

heights |> 
  group_by(sex) |>
  summarize(average = mean(height), standard_deviation = sd(height))

library(NHANES)

library(dslabs)
mean(na_example)
#> [1] NA
sd(na_example)
#> [1] NA
#> 
mean(na_example, na.rm = TRUE)
#> [1] 2.3
sd(na_example, na.rm = TRUE)
#> [1] 1.22
#> 
#> 
library(dplyr)

# Filter the dataset to select 20-to-29-year-old females
ref <- NHANES |>
  filter(AgeDecade == " 20-29") |>
  summarize(
    average_systolic_bp = mean(BPSysAve, na.rm = TRUE),
    sd_systolic_bp = sd(BPSysAve, na.rm = TRUE)
  )

# Print the results
print(ref)


library(dplyr)


# Use a pipe to filter the dataset and calculate the average systolic blood pressure
ref_avg <- NHANES %>%
  filter(AgeDecade == " 20-29") %>%
  summarize(
    average_systolic_bp = mean(BPSysAve, na.rm = TRUE)
  ) %>%
  pull(average_systolic_bp)

# Print the average systolic blood pressure
print(ref_avg)


# Use a pipe to filter the dataset and calculate the min and max systolic blood pressure
ref_range <- NHANES %>%
  filter(AgeDecade == " 20-29") %>%
  summarize(
    min_systolic_bp = min(BPSysAve, na.rm = TRUE),
    max_systolic_bp = max(BPSysAve, na.rm = TRUE)
  )

# Print the min and max systolic blood pressure values
print(ref_range)


# Group by Gender and AgeDecade, then calculate the average and standard deviation of systolic blood pressure
female_stat <- NHANES %>%
  filter(Gender == "female") %>%
  group_by(AgeDecade) %>%
  summarize(
    avg_systolic_bp = mean(BPSysAve, na.rm = TRUE),
    sd_systolic_bp = sd(BPSysAve, na.rm = TRUE)
  )

# Print the summary statistics for females by age group
print(female_stat)

# Check the unique values of the Gender column to verify if there are females
unique(NHANES$Gender)

# Check the structure of the NHANES dataset to see if there are any issues with the data
str(NHANES)


# Group by Gender and AgeDecade, then calculate the average and standard deviation of systolic blood pressure
male_stat <- NHANES %>%
  filter(Gender == "male") %>%
  group_by(AgeDecade) %>%
  summarize(
    avg_systolic_bp = mean(BPSysAve, na.rm = TRUE),
    sd_systolic_bp = sd(BPSysAve, na.rm = TRUE)
  )
print(male_stat)

# Filter the NHANES dataset for males aged 40-49
male_40_49 <- NHANES %>%
  filter(Gender == "male", AgeDecade == " 40-49")

# Group by Race1 and calculate the average systolic blood pressure
male_40_49_race <- male_40_49 %>%
  group_by(Race1) %>%
  summarize(avg_systolic_bp = mean(BPSysAve, na.rm = TRUE))

# Order the resulting table from lowest to highest average systolic blood pressure
male_40_49_race_ordered <- male_40_49_race %>%
  arrange(avg_systolic_bp)

# Print the resulting table
print(male_40_49_race_ordered)

class(murders[,4])
#> [1] "numeric"
#> 
as_tibble(murders)

murders

class(as_tibble(murders)[,4])
#> [1] "tbl_df"     "tbl"        "data.frame"
#> 
#> 
grades <- tibble(names = c("John", "Juan", "Jean", "Yao"), 
                 exam_1 = c(95, 80, 90, 85), 
                 exam_2 = c(90, 85, 85, 90))

grades

compute_s_n <- function(n) {
  sum(1:n)
}
n <- 1:25
s_n <- sapply(n, compute_s_n)

s_n

library(purrr)
s_n <- map(n, compute_s_n)
class(s_n)

s_n <- map_dbl(n, compute_s_n)
class(s_n)
#> [1] "numeric"
#> 
x <- c(-2, -1, 0, 1, 2)
case_when(x < 0 ~ "Negative", 
          x > 0 ~ "Positive", 
          TRUE  ~ "Zero")
#> [1] "Negative" "Negative" "Zero"     "Positive" "Positive"
#> 
#> 
murders |> 
  mutate(group = case_when(
    abb %in% c("ME", "NH", "VT", "MA", "RI", "CT") ~ "New England",
    abb %in% c("WA", "OR", "CA") ~ "West Coast",
    region == "South" ~ "South",
    TRUE ~ "Other")) |>
  group_by(group) |>
  summarize(rate = sum(total)/sum(population)*10^5) 
#> # A tibble: 4 × 2
#>   group        rate
#>   <chr>       <dbl>
#> 1 New England  1.72
#> 2 Other        2.71
#> 3 South        3.63
#> 4 West Coast   2.90
#> 
class(murders)

murders_tibble <- as_tibble(murders)

class(murders_tibble)

murders_tibble

# Grouping murders dataset by region
murders_grouped <- murders %>%
  group_by(region)

murders_grouped

# Write tidyverse code that is equivalent to this code:
#   
#   exp(mean(log(murders$population)))

library(dplyr)

murders %>%
  summarise(mean_log_population = mean(log(population))) %>%
  pull(mean_log_population) %>%
  exp()

exp(mean(log(murders$population)))

murders %>%
  summarise(mean_log_population = mean(log(. $ population))) %>%
  pull(mean_log_population) %>%
  exp()

library(purrr)
library(dplyr)

# Define a function to calculate the sum of 1 through n
sum_n <- function(n) {
  sum(1:n)
}

# Create a data frame using map_df
result <- map_df(1:100, ~ tibble(n = .x, s_n = sum_n(.x), s_n_2 = sum_n(.x)^2))

# Print the result
print(result)



library(dplyr)
library(dslabs)
library(data.table)

murders_dt <- as.data.table(murders)

select(murders, state, region) |> head(30)

murders_dt[, c("state", "region")] 

murders_dt[, .(state, region)] |> head(5)

murders_dt[, rate := total / population * 100000]

murders_dt |> head(5)

murders_dt[, ":="(rate = total / population * 100000, rank = rank(population))]

filter(murders, rate <= 0.7)

murders_dt[rate <= 0.7]

murders_dt[rate <= 0.7, .(state, rate)]

murders |> filter(rate <= 0.7) |> select(state, rate)

murders_dt[, rate := total / population * 100000]

murders_dt[, `:=`(rate = total / population * 100000, 
                  rank = rank(-rate))]

murders_dt |> head(10)

murders_dt[, .(state, abb)] |> head(10)

murders_dt[state == "New York"]

filter(murders, state =="New York")

murders_dt[rank <= 5]

# Assuming murders_dt is your data table
top_5_states <- murders_dt[order(-rate)][1:5]

top_5_states 

no_florida <- murders_dt[state != "Florida"]
no_florida 

no_south <- murders_dt[region == "South"]
nrow(no_south)

no_south_rm <- murders_dt[region != "South"]

no_south_rm
nrow(no_south_rm)

murders_dt[state %in% c("New York", "Texas")]

murders_new<- murders_dt[region %in% c("Northeast","West")]
murders_new

murders_new_s <- murders_new[,.(state)]
murders_new_s
nrow(murders_new_s)

murders_dt[population < 5000000 & region == "Northeast"]

my_states_1 <- murders_dt[region %in% c("Northeast","West") & rate < 1]

my_states_1[, .(state, rate, rank)]

data("heights")
heights_dt <- as.data.table(heights)

s <- heights |> summarize(avg = mean(height), sd = sd(height))
s


s <- heights |> 
  filter(sex == "Female") |>
  summarize(avg = mean(height), sd = sd(height))

s

s <- heights_dt[sex == "Female", .(avg = mean(height), sd = sd(height))]

s

heights_dt[, .(avg = mean(height), sd = sd(height)), by = sex]

murders_dt[order(population)]

murders_dt[order(population, decreasing = TRUE)] 

murders_dt[order(region, rate)] 

library(NHANES)
data("NHANES")

nh_dt <- as.data.table(NHANES)

nh_dt
ref <- nh_dt[AgeDecade == " 20-29", .(avg = mean(BPSysAve,na.rm = TRUE), sd = sd(BPSysAve,na.rm = TRUE))]

ref
ref_min_max <- nh_dt[AgeDecade == " 20-29", .(min_sys_BP = min(BPSysAve,na.rm = TRUE), max_sys_BP = max(BPSysAve,na.rm = TRUE))]

ref_min_max

heights_dt[, .(avg = mean(height), sd = sd(height)), by = sex]

nh_dt[Gender == "female", .(avg = mean(BPSysAve,na.rm = TRUE), sd = sd(BPSysAve,na.rm = TRUE)), by = AgeDecade]

nh_dt[Gender == "male", .(avg = mean(BPSysAve,na.rm = TRUE), sd = sd(BPSysAve,na.rm = TRUE)), by = AgeDecade]

# library(data.table)
# 
# # Assuming 'nh_dt' is your data.table
# female_stats <- nh_dt[Gender == "female", .(avg = mean(BPSysAve, na.rm = TRUE), 
#                                             sd = sd(BPSysAve, na.rm = TRUE)), 
#                       by = AgeDecade]

library(data.table)

# Assuming 'nh_dt' is your data.table
# Filter males between the ages of 40-49
male_40_to_49 <- nh_dt[Gender == "male" & AgeDecade == " 40-49"]
male_40_to_49
# Calculate average systolic blood pressure by race
avg_systolic_bp <- male_40_to_49[, .(avg_systolic_bp = mean(BPSysAve, na.rm = TRUE)), 
                                 by = Race1]

avg_systolic_bp
# Order the resulting table from lowest to highest average systolic blood pressure
ordered_avg_systolic_bp <- avg_systolic_bp[order(avg_systolic_bp)]

# Print the ordered table
print(ordered_avg_systolic_bp)

system.file(package = "dslabs")
system.file(package = "dslabs")
#> [1] "/Library/Frameworks/R.framework/Versions/4.2-arm64/Resources/library/dslabs"

dir <- system.file(package = "dslabs")
 list.files(dir)
 
 getwd()
 
 dir <- system.file(package = "dslabs")
 file_path <- file.path(dir, "extdata/murders.csv")
 
 file_path
 file.copy(file_path, "murders.csv")

 list.files("/Users/asekibaala") 

readLines("murders.csv", n = 5)
 
fn <- "calificaciones.csv"
file.copy(file.path(system.file("extdata", package = "dslabs"), fn), fn)

dat <- read.csv("murders.csv")
dat
#> [1] TRUE
readLines(fn, n = 1)
#> [1] "\"nombre\",\"f.n.\",\"estampa\",\"puntuaci\xf3n\""

x <- scan("murders.csv", sep = ",", what = "c")
x[1:10]
#>  [1] "state"      "abb"        "region"     "population" "total"     
#>  [6] "Alabama"    "AL"         "South"      "4779736"    "135"
library(readr)
dat <- read_csv("murders.csv")
guess_encoding("murders.csv")

guess_encoding("calificaciones.csv")

library(readxl)

library(data.table)
dat <- fread("murders.csv")

dat

url <- paste0("https://raw.githubusercontent.com/",
              "rafalab/dslabs/master/inst/extdata/murders.csv")

dat <- read.csv(url)
dat
download.file(url, "murders.csv")


tmp_filename <- tempfile()
download.file(url, tmp_filename)
dat <- read_csv(tmp_filename)
file.remove(tmp_filename)
dat



path <- system.file("extdata", package = "dslabs")
files <- list.files(path)
files

library(readr)

# Specify the path to the directory containing the file
path <- system.file("extdata", package = "dslabs")

# List all files in the directory
files <- list.files(path)

# Filter the olive file
olive_file <- files[grep("olive", files)]

# Read the olive file skipping the first line as the header
dat <- read_csv(file.path(path, olive_file), skip = 1)

dat

names(dat)

# Read the first line of the file
first_line <- readLines(file.path(path, olive_file), n = 1)

# Print the first line to see the column names
first_line

library(tidyverse)
library(dslabs)
head(heights)

index <- heights$sex == "Male"
index
x <- heights$height[index]

m <- sum(x) / length(x)

s <- sqrt(sum((x - mu)^2) / length(x))

m <- mean(x)
s <- sd(x)
c(average = m, sd = s)
#> average      sd 
#>   69.31    3.61
#>   
library(dslabs)
male <- heights$height[heights$sex == "Male"]
female <- heights$height[heights$sex == "Female"]

# Count the number of measurements for each group
num_male_measurements <- length(male)
num_female_measurements <- length(female)

# Print the number of measurements for each group
print(paste("Number of measurements for males:", num_male_measurements))
print(paste("Number of measurements for females:", num_female_measurements))

# Calculate percentiles for females
female_percentiles <- quantile(female, c(0.1, 0.3, 0.5, 0.7, 0.9))

# Calculate percentiles for males
male_percentiles <- quantile(male, c(0.1, 0.3, 0.5, 0.7, 0.9))

# Create a data frame with female_percentiles and male_percentiles
percentiles_df <- data.frame(
  percentile = c("10th", "30th", "50th", "70th", "90th"),
  female = female_percentiles,
  male = male_percentiles
)

# Print the data frame
print(percentiles_df)
head(heights)

library(dslabs)
x <- heights$height[heights$sex=="Male"]
mean(x<=72) - mean(x>69)

library(dplyr)
library(ggplot2)
library(dslabs)
#ggplot(data = murders)
murders |> ggplot()
P <- murders |> ggplot()
class(P)



help(geom_text)
murders |> ggplot() + geom_point(aes(population/10^6, total))
P + geom_point(aes(population/10^6, total)) +
  geom_text(aes(population/10^6, total, label = abb))


#refactor the above code
M <- murders |> ggplot(aes(population/10^6, total))
M +
  geom_point() +
  geom_text(aes(label = abb))

murders |> ggplot(aes(population/10^6, total)) +
  geom_point(size = 3) +
  geom_text(aes(label = abb), nudge_x = 1.5)

murders |> ggplot(aes(population/10^6, total)) +
  geom_point(aes(color = region), size = 3) +
  geom_text(aes(label = abb), nudge_x = 1.5)

murders |> ggplot(aes(population/10^6, total)) +
  geom_point(color = "blue", size = 3) 

p0 <- murders |> ggplot(aes(population/10^6, total))

p1 <- p0 +  geom_point(aes(color = region), size = 3)
p2 <- p1 + geom_text(aes(label = abb), nudge_x = 0.1)

p2
p3 <- p2 + scale_x_log10() + scale_y_log10() 
p3

p4 <- p3 + labs(title = "US Gun Murders in 2010",
                x = "Populations in millions (log scale)", 
                y = "Total number of murders (log scale)",
                color = "Region")
p4

r <- murders |> 
  summarize(rate = sum(total)/sum(population)*10^6) |> 
  pull(rate)

p5 <- p4 + 
  geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") 
p5
######################################using ggthemes#########################
library(ggthemes)
p6 <- p5 + theme_economist()

p6

p7 <- p6 + theme_fivethirtyeight()

p7

library(ggrepel)

p8 <- p1 + geom_text_repel(aes(label = abb), nudge_x = 0.1)

p9 <- p8 + scale_x_log10() + scale_y_log10()

p10 <- p9 + labs(title = "US Gun Murders in 2010",
                 x = "Populations in millions (log scale)", 
                 y = "Total number of murders (log scale)",
                 color = "Region")

p11 <- p10 + geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") 

p12 <- p11 + theme_economist()

p12


#putting it all together
library(ggthemes)
library(ggrepel)

r <- murders |> 
  summarize(rate = sum(total) /  sum(population) * 10^6) |>
  pull(rate)

murders |> 
  ggplot(aes(population/10^6, total)) +   
  geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
  geom_point(aes(col = region), size = 3) +
  geom_text_repel(aes(label = abb)) + 
  scale_x_log10() +
  scale_y_log10() +
  labs(title = "US Gun Murders in 2010",
       x = "Populations in millions (log scale)", 
       y = "Total number of murders (log scale)",
       color = "Region") +
  theme_economist()

murders |> ggplot(aes(region)) + geom_bar()

tab <- murders |> 
  count(region) |> 
  mutate(proportion = n/sum(n))

tab |> ggplot(aes(region, proportion)) + geom_col()

heights |> filter(sex == "Female") |> 
  ggplot(aes(height)) + 
  geom_histogram(binwidth = 1, fill = "blue", col = "black")

heights |> 
  filter(sex == "Female") |>
  ggplot(aes(height)) +
  geom_density(fill = "blue")

heights |> 
  filter(sex == "Female") |>
  ggplot(aes(height)) +
  geom_density(fill="blue", adjust = 2)

heights |> ggplot(aes(sex, height)) +
  geom_boxplot()

x <- expand.grid(x = 1:12, y = 1:10) |> mutate(z = 1:120) 

x |> ggplot(aes(x, y, fill = z)) + geom_raster()

x |> ggplot(aes(x, y, fill = z)) + 
  geom_raster() + 
  scale_fill_gradientn(colors =  terrain.colors(10, 1))


library(gridExtra)
grid.arrange(p5, p6, ncol = 2)


####################Exercises############################
p <- heights |> ggplot()
p
class(p)

?murders

murders |> ggplot(aes(x=population/10^6, y=total)) +
  geom_point()

murders |> ggplot(aes(population, total)) +
  geom_point()

help(geom_label)
murders |> ggplot(aes(population, total)) + 
  geom_label(aes(population, total,label=abb, colour=region))

p00 <- murders |> 
  ggplot(aes(population, total, label = abb, color = region)) +
  geom_label() 

p00

p01 <- p00 + scale_x_log10() + scale_y_log10()  +
  ggtitle("Gun murder data")

p01

p <- heights %>% ggplot(aes(x=height))
p <- heights %>% 
  ggplot(aes(height))
## add a layer to p
p + geom_histogram()

p + geom_histogram(binwidth = 1)

heights %>% 
  ggplot(aes(height)) + geom_density()

heights |> 
  ggplot(aes(height)) + geom_density()

heights %>% 
  ggplot(aes(x = height,group = sex)) +
  geom_density()

heights %>% 
  ggplot(aes(height, color = sex))+geom_density()

heights |> 
  ggplot(aes(height, fill = sex)) + 
  geom_density(alpha=0.2) 
help("geom_density")

color_blind_friendly_cols <- 
  c("#999999", "#E69F00", "#56B4E9", "#009E73", 
    "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

library(dslabs)

dat <- us_contagious_diseases |>  
  filter(year == 1967 & disease=="Measles" & !is.na(population)) |>
  mutate(rate = count / population * 10000 * 52 / weeks_reporting)

dat |> ggplot(aes(state, rate)) +
  geom_col() +
  coord_flip() 

state <- dat$state
rate <- dat$count/dat$population*10000*52/dat$weeks_reporting

state

#To redefine the state object so that the levels 
#are reordered according to the rate variable, you can use the following code:
state <- factor(state, levels = state[order(rate)])
print(state)
print(levels(state))


dat <- us_contagious_diseases |>  
  filter(year == 1967 & disease=="Measles" & !is.na(population)) |>
  mutate(rate = count / population * 10000 * 52 / weeks_reporting,
         state = factor(state, levels = state[order(rate)]))

dat |> ggplot(aes(state, rate)) +
  geom_col() +
  coord_flip()

library(dslabs)
murders |> mutate(rate = total/population*100000) |>
  group_by(region) |>
  summarize(avg = mean(rate)) |>
  mutate(region = factor(region)) |>
  ggplot(aes(region, avg)) +
  geom_col() + 
  ylab("Murder Rate Average")

library(tidyverse)
library(dslabs)
gapminder |> as_tibble()

gapminder |> 
  filter(year == 2015 & country %in% c("Sri Lanka","Turkey")) |> 
  select(country, infant_mortality)

filter(gapminder, year == 1962) |>
  ggplot(aes(fertility, life_expectancy)) +
  geom_point()

filter(gapminder, year == 1962) |>
  ggplot( aes(fertility, life_expectancy, color = continent)) +
  geom_point() 

library(geomtextpath)
gapminder |> 
  filter(country %in% countries) |> 
  ggplot(aes(year, life_expectancy, col = country, label = country)) +
  geom_textpath() +
  theme(legend.position = "none")

gapminder <- gapminder |>  
  mutate(dollars_per_day = gdp/population/365)

past_year <- 1970
gapminder |> 
  filter(year == past_year & !is.na(gdp)) |>
  ggplot(aes(dollars_per_day)) + 
  geom_histogram(binwidth = 1, color = "black")

gapminder |> 
  filter(year == past_year & !is.na(gdp)) |>
  ggplot(aes(log2(dollars_per_day))) + 
  geom_histogram(binwidth = 1, color = "black")

filter(gapminder, year == past_year) |>
  summarize(min = min(population), max = max(population))

gapminder |> 
  filter(year == past_year) |>
  ggplot(aes(log10(population))) +
  geom_histogram(binwidth = 0.5, color = "black")

gapminder |> 
  filter(year == past_year & !is.na(gdp)) |>
  ggplot(aes(dollars_per_day)) + 
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2")


gapminder <- gapminder |> 
  mutate(group = case_when(
    region %in% c("Western Europe", "Northern Europe","Southern Europe", 
                  "Northern America", 
                  "Australia and New Zealand") ~ "West",
    region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    region %in% c("Caribbean", "Central America", 
                  "South America") ~ "Latin America",
    continent == "Africa" & 
      region != "Northern Africa" ~ "Sub-Saharan",
    TRUE ~ "Others"))

gapminder <- gapminder |> 
  mutate(group = factor(group, levels = c("Others", "Latin America", 
                                          "East Asia", "Sub-Saharan",
                                          "West")))


p <- gapminder |> 
  filter(year == past_year & !is.na(gdp)) |>
  ggplot(aes(group, dollars_per_day)) +
  geom_boxplot() +
  scale_y_continuous(trans = "log2") +
  xlab("") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

p

p + geom_point(alpha = 0.5)

library(ggridges)
p <- gapminder |> 
  filter(year == past_year & !is.na(dollars_per_day)) |>
  ggplot(aes(dollars_per_day, group)) + 
  scale_x_continuous(trans = "log2") 
p  + geom_density_ridges() 

p + geom_density_ridges(jittered_points = TRUE, 
                        position = position_points_jitter(height = 0),
                        point_shape = '|', point_size = 3, 
                        point_alpha = 1, alpha = 0.7)



past_year <- 1970
present_year <- 2010
years <- c(past_year, present_year)
country_list <- gapminder |> 
  filter(year %in% c(present_year, past_year)) |>
  group_by(country) |>
  summarize(n = sum(!is.na(dollars_per_day)), .groups = "drop") |>
  filter(n == 2) |>
  pull(country)


gapminder |> 
  filter(year %in% years & country %in% country_list) |>
  mutate(west = ifelse(group == "West", "West", "Developing")) |>
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2") + 
  facet_grid(year ~ west)


gapminder |> 
  filter(year %in% years & country %in% country_list) |>
  ggplot(aes(group, dollars_per_day)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(trans = "log2") +
  xlab("") +
  facet_grid(. ~ year)

gapminder |> 
  filter(year %in% years & country %in% country_list) |>
  mutate(year = factor(year)) |>
  ggplot(aes(group, dollars_per_day, fill = year)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(trans = "log2") +
  xlab("") 

gapminder |> 
  filter(year %in% years & country %in% country_list) |>
  ggplot(aes(dollars_per_day)) +
  geom_density(fill = "grey") + 
  scale_x_continuous(trans = "log2") + 
  facet_grid(. ~ year)

p <- gapminder |> 
  filter(year %in% years & country %in% country_list) |>
  mutate(group = ifelse(group == "West", "West", "Developing")) |>
  ggplot(aes(dollars_per_day, y = after_stat(count), fill = group)) +
  scale_x_continuous(trans = "log2", limits = c(0.125, 300))
p + geom_density(alpha = 0.2) + facet_grid(year ~ .)

p + geom_density(alpha = 0.2, bw = 0.75) + facet_grid(year ~ .)


gapminder |> 
  filter(year %in% years & !is.na(dollars_per_day)) |>
  ggplot(aes(dollars_per_day, group)) + 
  scale_x_continuous(trans = "log2") + 
  geom_density_ridges(bandwidth = 1.5) +
  facet_grid(. ~ year)

gapminder |> 
  filter(year %in% years & country %in% country_list) |>
  group_by(year) |>
  mutate(weight = population/sum(population)*2) |>
  ungroup() |>
  ggplot(aes(dollars_per_day, fill = group)) +
  scale_x_continuous(trans = "log2", limits = c(0.125, 300)) + 
  geom_density(alpha = 0.2, bw = 0.75, position = "stack") + 
  facet_grid(year ~ .) 


library(tidyverse)
library(RColorBrewer)
library(dslabs)
names(us_contagious_diseases)

the_disease <- "Measles"
dat <- us_contagious_diseases |>
  filter(!state %in% c("Hawaii","Alaska") & disease == the_disease) |>
  mutate(rate = count / population * 10000 * 52 / weeks_reporting) |> 
  mutate(state = reorder(state, ifelse(year <= 1963, rate, NA), 
                         median, na.rm = TRUE)) 

dat |> filter(state == "California" & !is.na(rate)) |>
  ggplot(aes(year, rate)) +
  geom_line() + 
  ylab("Cases per 10,000")  + 
  geom_vline(xintercept = 1963, col = "blue")

dat |> ggplot(aes(year, state, fill = rate)) +
  geom_tile(color = "grey50") +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_gradientn(colors = brewer.pal(9, "Reds"), trans = "sqrt") +
  geom_vline(xintercept = 1963, col = "blue") +
  theme_minimal() +  
  theme(panel.grid = element_blank(), 
        legend.position = "bottom", 
        text = element_text(size = 8)) +
  labs(title = the_disease, x = "", y = "")

avg <- us_contagious_diseases |>
  filter(disease == the_disease) |> group_by(year) |>
  summarize(us_rate = sum(count, na.rm = TRUE) / 
              sum(population, na.rm = TRUE) * 10000)

dat |> 
  filter(!is.na(rate)) |>
  ggplot() +
  geom_line(aes(year, rate, group = state),  color = "grey50", 
            show.legend = FALSE, alpha = 0.2, linewidth = 1) +
  geom_line(mapping = aes(year, us_rate),  data = avg, linewidth = 1) +
  scale_y_continuous(trans = "sqrt", breaks = c(5, 25, 125, 300)) + 
  ggtitle("Cases per 10,000 by state") + 
  xlab("") + ylab("") +
  geom_text(data = data.frame(x = 1955, y = 50), 
            mapping = aes(x, y, label = "US average"), 
            color = "black") + 
  geom_vline(xintercept = 1963, col = "blue")

library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(dslabs)
data(us_contagious_diseases)

the_disease = "Smallpox"
dat <- us_contagious_diseases |> 
  filter(!state%in%c("Hawaii","Alaska") & disease == the_disease & weeks_reporting >= 10) |> 
  mutate(rate = count / population * 10000) |>
  mutate(state = reorder(state, rate))

dat %>% ggplot(aes(year, state, fill = rate)) + 
  geom_tile(color = "grey50") + 
  scale_x_continuous(expand=c(0,0)) + 
  scale_fill_gradientn(colors = brewer.pal(9, "Reds"), trans = "sqrt") + 
  theme_minimal() + 
  theme(panel.grid = element_blank()) + 
  ggtitle(the_disease) + 
  ylab("") + 
  xlab("")

data(us_contagious_diseases)

the_disease = "Smallpox"
dat <- us_contagious_diseases %>%
  filter(!state%in%c("Hawaii","Alaska") & disease == the_disease & weeks_reporting >= 10) %>%
  mutate(rate = count / population * 10000) %>%
  mutate(state = reorder(state, rate))

avg <- us_contagious_diseases %>%
  filter(disease==the_disease) %>% group_by(year) %>%
  summarize(us_rate = sum(count, na.rm=TRUE)/sum(population, na.rm=TRUE)*10000)

dat %>% ggplot() +
  geom_line(aes(year, rate, group = state),  color = "grey50", 
            show.legend = FALSE, alpha = 0.2, size = 1) +
  geom_line(mapping = aes(year, us_rate),  data = avg, size = 1, color = "black") +
  scale_y_continuous(trans = "sqrt", breaks = c(5,25,125,300)) + 
  ggtitle("Cases per 10,000 by state") + 
  xlab("") + 
  ylab("") +
  geom_text(data = data.frame(x=1955, y=50), mapping = aes(x, y, label="US average"), color="black") + 
  geom_vline(xintercept=1963, col = "blue")


library(dplyr)
library(ggplot2)
library(dslabs)
library(RColorBrewer)
data(us_contagious_diseases)

us_contagious_diseases %>% filter(state=="California" & weeks_reporting >= 10) %>% 
  group_by(year, disease) %>%
  summarize(rate = sum(count)/sum(population)*10000) %>%
  ggplot(aes(year, rate, color = disease)) + 
  geom_line()

library(dplyr)
library(ggplot2)
library(dslabs)
library(RColorBrewer)
data(us_contagious_diseases)

us_contagious_diseases %>% filter(!is.na(population)) %>% 
  group_by(year, disease) %>%
  summarize(rate = sum(count)/sum(population)*10000) %>%
  ggplot(aes(year, rate, color = disease)) + 
  geom_line()

library(tidyverse) 
library(dslabs)
path <- system.file("extdata", package = "dslabs")
filename <- file.path(path, "fertility-two-countries-example.csv")
wide_data <- read_csv(filename)

new_tidy_data <- wide_data |>
  pivot_longer(`1960`:`2015`, names_to = "year", values_to = "fertility")

head(new_tidy_data)

new_tidy_data <- wide_data |>
  pivot_longer(-country, names_to = "year", values_to = "fertility")


tidy_data <- gapminder |> 
  filter(country %in% c("South Korea", "Germany") & !is.na(fertility)) |>
  select(country, year, fertility)

new_tidy_data <- wide_data |>
  pivot_longer(-country, names_to = "year", values_to = "fertility") |>
  mutate(year = as.integer(year))

new_tidy_data |> 
  ggplot(aes(year, fertility, color = country)) + 
  geom_point()

new_wide_data <- new_tidy_data |> 
  pivot_wider(names_from = year, values_from = fertility)
select(new_wide_data, country, `1960`:`1967`)


path <- system.file("extdata", package = "dslabs")
filename <- "life-expectancy-and-fertility-two-countries-example.csv"
filename <-  file.path(path, filename)

raw_dat <- read_csv(filename)
select(raw_dat, 1:5)

dat <- raw_dat |> pivot_longer(-country)
head(dat)

dat$name[1:5]

dat |> separate_wider_delim(name, delim = "_",names = c("year", "name"))

dat |> separate_wider_delim(name, delim = "_", 
                            names = c("year", "name"), 
                            too_many = "merge") |>  pivot_wider() |>
  mutate(year = as.integer(year))

library(janitor)
names(dat) <- c("Country", "Year", "Fertility",  "Life Expectancy")
clean_names(dat) |> names()

data.frame(ids = letters[1:3], p = 1:3, d = 4:6) |> 
  column_to_rownames("ids") |>
  as.matrix() 

x <- read.csv(file.path(path, "murders.csv"), header = FALSE) |> 
  row_to_names(1)
names(x)

x

x <- bind_rows(x, x[1,])
get_dupes(x)

co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) |> 
  setNames(1:12) |>
  mutate(year = as.character(1959:1997))

# Pivot the wide dataset into a tidy format
co2_tidy <- co2_wide %>%
  pivot_longer(cols = -year, names_to = "month", values_to = "co2")

# Print the resulting tidy dataset
print(co2_tidy)


co2_tidy |> ggplot(aes(month, co2, color = year)) + geom_line()

class(co2_tidy$month)

# Convert month column to numeric
co2_tidy$month <- as.numeric(co2_tidy$month)

# Plot CO2 versus month with a different curve for each year
ggplot(co2_tidy, aes(month, co2, color = year)) + 
  geom_line()


load(admissions)
dat <- admissions |> select(-applicants)

data(admissions) 
dat<-admissions |> select(-applicants) 

dat %>% pivot_wider(names_from=gender,values_from=admitted)

tmp<-admissions %>% pivot_longer(-c(major, gender),names_to='name',values_to='value')

tmp<-tmp %>% unite(column_name, name, gender)

tmp %>% pivot_wider(names_from=column_name,values_from=value)

admissions %>% pivot_longer(-c(major, gender),names_to='name',values_to='value') %>% 
  unite(column_name, name, gender) %>% 
  pivot_wider(names_from=column_name,values_from=value)


library(tidyverse)
library(dslabs)
head(murders)
head(results_us_election_2016)


identical(results_us_election_2016$state, murders$state)


tab <- left_join(murders, results_us_election_2016, by = "state") |>
  select(-others) |> rename(ev = electoral_votes)
head(tab)

tab_1 <- slice(murders, 1:6) |> select(state, population)
tab_2 <- results_us_election_2016 |> 
  filter(state %in% c("Alabama", "Alaska", "Arizona", 
                      "California", "Connecticut", "Delaware")) |> 
  select(state, electoral_votes) |> rename(ev = electoral_votes)

left_join(tab_1, tab_2, by = "state")

tab_1 |> left_join(tab_2, by = "state")

tab_1 |> right_join(tab_2, by = "state")

inner_join(tab_1, tab_2, by = "state")

full_join(tab_1, tab_2, by = "state")

semi_join(tab_1, tab_2, by = "state")

anti_join(tab_1, tab_2, by = "state")

bind_cols(a = 1:3, b = 4:6)

tab_1 <- tab[, 1:3]
tab_2 <- tab[, 4:6]
tab_3 <- tab[, 7:8]
new_tab <- bind_cols(tab_1, tab_2, tab_3)
head(new_tab)

tab_1 <- tab[1:2,]
tab_2 <- tab[3:4,]
bind_rows(tab_1, tab_2)

library(Lahman)

top <- Batting |> 
  filter(yearID == 2016) |>
  arrange(desc(HR)) |>
  slice(1:10)

top |> as_tibble()

People |> as_tibble()

# Step 1: Filter and arrange the Batting data frame to get the top 10 hitters
top <- Batting %>%
  filter(yearID == 2016) %>%
  arrange(desc(HR)) %>%
  slice(1:10)

# Step 2: Join the People data frame to get the player names
top <- top %>%
  left_join(People, by = "playerID") %>%
  select(playerID, nameFirst, nameLast, HR) %>%
  arrange(desc(HR))

# Convert to a tibble for better readability
top <- as_tibble(top)

# Display the top home run hitters with their names
print(top)

# Step 1: Create the initial table of top home run hitters (from the previous exercise)
top <- Batting %>%
  filter(yearID == 2016) %>%
  arrange(desc(HR)) %>%
  slice(1:10) %>%
  left_join(People, by = "playerID") %>%
  select(playerID, nameFirst, nameLast, HR) %>%
  arrange(desc(HR)) %>%
  as_tibble()

# Step 2: Filter the Salaries data frame for the year 2016
salaries_2016 <- Salaries %>%
  filter(yearID == 2016)

# Step 3: Use right_join to merge the salaries with the existing table of top home run hitters
top_with_salaries <- top %>%
  right_join(salaries_2016, by = "playerID") %>%
  select(nameFirst, nameLast, teamID, HR, salary) %>%
  arrange(desc(HR))

# Convert to a tibble for better readability
top_with_salaries <- as_tibble(top_with_salaries)

# Display the final table
print(top_with_salaries)

library(data.table)
#> 
#> Attaching package: 'data.table'
#> The following objects are masked from 'package:lubridate':
#> 
#>     hour, isoweek, mday, minute, month, quarter, second, wday,
#>     week, yday, year
#> The following objects are masked from 'package:dplyr':
#> 
#>     between, first, last
#> The following object is masked from 'package:purrr':
#> 
#>     transpose
st <- as.Date("2024-03-04")
day(st)
#> [1] 4
mday(st)
#> [1] 4
#> 
library(dslabs)
library(dplyr)
data("pr_death_counts")
head(pr_death_counts)
data("us_contagious_diseases")

Sys.getlocale()

system("locale -a")

url <- paste0("https://en.wikipedia.org/w/index.php?title=",
              "Gun_violence_in_the_United_States_by_state",
              "&direction=prev&oldid=810166167")

library(tidyverse)
library(rvest)
h <- read_html(url)

class(h)

html_text(h)

tab <- h |> html_nodes("table")

tab
tab[[1]]

tab <- tab[[1]] |> html_table()

class(tab)

head(tab)

tab <- tab |> setNames(c("state", "population", "total", "murder_rate")) 
head(tab)

library(jsonlite)
nobel <- fromJSON("http://api.nobelprize.org/v1/prize.json")

nobel$prizes |>
  filter(category == "literature" & year == "1971") |> 
  pull(laureates) |>
  first() |>
  select(id, firstname, surname)

url <- "https://data.cdc.gov/resource/muzy-jte6.csv"
library(httr2)
response <- request(url) |> req_perform()

library(readr)
tab <- response |> resp_body_string() |> read_csv()

response <- request(url) |> 
  req_url_path_append("?$limit=100000") |> 
  req_perform() 

url <- "https://data.cdc.gov/resource/muzy-jte6.json"

tab <- request(url) |> 
  req_perform() |> 
  resp_body_string() |> 
  fromJSON(flatten = TRUE)


# Load the necessary package
library(rvest)

# Read the webpage into an object called h
url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)
html_text(h)

# Print the first part of the object to confirm
print(h)

# Extract all tables from the HTML content
tables <- h %>% html_table(fill = TRUE)

# Remove the first component of nodes
nodes <- nodes[-1]

# Convert each remaining node into a table using sapply
tables <- sapply(nodes, html_table, simplify = FALSE)

# Print the first few rows of each table
lapply(tables, head)

# Check the structure of each table
lapply(tables, str)

# Convert the 10th and 19th nodes into data frames
tab_1 <- html_table(nodes[[10]])
tab_2 <- html_table(nodes[[19]])

# Convert the 10th and 19th nodes into data frames
tab_1 <- html_table(nodes[[10]], fill = TRUE)
tab_2 <- html_table(nodes[[19]], fill = TRUE)

# Display the headers of each table to examine them
colnames(tab_1)
colnames(tab_2)

# Remove the first row and assign proper column names
tab_1 <- tab_1[-1, ]
colnames(tab_1) <- c("Team", "Payroll", "Average")

tab_2 <- tab_2[-1, ]
colnames(tab_2) <- c("Team", "Payroll", "Average")

# Use full_join to combine the tables based on the "Team" column
combined_table <- full_join(tab_1, tab_2, by = "Team", suffix = c("_1", "_2"))

# Use anti_join to find teams present in tab_1 but not in tab_2
missing_from_tab_2 <- anti_join(tab_1, tab_2, by = "Team")

# Use anti_join to find teams present in tab_2 but not in tab_1
missing_from_tab_1 <- anti_join(tab_2, tab_1, by = "Team")

# Fix the inconsistency in team names in tab_1
tab_1 <- tab_1 %>%
  mutate(Team = ifelse(Team == "N.Y. Yankees", "NY Yankees", Team))

# Join the tables and filter for Oakland and the Yankees
joined_tables <- full_join(tab_1, tab_2, by = "Team") %>%
  filter(Team %in% c("NY Yankees", "Oakland"))

# Show only the relevant columns (Team and Payroll)
result <- joined_tables %>%
  select(Team, Payroll.x, Payroll.y) %>%
  rename_with(~ c("Team", "Payroll_tab_1", "Payroll_tab_2"))


# Use full_join to combine the tables
#combined_table <- full_join(tab_1, tab_2, by = "X1")

# Extract all table nodes from the HTML content
nodes <- h %>% html_nodes("table")

html_text(nodes[[9]])

# Inspect the content of the 8th node
html_text(nodes[[4]])

# Convert the 8th entry of nodes into a table
table_8 <- html_table(nodes[[4]])

clean_table <- table_8 |> select("Team","Opening Day","Avg Salary")

# Print the first table to check
print(tables[[2]])


# Specify the URL
url2 <- "https://m.imdb.com/chart/bestpicture/"


# Read the HTML content of the webpage
webpage <- read_html(url2)
html_text(webpage)

tables1 <- webpage %>% html_table(fill = TRUE)

nodes1 <- webpage %>% html_nodes("table")

html_text(nodes1[[0]])

# Extract the titles of the movies that won Best Picture
movie_titles <- webpage %>%
  html_nodes(".lister-item-header a") %>%
  html_text()



# Load necessary libraries
library(rvest)

# Specify the URL
url2 <- "https://m.imdb.com/chart/bestpicture/"

# Read the HTML content of the webpage
webpage <- read_html(url2)

# Extract the titles of the movies that won Best Picture
movie_titles <- webpage %>%
  html_nodes(".lister-item-header a") %>%
  html_text()

# Print the extracted movie titles
print(movie_titles)

# Load necessary libraries
library(rvest)

# Specify the URL
url <- "https://m.imdb.com/chart/bestpicture/"

# Read the HTML content of the webpage
webpage <- read_html(url)

# Extract the titles of the movies that won Best Picture
movie_titles <- webpage %>%
  html_nodes(".winner") %>%
  html_text()

# Print the extracted movie titles
print(movie_titles)

# Load necessary libraries
library(rvest)

# Specify the URL
url <- "https://m.imdb.com/chart/bestpicture/"

# Read the HTML content of the webpage
webpage <- read_html(url)

# Extract the titles of the movies that won Best Picture
movie_titles <- webpage %>%
  html_nodes("h4") %>%
  html_text()

# Print the extracted movie titles
print(movie_titles)

# Load necessary libraries
library(rvest)

# Specify the URL
url <- "https://m.imdb.com/chart/bestpicture/"

# Read the HTML content of the webpage
webpage <- read_html(url)

# Print the HTML content of the webpage
print(webpage)

library(tidyverse)
library(stringr)

library(dslabs)
head(reported_heights)

class(reported_heights$height)

x <- as.numeric(reported_heights$height)

sum(is.na(x))

reported_heights |> 
  mutate(new_height = as.numeric(height)) |>
  filter(is.na(new_height)) |> 
  head(n = 10)


pattern <- ","
str_detect(c("1", "10", "100", "1,000", "10,000"), pattern) 
#> [1] FALSE FALSE FALSE  TRUE  TRUE
#> 


str_subset(reported_heights$height, "cm")
#> [1] "165cm"  "170 cm"
#> 
#
yes <- c("180 cm", "70 inches")
no <- c("180", "70''")
s <- c(yes, no)

str_detect(s, "cm") | str_detect(s, "inches")
#> [1]  TRUE  TRUE FALSE FALSE

str_detect(s, "cm|inches")
#> [1]  TRUE  TRUE FALSE FALSE

yes <- c("5", "6", "5'10", "5 feet", "4'11")
no <- c("", ".", "Five", "six")
s <- c(yes, no)
pattern <- "\\d"
str_detect(s, pattern)

str_view(s, pattern)

str_view(s, pattern, match = NA)

str_view(s, "[56]", match = NA)

yes <- as.character(4:7)
no <- as.character(1:3)
s <- c(yes, no)
str_detect(s, "[4-7]")

pattern <- "^\\d$"
yes <- c("1", "5", "9")
no <- c("12", "123", " 1", "a4", "b")
s <- c(yes, no)
str_view(s, pattern, match = NA)

pattern <- "^\\d{1,2}$"
yes <- c("1", "5", "9", "12")
no <- c("123", "a4", "b")
str_view(c(yes, no), pattern, match = NA)

pattern <- "^[4-7]'\\d{1,2}\"$"

yes <- c("5'7\"", "6'2\"",  "5'12\"")
no <- c("6,2\"", "6.2\"","I am 5'11\"", "3'2\"", "64")
str_detect(yes, pattern)
str_detect(no, pattern)

pattern <- "[^a-zA-Z]\\d"
yes <- c(".3", "+2", "-0","*4")
no <- c("A3", "B2", "C0", "E4")
str_detect(yes, pattern)

str_detect(no, pattern)

pattern_without_groups <- "^[4-7],\\d*$"

pattern_with_groups <-  "^([4-7]),(\\d*)$"

yes <- c("5,9", "5,11", "6,", "6,1")
no <- c("5'9", ",", "2,8", "6.1.1")
s <- c(yes, no)
str_detect(s, pattern_without_groups)

str_detect(s, pattern_with_groups)

str_match(s, pattern_with_groups)

str_extract(s, pattern_with_groups)

problems <- reported_heights |> 
  mutate(inches = suppressWarnings(as.numeric(height))) |>
  filter(is.na(inches) | inches < 50 | inches > 84) |>
  pull(height)
length(problems)


pattern <- "^[4-7]'\\d{1,2}\"$"
sum(str_detect(problems, pattern))

problems[c(2, 10, 11, 12, 15)] |> str_view(pattern)

str_subset(problems, "inches")

str_subset(problems, "''")

pattern <- "^[4-7]'\\d{1,2}$"

problems |> 
  str_replace("feet|ft|foot", "'") |> # replace feet, ft, foot with ' 
  str_replace("inches|in|''|\"", "") |> # remove all inches symbols
  str_detect(pattern) |> 
  sum()

pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
problems |> 
  str_replace("feet|ft|foot", "'") |> # replace feet, ft, foot with ' 
  str_replace("inches|in|''|\"", "") |> # remove all inches symbols
  str_detect(pattern) |> 
  sum()


pattern_with_groups <-  "^([4-7]),(\\d*)$"
yes <- c("5,9", "5,11", "6,", "6,1")
no <- c("5'9", ",", "2,8", "6.1.1")
s <- c(yes, no)
str_replace(s, pattern_with_groups, "\\1'\\2")

pattern_with_groups <- "^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$"

str_subset(problems, pattern_with_groups) |> head()

str_subset(problems, pattern_with_groups) |> 
  str_replace(pattern_with_groups, "\\1'\\2") |> head()

library(dslabs)
research_funding_rates |> 
  select("discipline", "success_rates_men", "success_rates_women")

library("pdftools")
temp_file <- tempfile()
url <- paste0("https://web.archive.org/web/20150927033124/",
              "https://www.pnas.org/content/suppl/2015/09/16/",
              "1510159112.DCSupplemental/pnas.201510159SI.pdf")
download.file(url, temp_file, mode = "w")
txt <- pdf_text(temp_file)
file.remove(temp_file)

raw_data_research_funding_rates <- txt[2]

tab <- str_split(raw_data_research_funding_rates, "\n+")

tab <- tab[[1]]

the_names_1 <- tab[3]
the_names_2 <- tab[4]

the_names_1 <- the_names_1 |>
  str_trim() |>
  str_replace_all(",\\s.", "") |>
  str_split("\\s{2,}", simplify = TRUE)
the_names_1 

the_names_2 <- the_names_2 |>
  str_trim() |>
  str_split("\\s+", simplify = TRUE)
the_names_2

tmp_names <- paste(rep(the_names_1, each = 3), the_names_2[-1], sep = "_")
the_names <- c(the_names_2[1], tmp_names) |>
  str_to_lower() |>
  str_replace_all("\\s", "_")
the_names

new_research_funding_rates <- tab[6:14] |>
  str_trim() |>
  str_split("\\s{2,}", simplify = TRUE) |>
  data.frame() |>
  setNames(the_names) |>
  mutate(across(-1, parse_number))
new_research_funding_rates |> as_tibble()

identical(research_funding_rates, new_research_funding_rates)

library(dslabs)

gapminder |> 
  filter(region == "Caribbean") |>
  ggplot(aes(year, life_expectancy, color = country)) +
  geom_line()

x <- levels(gapminder$country)
levels(gapminder$country) <- case_when(
  x == "Antigua and Barbuda" ~ "Barbuda",
  x == "Dominican Republic" ~ "DR",
  x == "St. Vincent and the Grenadines" ~ "St. Vincent",
  x == "Trinidad and Tobago" ~ "Trinidad",
  .default = x)
gapminder |> 
  filter(region == "Caribbean") |>
  ggplot(aes(year, life_expectancy, color = country)) +
  geom_line()


library(forcats)
gapminder$country <- 
  fct_recode(gapminder$country, 
             "Barbuda" = "Antigua and Barbuda",
             "DR" = "Dominican Republic",
             "St. Vincent" = "St. Vincent and the Grenadines",
             "Trinidad" = "Trinidad and Tobago")


fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf",
                  package="dslabs")
system2("open", args = fn)
library(pdftools)
library(stringr)
pdf_text(fn)

txt <- pdf_text(fn)

page_nine <- txt[9]

s <- str_split(page_nine, "\n")[[1]]

class(s)

length(s)

# Extract the ninth page
page_nine <- txt[9]

# Split the text of the ninth page into lines
s <- str_split(page_nine, "\n")

# Redefine s to be the first entry of the list
s <- s[[1]]

# Output the type of object and the number of entries
s_type <- class(s)
s_length <- length(s)

# Print the results
s_type
s_length

# Trim white spaces from each line
s <- str_trim(s)

# Output the first few lines to check the result
print(head(s))

# Find the rows with the header
header_index <- str_which(s, "2015")

# Output the header_index to check the result
print(header_index)


# Identify the row containing the header and save it
header <- s[header_index]

# Split the header row into column names and month using str_split
split_header <- str_split(header, "\\s+", simplify = TRUE)
month <- split_header[, 1]
column_names <- split_header[, -1]

# Print the results to check
print(month)
print(column_names)

s <- str_trim(s)

# Find the index of the row containing the word "totals"
tail_index <- str_which(s, regex("totals", ignore_case = TRUE))

# Output the tail_index to check the result
print(tail_index)


# Find the row with the header containing the year "2015"
header_index <- str_which(s, "2015")
header <- s[header_index]

# Split the header row into month and column names
split_header <- str_split(header, "\\s+", simplify = TRUE)
month <- split_header[, 1]
column_names <- split_header[, -1]

# Find the index of the row containing the word "totals"
tail_index <- str_which(s, regex("totals", ignore_case = TRUE))

# Output the results to check
list(
  header_index = header_index,
  header = header,
  month = month,
  column_names = column_names,
  tail_index = tail_index
)


library(tidyverse)
library(scales)
library(tidytext)
library(textdata)
library(dslabs)

range(trump_tweets$created_at)
names(trump_tweets)

trump_tweets$text[16412] |> str_wrap(width = options()$width) |> cat()

trump_tweets |> count(source) |> arrange(desc(n)) |> head(5)

campaign_tweets <- trump_tweets |> 
  filter(source %in% paste("Twitter for", c("Android", "iPhone")) &
           created_at >= ymd("2015-06-17") & 
           created_at < ymd("2016-11-08")) |>
  mutate(source = str_remove(source, "Twitter for ")) |>
  filter(!is_retweet) |>
  arrange(created_at) |> 
  as_tibble()

campaign_tweets |>
  mutate(hour = hour(with_tz(created_at, "EST"))) |>
  count(source, hour) |>
  group_by(source) |>
  mutate(percent = n / sum(n)) |>
  ungroup() |>
  ggplot(aes(hour, percent, color = source)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Hour of day (EST)", y = "% of tweets", color = "")


poem <- c("Roses are red,", "Violets are blue,", 
          "Sugar is sweet,", "And so are you.")
example <- tibble(line = c(1, 2, 3, 4),
                  text = poem)
example

i <- 3008

campaign_tweets$text[i] |> str_wrap(width = 65) |> cat()

campaign_tweets[i,] |> 
  unnest_tokens(word, text) |>
  pull(word) 


links_to_pics <- "https://t.co/[A-Za-z\\d]+|&amp;"
campaign_tweets[i,] |> 
  mutate(text = str_remove_all(text, links_to_pics))  |>
  unnest_tokens(word, text) |>
  pull(word)


tweet_words <- campaign_tweets |> 
  mutate(text = str_remove_all(text, links_to_pics))  |>
  unnest_tokens(word, text)



tweet_words |> 
  count(word) |>
  arrange(desc(n))

head(stop_words)

tweet_words <- campaign_tweets |> 
  mutate(text = str_remove_all(text, links_to_pics))  |>
  unnest_tokens(word, text) |>
  filter(!word %in% stop_words$word ) 


tweet_words <- campaign_tweets |> 
  mutate(text = str_remove_all(text, links_to_pics))  |>
  unnest_tokens(word, text) |>
  filter(!word %in% stop_words$word &
           !str_detect(word, "^\\d+$")) |>
  mutate(word = str_replace(word, "^'", ""))



android_vs_iphone <- tweet_words |>
  count(word, source) |>
  pivot_wider(names_from = "source", values_from = "n", values_fill = 0) |>
  mutate(p_a = Android / sum(Android), p_i = iPhone / sum(iPhone),
         percent_diff = (p_a - p_i) / ((p_a + p_i)/2) * 100)


android_vs_iphone |> filter(Android + iPhone >= 100) |>
  arrange(desc(percent_diff))

android_vs_iphone |> filter(Android + iPhone >= 100) |> 
  arrange(percent_diff)

get_sentiments("bing")

get_sentiments("afinn")

get_sentiments("nrc") |> count(sentiment)

nrc <- get_sentiments("nrc") |>
  select(word, sentiment)

tweet_words |> inner_join(nrc, by = "word", relationship = "many-to-many") |> 
  select(source, word, sentiment) |> 
  sample_n(5)

sentiment_counts <- tweet_words |>
  left_join(nrc, by = "word", relationship = "many-to-many") |>
  count(source, sentiment) |>
  pivot_wider(names_from = "source", values_from = "n") |>
  mutate(sentiment = replace_na(sentiment, replace = "none"))
sentiment_counts


sentiment_counts |>
  mutate(p_a = Android / sum(Android) , 
         p_i = iPhone / sum(iPhone), 
         percent_diff = (p_a - p_i) / ((p_a + p_i)/2) * 100) |>
  arrange(desc(percent_diff))


android_vs_iphone |> inner_join(nrc, by = "word") |>
  filter(sentiment == "disgust") |>
  arrange(desc(percent_diff))





get_sentiments("loughran") |> count(sentiment)

install.packages("gutenbergr")
library(gutenbergr)
library(stringr)

gutenberg_metadata

pride_prejudice_id <- gutenberg_metadata %>%
  filter(str_detect(title, regex("Pride and Prejudice", ignore_case = TRUE))) %>%
  select(gutenberg_id)

print(pride_prejudice_id)

# Load the metadata using gutenberg_works
gutenberg_works <- gutenberg_works()

# Find the ID of "Pride and Prejudice" using str_detect
pride_prejudice_works <- gutenberg_works %>%
  filter(str_detect(title, regex("Pride and Prejudice", ignore_case = TRUE)) & 
           language == "en") %>%
  select(gutenberg_id, title, author)

# Print the filtered works
print(pride_prejudice_works)

pride_prejudice_id <- pride_prejudice_works$gutenberg_id[2]

book <- gutenberg_download(pride_prejudice_id)

# Create a tidy table with all the words in the text
words <- book %>%
  unnest_tokens(word, text)

# Print the first few rows of the words table to check
print(head(words))

# Create a tidy table with all the words in the text
words <- book %>%
  unnest_tokens(word, text) %>%
  mutate(word_number = row_number())

# Remove stop words using anti_join
data("stop_words")
words <- words %>%
  anti_join(stop_words, by = "word")

# Remove numbers using a regular expression filter
words <- words %>%
  filter(!str_detect(word, "^[0-9]+$"))

# Print the first few rows of the cleaned words table to check
print(head(words))


# Remove underscores and other non-alphabetic characters from words
words <- words %>%
  mutate(word = str_replace_all(word, "[^a-zA-Z]", "")) %>%
  filter(word != "")

# Load the AFINN lexicon
afinn <- get_sentiments("afinn")

# Assign sentiment values to each word by joining with the AFINN lexicon
words_with_sentiment <- words %>%
  left_join(afinn, by = "word")

# Print the first few rows of the words_with_sentiment table to check
print(head(words_with_sentiment))


# Remove underscores and other non-alphabetic characters from words
words <- words %>%
  mutate(word = str_replace_all(word, "[^a-zA-Z]", "")) %>%
  filter(word != "")

# Convert words to lowercase to ensure case-insensitive matching
words <- words %>%
  mutate(word = tolower(word))

# Load the AFINN lexicon
afinn <- get_sentiments("afinn")

# Assign sentiment values to each word by joining with the AFINN lexicon using inner join
words_with_sentiment <- words %>%
  inner_join(afinn, by = "word")

# Print the first few rows of the words_with_sentiment table to check
print(head(words_with_sentiment))



# Create a plot of sentiment score versus location in the book and add a smooth line
ggplot(words_with_sentiment, aes(x = word_number, y = value)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", color = "blue") +
  labs(title = "Sentiment Score vs. Location in 'Pride and Prejudice'",
       x = "Word Number",
       y = "Sentiment Score") +
  theme_minimal()


# Compute page numbers assuming 300 words per page
words_with_sentiment <- words_with_sentiment %>%
  mutate(page = (word_number - 1) %/% 300 + 1) %>%
  group_by(page) %>%
  summarise(avg_sentiment = mean(value, na.rm = TRUE))

# Plot average sentiment score by page with smoother
ggplot(words_with_sentiment, aes(page, avg_sentiment)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "blue") +
  labs(title = "Average Sentiment Score by Page in 'Pride and Prejudice'",
       x = "Page Number",
       y = "Average Sentiment Score") +
  theme_minimal()

library(HistData)
x <- Galton$child


# Load the HistData package and the Galton dataset
library(HistData)
x <- Galton$child

# Compute the average
average <- mean(x)
average

# Compute the median
median_value <- median(x)
median_value


# Compute the median
median_value <- median(x)
median_value

# Compute the median absolute deviation (MAD)
mad_value <- mad(x)
mad_value


# Introduce an error by multiplying the first value by 10
x_with_error <- x
x_with_error[1] <- x_with_error[1] * 10

# Compute the new average after the error
average_with_error <- mean(x_with_error)
average_with_error

# Calculate how much the average grew
average_growth <- average_with_error - average
average_growth


# Compute the standard deviation of the original data
sd_original <- sd(x)
sd_original

# Compute the standard deviation after the error
sd_with_error <- sd(x_with_error)
sd_with_error

# Calculate the growth of the SD
sd_growth <- sd_with_error - sd_original
sd_growth


# Compute the median after the error
median_with_error <- median(x_with_error)
median_with_error

# Calculate the growth of the median
median_growth <- median_with_error - median_value
median_growth

# Compute the MAD after the error
mad_with_error <- mad(x_with_error)
mad_with_error

# Calculate the growth of the MAD
mad_growth <- mad_with_error - mad_value
mad_growth


# Function to calculate the average after changing the first entry to k
error_avg <- function(k) {
  # Load the HistData package and the Galton dataset
  library(HistData)
  x <- Galton$child
  
  # Modify the first entry of the vector x to the value k
  x[1] <- k
  
  # Return the average of the modified vector
  return(mean(x))
}

# Show the results for k = 10000
avg_10000 <- error_avg(10000)
avg_10000

# Show the results for k = -10000
avg_neg_10000 <- error_avg(-10000)
avg_neg_10000


# Load the dslabs package and the murders dataset
library(dslabs)
data(murders)

# Compute the murder rate for each state
# Murder rate is calculated as (total murders / population) * 100,000
murders$murder_rate <- (murders$total / murders$population) * 100000

# Create a boxplot comparing murder rates across regions
boxplot(murder_rate ~ region, data = murders,
        main = "Murder Rates by Region in the United States",
        xlab = "Region",
        ylab = "Murder Rate (per 100,000 people)",
        col = "lightblue")


# Load the necessary package
library(dplyr)

# Compute the murder rate for each state
murders <- murders %>%
  mutate(murder_rate = (total / population) * 100000)

# Calculate the median and IQR of murder rates for each region
murder_rate_summary <- murders %>%
  group_by(region) %>%
  summarize(
    median_murder_rate = median(murder_rate),
    IQR_murder_rate = IQR(murder_rate)
  )

# Print the results
print(murder_rate_summary)

# Load necessary packages
library(tidyverse)
library(lubridate)
library(dslabs)

# Load the dataset
data(reported_heights)

# Add a Year Column and Clean Height Data
reported_heights <- reported_heights %>%
  mutate(
    year = year(time_stamp),  # Extract the year from the time_stamp
    height = parse_number(height)  # Convert height to numeric
  ) %>%
  mutate(
    height = na_if(height, height < 54 | height > 72)  # Convert out-of-range values to NA
  )

# Stratify by sex and year, then calculate the percentage of NAs
na_percentage <- reported_heights %>%
  group_by(sex, year) %>%
  summarize(
    na_count = sum(is.na(height)),
    total_count = n(),
    na_percentage = (na_count / total_count) * 100
  )

na_percentage

library(tidyverse)  
reported_heights <- reported_heights |>
  mutate(original_heights = height, height = as.numeric(height))

# Examine rows that resulted in NAs
na_rows <- reported_heights %>%
  filter(is.na(height))

# Print the rows with NAs to understand why this is happening
na_rows

# Remove rows with NAs in height
cleaned_heights <- reported_heights %>%
  filter(!is.na(height))

# Compute summary statistics
summary_stats <- cleaned_heights %>%
  group_by(sex) %>%
  summarize(
    mean_height = mean(height),
    sd_height = sd(height),
    median_height = median(height),
    mad_height = mad(height)
  )

summary_stats


# Create boxplots summarizing the heights for males and females
boxplot(height ~ sex, data = cleaned_heights,
        main = "Boxplots of Heights by Sex",
        xlab = "Sex", ylab = "Height (inches)",
        col = "lightblue")

# Look at the largest 10 heights
largest_heights <- cleaned_heights %>%
  arrange(desc(height)) %>%
  slice(1:10)

largest_heights

# Hypothesis: These unusually large values might be due to data entry errors, 
# such as missing decimal points or incorrect unit conversions (e.g., centimeters instead of inches).

# Use Tukey's method to identify far outliers
outliers <- cleaned_heights %>%
  group_by(sex) %>%
  filter(height < (quantile(height, 0.25) - 1.5 * IQR(height)) |
           height > (quantile(height, 0.75) + 1.5 * IQR(height)))

outliers

# Comment: The outliers identified here are likely due to significant measurement errors or 
# data entry mistakes, such as recording heights in centimeters instead of inches.

#First, we use the function rep to generate the urn:
beads <- rep(c("red", "blue"), times = c(2,3))
beads

#and then use sample to pick a bead at random:
sample(beads, 1)

#To perform our first Monte Carlo simulation, we use the replicate function, 
#which allows us to repeat the same task any number of times. Here, we repeat the random event 10000 timmes
B <- 10000
events <- replicate(B, sample(beads, 1))

tab <- table(events)
tab

prop.table(tab)


events <- sample(beads, B, replace = TRUE)
prop.table(table(events))

x <- sample(beads, 5)


#Combinations and permutations
number <- "Three"
suit <- "Hearts"
paste(number, suit)

paste(letters[1:5], as.character(1:5))

expand.grid(pants = c("blue", "black"), shirt = c("white", "grey", "plaid"))

#Here is how we generate a deck of cards:
suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven", 
             "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(number = numbers, suit = suits)
deck <- paste(deck$number, deck$suit)

kings <- paste("King", suits)
mean(deck %in% kings)

library(gtools)
permutations(3, 2)

all_phone_numbers <- permutations(10, 7, v = 0:9)
n <- nrow(all_phone_numbers)
index <- sample(n, 5)
all_phone_numbers[index,]


hands <- permutations(52, 2, v = deck)

first_card <- hands[,1]
second_card <- hands[,2]

kings <- paste("King", suits)
sum(first_card %in% kings)

sum(first_card %in% kings & second_card %in% kings) / 
  sum(first_card %in% kings)


#if order doesnot matter use combinations not permutations 

combinations(3,2)

aces <- paste("Ace", suits)

facecard <- c("King", "Queen", "Jack", "Ten")
facecard <- expand.grid(number = facecard, suit = suits)
facecard <- paste(facecard$number, facecard$suit)

hands <- combinations(52, 2, v = deck)

mean(hands[,1] %in% aces & hands[,2] %in% facecard)

#Monte Carlo

hand <- sample(deck, 2)
(hand[1] %in% aces & hand[2] %in% facecard) | 
  (hand[2] %in% aces & hand[1] %in% facecard)


blackjack <- function(){
  hand <- sample(deck, 2)
  (hand[1] %in% aces & hand[2] %in% facecard) | 
    (hand[2] %in% aces & hand[1] %in% facecard)
}

blackjack()

B <- 10000
results <- replicate(B, blackjack())
mean(results)

#stick strategy:
B <- 10000
monty_hall <- function(strategy){
  doors <- as.character(1:3)
  prize <- sample(c("car", "goat", "goat"))
  prize_door <- doors[prize == "car"]
  my_pick  <- sample(doors, 1)
  show <- sample(doors[!doors %in% c(my_pick, prize_door)],1)
  stick <- my_pick
  stick == prize_door
  switch <- doors[!doors %in% c(my_pick, show)]
  choice <- ifelse(strategy == "stick", stick, switch)
  choice == prize_door
}
stick <- replicate(B, monty_hall("stick"))
mean(stick)

switch <- replicate(B, monty_hall("switch"))
mean(switch)

#Birthday problem
n <- 50
bdays <- sample(1:365, n, replace = TRUE)

duplicated(c(1, 2, 3, 1, 4, 3, 5))

any(duplicated(bdays))

B <- 10000
same_birthday <- function(n){
  bdays <- sample(1:365, n, replace = TRUE)
  any(duplicated(bdays))
}
results <- replicate(B, same_birthday(50))
mean(results)

compute_prob <- function(n, B = 10000){
  results <- replicate(B, same_birthday(n))
  mean(results)
}

n <- seq(1,60)
prob <- sapply(n, compute_prob)

library(tidyverse)
prob <- sapply(n, compute_prob)
qplot(n, prob)


# Number of each type of ball
cyan_balls <- 3
magenta_balls <- 5
yellow_balls <- 7

# Total number of balls
total_balls <- cyan_balls + magenta_balls + yellow_balls

# Probability of drawing a cyan ball
prob_cyan <- cyan_balls / total_balls
prob_cyan

# Probability of drawing a cyan ball
prob_cyan <- cyan_balls / total_balls

# Probability of NOT drawing a cyan ball
prob_not_cyan <- 1 - prob_cyan
prob_not_cyan



# First draw probability (cyan)
prob_first_cyan <- cyan_balls / total_balls

# Update the counts after the first draw
remaining_balls <- total_balls - 1
remaining_non_cyan <- (magenta_balls + yellow_balls)

# Second draw probability (not cyan)
prob_second_not_cyan <- remaining_non_cyan / remaining_balls

# Total probability of first cyan and second not cyan
total_prob <- prob_first_cyan * prob_second_not_cyan
total_prob


# First draw probability (cyan)
prob_first_cyan <- cyan_balls / total_balls

# Second draw probability (not cyan) with replacement
prob_second_not_cyan <- (magenta_balls + yellow_balls) / total_balls

# Total probability of first cyan and second not cyan with replacement
total_prob_with_replacement <- prob_first_cyan * prob_second_not_cyan
total_prob_with_replacement

# Probability of not rolling a 6 in a single roll
prob_not_six_single <- 5/6

# Probability of not rolling a 6 in 6 rolls
prob_not_six_six_rolls <- prob_not_six_single^6
prob_not_six_six_rolls


# Probability that the Cavs win a single game
prob_cavs_win <- 0.6

# Probability that the Celtics lose all 7 games
prob_celtics_lose_all <- prob_cavs_win^7

# Probability that the Celtics win at least one game
prob_celtics_win_at_least_one <- 1 - prob_celtics_lose_all

# Output the result
prob_celtics_win_at_least_one


library(tidyverse)
library(dslabs)
x <- heights %>% filter(sex == "Male") %>% pull(height)

F <- function(a) mean(x <= a)

1 - F(70)

mean(x <= 68.5) - mean(x <= 67.5)
#> [1] 0.115
mean(x <= 69.5) - mean(x <= 68.5)
#> [1] 0.119
mean(x <= 70.5) - mean(x <= 69.5)
#> [1] 0.122




pnorm(68.5, m, s) - pnorm(67.5, m, s) 
#> [1] 0.103
pnorm(69.5, m, s) - pnorm(68.5, m, s) 
#> [1] 0.11
pnorm(70.5, m, s) - pnorm(69.5, m, s) 

n <- length(x)
m <- mean(x)
s <- sd(x)
simulated_heights <- rnorm(n, m, s)


B <- 10000
tallest <- replicate(B, {
  simulated_data <- rnorm(800, m, s)
  max(simulated_data)
})

mean(tallest >= 7*12)

x <- seq(-4, 4, length.out = 100)
qplot(x, f, geom = "line", data = data.frame(x, f = dnorm(x)))

# Given values
mean_height <- 64
sd_height <- 3
height_threshold <- 60  # 5 feet in inches

# Calculate the probability
probability <- pnorm(height_threshold, mean = mean_height, sd = sd_height)
probability


# Given values
mean_height <- 64
sd_height <- 3
height_threshold <- 72  # 6 feet in inches

# Calculate the probability for greater than or equal to 6 feet
probability <- 1 - pnorm(height_threshold, mean = mean_height, sd = sd_height)
probability

# Given values
mean_height <- 64
sd_height <- 3
lower_bound <- 61
upper_bound <- 67

# Calculate the probability
probability <- pnorm(upper_bound, mean = mean_height, sd = sd_height) - pnorm(lower_bound, mean = mean_height, sd = sd_height)
probability


# Given values in centimeters
mean_height_cm <- 64 * 2.54
sd_height_cm <- 3 * 2.54
lower_bound_cm <- 61 * 2.54
upper_bound_cm <- 67 * 2.54

# Calculate the probability in centimeters
probability_cm <- pnorm(upper_bound_cm, mean = mean_height_cm, sd = sd_height_cm) - pnorm(lower_bound_cm, mean = mean_height_cm, sd = sd_height_cm)
probability_cm


# Calculate the probability within 1 standard deviation from the mean
probability_within_1sd <- pnorm(1) - pnorm(-1)
probability_within_1sd

# Given parameters
mean_height <- 69
sd_height <- 3

# Find the height corresponding to the 99th percentile
height_99th_percentile <- qnorm(0.99, mean = mean_height, sd = sd_height)
height_99th_percentile

beads <- rep( c("red", "blue"), times = c(2,3))
X <- ifelse(sample(beads, 1) == "blue", 1, 0)

ifelse(sample(beads, 1) == "blue", 1, 0)

ifelse(sample(beads, 1) == "blue", 1, 0)

ifelse(sample(beads, 1) == "blue", 1, 0)

color <- rep(c("Black", "Red", "Green"), c(18, 18, 2))

n <- 1000
X <- sample(ifelse(color == "Red", -1, 1),  n, replace = TRUE)
X[1:10]

X <- sample(c(-1, 1), n, replace = TRUE, prob = c(9/19, 10/19))

X <- sample(c(-1, 1), n, replace = TRUE, prob = c(9/19, 10/19))
S <- sum(X)
S
