library(dslabs)
library(tidyverse)
polls <- polls_us_election_2016 |> 
  filter(enddate >= "2016-10-31" & state == "U.S.") 

N <- polls$samplesize[1]
x_hat <- polls$rawpoll_clinton[1]/100

library(dslabs)
library(tidyverse)

# Load the data
data("polls_us_election_2016")

# Filter the polls
polls <- polls_us_election_2016 %>%
  filter(enddate >= "2016-10-31" & state == "U.S.")

# Get the sample size and estimated Clinton percentage for the first poll
N <- polls$samplesize[1]
x_hat <- polls$rawpoll_clinton[1] / 100

# Calculate the standard error
SE <- sqrt(x_hat * (1 - x_hat) / N)

# Calculate the margin of error for a 95% confidence interval
Z <- 1.96
ME <- Z * SE

# Construct the confidence interval
lower_bound <- x_hat - ME
upper_bound <- x_hat + ME

# Print the results
cat("95% Confidence Interval for the election night proportion p:\n")
cat("Lower bound:", lower_bound, "\n")
cat("Upper bound:", upper_bound, "\n")






#Q2
library(dslabs)
library(dplyr)

# Load the data
data("polls_us_election_2016")

# Filter the polls
polls <- polls_us_election_2016 %>%
  filter(enddate >= "2016-10-31" & state == "U.S.") %>%
  mutate(
    x_hat = rawpoll_clinton / 100,
    se_hat = sqrt(x_hat * (1 - x_hat) / samplesize),
    lower = x_hat - 1.96 * se_hat,
    upper = x_hat + 1.96 * se_hat
  ) %>%
  select(pollster, enddate, x_hat, lower, upper)

# Print the resulting data frame
print(polls)

#Q3

library(dslabs)
library(dplyr)

# Load the data
data("polls_us_election_2016")

# Filter the polls
polls <- polls_us_election_2016 %>%
  filter(enddate >= "2016-10-31" & state == "U.S.") %>%
  mutate(
    x_hat = rawpoll_clinton / 100,
    se_hat = sqrt(x_hat * (1 - x_hat) / samplesize),
    lower = x_hat - 1.96 * se_hat,
    upper = x_hat + 1.96 * se_hat,
    hit = lower <= 0.482 & upper >= 0.482
  ) %>%
  select(pollster, enddate, x_hat, lower, upper, hit)

# Print the resulting data frame
print(polls)








