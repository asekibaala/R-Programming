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


#Q4
library(dslabs)
library(dplyr)

# Load the data
data("polls_us_election_2016")

# Filter the polls and calculate confidence intervals
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

# Calculate the proportion of confidence intervals that included p
proportion_hit <- mean(polls$hit)

# Print the proportion
cat("Proportion of confidence intervals that included p:", proportion_hit, "\n")

#######
library(dslabs)
library(dplyr)

# Load the data
data("polls_us_election_2016")

# Filter the polls and calculate confidence intervals
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

# Calculate the proportion of confidence intervals that included p
proportion_hit <- mean(polls$hit)

# Print the proportion
cat("Proportion of confidence intervals that included p:", proportion_hit, "\n")

#Q6

library(dslabs)
library(dplyr)

# Load the data
data("polls_us_election_2016")

# Filter the polls and calculate confidence intervals for the difference
polls <- polls_us_election_2016 %>%
  filter(enddate >= "2016-10-31" & state == "U.S.") %>%
  mutate(
    mu_hat = rawpoll_clinton / 100 - rawpoll_trump / 100,
    se_hat = sqrt((mu_hat * (1 - mu_hat) + (1 - mu_hat) * mu_hat) / samplesize),
    lower = mu_hat - 1.96 * se_hat,
    upper = mu_hat + 1.96 * se_hat,
    hit = lower <= 0.021 & upper >= 0.021
  ) %>%
  select(pollster, enddate, mu_hat, lower, upper, hit)

# Print the resulting data frame
print(polls)

# Calculate the proportion of confidence intervals that included mu
proportion_hit <- mean(polls$hit)

# Print the proportion
cat("Proportion of confidence intervals that included mu:", proportion_hit, "\n")




library(dslabs)
library(dplyr)

# Load the data
data("polls_us_election_2016")

# Filter the polls and calculate confidence intervals for the difference
polls <- polls_us_election_2016 %>%
  filter(enddate >= "2016-10-31" & state == "U.S.") %>%
  mutate(
    mu_hat = rawpoll_clinton / 100 - rawpoll_trump / 100,
    se_hat = sqrt((rawpoll_clinton / 100 * (1 - rawpoll_clinton / 100) / samplesize) + 
                    (rawpoll_trump / 100 * (1 - rawpoll_trump / 100) / samplesize)),
    lower = mu_hat - 1.96 * se_hat,
    upper = mu_hat + 1.96 * se_hat,
    hit = lower <= 0.021 & upper >= 0.021
  ) %>%
  select(pollster, enddate, mu_hat, lower, upper, hit)

# Print the resulting data frame
print(polls)

# Calculate the proportion of confidence intervals that included mu
proportion_hit <- mean(polls$hit)

# Print the proportion
cat("Proportion of confidence intervals that included mu:", proportion_hit, "\n")


library(dslabs)
library(dplyr)

# Load the data
data("polls_us_election_2016")

# Filter the polls and calculate confidence intervals for the difference
polls <- polls_us_election_2016 %>%
  filter(enddate >= "2016-10-31" & state == "U.S.") %>%
  mutate(
    mu_hat = rawpoll_clinton / 100 - rawpoll_trump / 100,
    se_hat = sqrt((rawpoll_clinton / 100 * (1 - rawpoll_clinton / 100) / samplesize) + 
                    (rawpoll_trump / 100 * (1 - rawpoll_trump / 100) / samplesize)),
    lower = mu_hat - 1.96 * se_hat,
    upper = mu_hat + 1.96 * se_hat,
    hit = lower <= 0.021 & upper >= 0.021
  ) %>%
  select(pollster, enddate, mu_hat, lower, upper, hit)

# Print the resulting data frame
print(polls)

# Calculate the proportion of confidence intervals that included mu
proportion_hit <- mean(polls$hit)

# Print the proportion
cat("Proportion of confidence intervals that included mu:", proportion_hit, "\n")


library(dslabs)
library(dplyr)

# Load the data
data("polls_us_election_2016")

# Filter the polls and calculate confidence intervals for the difference
polls <- polls_us_election_2016 %>%
  filter(enddate >= "2016-10-31" & state == "U.S.") %>%
  mutate(
    mu_hat = rawpoll_clinton / 100 - rawpoll_trump / 100,
    se_hat = sqrt((rawpoll_clinton / 100 * (1 - rawpoll_clinton / 100) / samplesize) + 
                    (rawpoll_trump / 100 * (1 - rawpoll_trump / 100) / samplesize)),
    lower = mu_hat - 1.96 * se_hat,
    upper = mu_hat + 1.96 * se_hat,
    hit = lower <= 0.021 & upper >= 0.021
  ) %>%
  select(pollster, enddate, mu_hat, lower, upper, hit)

# Print the resulting data frame
print(polls)

# Calculate the proportion of confidence intervals that included mu
proportion_hit <- mean(polls$hit)

# Print the proportion
cat("Proportion of confidence intervals that included mu:", proportion_hit, "\n")


library(dslabs)
library(dplyr)
library(ggplot2)

# Load the data
data("polls_us_election_2016")

# Filter the polls and calculate the error
polls <- polls_us_election_2016 %>%
  filter(enddate >= "2016-10-31" & state == "U.S.") %>%
  mutate(
    mu_hat = rawpoll_clinton / 100 - rawpoll_trump / 100,
    error = mu_hat - 0.021
  )

# Create the plot
ggplot(polls, aes(x = pollster, y = error)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Error in Polls' Estimates of the Difference (mu)",
       x = "Pollster",
       y = "Error (mu_hat - 0.021)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))





library(dslabs)
library(dplyr)
library(ggplot2)

# Load the data
data("polls_us_election_2016")

# Filter the polls and calculate the error
polls <- polls_us_election_2016 %>%
  filter(enddate >= "2016-10-31" & state == "U.S.") %>%
  mutate(
    mu_hat = rawpoll_clinton / 100 - rawpoll_trump / 100,
    error = mu_hat - 0.021
  )

# Filter pollsters with five or more polls
polls_filtered <- polls %>%
  group_by(pollster) %>%
  filter(n() >= 5) %>%
  ungroup()

# Create the plot
ggplot(polls_filtered, aes(x = pollster, y = error)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Estimates of the Difference (mu)",
       x = "Pollster",
       y = "Error (mu_hat - 0.021)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))





##################12th-1-2025###############
# Parameters
N <- 1000
theta <- 0.5

# Simulate samples and calculate p-values
set.seed(123) # For reproducibility
p_values <- replicate(10000, {
  x <- rbinom(1, N, theta) / N
  z <- sqrt(N) * (x - theta) / sqrt(theta * (1 - theta))
  2 * (1 - pnorm(abs(z)))
})

# Report results
mean(p_values < 0.05) # Proportion of p-values below 0.05
mean(p_values < 0.01) # Proportion of p-values below 0.01


# Plot histogram
hist(p_values, main = "Histogram of p-values", xlab = "p-value", breaks = 30, col = "blue", border = "white")

# Parameters
theta <- 0.52

# Simulate samples and calculate p-values
p_values_52 <- replicate(10000, {
  x <- rbinom(1, N, theta) / N
  z <- sqrt(N) * (x - 0.5) / sqrt(0.5 * (1 - 0.5)) # Testing against null hypothesis theta = 0.5
  2 * (1 - pnorm(abs(z)))
})

# Report results
mean(p_values_52 < 0.05) # Proportion of p-values below 0.05
1 - mean(p_values_52 < 0.05) # 1 - power


# Define values for N and theta
values <- expand.grid(N = c(25, 50, 100, 500, 1000), theta = seq(0.51, 0.75, by = 0.01))

# Initialize results dataframe
results <- data.frame()

# Loop through combinations of N and theta
for (i in 1:nrow(values)) {
  N <- values$N[i]
  theta <- values$theta[i]
  
  # Simulate p-values
  p_vals <- replicate(10000, {
    x <- rbinom(1, N, theta) / N
    z <- sqrt(N) * (x - 0.5) / sqrt(0.5 * (1 - 0.5)) # Null hypothesis theta = 0.5
    2 * (1 - pnorm(abs(z)))
  })
  
  # Compute power (proportion of p-values < 0.05)
  power <- mean(p_vals < 0.05)
  
  # Store results
  results <- rbind(results, data.frame(N = N, theta = theta, power = power))
}

# Plot power as a function of N for each theta
library(ggplot2)

ggplot(results, aes(x = N, y = power, color = as.factor(theta), group = theta)) +
  geom_line() +
  labs(title = "Power as a Function of Sample Size (N)",
       x = "Sample Size (N)",
       y = "Power",
       color = expression(theta)) +
  theme_minimal()




