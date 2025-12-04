library(tidyverse)
library(ggplot2)
library(lubridate)
library(scales)
library(dplyr)
library(lubridate)

data <- read.csv('games_cleaned.csv')
data$winning_conference <- as.factor(data$winning_conference)
data <- data |>
  mutate(game_date = ymd_hms(game_date),
         year = year(game_date))

# Bayesian Inference

# Data for the years 2020 & 2021
old <- data |>
  filter(year == 2020 | year == 2021)

# Data for the years 2022 & 2023
recent <- data |>
  filter(year == 2022 | year == 2023)

# Prior ----------------------------------------------------------

# Prior Belief -- Medium Strength
# West Wins 60% of the games between conferences
a <- 60
b <- 40

# Prior Plot
theta_grid <- seq(0, 1, length.out = 1000)
prior_likelihood <- dbeta(theta_grid, a, b)
plot(theta_grid, prior_likelihood, type = "l")


# Old Analysis ------------------------------------------------------------

old$wins <- ifelse(old$winning_conference == "W", 1, 0)
old_y <- sum(old$wins)
old_n <- length(old$wins)

# Old Posterior Distribution Plot
old_a_star <- a + old_y
old_b_star <- b + old_n - old_y
theta_grid <- seq(0, 1, length.out = 1000)
old_posterior_likelihood <- dbeta(theta_grid, old_a_star, old_b_star)
plot(theta_grid, old_posterior_likelihood, type = "l")

# Plotting Prior vs Old Posterior
theta_grid <- seq(0,1, length.out = 1000)
plot(theta_grid, old_posterior_likelihood, type = 'l', lwd = 2, col = 'blue')
lines(theta_grid, prior_likelihood, type = 'l', lwd = 2, col = 'red')

# Learning More From Our Posterior
old_post_mean <- old_a_star/(old_a_star + old_b_star) 
qbeta(c(0.025, 0.975), old_a_star, old_b_star)


# Recent Analysis ---------------------------------------------------------

recent$wins <- ifelse(recent$winning_conference == "W", 1, 0)
recent_y <- sum(recent$wins)
recent_n <- length(recent$wins)

# Recent Posterior Distribution Plot
recent_a_star <- a + recent_y
recent_b_star <- b + recent_n - recent_y
theta_grid <- seq(0, 1, length.out = 1000)
recent_posterior_likelihood <- dbeta(theta_grid, recent_a_star, recent_b_star)
plot(theta_grid, recent_posterior_likelihood, type = "l")

# Plotting Prior vs Old Posterior
theta_grid <- seq(0,1, length.out = 1000)
plot(theta_grid, recent_posterior_likelihood, type = 'l', lwd = 2, col = 'darkgreen')
lines(theta_grid, prior_likelihood, type = 'l', lwd = 2, col = 'red')

# Learning More From Our Posterior
recent_post_mean <- recent_a_star/(recent_a_star + recent_b_star) 
qbeta(c(0.025, 0.975), recent_a_star, recent_b_star)

# Old vs Recent Analysis --------------------------------------------------

theta_old <- rbeta(10000, a + old_y, b + old_n - old_y)
theta_recent <- rbeta(10000, a + recent_y, b + recent_n - recent_y)
diff <- theta_old - theta_recent
mean(diff > 0)
plot(density(diff))
quantile(diff, c(0.025, 0.975))

theta_old_prior <- rbeta(10000, a, b)
theta_recent_prior <- rbeta(10000, a, b)
likelihood <- density(theta_old_prior - theta_recent_prior)
new_likelihood <- density(diff)

plot(new_likelihood, col = "black", main = "Posterior vs Prior Density of θOld - θRecent",
     xlab = "θOld - θRecent", ylab = "Density", xlim = c(-.2, .2))

lines(likelihood, col = "gray")

legend("topright",c("Posterior", "Prior"), col = c("black", "gray"),
       lty = c(1, 1))
