# let's create vector with mean 3 and standard deviation 1
# normally distributed in interval 0-5
# 100 observations

set.seed(123)
x <- rnorm(100, mean = 3, sd = 1)
hist(x, breaks = 20, col = "lightblue", main = "Histogram of x", xlab = "x")

# now let's make for loop to create 2*50*100 observations
# and store them in 50 vectors a1 - a50
# each vector will have different mean, starting from 0.5 and going up by 0.1

for (i in 1:50) {
  assign(paste0("a", i), rnorm(100, mean = 0.5 + 0.1 * i, sd = 1))
}

# next let's create 50 vectors A1-A50

for (i in 1:50) {
  assign(paste0("A", i), rnorm(100, mean = 3 + 0.1*i, sd = 1))
}

# now let's put all the vectors into one data frame

df <- data.frame()

for (i in 1:50) {
  df <- rbind(df, data.frame(condition = paste0("a", i), value = get(paste0("a", i))))
}

for (i in 1:50) {
  df <- rbind(df, data.frame(condition = paste0("A", i), value = get(paste0("A", i))))
}

# let's add x as a condition to the data frame

df <- rbind(df, data.frame(condition = "x", value = x))

# now let's make a boxplot

boxplot(value ~ condition, data = df, col = "lightblue", main = "Boxplot of x and aw", xlab = "condition", ylab = "value")

# and load standard lme4 package
# plus lmerTest for p-values

# library(lme4)
library(lmerTest)

# subset x as baseline and aa, Aa as conditions
# via tidyverse

library(tidyverse)

df2 <- df %>%
  filter(condition %in% c("x", "a1", "A1"))

# ggplot histogram for df2

library(ggplot2)

ggplot(df2, aes(x = value, fill = condition)) +
  geom_histogram(bins = 20, alpha = 0.5, position = "identity") +
  labs(title = "Histogram of x, a1 and A1", x = "value", y = "count") +
  theme_minimal()

# let's add to df2 simulated participants: each condition has 20 participants
# and each participant has 5 observations

df2 <- df2 %>%
  group_by(condition) %>%
  mutate(participant = rep(1:20, each = 5)) %>%
  ungroup()


# let's compute linear model with x as baseline

as.factor(df2$condition) -> df2$condition

as.factor(df2$participant) -> df2$participant

df2$condition <- relevel(df2$condition, ref = "x")

model <- lmer(value ~ condition + (1 | participant), data = df2, REML = FALSE)

summary(model)

# error message
# let's do without random effects

model2 <- lm(value ~ condition, data = df2)

summary(model2)


# let's load csv of student's names, semicolon separated

students <- read.csv("seznam_export\ (1).csv", sep = ";")

# add column condition to students data frame
# and fill it from ab to az
# but in reverse order

students$condition <- paste0("a", seq(2, 32, by = 1))
students$condition2 <- paste0("A", seq(2, 32, by = 1))

# students$condition <- sample(c("x", "aa"), nrow(students), replace = TRUE)
# students$condition <- paste0("a", letters[1:nrow(students)])

print(students)

write.csv(students, "students.csv", row.names = FALSE)

# Bayesian version

library(rstanarm)
library(bayestestR)

# rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# the same model in rstanarm

model_bayes <- stan_lmer(value ~ condition + (1|condition), data = df2, QR = TRUE)

summary(model_bayes)

# describe posterior distribution

plot(model_bayes)

# credibility intervals, ROPE

describe_posterior(model_bayes)

# graph

plot(estimate_density(model_bayes))
