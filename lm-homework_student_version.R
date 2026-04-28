# let's create vector with mean 3 and standard deviation 1
# normally distributed in interval 0-5
# 100 observations

set.seed(123)
x <- rnorm(100, mean = 3, sd = 1)
hist(x, breaks = 20, col = "lightblue", main = "Histogram of x", xlab = "x")

# now let's make for loop to create 72*100 observations
# and store them in 24 vectors a - x
# each vector will have different mean, starting from 0.5 and going up by 0.1

for (i in 1:50) {
  assign(paste0("a", i), rnorm(100, mean = 0.5 + 0.1 * i, sd = 1))
}

# next let's create 24 vectors A - X

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

model <- lmer(value ~ condition + (1|participant), data = df2, REML = FALSE)

summary(model)

# error message
# let's do without random effects

model2 <- lm(value ~ condition, data = df2)

summary(model2)
