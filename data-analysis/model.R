# Load the required libraries
library(pwr)
library(lsr)

# Load the experiment data
experiment_data <-
  read.csv("virtual-currencies-paper/data/experiment_data_formatted.csv")

# Summary statistics for the bid columns
summary(experiment_data$group_fraction_bid_in_pounds)
sd(experiment_data$group_fraction_bid_in_pounds, na.rm = TRUE)

summary(experiment_data$group_multiple_bid_in_pounds)
sd(experiment_data$group_multiple_bid_in_pounds, na.rm = TRUE)

# Plot a boxplot for the bid columns
boxplot(
  experiment_data$group_fraction_bid_in_pounds,
  experiment_data$group_multiple_bid_in_pounds,
  main = "Boxplot of fraction and multiple group bids (in Pounds)",
  names = c("Group Fraction", "Group Multiple"),
  xlab = "Variables",
  ylab = "Values"
)

# Perform a t-test to compare the means of the two groups
t.test(
  experiment_data$group_fraction_bid_in_pounds,
  experiment_data$group_multiple_bid_in_pounds
)

# Conduct a regression analysis to examine the relationship between
# the bid amounts and the demographic variables
model_group_fraction <-
  lm(group_fraction_bid_in_pounds ~ lived_abroad + travel_frequency + expenditure,
    data = experiment_data
  )
summary(model_group_fraction)

model_group_multiple <-
  lm(group_multiple_bid_in_pounds ~ lived_abroad + travel_frequency + expenditure,
    data = experiment_data
  )
summary(model_group_multiple)

# Conduct a power analysis to determine the power of the t test and
# sample size required for a power of 0.8 (80%)
effect_size <- cohensD(
  experiment_data$group_fraction_bid_in_pounds,
  experiment_data$group_multiple_bid_in_pounds
)
pwr.t2n.test(n1 = 21, n2 = 20, d = effect_size, sig.level = 0.05)
pwr.t.test(power = 0.8, d = effect_size, sig.level = 0.05)
