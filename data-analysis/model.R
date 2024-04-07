# Load the experiment data
experiment_data <- read.csv("virtual-currencies-paper/data/experiment_data.csv")

# Rename the columns to more meaningful names
colnames(experiment_data)[which(names(experiment_data) == "Q5a")] <- "group1_bid"
colnames(experiment_data)[which(names(experiment_data) == "Q5b")] <- "group2_bid"
colnames(experiment_data)[which(names(experiment_data) == "Q6")] <- "lived_abroad"
colnames(experiment_data)[which(names(experiment_data) == "Q7")] <- "travel_frequency"
colnames(experiment_data)[which(names(experiment_data) == "Q8")] <- "expenditure"

View(experiment_data)

# Convert the values of the bid columns to pounds
experiment_data$group1_bid_in_pounds <- experiment_data$group1_bid * 5
experiment_data$group2_bid_in_pounds <- experiment_data$group2_bid / 5

# Summary statistics for the bid columns
summary(experiment_data$group1_bid_in_pounds)
sd(experiment_data$group1_bid_in_pounds, na.rm = TRUE)

summary(experiment_data$group2_bid_in_pounds)
sd(experiment_data$group2_bid_in_pounds, na.rm = TRUE)

# Perform a t-test to compare the means of the two groups
t.test(experiment_data$group1_bid_in_pounds, experiment_data$group2_bid_in_pounds)

# Conduct a regression analysis to examine the relationship between the bid amounts and the demographic variables
model_group1 <- lm(group1_bid_in_pounds ~ lived_abroad + travel_frequency + expenditure, data = experiment_data)
summary(model_group1)

model_group2 <- lm(group2_bid_in_pounds ~ lived_abroad + travel_frequency + expenditure, data = experiment_data)
summary(model_group2)

# Plot a boxplot for columns the bid columns
boxplot(experiment_data$group1_bid_in_pounds, experiment_data$group2_bid_in_pounds,
    main = "Boxplot of group 1 and group 2 bids (in Pounds)",
    names = c("Group 1", "Group 2"),
    xlab = "Variables",
    ylab = "Values"
)

# Robustness checks
# TODO
