# Load the experiment data
experiment_data <- read.csv("virtual-currencies-paper/data/experiment_data.csv")

experiment_data$Q5a_in_pounds <- experiment_data$Q5a * 5
experiment_data$Q5b_in_pounds <- experiment_data$Q5b / 5

# Plot boxplots for columns Q5a and Q5b
boxplot(experiment_data$Q5a_in_pounds, experiment_data$Q5b_in_pounds,
    main = "Boxplot of Q5a and Q5b (in Pounds)",
    names = c("Q5a", "Q5b"),
    xlab = "Variables",
    ylab = "Values"
)
