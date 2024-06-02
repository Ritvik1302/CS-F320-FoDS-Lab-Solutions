library(tidyverse)
library(class)

heart <- read_csv("heart.csv", col_types = "nffnnffnfnfnff")
glimpse(heart)
summary(heart)

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

heart <- heart %>%
  mutate(age = normalize(age)) %>%
  mutate(restingBP = normalize(trestbps)) %>%
  mutate(cholesterol = normalize(chol)) %>%
  mutate(STdepression = normalize(as.numeric(oldpeak)))

summary(heart)

heart <- data.frame(heart)
heart_labels <- heart %>% select(target)
heart <- heart %>% select(-target)

set.seed(1234)
sample_index <- sample(nrow(heart), round(nrow(heart) * 0.75), replace = FALSE)
heart_train <- heart[sample_index, ]
heart_test <- heart[-sample_index, ]

heart_train_labels <- as.factor(heart_labels[sample_index, ])
heart_test_labels <- as.factor(heart_labels[-sample_index, ])

k_values <- 1:40
accuracies <- numeric(length(k_values))

for (k in k_values) {
  heart_pred <- knn(
    train = heart_train,
    test = heart_test,
    cl = heart_train_labels,
    k = k
  )
  heart_pred_table <- table(heart_test_labels, heart_pred)
  accuracies[k] <- sum(diag(heart_pred_table)) / nrow(heart_test)
}

accuracy_df <- data.frame(k = k_values, accuracy = accuracies)

ggplot(accuracy_df, aes(x = k, y = accuracy)) +
  geom_point() +
  geom_line() +
  labs(title = "K-NN Predictive Accuracy for Different K Values",
       x = "Number of Neighbors (k)",
       y = "Accuracy") +
  theme_minimal()
