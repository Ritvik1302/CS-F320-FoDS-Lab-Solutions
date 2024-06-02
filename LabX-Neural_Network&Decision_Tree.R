#1. Neural Network
library(neuralnet)
library(MASS)
library(ggplot2)

set.seed(500)

data <- Boston

apply(data, 2, function(x) sum(is.na(x)))

index <- sample(1:nrow(data), round(0.75 * nrow(data)))
train <- data[index, ]
test <- data[-index, ]

lm.fit <- glm(medv ~ ., data = train)
summary(lm.fit)

pr.lm <- predict(lm.fit, test)
MSE.lm <- sum((pr.lm - test$medv) ^ 2) / nrow(test)

maxs <- apply(data, 2, max)
mins <- apply(data, 2, min)
scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))
train_ <- scaled[index, ]
test_ <- scaled[-index, ]

n <- names(train_)
f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = "+")))

train_nn <- function(hidden_layers) {
  nn <- neuralnet(f, data = train_, hidden = hidden_layers, linear.output = TRUE)
  pr.nn <- compute(nn, test_[, 1:13])
  pr.nn_ <- pr.nn$net.result * (max(data$medv) - min(data$medv)) + min(data$medv)
  test.r <- (test_$medv) * (max(data$medv) - min(data$medv)) + min(data$medv)
  MSE.nn <- sum((test.r - pr.nn_) ^ 2) / nrow(test_)
  return(MSE.nn)
}

hidden_layer_configs <- list(
  c(5),
  c(5, 3),
  c(5, 3, 2),
  c(6, 4, 2),
  c(7, 5, 3)
)

MSE_results <- sapply(hidden_layer_configs, train_nn)

for (i in 1:length(hidden_layer_configs)) {
  cat("Hidden layers:", hidden_layer_configs[[i]], " - MSE:", MSE_results[i], "\n")
}

plot_data <- data.frame(
  Configuration = sapply(hidden_layer_configs, function(x) paste(x, collapse = "-")),
  MSE = MSE_results
)

ggplot(plot_data, aes(x = Configuration, y = MSE)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  labs(title = "MSE for Different Hidden Layer Configurations", x = "Hidden Layer Configuration", y = "MSE")

cat("MSE for Linear Model:", MSE.lm, "\n")

best_model_index <- which.min(MSE_results)
best_hidden_layers <- hidden_layer_configs[[best_model_index]]
nn_best <- neuralnet(f, data = train_, hidden = best_hidden_layers, linear.output = TRUE)
pr.nn_best <- compute(nn_best, test_[, 1:13])
pr.nn_best_ <- pr.nn_best$net.result * (max(data$medv) - min(data$medv)) + min(data$medv)

par(mfrow = c(1, 2))
plot(test$medv, pr.nn_best_, col = 'red', main = 'Real vs predicted NN', pch = 18, cex = 0.7)
abline(0, 1, lwd = 2)
legend('bottomright', legend = 'NN', pch = 18, col = 'red', bty = 'n')
plot(test$medv, pr.lm, col = 'blue', main = 'Real vs predicted lm', pch = 18, cex = 0.7)
abline(0, 1, lwd = 2)
legend('bottomright', legend = 'LM', pch = 18, col = 'blue', bty = 'n', cex = .95)

# 2. Decision Tree
library(C50)
library(gmodels)
library(caret)

income <- read.csv("income.csv", stringsAsFactors = TRUE)
str(income)

set.seed(1)

train_index <- createDataPartition(income$income, p = 0.75, list = FALSE)
income_train <- income[train_index, ]
income_test <- income[-train_index, ]

income_model <- C5.0(income_train[-13], income_train$income)

summary(income_model)

income_pred <- predict(income_model, income_test)

confusion_matrix <- confusionMatrix(income_pred, income_test$income)
print(confusion_matrix)

CrossTable(income_test$income, income_pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)
