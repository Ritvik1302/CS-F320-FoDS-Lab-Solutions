library(e1071)
library(ggplot2)
library(caret)

set.seed(1234)

n <- 1000

x1 <- runif(n, min = -2, max = 2)
x2 <- runif(n, min = -2, max = 2)

center1 <- c(-0.8, 0)
center2 <- c(0.8, 0)
radius <- 0.8

distance_to_center <- function(x, center) {
  sqrt((x[,1] - center[1])^2 + (x[,2] - center[2])^2)
}

points <- cbind(x1, x2)

y <- ifelse(
  distance_to_center(points, center1) <= radius | distance_to_center(points, center2) <= radius,
  -1,
  1
)

data <- data.frame(x1 = x1, x2 = x2, y = factor(y))

ggplot(data, aes(x = x1, y = x2, color = y)) +
  geom_point(alpha = 0.6) +
  scale_color_manual(values = c("-1" = "red", "1" = "blue")) +
  theme_minimal() +
  labs(title = "Binary Classification with Two Touching Circles",
       x = "x1",
       y = "x2",
       color = "Class") +
  theme(legend.position = "bottom")

trainIndex <- createDataPartition(data$y, p = .8, 
                                  list = FALSE, 
                                  times = 1)
trainData <- data[ trainIndex,]
testData  <- data[-trainIndex,]

svm_linear <- svm(y ~ ., data = trainData, kernel = "linear")
svm_quadratic <- svm(y ~ ., data = trainData, kernel = "polynomial", degree = 2)
svm_rbf <- svm(y ~ ., data = trainData, kernel = "radial")

pred_linear <- predict(svm_linear, testData)
pred_quadratic <- predict(svm_quadratic, testData)
pred_rbf <- predict(svm_rbf, testData)

accuracy_linear <- sum(pred_linear == testData$y) / nrow(testData)
accuracy_quadratic <- sum(pred_quadratic == testData$y) / nrow(testData)
accuracy_rbf <- sum(pred_rbf == testData$y) / nrow(testData)

print(paste("Accuracy with Linear Kernel: ", accuracy_linear))
print(paste("Accuracy with Quadratic Kernel: ", accuracy_quadratic))
print(paste("Accuracy with RBF Kernel: ", accuracy_rbf))

plot_decision_boundary <- function(model, data, title) {
  grid <- expand.grid(x1 = seq(-2, 2, length.out = 200),
                      x2 = seq(-2, 2, length.out = 200))
  
  grid$y <- predict(model, grid)
  
  ggplot() +
    geom_point(data = data, aes(x = x1, y = x2, color = y), alpha = 0.6) +
    geom_contour(data = grid, aes(x = x1, y = x2, z = as.numeric(y)), breaks = 0.5, color = "black") +
    scale_color_manual(values = c("-1" = "red", "1" = "blue")) +
    theme_minimal() +
    labs(title = title, x = "x1", y = "x2", color = "Class") +
    theme(legend.position = "bottom")
}

p1 <- plot_decision_boundary(svm_linear, data, "SVM with Linear Kernel")
p2 <- plot_decision_boundary(svm_quadratic, data, "SVM with Quadratic Kernel")
p3 <- plot_decision_boundary(svm_rbf, data, "SVM with RBF Kernel")

print(p1)
print(p2)
print(p3)
