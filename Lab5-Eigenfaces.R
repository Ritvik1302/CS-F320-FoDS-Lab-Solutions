# Load necessary libraries
library(tidyverse)
library(ggpubr)
library(recipes)

# Function to display a face image
showFace <- function(x) {
  x %>%
    as.numeric() %>%
    matrix(nrow = 64, byrow = TRUE) %>%
    apply(2, rev) %>%
    t %>%
    image(col = grey(seq(0, 1, length = 256)),
          xaxt = "n",
          yaxt = "n")
}

# Function to calculate difference
calDif <- function(x) {
  ((x - coefTestSel) %*% t(x - coefTestSel)) %>%
    sqrt
}

dataX <- read.csv("face_preprocessed_data.csv", header = FALSE) %>%
  data.frame()

par(mfrow = c(4, 10))
par(mar = c(0.05, 0.05, 0.05, 0.05))

dataY <- rep(1:7, each = 10) %>% data.frame() %>%
  mutate(index = row_number()) %>%
  select(index, label = 1)

set.seed(1234)
trainSampInd <- dataY %>%
  group_by(label) %>%
  sample_n(8) %>%
  arrange(index)

testSampInd <- setdiff(dataY, trainSampInd)

dataMat <- dataX %>%
  filter(row_number() %in% trainSampInd[, "index", drop = TRUE]) %>%
  data.matrix() %>%
  `rownames<-`(trainSampInd[, "label", drop = TRUE])

testDataMat <- dataX %>%
  filter(row_number() %in% testSampInd[, "index", drop = TRUE]) %>%
  data.matrix() %>%
  `rownames<-`(testSampInd[, "label", drop = TRUE])

dataMatCen <- scale(dataMat, center = TRUE, scale = FALSE)
covMat <- t(dataMatCen) %*% dataMatCen / (nrow(dataMat) - 1)
eig <- eigen(covMat)
eigVec <- eig$vectors
eigVal <- eig$values

svd_result <- svd(dataMatCen)
U <- svd_result$u
Sigma <- diag(svd_result$d)
V <- svd_result$v

eigVal <- svd_result$d ^ 2 / (ncol(dataMatCen) - 1)
varProp <- eigVal / sum(eigVal)
varCumProp <- cumsum(eigVal) / sum(eigVal)

par(mfrow = c(2, 1))
plot(varProp * 100, xlab = "Eigenvalues", ylab = "Percentage", main = "Proportion in the total variance")
plot(varCumProp * 100, xlab = "Eigenvalues", ylab = "Percentage", main = "Proportion of the cumulative variance in the total variance")

thresNum <- min(which(varCumProp > 0.90))
eigVecSel <- eigVec[, 1:thresNum]

par(mfrow = c(4, 4))
par(mar = c(0.05, 0.05, 0.05, 0.05))
for (i in 1:dim(eigVecSel)[2]) {
  showFace(eigVecSel[, i])
}

coefTrainFaces <- dataMatCen %*% eigVecSel %>%
  `rownames<-`(rownames(dataMat))
coefTestFaces <- testDataMat %>%
  apply(1, function(x) x - colMeans(dataMat)) %>%
  t %*% eigVecSel

par(mfrow = c(1, 2))
par(mar = c(0.05, 0.05, 0.05, 0.05))
showFace(dataMat[1, ])
showFace(coefTrainFaces[1, ] %*% t(eigVecSel) + colMeans(dataMat))

testRes <- data.frame(
  `Label of image` = NA,
  `Label identified in test` = NA,
  `Correct (1) / Wrong (0)` = NA,
  stringsAsFactors = FALSE
)
for (i in 1:nrow(coefTestFaces)) {
  coefTestSel <- coefTestFaces[i, , drop = FALSE]
  difCoef <- apply(coefTrainFaces, 1, calDif)
  testRes[i, 1] <- rownames(coefTestFaces)[i]
  testRes[i, 2] <- rownames(coefTrainFaces)[which.min(difCoef)]
}
testRes[, 3] <- ifelse(testRes[, 2] == testRes[, 1], 1, 0)

print(head(testRes))
shareCor <- sum(testRes[, 3]) / nrow(testRes)
print(shareCor)
