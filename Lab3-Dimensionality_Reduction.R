#1. SVD
A <- matrix(c(-1, 2, 2), nrow = 3) 

#  Compute the SVD using the `svd()` function in R. 
svd_result <- svd(A) 
svd_result
# Extract the matrices U, Σ, and V^T from the SVD result. 
U <- svd_result$u 
Sigma <- diag(svd_result$d) 
V <- svd_result$v 

#2.Reduced SVD
A <- matrix(c(2, 0, 8, 6, 0,
              1, 6, 0, 1, 7,
              5, 0, 7, 4, 0,
              7, 0, 8, 5, 0,
              0, 10, 0, 0, 7), nrow = 5, byrow = TRUE)

svd_result <- svd(A)

U <- svd_result$u
Sigma <- diag(svd_result$d)
V <- svd_result$v

rank_A <- sum(svd_result$d > 1e-10)

U_reduced <- U[, 1:rank_A]
Sigma_reduced <- Sigma[1:rank_A, 1:rank_A]
V_reduced <- V[, 1:rank_A]

#3.PCA
set.seed(1)
x<-c(2.5, 0.5, 2.2, 1.9, 3.1, 2.3, 2, 1, 1.5, 1.1) 
y<-c(2.4, 0.7, 2.2, 1.9, 3.1, 2.3, 2, 1, 1.5, 0.9) 

plot(x,y) 
dataset <- data.frame(x,y) 
dataset 

pca = prcomp(dataset,center=TRUE) 
pca 
summary(pca) 