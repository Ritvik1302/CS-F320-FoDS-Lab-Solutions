# Load the required library 
library(ggplot2)

# Objective function 
objective <- function(x) { 
  return(x^2) 
} 

# Derivative of objective function 
derivative <- function(x) { 
  return(2 * x) 
} 

# Gradient Descent 
gradient_descent <- function(objective, derivative, bounds, n_iter, step_size) { 
  solutions <- vector("numeric", n_iter) 
  scores <- vector("numeric", n_iter) 
   
  solution <- bounds[1, 1] + runif(1) * (bounds[1, 2] - bounds[1, 1]) 
   
  for (i in 1:n_iter) { 
    gradient <- derivative(solution) 
    
    solution <- solution - step_size * gradient 
     
    solution_eval <- objective(solution) 
     
    solutions[i] <- solution 
    scores[i] <- solution_eval 
    
    cat(sprintf(">%d f(%s) = %.5f\n", i, solution, solution_eval)) 
  } 
  
  return(list(solutions, scores)) 
} 

gradient_descent_nesterov <- function(objective, derivative, bounds, n_iter, step_size, momentum) { 
  solutions <- vector("numeric", n_iter) 
  scores <- vector("numeric", n_iter) 
  
  solution <- bounds[1, 1] + runif(1) * (bounds[1, 2] - bounds[1, 1]) 
  
  change <- 0.0 
  
  for (i in 1:n_iter) { 
    lookahead_solution <- solution - momentum * change
    
    gradient <- derivative(lookahead_solution) 
    
    new_change <- step_size * gradient + momentum * change 
    
    solution <- solution - new_change 
    
    change <- new_change 
    
    solution_eval <- objective(solution) 
    
    solutions[i] <- solution 
    scores[i] <- solution_eval 
    
    cat(sprintf(">%d f(%s) = %.5f\n", i, solution, solution_eval)) 
  } 
  
  return(list(solutions, scores)) 
} 

set.seed(1234) 
 
bounds <- matrix(c(-1.0, 1.0), ncol = 2) 
 
n_iter <- 30 
momentum <- 0.3 

step_size <- 0.1 

gd <- gradient_descent(objective, derivative, bounds, n_iter, step_size) 
gdm_nesterov <- gradient_descent_nesterov(objective, derivative, bounds, n_iter, step_size, momentum)

library(ggpubr) 
df <- data.frame(x = gd[[1]], y = gd[[2]])  
df <- df %>% mutate(x1 = gdm_nesterov[[1]], y1 = gdm_nesterov[[2]]) 
df 

inputs <- seq(from = bounds[1, 1], to = bounds[1, 2], by = 0.1) 

results <- objective(inputs) 

p1 <- ggplot(data = data.frame(x = inputs, y = results), aes(x = x, y = y)) + 
  geom_line() + 
  labs(title = "Objective Function: x^2", x = "x", y = "f(x)") + 
  theme_minimal() 

p2 <- ggplot(data = df, aes(x = x, y = y)) + 
  geom_point(color = "red") + 
  labs(title = "Gradient Descent", x = "x", y = "f(x)") + 
  theme_minimal() 
p3 <- ggplot(data = df, aes(x = x1, y = y1)) + 
  geom_point(color = "red") + 
  labs(title = "Nesterov Momentum Gradient Descent", x = "x", y = "f(x)") + 
  theme_minimal() 

# Show both plots side by side 
library(gridExtra) 
grid.arrange(p1, p2, p3, ncol = 3)
