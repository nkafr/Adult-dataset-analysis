costFunction  <- function(X, y) {
  #costfunction Computes cost for logistic regression
  
  function(theta) {

    m <- length(y); # m is the number of training examples
    
    #initialize J
    J <- 0
    
    #h is the hypothesis function
    h <- sigmoid(X %*% theta)
    #use a vectorized implementation insted of for loop for faster computation
    J <- (t(-y) %*% log(h) - t(1 - y) %*% log(1 - h)) / m  
    J
   
  }
}

grad <- function(X, y) {

  #   grad computes the gradient of the cost  w.r.t. to the parameters theta.
  function(theta) {
    
    m <- length(y); # m is the number of training examples
    
    #initialize grad vector
    grad <- matrix(0,dim(as.matrix(theta)))
    
    h <- sigmoid(X %*% theta)
    # calculate grads
    grad <- (t(X) %*% (h - y)) / m
    grad

    
  }
}