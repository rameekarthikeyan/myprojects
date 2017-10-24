Logistic <- function(X,Y,learning_rate = 0.005, numiterations = 10000, Xtest) {
#### X should be a data table and should have dimensions of number of features as number of rows and number of training examples as number of columns
#### Y should be a data table and should have one row and number of training examples as number of columns
#### this logistic regression code works well for binary classification 
#### Xtest should be a data table and it is the test set for which classification has to be performed

#### Defining Sigmoid function
sigm <- function(z) {
  1/(1+ exp(-z))
}
#### Converting data table X to matrix
m <- dim(X)[2]
if ((class(X))[[1]] != "matrix") {
X <- as.matrix(X)
}
#### Converting data table Y to matrix
if ((class(Y))[[1]] != "matrix") {
Y <- as.matrix(Y)
}
#### Converting data table Xtest to matrix
if ((class(Xtest))[[1]] != "matrix") {
  Xtest <- as.matrix(Xtest)
}
#### Defining the parameters - theta and b
theta <- matrix(rep(0, dim(X)[1]),nrow = dim(X)[1], ncol = 1)
theta <- t(theta)
b <- 0

for (iter in 1:numiterations) {
#### Defining the Linear part of logistic regression
powerp <- (theta %*% X) + b
#### Defining yhat which is sigmoid function
yhat <- sigm(powerp)
#### Defining Logistic loss
L <- -Y %*% t(log(yhat)) - ((1-Y) %*% t(log(1-yhat)))
#### Defininf the total cost 
cost <- (1/(1*m))*(sum(L))
if (iter %% 100 == 0) {
print(cost)
}
grad_theta <- (1/(1*m))*((yhat - Y) %*% t(X))
grad_b <- (1/(1*m))*sum((yhat - Y) %*% t(yhat-Y))
theta <- theta - (learning_rate*grad_theta)
b <- b - (learning_rate*grad_b)
}
#### Predicting the response as probabilities for Xtest
predictedprob <- sigm((theta %*% Xtest) + b)
#### Predicting the response as category for Xtest
predictedclass <- as.numeric(predictedprob > 0.5)
#### Returning the output of the logistic regression function
return(list(theta,b, predictedprob, predictedclass))
}
