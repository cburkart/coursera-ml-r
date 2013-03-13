# Define sigmoid function
sigmoid <- function(z) {
	1/(1+exp(-z))
}

# Define cost function
costFunction <- function(theta,X,y){
	m <- length(y)	
	J <- -1/m*( sum(y*log(sigmoid(X%*%theta))) + sum((1-y)*log(1-sigmoid(X%*%theta))))
	return(J)
}

# Define gradient
gradFunction <- function(theta,X,y){
	m <- length(y)
	grad <- 1/m*(t(X)%*%(sigmoid(X%*%theta)-y))
	return(as.numeric(grad))
}