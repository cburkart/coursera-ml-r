# Standard sigmoid function
sigmoid <- function(z) {
	1/(1+exp(-z))
}

# Define function to create 
mapFeature <- function(x1,x2,degree=6){
	# Note that x1 and x2 must be the same size
	# Also note that this adds an intercept term

	out <- matrix(1,length(x1))
	for (i in 1:degree){
		for (j in 0:i){
			out <- cbind(out, (x1^(i-j))*(x2^j));
		}
	}
	return(out)
}

# Define cost function
costFunctionReg <- function(theta, X, y, lambda) {
	m <- length(y)
	trunctheta <- theta[2:length(theta)]
	J = -1/m*( sum(y*log(sigmoid(X%*%theta))) + sum((1-y)*log(1-sigmoid(X%*%theta)))) + (lambda/(2*m))*sum(trunctheta^2);
}

# Define gradient
gradFunction <- function(theta,X,y,lambda){
	m <- length(y)
	thetablock <- rbind(0,matrix(1,length(theta)-1))
	grad <- 1/m*(t(X)%*%(sigmoid(X%*%theta)-y)) + (lambda/m)*thetablock*theta
	return(as.numeric(grad))
}

# Predictor function
predict <- function(theta, X){
	m <- nrow(X)
	p <- sigmoid(X%*%theta) > .5
}
