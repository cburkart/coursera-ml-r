# Define cost function
costFunctionReg <- function(theta, X, y, lambda) {
	m <- length(y)
	trunctheta <- theta[2:length(theta)]
	J = -1/m*( sum(y*log(sigmoid(X%*%theta))) + sum((1-y)*log(1-sigmoid(X%*%theta)))) + (lambda/(2*m))*sum(trunctheta^2);
}
