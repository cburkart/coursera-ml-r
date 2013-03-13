# Define cost function
costFunction <- function(theta,X,y){
	m <- length(y)	
	J <- -1/m*( sum(y*log(sigmoid(X%*%theta))) + sum((1-y)*log(1-sigmoid(X%*%theta))))
	return(J)
}
