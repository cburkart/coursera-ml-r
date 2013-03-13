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
