# Define gradient
gradFunction <- function(theta,X,y){
	m <- length(y)
	grad <- 1/m*(t(X)%*%(sigmoid(X%*%theta)-y))
	return(as.numeric(grad))
}