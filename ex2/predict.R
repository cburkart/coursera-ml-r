# Predictor function
predict <- function(theta, X){
	m <- nrow(X)
	p <- sigmoid(X%*%theta) > .5
}
