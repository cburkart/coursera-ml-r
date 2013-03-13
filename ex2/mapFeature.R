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