#rm(list = ls(all=TRUE))


# Load data
setwd('/Users/cburkart/GoodSync Folder/Data Science/Andrew Ng AI Course/coursera/ex2')

source('sigmoid.R')
source('mapFeature.R')
source('gradFunctionReg.R')
source('costFunctionReg.R')
source('predict.R')

setwd('/Users/cburkart/GoodSync Folder/Data Science/Andrew Ng AI Course/coursera/ex2/ex2')
data <- read.csv('ex2data2.txt',header=F);
names(data) <- c('Microchip Test 1','Microchip Test 2','Accepted')

X <- data[,1:2]
y <- data[,3]



# Plot data to understand the classification problem
#plot(X,pch=c(24,3)[y+1],bg='yellow')
#legend(.8,1,c('y = 1','y = 0'),pch=c(3,23),pt.bg='yellow')

# Add Polynomial Features
# Note that mapFeature also adds a column of ones for us, so the intercept term is taken care of
X <- mapFeature(X[,1], X[,2]);

m <- dim(X)[1]
n <- dim(X)[2]

# Initialize fitting parameters
initial_theta = matrix(0,ncol(X))

# Set regularization parameter lambda to 1
lambda <- 1.0;

result <- optim(initial_theta, costFunctionReg, gradFunction, method="BFGS", X=X, y=y, lambda=lambda, control=list(maxit=400))

theta <- result$par

p <- predict(theta,X)

message('Train Accuracy: ', mean((p == y)) * 100)

# Contour plot of decision boundary
X <- data[,1:2]
y <- data[,3]

plot(X,pch=c(24,3)[y+1],bg='yellow')
legend(.8,1,c('y = 1','y = 0'),pch=c(3,23),pt.bg='yellow')
u <- seq(from=-1, to=1.5, by=.05)
v <- u
z <- matrix(0,length(u),length(v))

for (i in 1:length(u)){
	for (j in 1:length(v)){
		z[i,j] = mapFeature(u[i],v[j])%*%theta
	}
}
contour(u,v,z,nlevels=1,add=T,lwd=2,col='darkblue',drawlabels=F)