rm(list = ls(all=TRUE))

# Grab the functions that are needed
setwd('/Users/cburkart/GoodSync Folder/Data Science/Andrew Ng AI Course/coursera/ex2/')
source('sigmoid.R')
source('costFunction.R')
source('gradFunction.R')


# Load data
setwd('/Users/cburkart/GoodSync Folder/Data Science/Andrew Ng AI Course/coursera/ex2/ex2')
data <- read.csv('ex2data1.txt',header=F);
names(data) <- c('Exam 1 Score','Exam 2 Score','Admitted')

X <- data[,1:2]
y <- data[,3]

m <- dim(X)[1]
n <- dim(X)[2]

# Plot data to understand the classification problem
plot(X,pch=c(21,3)[y+1],bg='yellow')
legend(83,100,c('Admitted','Not Admitted'),pch=c(3,21),pt.bg='yellow')

# Add intercept term
X <- as.matrix(cbind(matrix(1,length(X)),X))

# Initialize fitting parameters
initial_theta = matrix(0,n+1)

# Compute and display initial cost and gradient
cost <- costFunction(initial_theta, X, y)
gradient <- gradFunction(initial_theta, X, y)[2:length(costFunction(initial_theta, X, y))]

#costWrapper <- function(theta) {
#    costFunction(theta,X=X,y=y)
#}

#gradWrapper <- function(theta) {
#	gradFunction(theta,X=X,y=y)
#}

result <- optim(initial_theta, costFunction, gradFunction, method="BFGS", X=X, y=y, control=list(maxit=400))
