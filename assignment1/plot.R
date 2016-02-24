predict <- function (x, w) {
	intercept <- w[1]
	intercept + as.matrix(x) %*% w[2:length(w)]
}


### QUESTION 1 ###
squareloss <- function(y, predicted) {
	sum ((predicted - y)^2)
}

### QUESTION 2 ###
mycars <- read.csv("oldcars.csv", row.names=1)

linearmodel <- lm(qsec ~ hp + wt, data = mycars)
w <- linearmodel$coefficients
x <- mycars[,c("hp","wt")]
predicted <- predict(x,w)

plot (mycars[,"qsec"], predicted, xlab="Actual QSEC", ylab="Predicted QSEC", col="red")
abline (0,1, col="blue")
#squareloss
sqloss1 <- squareloss(mycars[,"qsec"], predicted)


