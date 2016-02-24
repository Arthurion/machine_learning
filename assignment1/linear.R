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

linearmodel <- lm(qsec ~ wt + cyl, data = mycars)
w <- linearmodel$coefficients
x <- mycars[,c("wt","cyl")]
predicted <- predict(x,w)

plot (mycars[,"qsec"], predicted, xlab="Actual QSEC", ylab="Predicted QSEC", col="red")
abline (0,1, col="blue")
#squareloss
sqloss1 <- squareloss(mycars[,"qsec"], predicted)


linearmodel <- lm(qsec ~ wt + cyl + gear, data = mycars)
w <- linearmodel$coefficients
x <- mycars[,c("wt","cyl","gear")]
predicted <- predict(x,w)

plot (mycars[,"qsec"], predicted, xlab="Actual QSEC", ylab="Predicted QSEC", col="red")
abline (0,1, col="blue")
#squareloss
sqloss2 <- squareloss(mycars[,"qsec"], predicted)

linearmodel <- lm(qsec ~ wt + hp + cyl + gear, data = mycars)
w <- linearmodel$coefficients
x <- mycars[,c("wt","hp","cyl","gear")]
predicted <- predict(x,w)

plot (mycars[,"qsec"], predicted, xlab="Actual QSEC", ylab="Predicted QSEC", col="red")
abline (0,1, col="blue")
#squareloss
sqloss3 <- squareloss(mycars[,"qsec"], predicted)

### QUESTION 3 ###
sqlossTraining <- numeric(100)
sqlossTest <- numeric(100)
for (i in 1:100) {
	n=dim(mycars)[1]
	chosen <- sample(1:n, n/2)	
	training <- mycars[chosen,]
	test <- mycars[-chosen,]

	linearmodel <- lm(qsec ~ wt + hp + cyl + gear, data = training)
	w <- linearmodel$coefficients
	x <- test[,c("wt","hp","cyl","gear")]
	predicted <- predict(x,w)

	#plot (test[,"qsec"], predicted, xlab="Actual QSEC", ylab="Predicted QSEC", col="red")
	#abline (0,1, col="blue")

	sqlossTraining[i] <- squareloss(training[,"qsec"], predict(training[,c("wt","hp","cyl","gear")], w))
	sqlossTest[i] <- squareloss(training[,"qsec"], predicted)
}

plot(1:100, sqlossTest, col="red", ylim=c(0, max(sqlossTest)), ylab="Testing squareloss (red) vs. training squareloss (green)")
points(1:100, sqlossTraining, col="green")

