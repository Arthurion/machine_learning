squareloss <- function (y, predicted) {
	sum ((y - predicted)^2)
}

predict <- function (x, w) {
	intercept <- w[1]
	intercept + as.matrix(x) %*% w[2:length(w)]
}

linearmodel <- lm(qsec ~ hp + wt, data = mycars)
w <- linearmodel$coefficients
x <- mycars[,c("hp","wt")]
predicted <- predict(x,w)
y <- mycars[,"qsec"]
"squareloss"
squareloss(y, predicted)
plot (mycars[,"qsec"],predicted, xlab="Actual QSEC", ylab="Predicted QSEC")
abline (0,1, col="blue")