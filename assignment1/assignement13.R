squareloss <- function (y, predicted) {
	sum ((y - predicted)^2)
}

predict <- function (x, w) {
	intercept <- w[1]
	intercept + as.matrix(x) %*% w[2:length(w)]
}

sqloss <- 0
for(i in 1:100)
{
	n=dim(mycars)[1]
	chosen <- sample(1:n,n/2)	
	training <- mycars[chosen, ]
	test <- mycars[-chosen, ]

	linearmodel <- lm(qsec ~ hp + wt, data = training)
	w <- linearmodel$coefficients
	x <- test[,c("hp","wt")]
	predicted <- predict(x,w)
	y <- test[,"qsec"]
	sqloss <- sqloss + squareloss(y, predicted)*2
}
"mean squareloss"
sqloss/100
# pdf(file="Test.pdf")
plot (test[,"qsec"],predicted, xlab="Actual QSEC", ylab="Predicted QSEC")
abline (0,1, col="blue")
# dev.off()