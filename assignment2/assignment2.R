library(rpart)

playTennis <- read.csv("playTennis.csv", row.names=1)
caravanTrain <- read.csv("CaravanTrain.csv", row.names=1)
caravanTest <- read.csv("CaravanTest.csv", row.names=1)

# Question 6
cfit <- rpart(Class ~ outlook + temperature + humidity + wind, data=playTennis, method='class', control=rpart.control(minsplit=1))
#cfit <- rpart(Class ~ ., data=playTennis, method='class', control=rpart.control(minsplit=1))
#print(cfit)
#par(mfrow = c(1,2), xpd = NA)
#plot(cfit)
#text(cfit, use.n=TRUE)


# Question 6 (suite)
caravanTrainModel <- rpart(Purchase ~ ., data=caravanTrain, control=rpart.control(minsplit=1, cp=0))
# le numéro de noeuds est dim(caravanTrainModel$frame)[1]
#sink(file="result.txt")
#print(caravanTrainModel)
plot(caravanTrainModel)
#text(caravanTrainModel, use.n=TRUE)


# Question 7
# confusion matrix of test set
t7_1 <- table(predict(caravanTrainModel, caravanTest, type='class'), caravanTest$Purchase)
print(t7_1)
accuracy7_1 <- (t7_1[1] + t7_1[4])/sum(t7_1)
cat("accuracy: ", accuracy7_1*100, "%\n")
# confusion matrix of train set
t7_2 <- table(predict(caravanTrainModel, caravanTrain, type='class'), caravanTrain$Purchase)
print(t7_2)
accuracy7_2 <- (t7_2[1] + t7_2[4])/sum(t7_2)
cat("accuracy: ", accuracy7_2*100, "%\n")
# Le training set n'est pas forcément bon, si les valeurs étaient inconsistent à la base lorsqu'on créait le decision tree.


# Question 8
n = dim(caravanTrain)[1]
for (i in 1:5) {
	chosen <- sample(1:n, n/4) # sample of 25%	
	training <- caravanTrain[chosen,]
	#test <- mycars[-chosen,]

	caravanTrainModelQuart <- rpart(Purchase ~ ., data=training, control=rpart.control(minsplit=1, cp=0))

	cat("\n\nrun ", i, " of 5:")
	# confusion matrix of test set (of ALL the testing set)
	t8_1 <- table(predict(caravanTrainModelQuart, caravanTest, type='class'), caravanTest$Purchase)
	print(t8_1)
	accuracy8_1 <- (t8_1[1] + t8_1[4])/sum(t8_1)
	cat("accuracy of test set: ", accuracy8_1*100, "%\n")
	# confusion matrix of train set
	t8_2 <- table(predict(caravanTrainModel, training, type='class'), training$Purchase)
	print(t8_2)
	accuracy8_2 <- (t8_2[1] + t8_2[4])/sum(t8_2)
	cat("accuracy of training set: ", accuracy8_2*100, "%\n")
}


# Question 9
n = dim(caravanTrain)[1]
for (i in c(.05, .1, .2, .5, .99)) {
	t9_1 <- 0
	t9_2 <- 0
	accuracy9_1 <- 0
	accuracy9_2 <- 0

	test_boxplot <- c()

	for (j in 1:10) {
		chosen <- sample(1:n, n*i) # sample of 5%, 10%, 20%, 50% or 90%
		if (j == 1) {
			cat("dim of chosen: ", length(chosen), "\n")
		}
		training <- caravanTrain[chosen,]
		#test <- mycars[-chosen,]

		caravanTrainModelQuart <- rpart(Purchase ~ ., data=training, control=rpart.control(minsplit=1, cp=0))
		test_boxplot <- c(test_boxplot, dim(caravanTrainModelQuart$frame)[1])
		cat("hello: ", dim(caravanTrainModelQuart$frame)[1], "\n")

		# confusion matrix of test set (of ALL the testing set)
		t9_1 <- t9_1 + table(predict(caravanTrainModelQuart, caravanTest, type='class'), caravanTest$Purchase)
		#accuracy9_1 <- (t9_1[1] + t9_1[4])/sum(t9_1)
		# confusion matrix of train set
		t9_2 <- t9_2 + table(predict(caravanTrainModel, training, type='class'), training$Purchase)
		#accuracy9_2 <- (t9_2[1] + t9_2[4])/sum(t9_2)
	}

	boxplot(test_boxplot)

	t9_1 <- t9_1 / 10
	t9_2 <- t9_2 / 10
	accuracy9_1 <- (t9_1[1] + t9_1[4])/sum(t9_1)
	accuracy9_2 <- (t9_2[1] + t9_2[4])/sum(t9_2)

	cat("accuracy of test set for ", i, ": ", accuracy9_1*100, "%\n")
	cat("accuracy of training set ", i, ": ", accuracy9_2*100, "%\n")
}


















