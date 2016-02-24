library(rpart)

caravanTrain <- read.csv("CaravanTrain.csv", row.names=1)
caravanTest <- read.csv("CaravanTest.csv", row.names=1)

sink(file="result.txt")

caravanTrainModel <- rpart(Purchase ~ ., data=caravanTrain, control=rpart.control(minsplit=1, cp=0.005, xval=10))
plot(caravanTrainModel)
text(caravanTrainModel, use.n=TRUE)
t <- prune(caravanTrainModel, cp=0.005, xval=0, minsplit=100)
plot(t)
text(t, use.n=TRUE)

summary(caravanTrainModel)
printcp(caravanTrainModel)