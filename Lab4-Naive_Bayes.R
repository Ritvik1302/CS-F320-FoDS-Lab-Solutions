#1.Titanic
library("e1071")

data = read.csv("titanic.csv",stringsAsFactors = FALSE)
data <- na.omit(data)
data

str(data)
data$Survived <- factor(data$Survived)

data$Name = NULL
data$PassengerId = NULL
data$Ticket = NULL
data$Fare = NULL
data$Cabin = NULL
data$Age = NULL

table(data$Survived)

prop.table(table(data$Survived))

classifier <- naiveBayes(data,data$Survived)
classifier

NB_Prediction = predict(classifier,data)
NB_Prediction
tab2 = table(NB_Prediction,data$Survived)
Accuracy = sum(diag(tab2)) / sum(tab2)
Accuracy

#2.Nursery
data = read.table("nursery.data",sep=",")
names(data) = c('parents','has_nurs','form','children','housing','finance','social','health','class')
data <- na.omit(data)

str(data)
data$finance <- factor(data$finance)

table(data$finance)

prop.table(table(data$finance))

classifier <- naiveBayes(data,data$finance,laplace = 1)
classifier

NB_Prediction = predict(classifier,data)
tab2 = table(NB_Prediction,data$finance)
Accuracy = sum(diag(tab2)) / sum(tab2)
Accuracy
