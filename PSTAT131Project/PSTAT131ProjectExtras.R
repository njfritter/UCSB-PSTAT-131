# Citation for using this dataset
"""
Using Data Mining To Predict Secondary School Student Alcohol Consumption. 
Fabio Pagnotta, Hossain Mohammad Amran 
Department of Computer Science,University of Camerino
"""

# This code was included in the UCI dataset to merge the two datasets
# There were two surveys performed, and some students took both
d1=read.table("~/Documents/student-mat.csv",sep=";",header=TRUE)
d2=read.table("~/Documents/student-por.csv",sep=";",header=TRUE)

print(nrow(d3)) # 382 students

# Most of these variables need to be converted
# They are numbers on a scale (ordinal)
# R studio is reading them in as integers and making them continuous
# This will be the case for both regression and classification
d2$age <- ordered(d2$age,
                  levels = c(15:22),
                  labels = c("15", "16", "17", "18", "19", "20", "21", "22"))

d2$famsize <- ordered(d2$famsize,
                      levels = c(0:1),
                      labels = c("0", "1"))

d2$Medu <- ordered(d2$Medu,
                   levels = c(0:4),
                   labels = c("0", "1", "2", "3", "4"))

d2$Fedu <- ordered(d2$Fedu,
                   levels = c(0:4),
                   labels = c("0", "1", "2", "3", "4"))

d2$traveltime <- ordered(d2$traveltime,
                         levels = c(1:4),
                         labels = c("1", "2", "3", "4"))

d2$studytime <- ordered(d2$studytime,
                        levels = c(1:4),
                        labels = c("1", "2", "3", "4"))

d2$failures <- ordered(d2$failures,
                       levels = c(0:3),
                       labels = c("0", "1", "2", "3"))

d2$famrel <- ordered(d2$famrel,
                     levels = c(1:5),
                     labels = c("1", "2", "3", "4", "5"))

d2$freetime <- ordered(d2$freetime,
                       levels = c(1:5),
                       labels = c("1", "2", "3", "4", "5"))

d2$goout <- ordered(d2$goout,
                    levels = c(1:5),
                    labels = c("1", "2", "3", "4", "5"))

d2$famrel <- ordered(d2$famrel,
                     levels = c(1:5),
                     labels = c("1", "2", "3", "4", "5"))

d2$Dalc <- ordered(d2$Dalc,
                   levels = c(1:5),
                   labels = c("1", "2", "3", "4", "5"))

d2$Walc <- ordered(d2$Walc,
                   levels = c(1:5),
                   labels = c("1", "2", "3", "4", "5"))

d2$health <- ordered(d2$health,
                     levels = c(1:5),
                     labels = c("1", "2", "3", "4", "5"))

# Check structure of dataset now
str(d2)

#math.grades.num <- d1
portug.grades.num <- d2

#math.grades.cat <- d1
portug.grades.cat <- d2

# Convert grades into factors
# This will be classification
# Turn grades into ordinal variables
portug.grades.cat$G1 <- ordered(portug.grades.cat$G1,
                                levels = c(0:20),
                                labels = c("0", "1", "2", "3", "4", "5", "6", "7",
                                           "8", "9", "10", "11", "12", "13", "14",
                                           "15", "16", "17", "18", "19", "20"))

portug.grades.cat$G2 <- ordered(portug.grades.cat$G2,
                                levels = c(0:20),
                                labels = c("0", "1", "2", "3", "4", "5", "6", "7",
                                           "8", "9", "10", "11", "12", "13", "14",
                                           "15", "16", "17", "18", "19", "20"))


portug.grades.cat$G3 <- ordered(portug.grades.cat$G3,
                                levels = c(0:20),
                                labels = c("0", "1", "2", "3", "4", "5", "6", "7",
                                           "8", "9", "10", "11", "12", "13", "14",
                                           "15", "16", "17", "18", "19", "20"))

str(portug.grades.cat)

# G2 has a huge effect on the tree
# It and G1 would be highly correlated and 
# Let's remove G1 and G2 to see how this impacts the data

portug.grades.cat.2 <- portug.grades.cat
portug.grades.cat.2$G1 <- NULL
portug.grades.cat.2$G2 <- NULL

# It did! So let's move onto the analysis


# Make decision trees
# Methods gotten from
# http://www.statmethods.net/advstats/cart.html
# http://stats.stackexchange.com/questions/49416/decision-tree-model-evaluation-for-training-set-vs-testing-set-in-r
require(rpart)

dec.tree.1 <- rpart(G3 ~ ., data = portug.grades.cat, method = "class") # Could also add "control" option

dec.tree.2 <- rpart(G3 ~ ., data = portug.grades.cat.2, method = "class") # Could also add "control" option

dec.tree.3 <- rpart(G3 ~ ., data = portug.grades.letter, method = "class") # Could also add "control" option

dec.tree.4 <- rpart(G3 ~ ., data = portug.grades.letter.2, method = "class") # Could also add "control" option


# Examine first decision tree
printcp(dec.tree.1)
plotcp(dec.tree.1)
print(dec.tree.1)
plot(dec.tree.1)
text(dec.tree.1, use.n=TRUE, all=TRUE, cex=.8)
summary(dec.tree.1)

# Examine second decision tree
printcp(dec.tree.2)
plotcp(dec.tree.2)
print(dec.tree.2)
plot(dec.tree.2)
text(dec.tree.2, use.n=TRUE, all=TRUE, cex=.8)
summary(dec.tree.2)

# Examine third decision three
printcp(dec.tree.3)
plotcp(dec.tree.3)
print(dec.tree.3)
plot(dec.tree.3)
text(dec.tree.2, use.n=TRUE, all=TRUE, cex=.8)
summary(dec.tree.3)

# Examine fourth decision tree
printcp(dec.tree.4)
plotcp(dec.tree.4)
print(dec.tree.4)
plot(dec.tree.4)
text(dec.tree.4, use.n = TRUE, all = TRUE, cex = .8)
summary(dec.tree.4)

# Prune first decision tree
pruned.dec.tree <- prune(dec.tree.1, cp = dec.tree.1$cptable[which.min(dec.tree.1$cptable[,"xerror"]),"CP"])
plot(pruned.dec.tree, uniform = TRUE, 
     main = "Pruned Classification Tree for First Decision Tree")
text(pruned.dec.tree, use.n = TRUE, all = TRUE, cex = .8)

# Prune second decision tree
pruned.dec.tree.2 <- prune(dec.tree.2, cp = dec.tree.2$cptable[which.min(dec.tree.2$cptable[,"xerror"]),"CP"])
plot(pruned.dec.tree.2, uniform = TRUE, 
     main = "Pruned Classification Tree for Second Decision Tree")
text(pruned.dec.tree.2, use.n = TRUE, all = TRUE, cex = .8)

# Prune third decision tree
pruned.dec.tree.3 <- prune(dec.tree.3, cp = dec.tree.3$cptable[which.min(dec.tree.3$cptable[,"xerror"]),"CP"])
plot(pruned.dec.tree.3, uniform = TRUE, 
     main = "Pruned Classification Tree for Third Decision Tree")
text(pruned.dec.tree.3, use.n = TRUE, all = TRUE, cex = .8)

# Prune fourth decision tree
pruned.dec.tree.4 <- prune(dec.tree.4, cp = dec.tree.4$cptable[which.min(dec.tree.4$cptable[,"xerror"]),"CP"])
plot(pruned.dec.tree.4, uniform = TRUE, 
     main = "Pruned Classification Tree for Fourth Decision Tree")
text(pruned.dec.tree.4, use.n = TRUE, all = TRUE, cex = .8)

# Using the third and fourth trees (trained on training set)
# Let us try and predict the test set

library(caret)
pred.dec.tree.1 <- predict(dec.tree.1, newdata = students.grades.test)
confusionMatrix(pred.dec.tree.1, students.grades.test$G3.y)


# Conditional inference trees via party
require(party)
cond.tree.1 <- ctree(G3 ~ ., data = portug.grades.cat)
plot(cond.tree.1)
print(cond.tree.1)

cond.tree.2 <- ctree(G3 ~ ., data = portug.grades.cat.2)
plot(cond.tree.2)
print(cond.tree.2)

# Make a random forest
require(randomForest)
rand.forest.1 <- randomForest(G3 ~ ., data = portug.grades.cat, subset = )

# Check results
print(rand.forest.1)
importance(rand.forest.1)

# Make another random forest w/o G1 or G2
rand.forest.2 <- randomForest(G3 ~ ., data = portug.grades.cat.2)

# Check results
print(rand.forest.2)
importance(rand.forest.2)

# Make random forest with training data
rand.forest.3 <- randomForest(G3 ~ ., data = portug.grades.letter)

# Check results
print(rand.forest.3)
importance(rand.forest.3)

# Anotha one
rand.forest.4 <- randomForest(G3 ~ ., data = portug.grades.letter.2) 

# Check results
print(rand.forest.4)
importance(rand.forest.4)

# Cross validated model
student.grades.matrix <- matrix(student.grades)
cross.val.model <- train(model <- train(G3 ~ ., 
                                        data = portug.grades.cat, 
                                        method='ctree', tuneLength=10,
                                        trControl=trainControl(
                                          method='cv', number=10, 
                                          classProbs=TRUE, 
                                          summaryFunction=twoClassSummary)))

cross.val.model.2 <- train(model <- train(G3 ~ ., 
                                          data = portug.grades.letter, 
                                          method='ctree', tuneLength=10,
                                          trControl=trainControl(
                                            method='cv', number=10, 
                                            classProbs=TRUE, 
                                            summaryFunction=twoClassSummary)))

control <- trainControl(method="repeatedcv", number=10, repeats=3)

model.1 <- train(student.grades.X.train, student.grades.Y.train,
                 method = "svmRadial",
                 preProcess = c("center", "scale"),
                 tuneLength = 10,
                 trControl=control)


# Let's get rid of some variables to see if we can increase accuracy

portug.grades.letter.3 <- subset(portug.grades.letter.2, 
                                 select = -c(paid, Pstatus, internet, nursery, address, 
                                             schoolsup, sex, romantic, famsize, famsup,
                                             higher, activities, guardian, school))

portug.rand.forest.letter.3 <- randomForest(G3 ~ ., data = portug.grades.letter.3, subset = train.ind, norm.votes=FALSE)
portug.rand.forest.letter.3
varImpPlot(portug.rand.forest.letter.3)
