# Citation for using this dataset
"""
Using Data Mining To Predict Secondary School Student Alcohol Consumption. 
Fabio Pagnotta, Hossain Mohammad Amran 
Department of Computer Science, University of Camerino
"""

# This code was included in the UCI dataset to merge the two datasets
# There were two surveys performed, and some students took both
d1=read.table("~/Documents/student-mat.csv",sep=";",header=TRUE)
d2=read.table("~/Documents/student-por.csv",sep=";",header=TRUE)

# For this project I will only be analyzing d2, which is the portugese class grades
# Most of these variables need to be converted
# They are numbers on a scale (ordinal)
# R studio is reading them in as integers and making them continuous
# This will be the case for both regression and classification
d2$age <- ordered(d2$age,
                  levels = c(15:22),
                  labels = c("15", "16", "17", "18", "19", "20", "21", "22"))

d2$famsize <- ordered(d2$famsize,
                      levels = c("LE3", "GT3"))

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

# Now everything but the grades are in the correct format
# We will make two sets of data in order to try different ways of classification
# We will try turning the grades into 21 ordinal classes (0 < 1, 1 < 2, 2 < 3 ... 19 < 20)
# Then we will convert the numbers into grades (A, B, C, D, and F) based on a traditional scale
# And lastly, we will try Pass/No Pass

#math.grades.num <- d1
portug.grades.num <- d2

#math.grades.cat <- d1
portug.grades.cat <- d2

# First let's do 21 ordinal classes 
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
# Now the data set is where we need it
# Split data into train and test set for classification
# Can use original dataset, number of rows does not chance
ratio = 0.80
train.num.cat <- ratio * nrow(portug.grades.cat)
train.ind.cat <- sample.int(nrow(portug.grades.cat), train.num.cat)

# Create X and Y variables
portug.grades.cat.Y <- portug.grades.cat$G3
portug.grades.cat.X <- subset(portug.grades.cat, select = -c(G3))

# Extract the training set using our train indices
portug.grades.cat.X.train <- portug.grades.cat.X[train.ind,]
portug.grades.cat.Y.train <- portug.grades.cat.Y[train.ind]
portug.grades.cat.train <- portug.grades.cat[train.ind,]

# Get the test set from the rest
portug.grades.cat.X.test <- portug.grades.cat.X[-c(train.ind),]
portug.grades.cat.Y.test <- portug.grades.cat.Y[-c(train.ind)]
portug.grades.cat.test <- portug.grades.cat[-train.ind,]


# Make a decision tree
require(tree)
portug.dec.tree.cat <- tree(G3 ~ ., data = portug.grades.cat, subset = train.ind.cat)
plot(portug.dec.tree.cat)
text(portug.dec.tree.cat, use.n=TRUE, all=TRUE, cex=.8)

# Predict test set results and compare with actual results
portug.dec.pred.cat <- predict(portug.dec.tree.cat, portug.grades.cat.test, type = "class")
conf.matrix.cat <- table(portug.dec.pred.cat, portug.grades.cat.Y.test)
conf.matrix.cat
# Check to see the number of correct predictions
sum(diag(conf.matrix.cat))

# Find test error rate
portug.dec.test.error = 1 - (sum(diag(conf.matrix.cat)) / length(portug.grades.cat.Y.test))
portug.dec.test.error

# Phew test error is around 51% - 53%
# Let's try pruning the tree
portug.dec.tree.cv <- cv.tree(portug.dec.tree.cat, FUN = prune.misclass)
portug.dec.tree.cv

# We wish to match up the size of the trees with the lowest number in the "dev" section
# size = 7 has the lowest dev value, so we will pick this
portug.dec.tree.prune <- prune.misclass(portug.dec.tree.cat, best = 7)
plot(portug.dec.tree.prune)
text(portug.dec.tree.prune, pretty = 0,
     main = "Pruned Classification Tree for 21 Ordinal Class Decision Tree")



# As we can see, the test error rate is still to high to make any meaningful prediction
# And it is highly dependend on G2 and pretty much nothing else
# So I will try a letter grades transformation first, then a pass/no pass transformation
# So let's transform the data so that we have actual grades
# The top 10% get A's
# The second 10% get B's
# And so on until F's
portug.grades.letter <- portug.grades.num
# Divide grades by 20 to get percentages

portug.grades.letter$G1 <- portug.grades.letter$G1 / 20
portug.grades.letter$G2 <- portug.grades.letter$G2 / 20
portug.grades.letter$G3 <- portug.grades.letter$G3 / 20

# And turn into grades
# Because I am using a typical A, B, C, D, F grade scale, no ordered transformation is needed
portug.grades.letter$G1 <- cut(portug.grades.letter$G1, c(0, 0.6, 0.7, 0.8, 0.9, Inf), right=FALSE, labels=c("F", "D", "C", "B", "A"))
portug.grades.letter$G2 <- cut(portug.grades.letter$G2, c(0, 0.6, 0.7, 0.8, 0.9, Inf), right=FALSE, labels=c("F", "D", "C", "B", "A"))
portug.grades.letter$G3 <- cut(portug.grades.letter$G3, c(0, 0.6, 0.7, 0.8, 0.9, Inf), right=FALSE, labels=c("F", "D", "C", "B", "A"))
#portug.grades.letter[grade.columns] <- lapply(portug.grades.letter[grade.columns], )


# Split data into train and test set
ratio = 0.80
train.num <- ratio * nrow(portug.grades.letter)
train.ind <- sample.int(nrow(portug.grades.letter), train.num)

# Create X and Y
portug.grades.letter.Y <- portug.grades.letter$G3
portug.grades.letter.X <- subset(portug.grades.letter, select = -c(G3))


# Extract the training set using our train indices
portug.grades.letter.X.train <- portug.grades.letter.X[train.ind,]
portug.grades.letter.Y.train <- portug.grades.letter.Y[train.ind]
portug.grades.letter.train <- portug.grades.letter[train.ind,]

# Get the test set from the rest
portug.grades.letter.X.test <- portug.grades.letter.X[-c(train.ind),]
portug.grades.letter.Y.test <- portug.grades.letter.Y[-c(train.ind)]
portug.grades.letter.test <- portug.grades.letter[-train.ind,]

# Try decision tree again
require(tree)
require(caret)
portug.dec.tree.letter <- tree(G3 ~ ., data = portug.grades.letter, subset = train.ind)
portug.dec.tree.letter
plot(portug.dec.tree.letter)
text(portug.dec.tree.letter, use.n=TRUE, all=TRUE, cex=.8)

portug.dec.pred.letter <- predict(portug.dec.tree.letter, portug.grades.letter.test, type = "class")
conf.matrix.letter <- table(portug.dec.pred.letter, portug.grades.letter.Y.test)
conf.matrix.letter
diag(conf.matrix.letter)

# Calculate test error rate; looks good
correct.letter <- sum(diag(conf.matrix.letter))
total.letter <- length(portug.grades.letter.Y.test)
total.letter
test.error.rate.letter <- 1 - (correct.letter / total.letter)
test.error.rate.letter

# 0.223 is pretty good!
# G2 has a huge effect on the tree
# G1 and G2 are probably highly correlated 
# Let's look at the correlation between G1 and G2
# And see if we need to remove one
# Need to use original dataset since G1 and G2 are still integers
cor(d2$G1, d2$G2)

# 86% is a high correlation coefficient
# So let's get rid of G1 to see if the other variables become more important

portug.grades.letter.2 <- portug.grades.letter
portug.grades.letter.2$G1 <- NULL

# Create X and Y for new data set
portug.grades.letter.2.Y <- portug.grades.letter.2$G3
portug.grades.letter.2.X <- subset(portug.grades.letter.2, select = -c(G3))


# Extract the training set using our train indices
portug.grades.letter.2.X.train <- portug.grades.letter.2.X[train.ind,]
portug.grades.letter.2.Y.train <- portug.grades.letter.2.Y[train.ind]
portug.grades.letter.2.train <- portug.grades.letter.2[train.ind,]

# Get the test set from the rest
portug.grades.letter.2.X.test <- portug.grades.letter.2.X[-c(train.ind),]
portug.grades.letter.2.Y.test <- portug.grades.letter.2.Y[-c(train.ind)]
portug.grades.letter.2.test <- portug.grades.letter.2[-train.ind,]


# And try decision trees again
portug.dec.tree.letter.2 <- tree(G3 ~ ., data = portug.grades.letter.2, subset = train.ind)
portug.dec.tree.letter.2
plot(portug.dec.tree.letter.2)
text(portug.dec.tree.letter.2, use.n=TRUE, all=TRUE, cex=.8)

portug.dec.pred.letter.2 <- predict(portug.dec.tree.letter.2, portug.grades.letter.2.test, type = "class")
conf.matrix.3 <- table(portug.dec.pred.letter.2, portug.grades.letter.2.Y.test)
conf.matrix.3
diag(conf.matrix.3)

# Calculate test error rate; looks good so far
correct.letter.2 <- sum(diag(conf.matrix.3))
total.letter.2 <- length(portug.grades.letter.2.Y.test)
total.letter.2
test.error.rate.letter.2 <- 1 - (correct.letter.2 / total.letter.2)
test.error.rate.letter.2

# Removing G1 slightly lowered the test error rate
# Let's see if it allowed other variables to become more "important"
# Using G1

require(randomForest)
portug.rand.forest.letter <- randomForest(G3 ~ ., data = portug.grades.letter, subset = train.ind, norm.votes=FALSE)
portug.rand.forest.letter
varImpPlot(portug.rand.forest.letter)

# Once again G1 and G2 is monopolizing the importance in the resulting model
# Let's try without G1
portug.rand.forest.letter.2 <- randomForest(G3 ~ ., data = portug.grades.letter.2, subset = train.ind, norm.votes=FALSE)
portug.rand.forest.letter.2
varImpPlot(portug.rand.forest.letter.2)

# It lowered the test error rate slightly
# And one variable moved up into the Importance plot via Gini Index









# Lastly let's try if we were to turn the grades into pass/no pass
portug.grades.pass <- portug.grades.num
# Divide grades by 20 to get percentages

portug.grades.pass$G1 <- portug.grades.pass$G1 / 20
portug.grades.pass$G2 <- portug.grades.pass$G2 / 20
portug.grades.pass$G3 <- portug.grades.pass$G3 / 20

# And turn into Pass/No Pass
# Because I am using a typical A, B, C, D, F grade scale, no ordered transformation is needed
portug.grades.pass$G1 <- cut(portug.grades.pass$G1, c(0, 0.7, Inf), right=FALSE, labels=c("No Pass", "Pass"))
portug.grades.pass$G2 <- cut(portug.grades.pass$G2, c(0, 0.7, Inf), right=FALSE, labels=c("No Pass", "Pass"))
portug.grades.pass$G3 <- cut(portug.grades.pass$G3, c(0, 0.7, Inf), right=FALSE, labels=c("No Pass", "Pass"))
#portug.grades.pass[grade.columns] <- lapply(portug.grades.pass[grade.columns], )

# Split data into train and test set
ratio = 0.80
train.num <- ratio * nrow(portug.grades.pass)
train.ind <- sample.int(nrow(portug.grades.pass), train.num)

# Create X and Y
portug.grades.pass.Y <- portug.grades.pass$G3
portug.grades.pass.X <- subset(portug.grades.pass, select = -c(G3))


# Extract the training set using our train indices
portug.grades.pass.X.train <- portug.grades.pass.X[train.ind,]
portug.grades.pass.Y.train <- portug.grades.pass.Y[train.ind]
portug.grades.pass.train <- portug.grades.pass[train.ind,]

# Get the test set from the rest
portug.grades.pass.X.test <- portug.grades.pass.X[-c(train.ind),]
portug.grades.pass.Y.test <- portug.grades.pass.Y[-c(train.ind)]
portug.grades.pass.test <- portug.grades.pass[-train.ind,]


# Decision tree
portug.dec.tree.pass <- tree(G3 ~ ., data = portug.grades.pass, subset = train.ind)
portug.dec.tree.pass
plot(portug.dec.tree.pass)
text(portug.dec.tree.pass, use.n=TRUE, all=TRUE, cex=.8)

portug.dec.pred.pass <- predict(portug.dec.tree.pass, portug.grades.pass.X.test, type = "class")
conf.matrix.4 <- table(portug.dec.pred.pass, portug.grades.pass.Y.test)
conf.matrix.4
diag(conf.matrix.4)

# Calculate test error rate; looks good so far
correct.pass <- sum(diag(conf.matrix.4))
total.pass <- length(portug.grades.pass.Y.test)
total.pass
test.error.rate.pass <- 1 - (correct.pass / total.pass)
test.error.rate.pass

# Random Forest
portug.rand.forest.pass <- randomForest(G3 ~ ., data = portug.grades.pass, subset = train.ind, norm.votes=FALSE)
portug.rand.forest.pass
varImpPlot(portug.rand.forest.pass)



# Now let's compare this to another classification algorithm
# A Support Vector Machine can utilize non linear decision boundaries to make a decsion
# So let's use a Support Vector Machine
require(caret)
control <- trainControl(method="repeatedcv", number=10, repeats=3)
portug.grades.svm.cv <- train(G3 ~ ., data = portug.grades.pass,
                 method = "svmRadial",
                 preProcess = c("center", "scale"),
                 tuneLength = 10,
                 trControl=control)
portug.grades.svm.cv

require(e1071)
portug.grades.svm <- svm(G3 ~ ., 
                         data = portug.grades.pass.train, 
                         kernel = "radial",
                         cost = 1,
                         sigma = 0.006904619)
summary(portug.grades.svm)

portug.grades.svm.pred <- predict(portug.grades.svm, portug.grades.pass.X.test)
conf.matrix.svm <- table(portug.grades.svm.pred, portug.grades.pass.Y.test)
conf.matrix.svm

# Get test error rate
correct.svm <- sum(diag(conf.matrix.svm))
total.svm <- length(portug.grades.pass.Y.test)
test.error.svm <- 1 - (correct.svm / total.svm)
test.error.svm

