######
# Building an ROC curve, computing area under ROC curve and confidence
# intervals for errors for comparing two clasifiers
#####


#####
# Model 1: Logistic Regression
######

library(ISLR)
data(Carseats)
set.seed(2)  # Set random seed for results being reproducible
dim(Carseats)  # Get dimension of dataset
High <- as.factor(ifelse(Carseats$Sales <=8,0,1))
Carseatslabel <- data.frame(Carseats[,-1] ,High)
train <- sample(1:nrow(Carseatslabel),200)  # Sample 50% of observations
Carseats.train <- Carseatslabel[train,]
Carseats.test <- Carseatslabel[-train,]  # Training data 

attach(Carseatslabel) # Use each column separately as a numeric vector
# Binary response variable

High.test <- High[-train]  # Test data

# Fit model on train set
glm.carseats <- glm(High~.,data = Carseats.train, family=binomial) 
# variable selection
summary(glm.carseats)
glm.carseats2 <- step(glm.carseats)
summary(glm.carseats2)

# Predict label
glm.probs<-predict(glm.carseats2,Carseats.test,type="response")
glm.pred=rep(0,nrow(Carseats.test))
glm.pred[glm.probs>0.5]=1

# Confusion matrix
conf.matrix1 <- table(glm.pred,High.test)

# Test Error
(error1 <- 1-sum(diag(conf.matrix1))/sum(conf.matrix1) )

detach(Carseatslabel)
####
# Model 2: k-NN
######

library(class) # Load library

str(Carseats)

# Drop discrete predictors and Sales
Carseats2 <- Carseats[,-c(1,7,10,11)]
colnames(Carseats2)


knn.fit <- knn(train=Carseats2[train,],
               test=Carseats2[-train,],
               cl= High[train],
               k=8,
               prob=TRUE)

# There is another knn function you could use to 
# take care of all variables

# Predicted posterior probability
p.hat.knn <- attr(knn.fit,"prob")

# This is not quite what we need since it gives us the proportion of the predicted class
# but we want the posterior probability of class=0 (in this case) given the x's
p.hat.knn[knn.fit==1] <- (1-p.hat.knn[knn.fit==1])

# validate: all cases with p>1/2 should be y=0 otherwise y=1
p.hat.knn[1:10]
knn.fit[1:10]

# Predicted class
y.hat.knn <- knn.fit

# Confusion matrix
conf.matrix2 <- table(High.test, # Actual
                      y.hat.knn)  # Predicted

# Test Error
(error2 <- 1-sum(diag(conf.matrix2))/sum(conf.matrix2))

###
# Construct ROC curve
###

# The ROCR package can be used to produce ROC curves such as those 
# in Figures 9.10 and 9.11. We first write a short function to plot an ROC curve 
# given a vector containing a numerical score for each observation, 
# pred, and a vector containing the class label for each observation, truth.

# install.packages("ROCR")
library(ROCR) 
# See http://cran.r-project.org/web/packages/ROCR/ROCR.pdf

rocplot=function(pred, truth, ...){
  predob = prediction(pred, truth)
  perf = performance (predob , "tpr", "fpr") 
  plot(perf ,...)
}

# First ROC curve for Logistic Regression
rocplot(pred=glm.pred,truth=High.test,type="S", 
        main="ROC Curve for test set")

# Produce ROC curve for knn
rocplot(pred=p.hat.knn,truth=High.test,add=T,type="S",col="red")

# Add diagonal
abline(c(0,1),lty=2)

# Add legend 
legend("bottomright",c("GLM","k-NN"),lty=1,col=c("black","red"))


#####
# Area under the curve
#####

auc=function(pred, truth, ...){
  predob = prediction(pred, truth)
  perf = performance (predob ,"auc")
  area=perf@y.values[[1]]
  return(area)
}

# GLM
auc(pred=glm.pred,truth=High.test)
# knn
auc(pred=p.hat.knn,truth=High.test)

# which one is better?

######
# Confidence Intervals for the difference in proportions among different k 
# different compering models from Learning model L1 and k other from Learning 
# model L2
########

# Obtain sample of indices for CV
GetFoldIndex <- function(n,n.folds){
  
  folds <- rep(1:n.folds,each=floor(n/n.folds))  
  remainder <- n-length(folds)
  
  if(remainder>0){
    folds <- c(folds,1:remainder)
  }
  
  folds <- sample(folds)
  
  return(folds)
}


MiscError <- function(X,cl,n.folds){
  # Args:
  # X: dataset with only explanatory variables. 
  # responseY : lables
  # m: max value for nearest neighbor
  # n.folds: Number of folds
  
  n <- NROW(X)
  
  # row: Learning model no i, i=1,2
  # cols: error with jth fold as test
  error.cv <- matrix(NA,2,n.folds) 
  
  # Compute folds
  folds <- GetFoldIndex(n,n.folds)
  
  for(j in 1:n.folds){    
    # Training data
    train.set <- X[folds!=j,]
    # Test data
    test.set <- X[folds==j,]
    
    # Vector of classes
    class.train <- cl[folds!=j]
    class.test <- cl[folds==j]
    
    #####
    # Nearest neighbors
    #####
    model.knn <- knn(train = train.set,
                     test = test.set,
                     cl = class.train,
                     k=7) # fixed value in k-nn after selecting k-optimal
    
    error <- table(model.knn,class.test)
    
    error.cv[1,j] <- (error[1,2] + error[2,1])/sum(error)
    
    ####
    ## GLM
    ####
    
    # Fit GLM
    glm.fit <- glm(class.train~., data=train.set,family=binomial)
    glm.fit2 <- step(glm.fit)
    glm.probs<-predict(glm.fit,test.set,type="response")
    
    # Predict label on test
    glm.probs<-predict(glm.fit,test.set,type="response")
    glm.pred=rep(0,nrow(test.set))
    glm.pred[glm.probs>0.5]=1
    error <- table(glm.pred,class.test) 
    error.cv[2,j] <- (error[1,2] + error[2,1])/sum(error)
  }
  
  rownames(error.cv)  <- c("k-NN","GLM")
  return(error.cv)
}


error.models <- MiscError(X=Carseats2[train,],
                          cl=High[train],
                          n.folds=10)

error.models

# Compute summary statistic for determining difference in accuracy
dj <- error.models[1,] - error.models[2,]

d.bar <- mean(dj)
sigma.d <- sum((dj-d.bar)^2)/10/(10-1) #10: number of folds
# var(dj)

# Confidence interval 
d.bar - qt(1-0.05/2,10-1)*sqrt(sigma.d)
d.bar + qt(1-0.05/2,10-1)*sqrt(sigma.d)
