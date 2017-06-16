# loading the packages
library('AppliedPredictiveModeling');
library(caret);library(ElemStatLearn)
library('pgmm');library(rpart)

# Load the cell segmentation data from the AppliedPredictiveModeling package
# using the commands:
data(segmentationOriginal)

# 1. Subset the data to a training set and testing set based on the 
# Case variable in the data set.
trainData <- subset(segmentationOriginal, Case == 'Train')
testData <- subset(segmentationOriginal, Case == 'Test')

# 2. Set the seed to 125 and fit a CART model with the rpart method using 
# all predictor variables and default caret settings.
set.seed(125)
mod1 <- train(Class~., method ="rpart", data=trainData)

# 3. In the final model what would be the final model prediction for cases 
# # with the following variable values:
# a. TotalIntench2 = 23,000; FiberWidthCh1 = 10; PerimStatusCh1=2
# 
# b. TotalIntench2 = 50,000; FiberWidthCh1 = 10;VarIntenCh4 = 100
# 
# c. TotalIntench2 = 57,000; FiberWidthCh1 = 8;VarIntenCh4 = 100
# 
# d. FiberWidthCh1 = 8;VarIntenCh4 = 100; PerimStatusCh1=2
mod1$finalModel
a<-data.frame(TotalIntench2 = 23000, FiberWidthCh1 = 10, PerimStatusCh1=2)
plot(mod1$finalModel,uniform=TRUE,main = "classification tree")
text(mod1$finalModel,use.n=TRUE, all=TRUE,cex=.8)


# If K is small in a K-fold cross validation is the bias in the estimate of out-of-sample (test set) accuracy smaller or bigger? If K is small is the variance in the estimate of out-of-sample (test set) accuracy smaller or bigger. Is K large or small in leave one out cross validation?

# The bias is larger and the variance is smaller. Under leave one out cross validation K is equal to one.


# Load the olive oil data using the commands:
data(olive)
olive$Area <- as.factor(olive$Area)
olive = olive[,-1]
mod2 <- train(Area~., method ="rpart", data=olive)
newdata = as.data.frame(t(colMeans(olive)))
predict(mod2,newdata = newdata)



# Load the South Africa Heart Disease Data and create training and test sets with the following code:
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]
# Then set the seed to 13234 and fit a logistic regression model (method="glm", be sure to specify family="binomial") with Coronary Heart Disease (chd) as the outcome and age at onset, current alcohol consumption, obesity levels, cumulative tabacco, type-A behavior, and low density lipoprotein cholesterol as predictors. Calculate the misclassification rate for your model using this function and a prediction on the "response" scale:
set.seed(13234)
mod3<- train(chd~age+alcohol+obesity+tobacco+typea+ldl,data=SAheart,method='glm', family="binomial")
missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}


# Load the vowel.train and vowel.test data sets:
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)
# Set the variable y to be a factor variable in both the training and test set. Then set the seed to 33833. Fit a random forest predictor relating the factor variable y to the remaining variables. Read about variable importance in random forests here: http://www.stat.berkeley.edu/~breiman/RandomForests/cc_home.htm#ooberr The caret package uses by default the Gini importance.
# 
# Calculate the variable importance using the varImp function in the caret package. What is the order of variable importance?
# 
# [NOTE: Use randomForest() specifically, not caret, as there's been some issues reported with that approach. 11/6/2016]
vowel.train$y<-as.factor(vowel.train$y)
vowel.test$y<-as.factor(vowel.test$y)
set.seed(33833)
mod4<-train(y~.,data=vowel.train,method='rf',prox=TRUE)
mod4<- randomForest(y ~ ., data=vowel.train, importance=0,proximity=TRUE)
round(importance(mod4), 2)
varImp(mod4)













