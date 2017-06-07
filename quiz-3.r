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



