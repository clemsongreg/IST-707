#  Assignment 7 - Comparing Handwriting (cont.)

library(e1071)
library(class)
#library(rpart)
#library(rpart.plot)
#library(rattle)
#library(naivebayes)
library(ggplot2)

fileloc <- "file:///C:/Users/Greg/Documents/School/IST 707/Week 7/Kaggle-digit-train.csv"

file <- read.csv(fileloc)

# --------- Set up Testing Data ---------------
# Create a test and training data set.  Test set created using every 10th
# value.  The training data uses the rest.  The test data set then is 
# stripped of the label, which is set to the side.
testnums1 <- seq(1,length(file$label), 10)

# Label data converted to a factor
str(file$label)
file$label <- as.factor(file$label)
str(file$label)


# Function normalize takes a column and normalizes the values based on the
# range from 0 - 255.
normalize <- function(num) {
  return(num / 255)
}

# A new dataframe was created applying the normalizing function to all
# observation rows, and add label back to the data.
file1 <- data.frame(lapply(file[,-1], normalize))

# Remove columns that add no value to the model.  These are columns where
# all values are zeroes, or nearly 0, as well as pixels that are almost all
# shaded in.

sums <- colSums(file1)
g1 <- ggplot(data=data.frame(sums), aes(x="", y=sums)) + geom_violin(color = "#522D80", fill = "#F66733")
g1 <- g1 + stat_summary(fun.y=mean, geom="point", size=6, color = "#522D80")
g1 <- g1 + theme_bw()
g1 <- g1 + ggtitle("Density Plot of Sum of Lightness For Each Pixel")
g1 <- g1 + ylab("Sum of Lightness")
g1 <- g1 + xlab("")
g1

file1 <- file1[,-(which(sums<1000))]



file1$label <- file$label

# Dataset is split into the testing and training data using the testnums 
# sequence grabbed previously.  Both test and training data has labels 
# removed and put in a separate vector

testData1 <- file1[testnums1, -(which(colnames(file1) == "label"))]
testLabels <- file1$label[testnums1]

# There is too much data in the training data, so a smaller, random sample
# of training data is used.  This number may be adjusted given processing
# cost vs accuracy.  Start at n=10,000
trainData1 <- file1[-testnums1, ]
trainLabels <- file1$label[-testnums1]
set.seed(717)
sampleset <- sample(nrow(trainData1), 20000, replace=FALSE)
trainData1Samp <- trainData1[sampleset,]
trainData1SampLabs <- trainLabels[sampleset]

# Check that all 10 digits are relatively well-balanced
table(testLabels)
table(trainData1SampLabs)

# ----------------- SVM Method --------------
# Using the sample training data, create a polynomial SVM.  Initial cost
# paramater is 0.1
mod1 <- svm(label ~ ., data=trainData1Samp, kernel = "polynomial", cost = 2.0)

mod1
pred1 <- predict(mod1, testData1)
tab1 <- table(pred1, testLabels)
acc <- sum(diag(tab1))/4200
acc
tab1

# Using the sample training data, create an SVM using Gaussian kernel.
# start at same cost - 2.0 as Polynomial SVM

mod2 <- svm(label ~ ., data=trainData1Samp, kernel = "radial", cost = 2.0)
mod2
pred2 <- predict(mod2, testData1)
tab2 <- table(pred2, testLabels)
acc2 <- sum(diag(tab2))/4200
acc2
tab2
# -----------------  KNN Method --------------


# Using Square Root of the training data count as starting basis for k
k = sqrt(nrow(trainData1Samp))
#k = 10

# Create an unlabeled training data set and training data labels
trainData1SampNoLabs <- trainData1Samp[,-which(colnames(trainData1Samp) == "label")]

mod3 <- class::knn(train=trainData1SampNoLabs, test=testData1, cl=trainData1SampLabs, k=10, prob=TRUE)

tab3 <- table(mod3, testLabels)
acc3 <- sum(diag(tab3))/4200
acc3
tab3

# -----------  Submission to Kaggle ---------------
submitfile = "file:///C:/Users/Greg/Documents/School/IST 707/Week 7/Kaggle-digit-test.csv"
submitTest <- read.csv(submitfile)
submitTest <- data.frame(lapply(submitTest[,-1], normalize))
submitPred <- predict(mod2, submitTest)
table(submitPred)

write.csv(submitPred, file = "file:///C:/Users/Greg/Documents/School/IST 707/Week 8/submission.csv")
