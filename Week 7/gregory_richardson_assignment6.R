#  Assignment 6 - Comparing Handwriting

library(e1071)
library(rpart)
library(rpart.plot)
library(rattle)
library(naivebayes)
library(ggplot2)

fileloc <- "file:///C:/Users/Greg/Documents/School/IST 707/Week 7/Kaggle-digit-train.csv"

file <- read.csv(fileloc)


# Remove all columns where there are no values as they won't give any real
# information
#sums <- colSums(file)
#file1 <- file[,-(which(sums==0))]

# Change labels to a factor
file$label <- as.factor(file$label)

str(file)

# -------------  Decision Tree Model -------------------

# First model is a decision tree analysis.Test Data used to predict results
# This model uses the raw data, with the pixel data left as integers.

# Create a test and training data set.  Test set created using every 10th
# value.  The training data uses the rest.  The test data set then is 
# stripped of the label, which is set to the side.
testnums1 <- seq(1,length(file$label), 10)
fullTestData1 <- file[testnums1,]
trainData1 <- file[-testnums1,]
testLabels1 <- fullTestData1$label
testData1 <- fullTestData1[,-1]

# Check to make sure both sets are relatively well-balanced
table(trainData1$label)
table(testLabels1)

dtMod <- rpart(label ~ ., data=trainData1, method = "class", control = rpart.control(xval = 10, cp=0.001, minsplit = 2))
fancyRpartPlot(dtMod, main = "Handwriting Decision Tree", sub = NULL)
str(dtMod)
dtPred <- predict(dtMod, testData1, type="class")
table(testLabels1, dtPred)

# -------------  Naive-Bayes Model ---------------

# Create a second file set for use with NB modelling
file1 <- file

# Quick look at a random 10 pixels to confirm non-normal distribution
# A random sample of 5 pixel values was selected
randPix <- sample(2:length(file1[]), 5)

# The 5 lucky pixels were melted into a single dataframe for plotting
normdf <- reshape2::melt(file1[,randPix], id.vars=NULL)

# A violin plot of the 5 random variables was created
g1 <- ggplot(data=normdf, aes(x=variable, y=value)) 
g1 <- g1 + geom_violin(scale="width", color="#522D80", fill = "#F66733") 
g1 <- g1 + stat_summary(fun.y = "mean", geom = "point", size=2, color="#522D80")
g1 <- g1 + scale_y_continuous("Darkness Value", limits = c(0, 255))
g1 <- g1 + ggtitle("Violin Plot for 5 Random Pixels")
g1

# Columns contain values that range from 0 to 255 based on darkness.  Since 
# it is unlikely that values will be in a normal distribution, it would be
# better to discretize these values before proceeding. 


binning <- function(pixel) {
  return(
    cut(pixel
        , breaks = c(0, 1, 75, 125, 250, 255, Inf)
        , labels = c("None", "Trace", "V. Light", "Light", "Medium", "Dark")
        , right = FALSE
        , include.lowest = TRUE 
        , ordered_result = TRUE
        )
    )
}

# Apply function to dataset
for (col in colnames(file1[-1]))
     file1[,col] <- binning(file1[,col])

summary(file1[,randPix])

randDisc <- reshape2::melt(file1[,randPix], id.vars=NULL)
randDisc$value <- factor(randDisc$value, levels = c("None", "Trace", "V. Light", "Light", "Medium", "Dark"))
g2 <- ggplot(data=randDisc, aes(x=variable, fill = factor(value, levels = c("Dark", "Medium", "Light", "V. Light", "Trace", "None")))) + geom_bar(color = "#522D80")
g2 <- g2 + scale_fill_manual(name = "Darkness Level", values = c("#252525", "#636363", "#969696", "#bdbdbd", "#d9d9d9", "#f7f7f7"))
g2 <- g2 + theme_light()
g2 <- g2 + scale_y_continuous("# of Observations")
g2 <- g2 + scale_x_discrete("Random Pixel")
g2 <- g2 + ggtitle("Discretized Results for Random Pixels")
g2

        
# Create a test and training data set.  Test set created using every 10th
# value.  The training data uses the rest.  The test data set then is 
# stripped of the label, which is set to the side.
testnums <- seq(1,length(file1$label), 10)
fullTestData <- file1[testnums,]
trainData <- file1[-testnums,]
testLabels <- fullTestData$label
testData <- fullTestData[,-1]

# Check to make sure both sets are relatively well-balanced
table(trainData$label)
table(testLabels)

# Second model uses basic naive Bayes with no major modifications.  Model
# then used to predict results from the test set
nbMod <- naiveBayes(label ~ ., data=trainData, laplace = 1, type="class")
nbPred <- predict(nbMod, testData)
table(testLabels, nbPred)

# ----------------  Kaggle Test Data ------------
# Read in the Kaggle Test Data
fileloc <- "file:///C:/Users/Greg/Documents/School/IST 707/Week 7/Kaggle-digit-test.csv"
kagTest <- read.csv(fileloc)

# Apply function to dataset
for (col in colnames(kagTest[-1]))
  kagTest[,col] <- binning(kagTest[,col])

kagPred <- predict(nbMod, kagTest)
table(kagPred)

