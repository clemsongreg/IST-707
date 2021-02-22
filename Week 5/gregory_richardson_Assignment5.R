# Week 5 Assignment - Determining Authorship via Decision Tree
# Greg Richardson
# IST 707

# Load required packages
library(tm)
library(rpart)
library(rpart.plot)
library(rattle)
#library(SnowballC)
#library(lsa)
library(ggplot2)
#library(factoextra)
#library(cluster)

# Part 1 - Bring in Essays and Format

# Location of the folder containing the raw text forms for the essays.  
# There are 85 in total.
loc <- "C:/Users/Greg/Documents/School/IST 707/Week 4/fedPapers/txt"

# Bring the 85 essays into a single Corpus called "papers".  A quick count
# to make sure all 85 were brought in successfully.
papers <- Corpus(DirSource(loc)) 
length(papers)

# The disputed essays are believed to be either Hamilton or Madison.  Some
# of the Federalist Papers were written by Jay however, and a few co-
# authored by Hamilton and Madison.  These essays are removed to restrict
# clustering to only Hamilton and Madison.

# Removing all essays that were written by Jay.  To do this, all papers 
# with Jay in the title are removed.  There are 5 in total.
papers <- papers[-which(grepl("Jay", names(papers), fixed=TRUE))]

# Removing all essays that were written by both Hamilton and Madison.  
# These essays have 'HM' in their title.  They are removed as well.  There 
# are 3 of these.  Total count of essays should be 77.
papers <- papers[-which(grepl("HM", names(papers), fixed=TRUE))]
length(papers)

# Part 2 - Create a DTM with remaining essays

# A Document Term Matrix was created using the remaining 77 essays.
# Some basic data cleaning was performed as well, inclusing removing
# all words with lengths under 4 and over 10, removing punct,
# numbers, and transforming to all lowercase letters.  In addition, words
# appearing in more than 70% and less than 8% of all essays were removed.


fedDTM <- DocumentTermMatrix(papers, 
                             control = list(language = "english",
                                            stopwords = TRUE,
                                            wordLengths = c(4,10), 
                                            removePunctuation = TRUE,
                                            removeNumbers = TRUE,
                                            tolower = TRUE,
                                            #stemming = TRUE,
                                            remove_separators = TRUE,
                                            bounds = list(global = c(8,70))
   #                                         ,weighting = function (x) 
  #                                            weightTfIdf(x, normalize=TRUE)
                                            
                             )
)


inspect(fedDTM)


# The DTM was converted to a dataframe for analysis
fedMat <- as.matrix(fedDTM)

fedDf <- as.data.frame(fedMat)


# Need to remove some words which are spoiling the model
badwords <- c("madison", "james", "alexander", "hamilton")
fedDf <- fedDf[,-(match(badwords, names(fedDf)))]

totwords <- rowSums(fedDf)
fedDf <- fedDf/totwords

# Create a sum of frequencies for frequency analysis and word clouds
sumwords <- colSums(fedDf)

wcolor <- RColorBrewer::brewer.pal(5,"Dark2")

wordcloud::wordcloud(names(sumwords), sumwords, scale=c(2,0.5),  
                     random.order=FALSE, random.color = TRUE, 
                     colors = wcolor, max.words = 75)

# List of the Top 10 words in the dataset
data.frame("Sum Freq" = head(sort(sumwords, decreasing=TRUE), 10))

# Create 2 new sets of frequencies - one for Hamilton and one for Madison
# and then create top 10 lists and wordclouds for each.
hamwords <- colSums(fedDf[which(grepl("Hamilton", rownames(fedDf), 
                                      fixed = TRUE)),])
data.frame("Sum Freq" = head(sort(hamwords, decreasing=TRUE), 10))

madwords <- colSums(fedDf[which(grepl("Madison", rownames(fedDf),
                                      fixed = TRUE)),])
data.frame("Sum Freq" = head(sort(madwords, decreasing=TRUE), 10))

wordcloud::wordcloud(names(hamwords), hamwords, scale=c(3,0.5), 
                     random.order = FALSE, random.color = TRUE, 
                     colors = "red", max.words = 50 )

wordcloud::wordcloud(names(madwords), madwords, scale=c(3,0.5),
                     random.order = FALSE, random.color = TRUE, 
                     colors = "blue", max.words = 50 )

# The values need to be discretized before decision tree analysis can
# work.  

# A function was created which takes the normalized data for a column and
# converts it into 11 discrete bins from 0 up to > 0.01.  
#disCount <- function(column) {
 # return(cut(column, 
  #          breaks=c(0, #0.001, 
   #                  0.002, #$0.003, 
    #                 0.004, 
     #                #0.005,
      #               0.006, #.007, 
       #              0.008, #0.009, 
        #             0.010, 1.0), 
         #   right=FALSE))
#}

# The function is applied to all columns in the dataframe.
#for (x in colnames(fedDf))
 # fedDf[,x] <- disCount(fedDf[,x])
  #print(fedDf[1,x])


# A label is created to the matrix, with either "Hamilton", "Madison" or "?"
# based on authorship of the essay
fedDf$Author <- NA
fedDf$Author[which(grepl("Hamilton",rownames(fedDf), fixed=TRUE))] <- "Hamilton"
fedDf$Author[which(grepl("Madison", rownames(fedDf), fixed=TRUE))] <- "Madison"
fedDf$Author[which(grepl("disp", rownames(fedDf), fixed=TRUE))] <- NA
fedDf$Author <- as.factor(fedDf$Author)


# The dataset at this point was split into 3 groups by Author.  The disputed
# essays were stored to the side, while a representative sample from each
# of the Hamilton and Madison essays were recombined into both training and
# testing data.

dispDf <- fedDf[which(is.na(fedDf$Author)),]
hamDf <- fedDf[which(fedDf$Author == "Hamilton"),]
madDf <- fedDf[which(fedDf$Author == "Madison"),]

# There are 51 Hamilton and 15 Madison essays.  A test data set of 13
# essays (~ 20%) was pulled using roughly the same ratio of Ham:Mad - that
# is 10 Hamilton and 3 Madison.  The remaining essays were combined into a 
# training set.

hamSamp <- sample.int(nrow(hamDf), replace=FALSE)
madSamp <- sample.int(nrow(madDf), replace=FALSE)
testDf <- rbind(hamDf[hamSamp[1:10],], madDf[madSamp[1:3],])
trainDf <- rbind(hamDf[hamSamp[11:51],], madDf[madSamp[4:15],])

# Initial Decision Tree model of training data. No real modifications to 
# the model's control.

mod1 <- rpart(trainDf$Author ~ ., data = trainDf, method="class")
summary(mod1)
fancyRpartPlot(mod1, sub="Federalist Papers Decision Tree- Model 1")
pred1 <- predict(mod1, testDf, type="class")
testtab1 <- table(pred1, testDf$Author)
testtab1
disp1 <- predict(mod1, dispDf, type="class")
table(disp1)

# ---- Model 2 - Attempts to Improve Performance

mod2 <- rpart(trainDf$Author ~ ., data = trainDf, method = "class", control = rpart.control(xval=10, cp=0, minbucket = 1))
summary(mod2)
fancyRpartPlot(mod2, sub="Federalist Papers Decision Tree- Model 2")
pred2 <- predict(mod2, testDf, type="class")
testtab2 <- table(pred2, testDf$Author)
testtab2
disp2 <- predict(mod2, dispDf, type="class")
table(disp2)

plotcp(mod2)

