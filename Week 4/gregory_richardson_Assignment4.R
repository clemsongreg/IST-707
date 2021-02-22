# Week 4 Assignment - Determining Authorship
# Greg Richardson
# IST 707

# Load required packages
library(tm)
library(SnowballC)
library(lsa)
library(ggplot2)
library(factoextra)
library(cluster)

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
                                            stopwords = FALSE, 
                                            #stopwords = stops,
                                            wordLengths = c(4,10), 
                                            removePunctuation = TRUE,
                                            removeNumbers = TRUE,
                                            tolower = TRUE,
                                            #stemming = TRUE,
                                            remove_separators = TRUE,
                                            bounds = list(global = c(8,70))
                                            #weighting = function (x) weightTfIdf(x, normalize=TRUE)
                                            
                                            )
                             )


inspect(fedDTM)



# The DTM was converted to a matrix for analysis
fedMat <- as.matrix(fedDTM)


# A second matrix was created containing normalized values for each term in
# the matrix.
sums <- rowSums(fedMat)
fedMatN <- fedMat / sums


# A second DTM was produced using essentially the same setup as before, but
# with stopwords removed.  This dataset was used to explore the data only 
# to look at what were most frequently occuring important words in the data

fedDTMstop <- DocumentTermMatrix(papers, 
                                 control = list(language = "english",
                                                stopwords = TRUE, 
                                                #stopwords = stops,
                                                wordLengths = c(4,10), 
                                                removePunctuation = TRUE,
                                                removeNumbers = TRUE,
                                                tolower = TRUE,
                                                #stemming = TRUE,
                                                remove_separators = TRUE,
                                                bounds = list(global = c(8,70))
                                                #weighting = function (x) weightTfIdf(x, normalize=TRUE)
                                                
                                 )
)

# --------  Part 3: Exploring Dataset ----------
# Selecting a color scheme for word cloud views
RColorBrewer::display.brewer.all()
wcolor <- RColorBrewer::brewer.pal(5,"Dark2")

# Create a list called sumwords, which sums up all occurences of words 
# across all essays.  This uses the 2nd DTM with stopwords removed.  
#Produce a wordcloud with the top 50 words found in the essays.
sumwords <- colSums(as.matrix(fedDTMstop))
wordcloud::wordcloud(names(sumwords), sumwords, scale=c(2,0.5),  
                     random.order=FALSE, random.color = TRUE, 
                     colors = wcolor, max.words = 50)

# List of the Top 10 words in the dataset
head(sort(sumwords, decreasing=TRUE), 10)

# List of the Top words in Hamilton's essays
hamSum <- colSums(as.matrix(fedDTMstop)[which(grepl("Hamilton", rownames(fedMat), fixed=TRUE)),])
hamSum <- hamSum[sort.list(hamSum, decreasing = TRUE)]
head(hamSum, 10)
wordcloud::wordcloud(names(hamSum), hamSum, scale=c(3,0.5), random.order = FALSE, random.color = TRUE, colors = "red", max.words = 50 )

# List of the TOp Words in Madison's essays
madSum <- colSums(as.matrix(fedDTMstop)[which(grepl("Madison", rownames(fedMat), fixed=TRUE)),])
madSum <- madSum[sort.list(madSum, decreasing = TRUE)]
head(madSum, 10)
wordcloud::wordcloud(names(madSum), madSum,scale=c(3,.5), random.order = FALSE, random.color = TRUE, colors = "blue", max.words = 50 )


# Part 4 - Distance Measures ------------

# A distance matrix was created using Euclidean distance between the points
# of the non-normalized matrix.
dist_E <- dist(fedMat, method="euclidean")
summary(dist_E)

# A Hierarchical Cluster Analysis was created using the distance matrix 
# produced.  The ward.D method was determined to be the most effective.
hac1 <- hclust(dist_E, method="ward.D")
plot(hac1)

# A cosine dissimilarity matrix was created as well, and a second model
# tried, but the results were never strong enough to continue with.
#cossim <- fedMat / sqrt(rowSums(fedMat * fedMat))
#cossim <- cossim %*% t(cossim)
#dist_C <- as.dist(1-cossim)
#summary(dist_C)

#hac2 <- hclust(dist_C, method="ward.D")
#plot(hac2)


# After analysis of the HAC model, 2 points were chosen as initial 
# starting centroids for the K-Means analysis.  They were stored in a new
# smaller matrix:
centerpts <- fedMatN[c("Hamilton_fed_22.txt", "Madison_fed_37.txt"),]

# K-Means model was created.  Initially used random starting points but 
# kept getting very noisy results.  When the centerpoints above were used
# as initial centroids, the model became MUCH more accurate in its 
# predictions.
k1 <- kmeans(fedMatN, centers = centerpts)
k1

# Visual interpretation of the k-means clusters
g1 <- fviz_cluster(k1, fedMatN)
g1

# Some plots of the data
fedDf <- as.data.frame(fedMat)
fedDf$cluster <- as.factor(k1$cluster)
levels(fedDf$cluster) <- c("Hamilton", "Madison")
gg1 <- ggplot(fedDf, aes(x=upon, y=powers, color=cluster, shape=cluster)) + geom_point(size=3.5, position = "jitter")
gg1 <- gg1 + labs(x="Word: Upon", y="Word: Powers", title="Comparison of the Words: 'Upon' and 'Powers'")
gg1
