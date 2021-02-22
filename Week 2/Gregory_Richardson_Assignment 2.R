# Assignment 2 Source Code
# Gregory Richardson

# Packages that are needed:
library(ggplot2)
library(reshape2)

# Location of the CSV file to be read in
loc <- "C:/Users/Greg/Documents/School/IST 707/Week 2/data-storyteller.csv"

# Read in the file into a dataframe called 'school'
school <- read.csv(loc)

# Adjust the column names for easier data manipulation
cols <- c("School", "Section", "vAhead", "Middling", "Behind", "mBehind", "vBehind", "Completed")
colnames(school) <- cols

# -------------- Data Exploration ----------------

# Look at the School variable
summary(school$School)

# Store a list of the integer variable names and formal names for easy access
cats <- c("Completed","vAhead", "Middling", "Behind", "mBehind", "vBehind")
long <- c("Completed","Very Ahead", "Middling", "Behind", "More Behind", "Very Behind")

# Create a new column with the total number of students in each section
school$students <- rowSums(school[cats])

# Summary and visual description of the number of students in each section
summary(school$students)

g1 <- ggplot(school, aes(students)) + geom_histogram(binwidth = 10, center = 5, color="black", fill="#522D80")
g1 <- g1 + scale_x_continuous(breaks=seq(0,150,10), minor_breaks = FALSE)
g1 <- g1 + labs(title = "# of Students in Each Section")
g1

# Create a list of the totals for each category
totals <- colSums(school[cats])

# Graph of the overall status of the course across all schools and sections
g2 <- ggplot(data=NULL, aes(x=factor(names(totals), levels=cats), y=as.numeric(totals)))
g2 <- g2 + geom_bar(stat="identity", fill= "#F66733")
g2 <- g2 + scale_x_discrete(labels = long)
g2 <- g2 + labs(x="Lesson Status", y="Number of Students", title = "Overview of Course Status Across All Schools")
g2

# Create new columns transforming the counts to percentages of section population
school$pComp <- round(school$Completed / school$students * 100,2)
school$pVAhead <- round(school$vAhead / school$students * 100,2)
school$pMid <- round(school$Middling / school$students * 100,2)
school$pBehind <- round(school$Behind / school$students * 100,2)
school$pMBehind <- round(school$mBehind / school$students * 100,2)
school$pVBehind <- round(school$vBehind / school$students * 100,2)

# Labels of percentage columns
pcats <- c("pComp", "pVAhead", "pMid", "pBehind", "pMBehind", "pVBehind")

# Melt the percentage data into a new data frame so that charts can be made
mschool <- melt(school, id.vars = c("School", "Section"), measure.vars = pcats)

# Summary and a boxplot of the completion status grouping by schools
summary(school[,pcats])

g3 <- ggplot(data=mschool, aes(y=value, x=variable, color=School)) + geom_boxplot()
g3 <- g3 + scale_x_discrete(labels = long)
g3 <- g3 + labs(x="Lesson Status", y="% of Class", title="Boxplot of Course Status By School")
g3


# ----------------------- Analysis -------------------
# Explore the overall data by grouping into 3 bins - Ahead, At Pace, and
# Lagging.  For this, the categories Completed and Very Ahead were totaled,
# Middling and Behind, and More Behind and Very Behind.  Results put into 
# new columns and then aggregated.
school$Ahead <-  school$Completed + school$vAhead
school$Mid <- school$Middling + school$Behind
school$Lagging <- school$mBehind + school$vBehind

compTotals <- colSums(school[,c("Ahead", "Mid", "Lagging")])
compTotals

g4 <- ggplot(data=NULL, aes(x=factor(names(compTotals), levels=c("Ahead", "Mid", "Lagging")), y=as.numeric(compTotals)))
g4 <- g4 + geom_bar(stat="identity", fill= "#F66733", color="black")
g4 <- g4 + labs(x="Lesson Status", y="Percent of Students", title = "Simplified Course Status Across All Schools")
g4

# First step is to explore relationship between school and # of students.
# This is done graphically, and then with an ANOVA to test for difference

g5 <- ggplot(school, aes(x=School, y=students, fill=School)) + geom_dotplot(binaxis = "y", stackdir = "center", binwidth = 5)
g5 <- g5 + labs(x="School", y="Number of Students", title = "Dotplot of Class Size")
g5

aggregate(school$students, by=list(school$School), FUN=median)

a0 <- aov(students ~ School, school)
summary(a0)
TukeyHSD(a0)

# Next step is to run one-way ANOVA tests comparing schools for 
# each category.  For any categories that show a significant difference, a
# Tukey's HSD test was run to identify which pairs showed a significant
# difference.


# First test for percent Completed status
a1 <- aov(pComp ~ School, school)
summary(a1)

# Second test for percent Middling
a2 <- aov(pMid ~ School, school)
summary(a2)

TukeyHSD(a2)

g6 <- ggplot(school, aes(x=students, y=pMid, color=School, size=2)) + geom_point()
g6 <- g6 + labs(x="Number of Students", y="% Middling", title = "Middling Status by Class Size and School")
g6 <- g6 + guides(size="none")
g6

# Third test for percent Behind
a3 <- aov(pBehind ~ School, school)
summary(a3)

# 4th test for More Behind
a4 <- aov(pMBehind ~ School, school)
summary(a4)

# 5th test for Very Behind
a5 <- aov(pVBehind ~ School, school)
summary(a5)

TukeyHSD(a5)
g7 <- ggplot(school, aes(x=students, y=pVBehind, color=School, size=2)) + geom_point()
g7 <- g7 + labs(x="Number of Students", y="% Very Behind", title = "Very Behind Status by Class Size and School")
g7 <- g7 + guides(size="none")
g7

# In order to run a correlation matrix using the Schools as variables, need
# to transform the School variables into individual binary variables (T/F). 
# Creating a new data frame called 'cschool' with these transformations, 
# along with the number of students and percent statuses
cschool <- school[,c(pcats, "students")]
cschool$schoolA <- school$School == "A"
cschool$schoolB <- school$School == "B"
cschool$schoolC <- school$School == "C"
cschool$schoolD <- school$School == "D"
cschool$schoolE <- school$School == "E"

# Correlation matrix looking for correlations between schools and class
# size and completion status
cor(x=cschool[,c("students", "schoolA", "schoolB", "schoolC", "schoolD", "schoolE")], y=cschool[,pcats])




