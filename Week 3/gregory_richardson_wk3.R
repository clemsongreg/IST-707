# Week 3 Assignment - Bank Data Rules
# Gregory Richardson
# IST 707

# ---------- Required Packages ------------
library(ggplot2)

library(arules)

library(arulesViz)

# --------- Load the Data -----------------
# File location is stored in variable loc.  Change as needed
loc <- "C:/Users/Greg/Documents/School/IST 707/Week 3/bankdata_csv_all.csv"

# File is read into a dataframe called bdata
bdata <- read.csv(loc)

# ------------ Data Processing and Cleaning ------------
# The data is in record form, which will not work for association rules
# mining using the 'arules' package.  Data needs to be transformed to a 
# transaction-type dataframe.  In addition, numeric columns will need to 
# be changed to discrete categories.  Finally, unnecessary columns are 
# removed to make for a cleaner mining dataframe.

# To begin with - a quick look at the structure of the current dataframe
# and then exploration of each individual column.
str(bdata)

# First column - id - unnecessary for AR mining.  Removing from the dataset
bdata <- bdata[,-1]

#  Age column is next.  Initially in numeric format, converting to discrete
# factors
summary(bdata$age)

# Ages range from 18 to 67.  For convenience, discrete factors are made 
# splitting the data by decades.  
bdata$age <- cut(bdata$age, breaks = c(10,20,30,40,50,60,70),  right=FALSE, labels = c("teens", "twenties", "thirties", "forties", "fifties", "sixties"))
summary(bdata$age)

ageplot <- ggplot(bdata, aes(x=age)) + geom_bar(color="#522D80", fill = "#F66733")
ageplot <- ageplot + scale_y_continuous(breaks=seq(0,160,20))
ageplot <- ageplot + theme_bw()
ageplot <- ageplot + labs(title = "Count of Customers by Age")
ageplot

# The second variable - sex - is already ready for arules mining.  Brief
# exploration of the data:
summary(bdata$sex)

# Plot the proportions of gender in each age group
sexage <- ggplot(bdata, aes(x=age, fill=sex)) + geom_bar(position="fill")
sexage <- sexage + labs (y="% of Age Group", x= "Age Group", title = "Gender by % of Age Group")
sexage <- sexage + scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1.0), labels=c("0%", "25%", "50%", "75%", "100%"))
sexage <- sexage + scale_fill_discrete(name = "Gender")
sexage 

# The third variable is region.  This variable is also in a suitable form 
# for arules mining.  Some brief exploration of the data:

summary(bdata$region)

agereg <- ggplot(bdata, aes(x=age, fill=region)) + geom_bar(position="fill")
agereg <- agereg + labs (y="% of Age Group", x= "Age Group", title = "Region by % of Age Group")
agereg <- agereg + scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1.0), labels=c("0%", "25%", "50%", "75%", "100%"))
agereg <- agereg + scale_fill_discrete(name = "Region")
agereg 

# Next variable is income.  Like age, the income is in a numeric form and 
# needs to be discretized.  

summary(bdata$income)

# Cut the data into 10k groups
bdata$income <- cut(bdata$income, breaks = c(0,10000, 20000, 30000, 40000, 50000, 60000, 70000), labels=c("<10k", "10-20k", "20-30k", "30-40k", "40-50k", "50-60k", "60-70k"),right = FALSE)
summary(bdata$income)

# Plot of the income by age groups
ageinc <- ggplot(bdata, aes(x=age, fill=income)) + geom_bar(position="fill")
ageinc <- ageinc + labs (y="% of Age Group", x= "Age Group", title = "Income by % of Age Group")
ageinc <- ageinc + scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1.0), labels=c("0%", "25%", "50%", "75%", "100%"))
ageinc <- ageinc + scale_fill_manual(name = "Income", values=c("#edf8fb", "#ccece6", "#99d8c9", "#66c2a4", "#41ae76", "#238b45", "#005824"))
ageinc

# Next variable is Married.  Current factors are "Yes" and "No".  This will
# create difficulty when creating a transaction list as there are multiple
# Yes/No variables in this dataset.  To fix, factors need to be changed to
# "Married" and "Not Married".  
summary(bdata$married)

# Change the levels from No and Yes to Unmarried and Married.  Recheck 
# summary to confirm the change is correct
levels(bdata$married) <- c("Unmarried", "Married")
summary(bdata$married)

agemar <- ggplot(bdata, aes(x=age, y=region, fill=married)) + geom_tile(color="white")
agemar <- agemar + labs (y="Marital Status", x= "Age Group", title = "Income by Age and Marital Status")
agemar <- agemar + scale_fill_manual(name = "Income Bracket", values=c("#F66733", "#522D80"))
agemar

# Number of Children is next.  Simply needs to be converted from numeric to
# factors.  # children range from 0 to 3.
bdata$children <- as.factor(bdata$children)
summary(bdata$children)

# Explore number of children in age groups and income level
marchil <- ggplot(bdata, aes(x=married, y=children, fill=income)) + geom_tile(color="white")
marchil <- marchil + labs (y="# of Children", x= "Marital Status", title = "Income by Marital Status and # of Children")
marchil <- marchil + scale_fill_manual(name = "Income Bracket", values=c("#edf8fb", "#ccece6", "#99d8c9", "#66c2a4", "#41ae76", "#238b45", "#005824"))
marchil

# Like with marital status, car status factors need to changed as well.
# Changing "No" and "Yes" factors to "Car" and "No Car"
summary(bdata$car)
levels(bdata$car) <- c("No Car", "Car")
summary(bdata$car)

inccar <- ggplot(bdata, aes(x=income, fill=car)) + geom_bar(position="fill")
inccar <- inccar + labs (y="% of Bracket", x= "Income Bracket", title = "Car Status by % Income Bracket")
inccar <- inccar + scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1.0), labels=c("0%", "25%", "50%", "75%", "100%"))
inccar <- inccar + scale_fill_manual(name = "Car Status", values=c("#F66733", "#522D80"))
inccar

# Savings Account changed like car to "No Savings" and "Savings"
summary(bdata$save_act)
levels(bdata$save_act) <- c("No Savings", "Savings")
summary(bdata$save_act)

incsav <- ggplot(bdata, aes(x=age, y=income, fill=save_act)) + geom_tile(color="white")
incsav <- incsav + labs (y="Income Bracket", x= "Age Range", title = "Savings Account by Age and Income")
incsav <- incsav + scale_fill_manual(name = "Savings Status", values=c("#F66733", "#522D80"))
incsav

# Current account status changed to "Inactive" and "Active"
summary(bdata$current_act)
levels(bdata$current_act) <- c("Inactive", "Active")
summary(bdata$current_act)

inccur <- ggplot(bdata, aes(x=age, y=income, fill=current_act)) + geom_tile(color="white")
inccur <- inccur + labs (y="Income Bracket", x= "Age Range", title = "Account Status by Age and Income")
inccur <- inccur + scale_fill_discrete(name = "Account Status")
inccur

# Mortgage levels changed to "No Mortgage" and "Mortgage"
summary(bdata$mortgage)
levels(bdata$mortgage) <- c("No Mortgage", "Mortgage")
summary(bdata$mortgage)

incmor <- ggplot(bdata, aes(x=age, fill=mortgage)) + geom_bar(position="fill")
incmor <- incmor + labs (y="% of Age Group", x= "Age Range", title = "Mortgage Status by Age")
incmor <- incmor + scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1.0), labels=c("0%", "25%", "50%", "75%", "100%"))
incmor <- incmor + scale_fill_discrete(name = "Mortgage Status")
incmor

# Finally, the last column holds the customers response.  Factors changed
# to "No PEP" and "PEP"
summary(bdata$pep)
levels(bdata$pep) <- c("No PEP", "PEP")
summary(bdata$pep)

incpep <- ggplot(bdata, aes(x=age, y=income, fill=pep)) + geom_tile(color="white")
incpep <- incpep + labs (y="Income Bracket", x= "Age Range", title = "PEP Result by Age and Income")
incpep <- incpep + scale_fill_manual(name = "PEP Result", values=c("#F66733", "#522D80"))
incpep

# -------------- Modeling -------------------------------

# Using the 'arules' package, begin analyzing the data for any association
# rules.  

# Model 1 was set up with no specific left or right hand side.  It is 
# simply looking for generic patterns in the data, describing typical
# customer trends in the bank's data.

# This model is mostly to look for patterns not having to do with the PEP
# campaign.  Therefore, a new dataframe was created excluding the PEP 
# results.
bdatamod <- bdata[,1:10]

# Model 1 is set up looking for trends with fairly high frequency, with a
# little less confidence as compensation.  
mod1 <- arules::apriori(bdatamod, parameter = list(supp = 0.2, conf = 0.75))
mod1 <- sort(mod1, by="confidence", decreasing = TRUE)
inspect(mod1[1:10])
mod1 <- sort(mod1, by="support", decreasing=TRUE)
inspect(mod1[1:10])
mod1 <- sort(mod1, by="lift", decreasing = TRUE)
inspect(mod1[1:10])
summary(mod1)

plot(mod1, method="graph", engine = "interactive")

# Model 2 looks at which customers opened PEP accounts.  The model uses a 
# low support level with a high confidence to highly focus on even small 
# segments of the population very likely to open an account.  The model
# also sets the RHS of the rules to 'pep=PEP', so only rules where accounts
# were opened are considered.

mod2 <- arules::apriori(bdata, parameter = list(supp = 0.03, conf=0.95), appearance = list(default= "lhs", rhs = "pep=PEP"), control = list(verbose=F))
summary(mod2)
mod2 <- sort(mod2, by="lift", decreasing= TRUE)
inspect(mod2[1:10])
mod2<- sort(mod2, by="confidence", decreasing = TRUE)
inspect(mod2[1:10])
mod2 <- sort(mod2, by="support", decreasing=TRUE)
inspect(mod2[1:5])

plot(mod2[1:10], method="graph", engine = "interactive")

# The third model seeks to understand who is NOT opening a PEP account.  This
# model sets the RHS to "no PEP" and uses slightly more open support and conf
# values than model 2.  In addition, the model is restricted to only rules
# with a max length of 4.
mod3 <- arules::apriori(bdata, parameter = list(supp = 0.04, conf=0.85, maxlen=4), appearance = list(default= "lhs", rhs = "pep=No PEP"), control = list(verbose=F))
summary(mod3)
mod3 <- sort(mod3, by="lift", decreasing=TRUE)
inspect(mod3[1:5])
mod3<- sort(mod3, by="confidence", decreasing = TRUE)
inspect(mod3[1:5])
mod3 <- sort(mod3, by="support", decreasing=TRUE)
inspect(mod3[1:5])

plot(mod3[1:10], method="graph", engine = "interactive")
