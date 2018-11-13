#Data Analytics Assignment 6.1 Session 6

#Import Bank MArketing Dataset 

bank <- read.csv("bank-additional.csv", sep=";")
View(bank)
dim(bank)      # 4119 observations with 21 attributes
str(bank)      # All have the correct class

# Q a. Create a visual for representing missing values in the dataset.
#Solution:
psych::describe(bank)
library(VIM)

missing <- bank
missing[missing == "unknown"] <- NA

aggr(missing, col=c('blue', 'red'),
     numbers=TRUE, sortvars= TRUE,
     labels=names(missing), cex.axis=0.5,
     gap=3, ylab=c("missing data","pattern"))

sapply(missing, function(x) sum(is.na(x)))

#Q b. Show a distribution of clients based on a Job.
#Solution:

t <- table(bank$job)
# distribution in tabular form
t    

# distribution in graphical form
title <- barplot(t, xlab = "Job", ylab = "Numbers", main = "Clients based on Job",
                 col = heat.colors(12), las=3)
text(title, 0, t, pos = 3, srt = 90)

# Q c. Check whether is there any relation between Job and Marital Status?
#SOlution
# Ho : There is NO association between Job and Marital Status

chisq.test(missing$job, missing$marital)
# Since P Value is less than 0.05 , 
# there is association between Job and Marital status at 95% confidence level
# Since NA values are very less, are omitted
# -------------------------------------------------------------------

# Q d. Check whether is there any association between Job and Education?
#Solution:

# Ho : There is NO association between Job and Education.

chisq.test(missing$job, missing$education)

# Since the P value is less than 0.05,
# there is association between Job and Education at 95% confidence level
# Since NA values are very less, are omitted


