# Data: https://www.kaggle.com/wendykan/lending-club-loan-data/data
loan <- read.csv("loan.csv", stringsAsFactors = FALSE)
loanT <- loan
# Check dictionary

# Understand the data
summary(loan)
str(loan)
dim(loan)
head(loan)
colnames(loan)

# Check dictionary and cluster features into groups for better understanding.
length(unique(loan$id))
length(unique(loan$member_id))

# check how many NAs in each feature
length(which(is.na(loan$annual_inc)))
num.NA <- sort(sapply(loan, function(x) {sum(is.na(x))}), 
               decreasing=TRUE)
# The percentage of data missing in train.
# https://www.r-bloggers.com/r-tutorial-on-the-apply-family-of-functions/
sum(is.na(loan)) / (nrow(loan) *ncol(loan))
# Find out variables with largest number of missing values

# How to treat missing values
# (1) Remove features with too many missing value, 
#     or remove all rows with NA if you have a lot of data
# (2) If not missing at random, add new level to represent NA, impute with 0,
#     or generate new feature.
# (3) If missing at random, imputation using summary stats like mean or median,
#     or modeling way.
#     Example: use library(mice)
#     https://www.r-bloggers.com/imputing-missing-data-with-r-mice-package/

remain.col <- names(num.NA)[which(num.NA <= 0.8 * dim(loan)[1])]
loan <- loan[, remain.col]
loan$annual_inc[which(is.na(loan$annual_inc))] <- median(loan$annual_inc, na.rm = T)

# How do we explore data?
# 1) Numeric variables
# 2) Categorical variables
# 3) Numeric feature with numerical response
# 4) Categorical feature with numerical response
# 5) Numeric feature with categorical response
# 6) Categorical feature with categorical response

# 1) Numeric variables including both feature and response
mean(loan$int_rate)
sd(loan$int_rate)
median(loan$int_rate)
quantile(loan$int_rate, c(0.1, 0.25, 0.5, 0.75, 0.9))
plot(density(loan$int_rate))
# Q1 - 1.5IQR, Q1, median, Q3, Q3 + 1.5IQR, where IQR is interquartile range: Q3 - Q1
boxplot(loan$int_rate)
boxplot(int_rate ~ grade, data = loan)

mean(loan$annual_inc)
sd(loan$annual_inc)
median(loan$annual_inc)
quantile(loan$annual_inc, c(0.1, 0.25, 0.5, 0.75, 0.9))
plot(density(loan$annual_inc))
plot(density(log(loan$annual_inc)))
# Q1 - 1.5IQR, Q1, median, Q3, Q3 + 1.5IQR, where IQR is interquartile range: Q3 - Q1
boxplot(log(loan$annual_inc + 1))

# 2) Categorical variables
sort(table(loan$loan_status))
round(sort(table(loan$loan_status)) / dim(loan)[1] * 100, 2)
barplot(sort(table(loan$loan_status), decreasing = TRUE))
# remove certain string from loan_status
loan$loan_status <- gsub('Does not meet the credit policy. Status:',
                         '', loan$loan_status)
sort(table(loan$loan_status))
loan$loan_status_1 <- with(loan, ifelse(loan_status %in% c('Current', 'Fully Paid', 'Issued'),
                                        1, 0))
table(loan$loan_status_1)

sort(table(loan$purpose))
round(sort(table(loan$purpose)) / dim(loan)[1] * 100, 2)
barplot(sort(table(loan$purpose), decreasing = TRUE))

# 3) Numeric variable with numerical response, interest rate
with(loan[1:10000, ], plot(log(annual_inc + 1), int_rate))
library(corrplot)
correlations <- cor(loan[, c('int_rate', 'total_acc', 'acc_now_delinq', 'annual_inc',
                             'dti', 'loan_amnt')]) 
# possible to see NA if features has missing value
correlations <- cor(loan[, c('int_rate', 'total_acc', 'acc_now_delinq', 'annual_inc',
                             'dti', 'loan_amnt')], 
                    use = "pairwise.complete.obs")
corrplot(correlations, method = "square", tl.cex = 1, type = 'lower')
# http://www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram

# 4) Categorical variable with numerical response
boxplot(subset(loan, term == ' 36 months')$int_rate,
        subset(loan, term == ' 60 months')$int_rate)
boxplot(int_rate ~ purpose, data = loan)

library(zoo)
head(loan$issue_d)
# How to solve R problems, e.g, search Google for date MMM YYYY format in R: Date formatting MMM-YYYY - Stack Overflow
as.Date(as.yearmon(loan$issue_d[1:5], "%b-%Y"))
loan$issue_d_1 <- as.Date(as.yearmon(loan$issue_d, "%b-%Y"))
loan$issue_year <- format(loan$issue_d_1, '%Y')
loan$issue_mon <- format(loan$issue_d_1, '%m')
int.rate.by.time <- by(loan, loan$issue_d_1, function(x) {
  return(median(x$int_rate))
})
plot(as.Date(names(int.rate.by.time)), int.rate.by.time, type = 'l')

int.rate.by.year <- by(loan, loan$issue_year, function(x) {
  return(median(x$int_rate))
})
plot(names(int.rate.by.year), int.rate.by.year, type = 'l')

# Not only see the median by time, but also distribution by time.
library(lattice)
bwplot(int_rate ~ issue_year, data = loan)

# 5) Numeric variable with categorical response
boxplot(log(subset(loan, loan_status_1 == 0)$annual_inc + 1),
        log(subset(loan, loan_status_1 == 1)$annual_inc + 1))
with(subset(loan, loan_status_1 == 1), plot(density(log(1 + annual_inc))))
with(subset(loan, loan_status_1 == 0), lines(density(log(1 + annual_inc)), col = 'red'))

# 6) Categorical variable with categorical response
table(loan$loan_status_1, loan$purpose)
table(loan$loan_status_1, loan$purpose) / as.numeric(table(loan$loan_status_1))
barplot(table(loan$loan_status_1, loan$purpose))
barplot(table(loan$loan_status, loan$purpose), col = c(1:14))

library(tabplot)
tableplot(loan, select = c('int_rate', 'acc_now_delinq', 'annual_inc',
                           'loan_amnt', 'term', 'purpose'))
tableplot(loan, select = c('loan_status_1', 'acc_now_delinq', 'annual_inc',
                           'loan_amnt', 'term'))
# up to here, first week

org.data <- read.csv("LCFromWebsite_2007_2011.csv", stringsAsFactors = FALSE) #
org.data <- read.table("LCFromWebsite_2007_2011.csv", stringsAsFactors = FALSE, 
                       fill = TRUE, sep = ",", skip = 1, header = T)









