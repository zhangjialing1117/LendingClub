##############################################################################
##############################################################################
###                                                                        ###
###               Project: Annalysis of Lending Club data                  ###
###                                                                        ###
##############################################################################
##############################################################################


#######################################################################################################################
# A.Get Data
# 1, Understand what’s the data you need
# 2, Find data source (if existing, if not existing need to define logging schema and work with engineers to get the data)
#######################################################################################################################

# Loading Libraries, and data
library(ggplot2)
library(dplyr)
library(reshape2)
#library(ggthemes)
library(maps)
library(RColorBrewer)
library(lattice)
#library(plotly)

# setting up and load data
rm(list=ls()) # removes all objects from the current workspace (R memory)
setwd("~/Desktop/MOOC/R/lending club")
report_date=20181218

#LCA = read.csv("LC2007-2012.csv", stringsAsFactors = FALSE, header = T, skip = 1)
#LCB = read.csv("LC2012-2013.csv", stringsAsFactors = FALSE, header = T, skip = 1)
#LCC = read.csv("LC2013-2014.csv", stringsAsFactors = FALSE, header = T, skip = 1)
#LCD = read.csv("LC2014-2015.csv", stringsAsFactors = FALSE, header = T, skip = 1)
#LCE = rbind(LCA, LCB, LCC, LCD)
#LC = tbl_df(LCE)
#save(LC, file = "LC.RData")
#rm(list=ls())
loan0<-read.csv("loan.csv", stringsAsFactors = FALSE)
loan=loan0

#a = c("id","member_id","loan_amnt","funded_amnt","funded_amnt_inv","term","int_rate","installment","grade","sub_grade","emp_title","emp_length","home_ownership","annual_inc","verification_status","issue_d","loan_status","pymnt_plan","url","desc","purpose","title","zip_code","addr_state","dti","delinq_2yrs","earliest_cr_line","fico_range_low","fico_range_high","inq_last_6mths","mths_since_last_delinq","mths_since_last_record","open_acc","pub_rec","revol_bal","revol_util","total_acc","initial_list_status","out_prncp","out_prncp_inv","total_pymnt","total_pymnt_inv","total_rec_prncp","total_rec_int","total_rec_late_fee","recoveries","collection_recovery_fee","last_pymnt_d","last_pymnt_amnt","next_pymnt_d","last_credit_pull_d","last_fico_range_high","last_fico_range_low","collections_12_mths_ex_med","mths_since_last_major_derog","policy_code")
#b = c("36805548","39558264","10400","10400","10400"," 36 months","  6.99%","321.08","A","A3","Truck Driver Delivery Personel","8 years","MORTGAGE","58000","not verified","Dec-2014","Current","n","https://www.lendingclub.com/browse/loanDetail.action?loan_id=36805548","","credit_card","Credit card refinancing","937xx","CA","14.92","0","Sep-1989","710","714","2","42","","17","0","6133","31.6%","36","w","8544.32","8544.32","2237.46","2237.46","1855.68","381.78","0.0","0.0","0.0","Aug-2015","321.08","Sep-2015","Aug-2015","679","675","0","59","1")
#x = data.frame(a,b)
#print(x)

#######################################################################################################################
# B. Data Preprocessing
# 1, Validate data (understand definition, quality check, data inconsistency)
# 2, Clean data (missing data, invalid values, duplicate record, etc)
# 3, Data Transformation & Aggregation, etc
#######################################################################################################################

# Understand the data
summary(loan0)
str(loan0)
dim(loan0) # 887379*74
head(loan0)
#colnames(loan0),rownames(loan0)
#unique(loan0)
length(loan0)
loan0$dti

# Check dictionary and cluster features into groups for better understanding.
length(unique(loan0$id))
length(unique(loan0$member_id))

#######################################################################################################################
# Data Cleaning
#######################################################################################################################
# Find out variables with largest number of missing values
# How to treat missing values
# (1) Remove features with too many missing value, 
#     or remove all rows with NA if you have a lot of data
# (2) If not missing at random, add new level to represent NA, impute with 0,
#     or generate new feature.
# (3) If missing at random, imputation using summary stats like mean or median,
#     or modeling way.
#     Example: use library(mice)
# (4) Use model from not na to predict (mice) na
#     https://www.r-bloggers.com/imputing-missing-data-with-r-mice-package/
#######################################################################################################################

# Check how many NAs in each feature
## sapply(), for each column apply the function
## function: how many columns are empty
## sort
length(which(is.na(loan0$annual_inc)))
num.NA <- sort(sapply(loan0, function(x) {sum(is.na(x))}), decreasing=TRUE)
print(num.NA)
# The percentage of data missing in train.
# https://www.r-bloggers.com/r-tutorial-on-the-apply-family-of-functions/
sum(is.na(loan0)) / (nrow(loan0) *ncol(loan0))

# Remove features with too many missing value, or remove all rows with NA if you have a lot of data
remain.col <- names(num.NA)[which(num.NA <= 0.8 * dim(loan0)[1])]
names(num.NA)[(num.NA>0.8*dim(loan0)[1])]
loan1 <- loan0[, remain.col]

#loan1
dim(loan1) # 887379* 57
num.NAnew <- sort(sapply(loan1, function(x) {sum(is.na(x))}), decreasing=TRUE)
names(num.NAnew)[(num.NAnew>0.8*dim(loan)[1])]
num.NAnew

loan1$mths_since_last_major_derog # 665676
loan1$mths_since_last_delinq # 454312
loan1$tot_coll_amt  # 70276
loan1$tot_cur_bal # 70276
loan1$total_rev_hi_lim # 70276 
loan1$revol_util # 502 
loan1$collections_12_mths_ex_med # 145
loan1$delinq_2yrs # 29
loan1$inq_last_6mths # 29
loan1$open_acc # 29
loan1$pub_rec # 29
loan1$total_acc # 29
loan1$acc_now_delinq # 29
loan1$annual_inc # 4

### need to know whether annual_inc is sensitive to outlier ###
## use mean (sensitive to outlier, use when no outlier)/median (there are outliers) to replace
x <- na.omit(loan1$annual_inc)
boxplot(x)
loan1$annual_inc[which(is.na(loan1$annual_inc))] <- median(loan1$annual_inc, na.rm = T)
boxplot(loan1$annual_inc)
#y=sort(sapply(loan1, function(x){sum(is.na(x))}),decreasing=TRUE)
length(which(is.na(loan1$annual_inc)))

# Check if missing NA at the same position
all.equal(which(is.na(loan1$delinq_2yrs)),
          which(is.na(loan1$inq_last_6mths)),
          which(is.na(loan1$open_acc)),
          which(is.na(loan1$pub_rec)),
          which(is.na(loan1$total_acc)),
          which(is.na(loan1$acc_now_delinq)))
identical(which(is.na(loan1$tot_coll_amt)),
          which(is.na(loan1$tot_cur_bal)),
          which(is.na(loan1$total_rev_hi_lim)))
#x=na.omit(loan1$tot_coll_amt)
#boxplot(x)

## use MICE package, use modeling to predict NA, 可以和use mean/median 进行比较 ##

#######################################################################################################################
# C. Explore data
# 1, Variable identification
#    Different data type needs different analysis method 
#    type of variable: predictors, response
#    data type: character, numeric    - variable category: continuous, categorical
# 2, Exploratory visualization (correlation matrix, scatter plot, etc)
#    Multi-collinearity (frequently asked)
#    Normality (frequently asked)
# 3, Variable reduction
#    Principle Component Analysis (hard to interpret)
# 4, Variable Creation (feature engineering)
#    Good features are usually more important than fancy models
#    We need domain knowledge	
#######################################################################################################################
# 1) Numeric variables
# 2) Categorical variables 
#   (term, grade, sub_grade, emp_title, emp_length, home_ownership,
#   verificatio_status, issue_d, loan_status, pymnt_plan,url, desc, purpose, title, 
#   zip_code, addr_state, earliest_cr_line, initial_list_status, last_pymnt_d,
#   next_pymnt_d, last_credit_pull_d, application_type, verification_status_joint)
# 3) Numeric feature with numerical response
# 4) Categorical feature with numerical response
# 5) Numeric feature with categorical response
# 6) Categorical feature with categorical response
#######################################################################################################################
str(loan1)
sapply(loan1, class)

data_types <- function(frame) {
  res <- lapply(frame, class)
  res_frame <- data.frame(unlist(res))
  barplot(table(res_frame), main="Data Types", col="steelblue", ylab="Number of Features")
}
data_types(loan1)

summary.default(loan1)

# 1) Numeric variables
# simple Plot (boxplot, density, histogram)
#######################################################################################################################
# int_rate VS grade
#######################################################################################################################
density(loan1$int_rate)

par(mfrow=c(1,2)) 
hist(loan1$int_rate, main='Histogram')
plot(density(loan1$int_rate), main='Density')

mean(loan1$int_rate)
sd(loan1$int_rate)
median(loan1$int_rate)
quantile(loan1$int_rate, c(0.1, 0.25, 0.5, 0.75, 0.9))

boxplot(loan1$int_rate)
# Q1 - 1.5IQR, Q1, median, Q3, Q3 + 1.5IQR, where IQR is interquartile range: Q3 - Q1
#par(mfrow=c(1,1))
boxplot(int_rate ~ grade, data = loan1, xlab="grade", ylab="int_rate")
        #col=topo.colors(3))
#legend("bottomleft", inset=.02, title="Number of Cylinders",
#       c("4","6","8"), fill=topo.colors(3), horiz=TRUE, cex=0.8)

#######################################################################################################################
# int_rate VS purpose, term
#######################################################################################################################
boxplot(int_rate ~ purpose, data = loan1)
boxplot(int_rate ~ term, data=loan1)

#######################################################################################################################
# annual_inc_log VS grade
#######################################################################################################################
# Check if Annual Income include Zero, before do the Log scale (Can i use log10 ????)
min(loan1$annual_inc)
mean(loan1$annual_inc)
sd(loan1$annual_inc)
median(loan1$annual_inc)
quantile(loan1$annual_inc, c(0.1, 0.25, 0.5, 0.75, 0.9))
plot(density(loan1$annual_inc)) ## Skewed, 不太好用prediction

par(mfrow=c(1,2))
plot(density(log10(loan1$annual_inc)))
plot(density(log(loan1$annual_inc)))

boxplot(loan1$annual_inc)
# Q1 - 1.5IQR, Q1, median, Q3, Q3 + 1.5IQR, where IQR is interquartile range: Q3 - Q1

which(loan1$annual_inc == 0)
loan1$annual_inc[ which(loan1$annual_inc == 0)]
loan1$annual_inc_log=log(loan1$annual_inc+1)
loan1$annual_inc_log10=log10(loan1$annual_inc+1)
quantile(loan1$annual_inc_log10, c(0.1, 0.25, 0.5, 0.75, 0.9))

par(mfrow=c(1,2))
boxplot(loan1$annual_inc_log)
boxplot(loan1$annual_inc_log10)

par(mfrow=c(1,2))
boxplot(annual_inc_log10 ~ grade,data=loan1,xlab="grade", ylab="annual_income_log10")
boxplot(annual_inc_log ~ grade,data=loan1,xlab="grade", ylab="annual_income_log")

#######################################################################################################################
# annual_inc_log VS subgrade, term
#######################################################################################################################
par(mfrow=c(1,1))
boxplot(annual_inc_log ~ sub_grade,data=loan1)
boxplot(annual_inc_log ~ term, data=loan1)

# 2) Categorical variables (table, sort table, barplot)
#   (term, grade, sub_grade, emp_title, emp_length, home_ownership,
#   verificatio_status, issue_d, loan_status, pymnt_plan,url, desc, purpose, title, 
#   zip_code, addr_state, earliest_cr_line, initial_list_status, last_pymnt_d,
#   next_pymnt_d, last_credit_pull_d, application_type, verification_status_joint)

table(loan1$grade) 
## table is more like value count in Python
table(loan1$term)
table(loan1$loan_status)
table(loan1$home_ownership)

sort(table(loan1$emp_length))
length(which(loan1$emp_length == "n/a")) # How to process data with "n/a"

sort(table(loan1$grade))
sort(table(loan1$loan_status))

loan_status_perc=round(sort(table(loan1$loan_status)) / dim(loan1)[1] * 100, 2)
par(mfrow=c(1,1))
barplot(sort(table(loan1$loan_status), decreasing = TRUE))
barplot(sort(loan_status_perc, decreasing = TRUE)) # percentage

# remove certain string from loan_status
loan1$loan_status <- gsub('Does not meet the credit policy. Status:','', loan1$loan_status)
sort(table(loan1$loan_status))
loan_status_perc=round(sort(table(loan1$loan_status)) / dim(loan1)[1] * 100, 2)
barplot(sort(loan_status_perc, decreasing = TRUE))

#######################################################################################################################
# loan_status categories re-organize
#######################################################################################################################
loan1$loan_status_1 <- with(loan1, ifelse(loan_status %in% c('Current', 'Fully Paid', 'Issued'),1, 0))
table(loan1$loan_status_1)

sort(table(loan1$purpose))
purpose_perc=round(sort(table(loan1$purpose)) / dim(loan1)[1] * 100, 2)
barplot(sort(purpose_perc, decreasing = TRUE))

# 3) Numeric feature with numerical response (correlation)
with(loan1[1:10000, ], plot(log(annual_inc + 1), int_rate))
#with(loan1[1:10000, ], plot(annual_inc, int_rate))
library(corrplot)
correlations <- cor(loan1[, c('int_rate', 'total_acc', 'acc_now_delinq', 'annual_inc','dti', 'loan_amnt')]) 
# possible to see NA if features has missing value
correlations <- cor(loan1[, c('int_rate', 'total_acc', 'acc_now_delinq', 'annual_inc',
                             'dti', 'loan_amnt')], use = "pairwise.complete.obs")
corrplot(correlations, method = "square", tl.cex = 1, type = 'lower')
# http://www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram

# 4) Categorical variable with numerical response (Time: as.Date(zoo), bwplot (lattice))
boxplot(subset(loan1, term == ' 36 months')$int_rate,
        subset(loan1, term == ' 60 months')$int_rate)
boxplot(int_rate ~ term, data = loan1)

boxplot(int_rate ~ purpose, data = loan1)
sort(table(loan1$purpose),decreasing = TRUE)

library(zoo)
head(loan1$issue_d)
# How to solve R problems, 
# e.g, search Google for date MMM YYYY format in R: Date formatting MMM-YYYY - Stack Overflow
as.Date(as.yearmon(loan1$issue_d[1:5], "%b-%Y"))

loan1$issue_d_1 <- as.Date(as.yearmon(loan1$issue_d, "%b-%Y"))
loan1$issue_year <- format(loan1$issue_d_1, '%Y')
loan1$issue_mon <- format(loan1$issue_d_1, '%m')

# "by" function: split and groupby the data frame 
par(mfrow=c(1,2))
int.rate.by.time <- by(loan1, loan1$issue_d_1, function(x) {return(median(x$int_rate))}) ###!!!!important
plot(as.Date(names(int.rate.by.time)), int.rate.by.time, type = 'p')

int.rate.by.year <- by(loan1, loan1$issue_year, function(x) {return(median(x$int_rate))})
plot(names(int.rate.by.year), int.rate.by.year, type = 'p')
# Not only see the median by time, but also distribution by time.

library(lattice)
bwplot(int_rate ~ issue_year, data = loan1)
#plot(names(int.rate.by.year), int.rate.by.year, type = 'p')

# 5) Numeric variable with categorical response
#subset: return subsets of vectors that meet conditions
boxplot(log(subset(loan1, loan_status_1 == 0)$annual_inc + 1),
        log(subset(loan1, loan_status_1 == 1)$annual_inc + 1))
loan1$loan_status_1
boxplot(annual_inc_log ~ loan_status_1, data=loan1)

par(mfrow=c(1,2))
#loan1$loan_status_1 <- with(loan1, ifelse(loan_status %in% c('Current', 'Fully Paid', 'Issued'),1, 0))
with(subset(loan1, loan_status_1 == 1), plot(density(log(1 + annual_inc))))
with(subset(loan1, loan_status_1 == 0), plot(density(log(1 + annual_inc))))
table(loan1$loan_status_1)
par(mfrow=c(1,1))
with(subset(loan1, loan_status_1 == 1), plot(density(log(1 + annual_inc))))
with(subset(loan1, loan_status_1 == 0),lines(density(log(1 + annual_inc)), col = 'red'))

# 6) Categorical variable with categorical response
table(loan1$loan_status_1, loan1$purpose)
table(loan1$loan_status_1, loan1$purpose) / as.numeric(table(loan1$loan_status_1)) *100
#boxplot(int_rate ~ purpose, data = loan1)
barplot(table(loan1$loan_status_1, loan1$purpose))
barplot(table(loan1$loan_status_1, loan1$purpose) / as.numeric(table(loan1$loan_status_1)))

unique(loan1$purpose)
unique(loan1$loan_status)
barplot(table(loan1$loan_status, loan1$purpose), col = c(1:14),legend.text = TRUE,
        args.legend = list(x = "topright", bty = "n", inset=c(-0.15, 0)))#??
barplot(table(loan1$loan_status_1, loan1$purpose) / as.numeric(table(loan1$loan_status_1)),
        legend.text = TRUE, args.legend = list(x = "topright", bty = "n", inset=c(-0.15, 0)))

#######################################################################################################################
# How to understand tableplot??
#######################################################################################################################
library(tabplot)
tableplot(loan1, select = c('int_rate', 'acc_now_delinq', 'annual_inc','loan_amnt', 'term', 'purpose'))
tableplot(loan1, select = c('loan_status_1', 'acc_now_delinq', 'annual_inc','loan_amnt', 'term'))
# A tableplot is a visualisation of (large) multivariate datasets. 
# Each column represents a variable and each row bin is an aggregate of a certain number of records. 
# For numeric variables, a bar chart of the mean values is depicted. 
# For categorical variables, a stacked bar chart is depicted of the proportions of categories. 
# Missing values are taken into account.
plot(density(loan1$int_rate))

#######################################################################################################################
# Feature Reduction and Creation
# First think about what features could be included in the model
# i.e., what features would be available during model building. Work example.
# e.g., loan payment features will not be available when deciding interest rate.

# Second think about what features should be included in the model
# i.e., Remove features using intuition, Remove features with unique value per row or no variance. 
#       Remove redundant features
# e.g., id, member.id
#######################################################################################################################
# Based on dictionary, and feature study, delete redudent variable
# check the missing data reason: 1) systematic 2) random
num.value <- sapply(loan1, function(x){return(length(unique(x)))})
#num.value1 <- sapply(loan1, function(x){length(unique(x))})
# Delete redudent features
which(num.value == 1) # -> feature: policy_code
which(num.value == dim(loan1)[1]) # -> feature: id, member_id, url
colnames(loan1)

#######################################################################################################################
# feature creation
#######################################################################################################################
# since this feature is already deleted, here is just an example
#summary(loan0$dti_joint)
#boxplot(loan0$dti_joint)
#table(loan0$application_type)
#with(subset(loan0, is.na(dti_joint)), table(application_type))
# subset: Return subsets of vectors, matrices or data frames which meet conditions
#subset(loan0,is.na(dti_joint) & application_type=='JOINT')

#loan0$dti <- ifelse(!is.na(loan0$dti_joint), loan0$dti_joint, loan0$dti)
#sum(is.na(loan0$dti_joint))
#length(which(is.na(loan0$dti_joint)))
length(which(is.na(loan0$annual_inc_joint)))
loan1$annual_inc <- ifelse(!is.na(loan0$annual_inc_joint), loan0$annual_inc_joint, loan1$annual_inc)
length(which(is.na(loan1$annual_inc)))

#######################################################################################################################
# verification_status, verification_status_joint contains "n/a" data
#######################################################################################################################
# combine 2 features into 1
#######################################################################################################################
unique(loan1$verification_status)
table(loan1$verification_status)
unique(loan1$verification_status_joint)
table(loan1$verification_status_joint)
loan1$verification_status <- ifelse(loan1$verification_status_joint != "",
                                    loan1$verification_status_joint, loan1$verification_status)

#######################################################################################################################
# Categorical feature: one hot encoding is not needed here, but still need to re-organize the variable
# either through recreate new variables or change into booleans
#######################################################################################################################
length(unique(loan1$addr_state)) 
#51, 太多了(51-1=50个boolean, one-hot encoding)，
#不适合加到model, 增加了computation requirement
#人为grouping, intuition：based on region, economical, based on int_rate
# by function:
int_state <- by(loan, loan$addr_state, function(x) {return(mean(x$int_rate))})
#人为grouping###学会！！！！
loan$state_mean_int <-
  ifelse(loan$addr_state %in% names(int_state)[which(int_state <= quantile(int_state, 0.25))], 
         'low', ifelse(loan$addr_state %in% names(int_state)[which(int_state <= quantile(int_state, 0.5))],
                       'lowmedium', ifelse(loan$addr_state %in% names(int_state)[which(int_state <= quantile(int_state, 0.75))], 
                                           'mediumhigh', 'high')))
table(loan$state_mean_int)

unique(loanT$home_ownership)
loan$home_ownership <- ifelse(loan$home_ownership %in% c('ANY', 'NONE', 'OTHER'), 
                              'OTHER',loan$home_ownership)
table(loan$home_ownership)
#######################################################################################################################

library(zoo)
loan$issue_d_1 <- as.Date(as.yearmon(loan$issue_d, "%b-%Y"))
loan$issue_year <- as.character(format(loan$issue_d_1, "%Y"))
loan$issue_mon <- as.character(format(loan$issue_d_1, "%m"))
int.by.year <- by(loan, loan$issue_year, function(x){return(mean(x$int_rate))})
plot(int.by.year)
int.by.mon <- by(loan, loan$issue_mon, function(x){return(mean(x$int_rate))})
plot(int.by.mon)


#######################################################################################################################
#######################################################################################################################
# Overall, this part shows the concise version of the above codes:
loan <- read.csv('loan.csv', stringsAsFactors = FALSE)
loanT <- loan
loan$dti <- ifelse(!is.na(loan$dti_joint), loan$dti_joint, loan$dti)
loan$annual_inc <- ifelse(!is.na(loan$annual_inc_joint), loan$annual_inc_joint, loan$annual_inc)
num.NA <- sort(sapply(loan, function(x) {sum(is.na(x))}), decreasing=TRUE)
remain.col <- names(num.NA)[which(num.NA <= 0.8 * dim(loan)[1])]
loan <- loan[, remain.col]
loan$home_ownership <- ifelse(loan$home_ownership %in% c('ANY', 'NONE', 'OTHER'), 'OTHER',
                              loan$home_ownership)
int_state <- by(loan, loan$addr_state, function(x) {
  return(mean(x$int_rate))
})
loan$state_mean_int <-
  ifelse(loan$addr_state %in% names(int_state)[which(int_state <=
                                                       quantile(int_state, 0.25))], 'low',
         ifelse(loan$addr_state %in% names(int_state)[which(int_state <=
                                                              quantile(int_state, 0.5))],'lowmedium',
                ifelse(loan$addr_state %in% names(int_state)[which(int_state <= quantile(int_state, 0.75))], 
                       'mediumhigh', 'high')))
loan$tot_cur_bal[which(is.na(loan$tot_cur_bal))] <- median(loan$tot_cur_bal, na.rm = T)
loan$total_acc[which(is.na(loan$total_acc))] <- median(loan$total_acc, na.rm = T)
loan$open_acc[which(is.na(loan$open_acc))] <- median(loan$open_acc, na.rm = T)
loan$annual_inc[which(is.na(loan$annual_inc))] <- median(loan$annual_inc, na.rm = T)
#######################################################################################################################
#######################################################################################################################
