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
names(num.NAnew)[(num.NAnew>0.8*dim(loan1)[1])]
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
          which(is.na(loan1$open_acc)), # 29
          which(is.na(loan1$pub_rec)), 
          which(is.na(loan1$total_acc)), # 29
          which(is.na(loan1$acc_now_delinq)))

identical(which(is.na(loan1$tot_coll_amt)),
          which(is.na(loan1$tot_cur_bal)), # 70276
          which(is.na(loan1$total_rev_hi_lim)))
#x=na.omit(loan1$tot_coll_amt)

# missing 29
boxplot(loan1$open_acc)
quantile(loan1$open_acc, c(0.1, 0.25, 0.5, 0.75, 0.9),na.rm = T)
plot(density(loan1$open_acc, na.rm = T))
loan1$open_acc[which(is.na(loan1$open_acc))] <- median(loan1$open_acc, na.rm = T) # 29
# missing 29
boxplot(loan1$total_acc)
quantile(loan1$total_acc, c(0.1, 0.25, 0.5, 0.75, 0.9),na.rm = T)
plot(density(loan1$total_acc, na.rm = T))
plot(density(log(loan1$total_acc), na.rm = T))
loan1$total_acc[which(is.na(loan1$total_acc))] <- median(loan1$total_acc, na.rm = T) # 29
# missing 70276
unique(loan1$tot_cur_bal) 
boxplot(loan1$tot_cur_bal)
quantile(loan1$tot_cur_bal, c(0.1, 0.25, 0.5, 0.75, 0.9),na.rm = T)
plot(density(loan1$tot_cur_bal, na.rm = T))
plot(density(log(loan1$tot_cur_bal), na.rm = T))
loan1$tot_cur_bal[which(is.na(loan1$tot_cur_bal))] <- median(loan1$tot_cur_bal, na.rm = T) # 70276
# missing 70276
unique(loan1$total_rev_hi_lim) 
boxplot(loan1$total_rev_hi_lim)
quantile(loan1$total_rev_hi_lim, c(0.1, 0.25, 0.5, 0.75, 0.9),na.rm = T)
plot(density(loan1$total_rev_hi_lim, na.rm = T))
plot(density(log(loan1$total_rev_hi_lim), na.rm = T))
loan1$total_rev_hi_lim[which(is.na(loan1$total_rev_hi_lim))] <- median(loan1$total_rev_hi_lim, na.rm = T) # 70276
#######################################################################################################################
# Need to find a way to imputate the missing value in these following features
# missing 29
unique(loan1$pub_rec) 
boxplot(loan1$pub_rec)
quantile(loan1$pub_rec, c(0.1, 0.25, 0.5, 0.75, 0.9),na.rm = T)
plot(density(loan1$pub_rec, na.rm = T))
plot(density(log(loan1$pub_rec), na.rm = T))
# missing 29
unique(loan1$delinq_2yrs) 
boxplot(loan1$delinq_2yrs)
quantile(loan1$delinq_2yrs, c(0.1, 0.25, 0.5, 0.75, 0.9),na.rm = T)
plot(density(loan1$delinq_2yrs, na.rm = T))
plot(density(log(loan1$delinq_2yrs), na.rm = T))
# missing 29
unique(loan1$acc_now_delinq) 
boxplot(loan1$acc_now_delinq)
quantile(loan1$acc_now_delinq, c(0.1, 0.25, 0.5, 0.75, 0.9),na.rm = T)
plot(density(loan1$acc_now_delinq, na.rm = T))
plot(density(log(loan1$acc_now_delinq), na.rm = T))
## Consider drop these 29 entries
# missing 70276
unique(loan1$tot_coll_amt) 
boxplot(loan1$tot_coll_amt)
quantile(loan1$tot_coll_amt, c(0.1, 0.25, 0.5, 0.75, 0.9),na.rm = T)
plot(density(loan1$tot_coll_amt, na.rm = T))
plot(density(log(loan1$tot_coll_amt), na.rm = T))
#loan1$tot_coll_amt[which(is.na(loan1$tot_coll_amt))] <- median(loan1$tot_coll_amt, na.rm = T) # 70276
#######################################################################################################################
# Other features' study
#loan1$recoveries
boxplot(loan1$recoveries)
quantile(loan1$recoveries, c(0.1, 0.25, 0.5, 0.75, 0.9),na.rm = T)
plot(density(loan1$recoveries, na.rm = T))
plot(density(log(loan1$recoveries), na.rm = T))

#inq_last_6mths
boxplot(loan1$inq_last_6mths)
quantile(loan1$inq_last_6mths, c(0.1, 0.25, 0.5, 0.75, 0.9),na.rm = T)
plot(density(loan1$inq_last_6mths, na.rm = T))
plot(density(log(loan1$inq_last_6mths), na.rm = T))
#######################################################################################################################
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
int_state <- by(loan1, loan1$addr_state, function(x) {return(mean(x$int_rate))})
#人为grouping###学会！！！！
loan1$state_mean_int <-
  ifelse(loan1$addr_state %in% names(int_state)[which(int_state <= quantile(int_state, 0.25))], 
         'low', ifelse(loan1$addr_state %in% names(int_state)[which(int_state <= quantile(int_state, 0.5))],
                       'lowmedium', ifelse(loan1$addr_state %in% names(int_state)[which(int_state <= quantile(int_state, 0.75))], 
                                           'mediumhigh', 'high')))
sort(table(loan1$state_mean_int))
par(mfrow=c(1,1))
barplot(table(loan1$state_mean_int))

unique(loan1$home_ownership)
loan1$home_ownership <- ifelse(loan1$home_ownership %in% c('ANY', 'NONE', 'OTHER'), 
                              'OTHER',loan1$home_ownership)
table(loan1$home_ownership)
#######################################################################################################################
# Timestamp
library(zoo)
loan1$issue_d_1 <- as.Date(as.yearmon(loan1$issue_d, "%b-%Y"))
loan1$issue_year <- as.character(format(loan1$issue_d_1, "%Y"))
loan1$issue_mon <- as.character(format(loan1$issue_d_1, "%m"))

int.by.year <- by(loan1, loan1$issue_year, function(x){return(mean(x$int_rate))})
plot(int.by.year)

int.by.mon <- by(loan1, loan1$issue_mon, function(x){return(mean(x$int_rate))})
plot(int.by.mon)
#######################################################################################################################
#######################################################################################################################
# Overall, this part shows the concise version of the above codes:
loan0 <- read.csv('loan.csv', stringsAsFactors = FALSE)
loan <- loan0
loan0$dti <- ifelse(!is.na(loan0$dti_joint), loan0$dti_joint, loan0$dti)
loan0$annual_inc <- ifelse(!is.na(loan0$annual_inc_joint), loan0$annual_inc_joint, loan0$annual_inc)
num.NA <- sort(sapply(loan0, function(x) {sum(is.na(x))}), decreasing=TRUE)
remain.col <- names(num.NA)[which(num.NA <= 0.8 * dim(loan0)[1])]
loan1 <- loan0[, remain.col]

loan1$home_ownership <- ifelse(loan1$home_ownership %in% c('ANY', 'NONE', 'OTHER'), 'OTHER',
                              loan1$home_ownership)
int_state <- by(loan1, loan1$addr_state, function(x) {return(mean(x$int_rate))})
loan1$state_mean_int <-
  ifelse(loan1$addr_state %in% names(int_state)[which(int_state <=quantile(int_state, 0.25))], 'low',
         ifelse(loan1$addr_state %in% names(int_state)[which(int_state <=quantile(int_state, 0.5))],'lowmedium',
                ifelse(loan1$addr_state %in% names(int_state)[which(int_state <= quantile(int_state, 0.75))], 
                       'mediumhigh', 'high')))

loan1$tot_cur_bal[which(is.na(loan1$tot_cur_bal))] <- median(loan1$tot_cur_bal, na.rm = T)
loan1$total_acc[which(is.na(loan1$total_acc))] <- median(loan1$total_acc, na.rm = T)
loan1$open_acc[which(is.na(loan1$open_acc))] <- median(loan1$open_acc, na.rm = T)
loan1$annual_inc[which(is.na(loan1$annual_inc))] <- median(loan1$annual_inc, na.rm = T)
#######################################################################################################################
#######################################################################################################################

#######################################################################################################################
# D. Build Model
# Start with Simple Models! Interpretation is often more important than accuracy
# 1, Validate your assumptions (frequently asked)
# 2, Split data into Train/Validate/Test (Industry sometimes train/test)
# 3, Select your model, select your features (understand the pros & cons of each model)
#######################################################################################################################
# Split data into train and test
set.seed(1)
train.ind <- sample(1:dim(loan1)[1], 0.7 * dim(loan1)[1])
train <- loan1[train.ind, ]
dim(train)
test <- loan1[-train.ind, ]
dim(train)
#######################################################################################################################
# E. Model Validation & Iteration
#    Evaluation Metrics 
# 1, Define evaluation metrics
#    MSE, MAE, Weighted MSE, etc
# 2, Compare performance of multiple models
# 3, Tune model for better performance. 
#    Change model
#    Add / delete features, interaction terms
#    Change model parameters
# Features > Model > Parameter , data size -> the large, the better
#######################################################################################################################
# 1) Regression: Linear Regression (lm)
# Metrics: MSE, R Squared, Adjusted R Squared
#######################################################################################################################
# Select features, goal: recall predict int_rate, find out available and valid features
colnames(loan1)
# make sense/available: addr_state, annual_inc, dti, emp_length, grade, home_ownership, inq_last_6mths,
#                       loan_amnt, mths_since_last_delinq, open_acc, pub_rec (most are zeros),
#                       purpose, sub_grade, term, total_acc, total_rev_hi_lim, inq_last_6mths,
#                       acc_now_delinq, tot_coll_amt(too many NA), tot_cur_bal (most NA saved as median)
# nor so sure: funded_amnt, last_credit_pull_d
#loan1$tot_cur_bal
#quantile(loan1$tot_coll_amt, c(0.1, 0.25, 0.5, 0.75, 0.9),na.rm = T)
#454312/dim(loan1)[1]*100
#loan1$mths_since_last_major_derog # 665676
#loan1$mths_since_last_delinq # 454312
#######################################################################################################################
# lm model directly handle categorical data, but will choose one column in it as the base!
# Model1: too many dummies created by addr_state
mod1 <- lm(int_rate ~ addr_state + home_ownership + annual_inc + dti +
             + term + loan_amnt + total_acc + tot_cur_bal + open_acc,
           data = train)
summary(mod1) 
# Residual standard error: 3.86 on 621104 degrees of freedom
# Multiple R-squared:  0.2242,	Adjusted R-squared:  0.2242 
# F-statistic:  2992 on 60 and 621104 DF,  p-value: < 2.2e-16

# The most signif features: addr_stateCO, addr_stateMA, addr_stateME, addr_stateMN, addr_stateMT,
#                           addr_stateND, addr_stateNE, addr_stateNH, addr_stateOH, addr_statePA,
#                           addr_stateRI, addr_stateVT, addr_stateWI, addr_stateWV, 
# home_ownershipOTHER, home_ownershipOWN, home_ownershipRENT, annual_inc, dti, term 60 months,
# loan_amnt,total_acc, tot_cur_bal, open_acc

#######################################################################################################################
train.sub <- train[, c('int_rate', 'state_mean_int', 'home_ownership', 'annual_inc', 'dti',
                       'term', 'loan_amnt', 'total_acc', 'tot_cur_bal', 'open_acc')]
dim(train.sub) # 621165 * 10
num.NA <- sort(sapply(train.sub, function(x) { sum(is.na(x))} ), decreasing = TRUE)

train.sub$tot_cur_bal[which(is.na(train.sub$tot_cur_bal))] <- median(train.sub$tot_cur_bal, na.rm = T)
train.sub$total_acc[which(is.na(train.sub$total_acc))] <- median(train.sub$total_acc, na.rm = T)
train.sub$open_acc[which(is.na(train.sub$open_acc))] <- median(train.sub$open_acc, na.rm = T)
train.sub$annual_inc[which(is.na(train.sub$annual_inc))] <- median(train.sub$annual_inc, na.rm = T)

mod1.1 <- lm(int_rate ~ ., data = train.sub)
summary(mod1.1)
# Residual standard error: 3.862 on 621151 degrees of freedom
# Multiple R-squared:  0.2237,	Adjusted R-squared:  0.2237 
# F-statistic: 1.377e+04 on 13 and 621151 DF,  p-value: < 2.2e-16
alias(mod1)
#Model :
#  int_rate ~ addr_state + home_ownership + annual_inc + dti + +term + 
#loan_amnt + total_acc + tot_cur_bal + open_acc

# See extremely small estimate, e.g., loan_amnt, because of the magnitude, to compare the relative importance of features

# Rows with any NA will be removed.??
#######################################################################################################################
# Extend the training dataset included the dummies created by categorical variables 
train.sub.matrix <- model.matrix( ~., train.sub)
head(train.sub.matrix)
dim(train.sub.matrix)
colnames(train.sub.matrix)
colnames(train.sub)

# Split label and features
x <- train.sub.matrix[, -2] #不考虑第两列
y <- train.sub.matrix[, 2]#只考虑第二列， int_rate

# Calculate by hands !! UNDERSTAND THE EQUATION
# to calculate the XT*X
z=t(x) %*% x
# If there is error, due to only taking matrix as argument
# x <- as.matrix(x)

# note that X dim is n * (p+1), XT*X dim is (p+1) * (p+1)
# inverse
xtxi <- solve(t(x) %*% x)
# beta estimator
xtxi %*% t(x) %*% y
# compare with model fitted coefficient
coef(mod1.1)

#######################################################################################################################
# Standardize
# standardizing won't change the significant of features, but the estimate will change.
train.sub <- train[, c('int_rate', 'state_mean_int', 'home_ownership', 'annual_inc', 'dti',
                       'term', 'loan_amnt', 'total_acc', 'tot_cur_bal', 'open_acc')]
train.sub.scale <- train.sub
colnames(train.sub.scale)
# Scale ONLY numerical
train.sub.scale[, c(4,5,7,8,9,10)] <- scale(train.sub.scale[, c(4,5,7,8,9,10)])

mod2 <- lm(int_rate ~ state_mean_int + home_ownership + annual_inc + dti +
             + term + loan_amnt + total_acc + tot_cur_bal + open_acc,
           data = as.data.frame(train.sub.scale))
summary(mod2)
# Residual standard error: 3.862 on 621151 degrees of freedom
# Multiple R-squared:  0.2237,	Adjusted R-squared:  0.2237 
# F-statistic: 1.377e+04 on 13 and 621151 DF,  p-value: < 2.2e-16

# The most signif features: ALL
#######################################################################################################################
# Consider skewed variables
train$annual_inc_log=log(train$annual_inc+1)

train.sub <- train[, c('int_rate', 'state_mean_int', 'home_ownership', 'annual_inc_log', 'dti',
                       'term', 'loan_amnt', 'total_acc', 'tot_cur_bal', 'open_acc')]
train.sub.scale <- train.sub
colnames(train.sub.scale)
train.sub.scale[, c(4,5,7,8,9,10)] <- scale(train.sub.scale[, c(4,5,7,8,9,10)])

mod3 <- lm(int_rate ~ state_mean_int + home_ownership + annual_inc_log + dti +
             + term + loan_amnt + total_acc + tot_cur_bal + open_acc,
           data = as.data.frame(train.sub.scale))
summary(mod3)
# Residual standard error: 3.843 on 621151 degrees of freedom
# Multiple R-squared:  0.2313,	Adjusted R-squared:  0.2312 
# F-statistic: 1.437e+04 on 13 and 621151 DF,  p-value: < 2.2e-16

# The most signif features: ALL
#######################################################################################################################
# Consider other skewed variables
# make sense/available: addr_state/state_mean_int, annual_inc_log, dti, emp_length, grade, 
#                       home_ownership, inq_last_6mths, loan_amnt, mths_since_last_delinq, open_acc,
#                       purpose, sub_grade, term, total_acc, total_rev_hi_lim, pub_rec (most are zeros),
#                       acc_now_delinq, tot_coll_amt(too many NA), tot_cur_bal (most NA saved as median)


# train$mths_since_last_delinq
# sum(is.na(train$mths_since_last_delinq))
# unique(loan1$dti) 
# boxplot(loan1$dti)
# quantile(loan1$loan_amnt, c(0.1, 0.25, 0.5, 0.75, 0.9),na.rm = T)
# plot(density(loan1$total_acc, na.rm = T))
# plot(density(log(loan1$total_acc), na.rm = T))
# not considered:mths_since_last_delinq,purpose,total_rev_hi_lim,pub_rec,acc_now_delinq,tot_coll_amt

train.sub <- train[, c('int_rate', 'state_mean_int', 'home_ownership', 'annual_inc_log', 'dti',
                       'term', 'loan_amnt', 'total_acc', 'tot_cur_bal', 'open_acc','inq_last_6mths',
                       'emp_length', 'grade','sub_grade')]

train.sub.scale <- train.sub
colnames(train.sub.scale)
train.sub.scale[, c(4,5,7,8,9,10,11)] <- scale(train.sub.scale[, c(4,5,7,8,9,10,11)])

mod4 <- lm(int_rate ~ state_mean_int + home_ownership + annual_inc_log + dti +
             + term + loan_amnt + total_acc + tot_cur_bal + open_acc+
             + emp_length + grade + sub_grade + inq_last_6mths,
           data = as.data.frame(train.sub.scale))
summary(mod4)

# Residual standard error: 0.8713 on 621087 degrees of freedom
# (18 observations deleted due to missingness)
# Multiple R-squared:  0.9605,	Adjusted R-squared:  0.9605 
# F-statistic: 2.558e+05 on 59 and 621087 DF,  p-value: < 2.2e-16

# The less signif features: tot_cur_bal, emp_lengthn/a
# If seeing NA in coefficient, it means almost perfect correlation between features

#######################################################################################################################
# mths_since_last_delinq, pub_rec, acc_now_delinq, tot_coll_amt (too many NA)
sum(is.na(train$total_rev_hi_lim))
train$total_rev_hi_lim[which(is.na(train$total_rev_hi_lim))] <- median(train$total_rev_hi_lim, na.rm = T) 

sum(is.na(train$inq_last_6mths))
boxplot(train$inq_last_6mths)
quantile(loan1$inq_last_6mths, c(0.1, 0.25, 0.5, 0.75, 0.9),na.rm = T)
plot(density(loan1$inq_last_6mths, na.rm = T))
train$inq_last_6mths[which(is.na(train$inq_last_6mths))] <- median(train$inq_last_6mths, na.rm = T) 

train.sub <- train[, c('int_rate', 'state_mean_int', 'home_ownership', 'annual_inc_log', 'dti',
                       'term', 'loan_amnt', 'total_acc', 'tot_cur_bal', 'open_acc','inq_last_6mths',
                       'emp_length', 'grade','sub_grade','purpose','total_rev_hi_lim')]

train.sub.scale <- train.sub
colnames(train.sub.scale)
train.sub.scale[, c(4,5,7,8,9,10,11,16)] <- scale(train.sub.scale[, c(4,5,7,8,9,10,11,16)])

mod5 <- lm(int_rate ~ state_mean_int + home_ownership + annual_inc_log + dti +
             + term + loan_amnt + total_acc + tot_cur_bal + open_acc+
             + emp_length + grade  + sub_grade + inq_last_6mths + 
             + purpose + total_rev_hi_lim,
           data = as.data.frame(train.sub.scale))
summary(mod5)

# Residual standard error: 0.8704 on 621091 degrees of freedom
# Multiple R-squared:  0.9606,	Adjusted R-squared:  0.9606 
# F-statistic: 2.072e+05 on 73 and 621091 DF,  p-value: < 2.2e-16

# The most signif features: 
#######################################################################################################################
# mths_since_last_delinq,tot_coll_amt(too many NA)
sum(is.na(train$acc_now_delinq))
boxplot(train$acc_now_delinq)
quantile(loan1$acc_now_delinq, c(0.1, 0.25, 0.5, 0.75, 0.9),na.rm = T)
plot(density(loan1$acc_now_delinq, na.rm = T))
train$acc_now_delinq[which(is.na(train$acc_now_delinq))] <- median(train$acc_now_delinq, na.rm = T) 

sum(is.na(train$pub_rec))
boxplot(train$pub_rec)
quantile(loan1$pub_rec, c(0.1, 0.25, 0.5, 0.75, 0.9),na.rm = T)
plot(density(loan1$pub_rec, na.rm = T))
train$pub_rec[which(is.na(train$pub_rec))] <- median(train$pub_rec, na.rm = T) 

train.sub <- train[, c('int_rate', 'state_mean_int', 'home_ownership', 'annual_inc_log', 'dti',
                       'term', 'loan_amnt', 'total_acc', 'tot_cur_bal', 'open_acc','inq_last_6mths',
                       'purpose', 'total_rev_hi_lim', 'pub_rec', 'acc_now_delinq',
                       'emp_length', 'grade','sub_grade')]

train.sub.scale <- train.sub
colnames(train.sub.scale)

train.sub.scale[, c(4,5,7,8,9,10,11,13,14,15)] <- scale(train.sub.scale[, c(4,5,7,8,9,10,11,13,14,15)])

mod6 <- lm(int_rate ~ state_mean_int + home_ownership + annual_inc_log + dti +
             + term + loan_amnt + total_acc + tot_cur_bal + open_acc+
             + emp_length + grade  + sub_grade + inq_last_6mths + 
             + purpose + total_rev_hi_lim + pub_rec + acc_now_delinq,
           data = as.data.frame(train.sub.scale))
summary(mod6)

# Residual standard error: 0.8687 on 621089 degrees of freedom
# Multiple R-squared:  0.9607,	Adjusted R-squared:  0.9607 
# F-statistic: 2.025e+05 on 75 and 621089 DF,  p-value: < 2.2e-16

#######################################################################################################################
# Check correlation between variables
library(corrplot)
#'x' must be numeric
correlations <- cor(train[, c('annual_inc_log', 'dti','loan_amnt', 'total_acc', 'tot_cur_bal', 
                              'open_acc','inq_last_6mths', 'total_rev_hi_lim', 'pub_rec', 
                              'acc_now_delinq')]) 
corrplot(correlations, method = "square", tl.cex = 1, type = 'lower')

# loan_amnt ~ annual_inc_log   corr: 0.52050560
# tot_cur_bal ~ annual_inc_log corr: 0.51569607
# open_acc ~ total_acc corr: 0.69525222


# Try not considering "open_acc" and "total_acc" together

# add Regularization if there exist is multi-collinearity 

#######################################################################################################################
#######################################################################################################################
# Overall, this part shows the concise version of the above codes:
set.seed(1)
train.ind <- sample(1:dim(loan1)[1], 0.7 * dim(loan1)[1])
train <- loan1[train.ind, ]
test <- loan1[-train.ind, ]

train$total_rev_hi_lim[which(is.na(train$total_rev_hi_lim))] <- median(train$total_rev_hi_lim, na.rm = T) 
train$inq_last_6mths[which(is.na(train$inq_last_6mths))] <- median(train$inq_last_6mths, na.rm = T) 
train$acc_now_delinq[which(is.na(train$acc_now_delinq))] <- median(train$acc_now_delinq, na.rm = T) 
train$pub_rec[which(is.na(train$pub_rec))] <- median(train$pub_rec, na.rm = T) 

train$annual_inc_log=log(train$annual_inc+1)

train.sub <- train[, c('int_rate', 'state_mean_int', 'home_ownership', 'annual_inc_log', 'dti',
                       'term', 'loan_amnt', 'total_acc', 'tot_cur_bal', 'open_acc','inq_last_6mths',
                       'purpose', 'total_rev_hi_lim', 'pub_rec', 'acc_now_delinq',
                       'emp_length', 'grade','sub_grade')]
train.sub.scale <- train.sub
colnames(train.sub.scale)
train.sub.scale[, c(4,5,7,8,9,10,11,13,14,15)] <- scale(train.sub.scale[, c(4,5,7,8,9,10,11,13,14,15)])

mod_summary <- lm(int_rate ~ state_mean_int + home_ownership + annual_inc_log + dti +
             + term + loan_amnt + total_acc + tot_cur_bal + open_acc+
             + emp_length + grade  + sub_grade + inq_last_6mths + 
             + purpose + total_rev_hi_lim + pub_rec + acc_now_delinq,
           data = as.data.frame(train.sub.scale))
summary(mod_summary)
# Residual standard error: 0.8687 on 621089 degrees of freedom
# Multiple R-squared:  0.9607,	Adjusted R-squared:  0.9607 
# F-statistic: 2.025e+05 on 75 and 621089 DF,  p-value: < 2.2e-16

# THINK OF DELETE OR TRANSFORM "purpose"
mod_summary_1 <- lm(int_rate ~ state_mean_int + home_ownership + annual_inc_log + dti +
                    + term + loan_amnt + total_acc + tot_cur_bal + open_acc+
                    + emp_length + grade  + sub_grade + inq_last_6mths + 
                   + total_rev_hi_lim + pub_rec + acc_now_delinq,
                  data = as.data.frame(train.sub.scale))
summary(mod_summary_1)
# Residual standard error: 0.8695 on 621102 degrees of freedom
# Multiple R-squared:  0.9606,	Adjusted R-squared:  0.9606 
# F-statistic: 2.445e+05 on 62 and 621102 DF,  p-value: < 2.2e-16

#######################################################################################################################
#######################################################################################################################

#######################################################################################################################
# mod3, mod5, mod_summary compare
head(mod3$residuals)
head(mod5$residuals)
head(mod_summary$residuals)
head(mod_summary_1$residuals)

summary(mod3) # of parameter: 13
summary(mod5) # of parameter: 73
summary(mod_summary) # of parameter: 75
summary(mod_summary_1) # of parameter: 62

# cal residual (SSE)
sum(mod3$residuals^2)/(dim(train.sub)[1]-1-13) #df=n-1-p
# 14.76654
sum(mod5$residuals^2)/(dim(train.sub)[1]-1-73) #df=n-1-p
# 0.7576222
sum(mod_summary$residuals^2)/(dim(train.sub)[1]-1-75) #df=n-1-p
# 0.7547266
sum(mod_summary_1$residuals^2)/(dim(train.sub)[1]-1-62) #df=n-1-p
# 0.756033

# use for estimate error term standard deviation sigma
# e.g.:
sqrt(sum(mod3$residuals^2)/(dim(train.sub)[1]-1-13))

# 手动算R^2, F test=SSE/(SSR+SSE)
# e.g: mod3, sigma estimator, there are 13 features in total, not including beta0, 
summary(mod3)
# Residual standard error: 3.843 on 621151 degrees of freedom
# Multiple R-squared:  0.2313,	Adjusted R-squared:  0.2312 
# F-statistic: 1.437e+04 on 13 and 621151 DF,  p-value: < 2.2e-16

# res = actual - fitted
head(mod3$res)
# MSE:
sqrt(sum(mod3$res^2)/(dim(train.sub)[1] - 1-13)) # dim(train.sub)[1] - 14 = 621151 degree of freedom
# 3.842726

# R square: 1 - sum_square_residual / sum_square_total
1 - sum(mod3$res^2)/sum((train.sub$int_rate - mean(train.sub$int_rate))^2)
# 0.2312428

# adjusted R square
1 - (sum(mod3$res^2)/sum((train.sub$int_rate - mean(train.sub$int_rate))^2)) * 
  (dim(train.sub)[1] - 1) /(dim(train.sub)[1] - 13 - 1)
# 0.2312589

# small p, R square adjusted is very similar to R square.

# Evaluate on Test data
# F test score, go to slides.
y <- train.sub$int_rate
sst = sum((y - mean(y))^2) # sum of square total, df = n - 1 = 621164
ssr = sum(mod3$res^2) #  sum of square residual, df = n-1-p = n - 14= 621151
ssm = sum((y - mean(y))^2) - sum(mod3$res^2) # sum of square model, df = 13
Fstats = (ssm)/(13) / (ssr / (dim(train.sub)[1] - 13 -1))
1 - pf(Fstats, 13, (dim(train.sub)[1] - 13 - 1)) # def = p and n-1-p
# 0

# residual = observed - fitted
head(sort(mod3$res))
mod3$res[which.min(mod3$res)]
mod3$res[which.max(mod3$res)]
plot(mod3$fit, mod3$res, xlab = 'Fitted', ylab = 'residual')
#min(mod3$fit)
summary(mod3$fitted.values)
#plot(mod3$fit, train.sub$int_rate -mod3$fit, xlab = 'Fitted', ylab = 'residual')

# eg: mod_summary
head(sort(mod_summary$res))
mod_summary$res[which.min(mod_summary$res)]
mod_summary$res[which.max(mod_summary$res)]
plot(mod_summary$fit, mod_summary$res, xlab = 'Fitted', ylab = 'residual')
#min(mod_summary$fit)

#######################################################################################################################
#######################################################################################################################\
# 对response/Y 求log, 和对feature求log是很常见的！！！！！！！
#  Constrain response y into a specific range, e.g., >0, use log to predict
#  See data points with negative fitted value, 
#  we should not predict negative interest rate
#######################################################################################################################
#######################################################################################################################
# mod3 <- lm(int_rate ~ state_mean_int + home_ownership + annual_inc_log + dti +
#             + term + loan_amnt + total_acc + tot_cur_bal + open_acc,
#           data = as.data.frame(train.sub.scale))
# summary(mod3)

# Residual standard error: 3.843 on 621151 degrees of freedom
# Multiple R-squared:  0.2313,	Adjusted R-squared:  0.2312 
# F-statistic: 1.437e+04 on 13 and 621151 DF,  p-value: < 2.2e-16

# summary(mod3_1$fitted.values)

mod3_1 <- lm(log(int_rate) ~ state_mean_int + home_ownership + annual_inc_log + dti +
               + term + loan_amnt + total_acc + tot_cur_bal + open_acc,
             data = as.data.frame(train.sub.scale))
# fitted or predicted interest rate
summary(exp(mod3_1$fitted.values))
summary(mod3_1)
# Residual standard error: 0.3046 on 621151 degrees of freedom
# Multiple R-squared:  0.2247,	Adjusted R-squared:  0.2247 
# F-statistic: 1.385e+04 on 13 and 621151 DF,  p-value: < 2.2e-16

plot(exp(mod3_1$fit), train.sub$int_rate -exp(mod3_1$fit), xlab = 'Fitted', ylab = 'residual')
#######################################################################################################################
# mod_summary <- lm(int_rate ~ state_mean_int + home_ownership + annual_inc_log + dti +
#                    + term + loan_amnt + total_acc + tot_cur_bal + open_acc+
#                    + emp_length + grade  + sub_grade + inq_last_6mths + 
#                    + purpose + total_rev_hi_lim + pub_rec + acc_now_delinq,
#                  data = as.data.frame(train.sub.scale))
# summary(mod_summary)
# Residual standard error: 0.8687 on 621089 degrees of freedom
# Multiple R-squared:  0.9607,	Adjusted R-squared:  0.9607 
# F-statistic: 2.025e+05 on 75 and 621089 DF,  p-value: < 2.2e-16

mod_summary_2 <- lm(log(int_rate) ~ state_mean_int + home_ownership + annual_inc_log + dti +
                    + term + loan_amnt + total_acc + tot_cur_bal + open_acc+
                    + emp_length + grade  + sub_grade + inq_last_6mths + 
                    + purpose + total_rev_hi_lim + pub_rec + acc_now_delinq,
                  data = as.data.frame(train.sub.scale))
# fitted or predicted interest rate
summary(mod_summary_2)
# Residual standard error: 0.06471 on 621089 degrees of freedom
# Multiple R-squared:  0.965,	Adjusted R-squared:  0.965 
# F-statistic: 2.284e+05 on 75 and 621089 DF,  p-value: < 2.2e-16

summary(exp(mod_summary_2$fitted.values))
plot(exp(mod_summary_2$fit), train.sub$int_rate -exp(mod_summary_2$fit), xlab = 'Fitted', ylab = 'residual')

plot(exp(mod_summary_2$fit))
#######################################################################################################################
#######################################################################################################################\
## including feature that are skewed in distribution (density plot)
# 存在outlier, this feature's coefficient is not accurate!!!!
# 希望distribution变得symmetric一点, 可以直接取log,  前提是不存在<=0; 存在=0，则取log()+1
#######################################################################################################################\
#######################################################################################################################\
plot(density(loan1$annual_inc)) 
plot(density(log(loan1$annual_inc)))

# still large residuals for some data points. Check the reason.
train.sub <- train[, c('int_rate', 'state_mean_int', 'home_ownership', 'annual_inc', 'dti',
                       'term', 'loan_amnt', 'total_acc', 'tot_cur_bal', 'open_acc')]
train.sub.scale <- train.sub
colnames(train.sub.scale)
train.sub.scale[, c(4,5,7,8,9,10)] <- scale(train.sub.scale[, c(4,5,7,8,9,10)])

mod2 <- lm(int_rate ~ state_mean_int + home_ownership + annual_inc + dti +
             + term + loan_amnt + total_acc + tot_cur_bal + open_acc,
           data = as.data.frame(train.sub.scale))
summary(mod2)
# Residual standard error: 3.862 on 621151 degrees of freedom
# Multiple R-squared:  0.2237,	Adjusted R-squared:  0.2237 
# F-statistic: 1.377e+04 on 13 and 621151 DF,  p-value: < 2.2e-16
#######################################################################################################################\
mod2_1 <- lm(log(int_rate) ~ state_mean_int + home_ownership + annual_inc + dti +
               + term + loan_amnt + total_acc + tot_cur_bal + open_acc,
             data = as.data.frame(train.sub.scale))
summary(mod2_1)
# Residual standard error: 0.3062 on 621151 degrees of freedom
# Multiple R-squared:  0.2163,	Adjusted R-squared:  0.2163 
# F-statistic: 1.319e+04 on 13 and 621151 DF,  p-value: < 2.2e-16
pred <- round(exp(predict(mod2_1, train.sub)), 2)
ind <- which(mod2_1$fitted <= log(4.5))
cbind(train.sub[ind, ], pred = pred[ind], 
      res = train.sub[ind, 'int_rate'] - pred[ind])
#######################################################################################################################\
# 对feature取log
mod2_2 <- lm(log(int_rate) ~ state_mean_int + home_ownership + log(annual_inc) + dti +
               + term + loan_amnt + total_acc + tot_cur_bal + open_acc,
             data = train.sub)
summary(mod2_2)
# Residual standard error: 0.3046 on 621151 degrees of freedom
# Multiple R-squared:  0.2247,	Adjusted R-squared:  0.2247 
# F-statistic: 1.385e+04 on 13 and 621151 DF,  p-value: < 2.2e-16
summary(exp(mod2_2$fitted.values))
pred <- round(exp(predict(mod2_2, train.sub)), 2)
ind <- which(mod2_2$fitted <= log(4.5))
cbind(train.sub[ind, ], pred = pred[ind], 
      res = train.sub[ind, 'int_rate'] - pred[ind])
plot(exp(mod2_2$fit), train.sub$int_rate -exp(mod2_2$fit), xlab = 'Fitted', ylab = 'residual')#对feature取log
#######################################################################################################################\
plot(mod2)
# first plot we can check unbiased/biased and homo/hetero of the residual
# Def not having homo, reason is model miss important features.
# second plot to check the normality of the residual. 
# qqplot: for ith percentile data point, find ith percentile in normal distribution.

#######################################################################################################################
#######################################################################################################################
# Regularization
# Glmnet is a package that fits a generalized linear model via penalized maximum likelihood. 
library(glmnet)
# https://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html
# glmnet已经做了standardrization了！！！！！
# glmnet only takes matrix, can use is.data.frame() or is.matrix() to test
# glmnet standardizes every feature, even categorical feature
# http://stackoverflow.com/questions/17887747/how-does-glmnets-standardize-argument-handle-dummy-variables
#######################################################################################################################
#######################################################################################################################
train.sub <- train[, c('int_rate', 'state_mean_int', 'home_ownership', 'annual_inc_log', 'dti',
                       'term', 'loan_amnt', 'total_acc', 'tot_cur_bal', 'open_acc','inq_last_6mths',
                       'purpose', 'total_rev_hi_lim', 'pub_rec', 'acc_now_delinq',
                       'emp_length', 'grade','sub_grade')]

ind = train.sub[, -1] # x: feature, indepent
ind <- model.matrix( ~., ind) #"~.": all columns
# model.matrix 好处:categorical feature -> boolean feature, data frame-> matrix
dep <- train.sub[, 1] # y: respone

# fit linear regression + regularization
#  LASSO
fit1 <- glmnet(x=ind, y=dep,alpha=1) # default is alpha = 1, lasso
plot(fit1, label = T) # label=T  tell which features
colnames(ind)
# plot(fit1)
# x-axis: l1-norm (sum of absolute of all coefficient, except non interception coefficient)
# top x-axis: num of non-zero estimators(beta)
# 最后被push成0的feature会比较重要，随lamda变大 -> feature "grade" is really important

# or plot like this
plot(fit1, xvar='lambda',label=T)
#看每一个feature的含义
vnam=coef(fit1)
# 2 intercept "." is zero,
#每一列代表lambda变化(从左往右),lambda变小
vnam=vnam[-c(1,2),ncol(vnam)]
#见下面例子
#plot(fit1,label = T,xvar='lambda',yaxt='n',xlab='',)
summary(fit1)
#######################################################################################################################
train.sub <- train[, c('int_rate', 'state_mean_int', 'home_ownership', 'annual_inc_log', 'dti',
                       'term', 'loan_amnt', 'total_acc', 'tot_cur_bal', 'open_acc','inq_last_6mths',
                       'purpose', 'total_rev_hi_lim', 'pub_rec', 'acc_now_delinq',
                       'emp_length', 'grade','sub_grade')]
train.sub.scale <- train.sub
colnames(train.sub.scale)
train.sub.scale[, c(4,5,7,8,9,10,11,13,14,15)] <- scale(train.sub.scale[, c(4,5,7,8,9,10,11,13,14,15)])

ind = train.sub.scale[, -1]
ind <- model.matrix( ~., ind)
dep <- train.sub.scale[, 1]
fit <- glmnet(x=ind, y=dep) # default is alpha = 1, lasso
plot(fit, label = T)
par(mfrow = c(1, 2))
# Understand the plot
# The top row indicates the number of nonzero coefficients at the current λ,
# which is the effective degrees of freedom (df) for the lasso.
# y axis is the value of coefficient
# x axis is the sum of absolute value of coefficients (L1 norm), or log(lambda)
plot(fit, label = T)
plot(fit, xvar = "lambda", label = T)

vnat=coef(fit) # why do we see two intercepts, one is from model.matrix, one is default added in glmnet
vnat <- vnat[-c(1,2), ncol(vnat)] # remove the intercept, and get the coefficients at the end of the path
# default is par(mar=c(5.1,4.1,4.1,2.1), bottom, lef, top, right
par(mar = c(5.1,6,4.1,2.1))
par(mfrow = c(1, 1))
plot(fit, xvar = 'lambda', label = T, yaxt='n', ylab = "")
#axis(2, at=vnat,line=-.5,label = colnames(ind)[-1],las = 2, cex.axis=0.5)
print(fit)
# Df is the non zero beta, 
#######################################################################################################################

# saturated model is a model with a parameter for every observation so that the data are fitted exactly.
# Deviance_model = 2*(loglikelihood_saturate_model - loglikelihood_current_model)
# Deviance_null = 2*(loglikelihood_saturate_model - loglikelihood_intercept_only_model)
# Deviance percentage = 1 -  Deviance_model / Deviance_null
# Deviance as R^2
# lambda value

#Deviance_model

coef(fit, s = 1/exp(2)) # s stands for lambda
coef(fit, s = 1/exp(8))
#######################################################################################################################
# How to choose lambda, CROSS-VALIDATION-> cv.glmnet, k-fold validation
# We can choose lambda by checking the picture, Still kinda subjective
# use cross validation to get optimal value of lambda, 
cvfit <- cv.glmnet(ind, dep)
plot(cvfit)

## important
# Two selected lambdas are shown, 
cvfit$lambda.min # value of lambda gives minimal mean cross validated error
cvfit$lambda.1se # most regularized model such that error is within one std err of the minimum
x = coef(cvfit, s = "lambda.min")
coef(cvfit, s = "lambda.1se")

#######################################################################################################################
#######################################################################################################################
# LOGISTIC REGRESSION, CATEGORICAL RESPONSE (Y)
# Goal: Predict Loan Status
#######################################################################################################################
#######################################################################################################################
sort(table(loan1$loan_status))
loan1$loan_status <- gsub('Does not meet the credit policy. Status:',
                         '', loan1$loan_status)
sort(table(loan1$loan_status))

# status in between, hard to predict, thus NOT CONSIDERTION THESE LOAN
# USE SUBSET for future prediction
loan1 <- subset(loan1, !loan_status %in% c('Current', 'Issued')) 
loan1$loan_status_binary <- with(loan1, ifelse(loan_status == 'Fully Paid', 1, 0))

loan1$log_annual_inc <- log(loan1$annual_inc + 1)

loan1$state_mean_int <-
  ifelse(loan1$addr_state %in% names(int_state)[which(int_state <=quantile(int_state, 0.25))], 'low',
         ifelse(loan1$addr_state %in% names(int_state)[which(int_state <=quantile(int_state, 0.5))],'lowmedium',
                ifelse(loan1$addr_state %in% names(int_state)[which(int_state <= quantile(int_state, 0.75))], 
                       'mediumhigh', 'high')))
#split data
train.ind <- sample(1:dim(loan1)[1], 0.7 * dim(loan1)[1])

# update features and response
train.sub <- loan1[train.ind, c('loan_status_binary', 'state_mean_int', 'home_ownership', 'log_annual_inc', 'dti',
                               'term', 'loan_amnt', 'total_acc', 'tot_cur_bal', 'open_acc')]

# use "relevel" for categorical feature, to set base/level, 
# think: 没出现的就是dummy variable里面-1的这个as base
# relevel to take factor数据
train.sub$state_mean_int <- relevel(as.factor(train.sub$state_mean_int), ref = 'low')

# fit Logistic Regression, generate linear regression(glm)
logis.mod <- glm(loan_status_binary ~ ., train.sub, family = 'binomial') # binary classification
summary(logis.mod) # didn't include good performance evaluation

test <-  loan1[-train.ind, c('loan_status_binary', 'state_mean_int', 'home_ownership', 'log_annual_inc', 'dti',
                            'term', 'loan_amnt', 'total_acc', 'tot_cur_bal', 'open_acc')]

#######################################################################################################################
library(pROC)
pred <- predict(logis.mod, test)
head(pred)
plot.roc(test$loan_status_binary, pred, print.auc=TRUE)
# how to know area under roc curve, AUC
# THUS, in order to improve the AUC, we need to add more features in the model!!!


# find the best lambda
train.sub.scale <- train.sub
train.sub.scale[, c(4,5,7,8,9,10)] <- scale(train.sub.scale[, c(4,5,7,8,9,10)])
ind = train.sub.scale[, -1]
ind <- model.matrix( ~., ind)
dep <- train.sub.scale[, 1]
logis.cvfit <- cv.glmnet(ind, dep, family = 'binomial')
plot(logis.cvfit)

#######################################################################################################################
## 推展 面试题
# 1. write a function to realize gradient descent in R 
# (https://www.r-bloggers.com/implementing-the-gradient-descent-algorithm-in-r/)
# https://www.ocf.berkeley.edu/~janastas/stochastic-gradient-descent-in-r.html

# 2. write cross validation in R
# https://www.analyticsvidhya.com/blog/2018/05/improve-model-performance-cross-validation-in-python-r/
# http://www.milanor.net/blog/cross-validation-for-predictive-analytics-using-r/

#把theory转换成code 
#######################################################################################################################

# Hypothesis Testing
# understand t.test
# t.test(x, y = NULL, alternative = c("two.sided", "less", "greater"), 
# mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95, ...)
# Welch t-test (var.equal = FALSE) and student t-test(var.equal = TRUE)
t.test(int_rate ~ term, data = loan) 
#H0: u1(population mean) using sample(WHEN term=36's int_rate) == u2 (population mean) using sample(WHEN term=60's int_rate)

# How to calculate the stats by hand
short_term <- subset(loan, term == ' 36 months')
long_term <- subset(loan, term == ' 60 months')
stderr <- sqrt(var(short_term$int_rate) / dim(short_term)[1] +
                 var(long_term$int_rate) / dim(long_term)[1])
t.score <- (mean(short_term$int_rate) - mean(long_term$int_rate)) / stderr
# df follows complicated formula in slide (could be approximately in t.test).
p.val <- 2 * pt(t.score, df = 467040)

# understand chi-square test
# Check if grade has same distribution in short term and long term loans,
# what's null hypo and alternative hypo here?
round(with(loan, table(term, grade)) / as.numeric(table(loan$term)), 2)
with(loan, chisq.test(grade, term))
#with(loan, table(term, grade))==table(loan$term,loan$grade)###
# p value
1 - pchisq(176070, df=6)

# if in chisq.test there is warning due to 0 cell, causing some expected values is < 5
# try fisher.test() 

# calculate the chi square stats
# see if term and grade have the same distribution? Or compare 2 population variance?
# expected value should be: 
apply(with(loan, table(term, grade)), 1, sum) # row sum
# 36 months  60 months 
#  621125     266254
apply(with(loan, table(term, grade)), 2, sum) # col sum
# A      B      C      D       E      F       G 
# 148202 254535 245860 139542  70705  23046   5489
observed <- with(loan, table(term, grade))
# expected value in cells should be:
num.grade <- apply(observed, 2, sum)
perc.term <- apply(observed, 1, sum)/dim(loan)[1]
expected <- rbind(num.grade * perc.term[1], num.grade * perc.term[2])
rownames(expected) <- c('short term', 'long term')
sum((observed - expected)^2/expected)

# sigma estimator, there are 13 features in total, not including beta0, 
# res = actual - fitted
head(mod2$res)
# MSE:
sqrt(sum(mod2$res^2)/(dim(train.sub)[1] - 14)) # dim(train.sub)[1] - 14 = 621151 degree of freedom

# R square: 1 - sum_square_residual / sum_square_total
1 - sum(mod2$res^2)/sum((train.sub$int_rate - mean(train.sub$int_rate))^2)
# adjusted R square
1 - (sum(mod2$res^2)/sum((train.sub$int_rate - mean(train.sub$int_rate))^2)) * 
  (dim(train.sub)[1] - 1) /(dim(train.sub)[1] - 13 - 1)
# small p, R square adjusted is very similar to R square.

# F test score, go to slides.
y <- train.sub$int_rate
sst = sum((y - mean(y))^2) # sum of square total, df = n - 1 = 621164
ssr = sum(mod2$res^2) #  sum of square residual, df = n-1-p = n - 14= 621151
ssm = sum((y - mean(y))^2) - sum(mod2$res^2) # sum of square model, df = 13
Fstats = (ssm)/(13) / (ssr / (dim(train.sub)[1] - 13 -1))
1 - pf(Fstats, 13, (dim(train.sub)[1] - 13 - 1)) # def = p and n-1-p

# residual = observed - fitted
head(sort(mod2$res))
mod2$res[which.min(mod2$res)]
mod2$res[which.max(mod2$res)]
plot(mod2$fit, mod2$res, xlab = 'Fitted', ylab = 'residual')


# org.data <- read.csv("LCFromWebsite_2007_2011.csv", stringsAsFactors = FALSE) #
# org.data <- read.table("LCFromWebsite_2007_2011.csv", stringsAsFactors = FALSE, 
#                       fill = TRUE, sep = ",", skip = 1, header = T)
