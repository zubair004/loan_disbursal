rm(list=setdiff(ls(), ''))

library(data.table)
library(dplyr)
library(ggplot2)

setwd('C:/Users/v-zuber.qureshi/Pictures/Saved Pictures/DSIP/DSIP/DS- 42H/DS- Assignment/')

#LOading the data
repayment = data.table(read.csv('repayment_.csv'))
lead = data.table(read.csv('lead_.csv'))
names(lead)[names(lead) %in% 'Cust.Code'] = 'CUST.CODE'

########################## Basic exploration
View(head(lead))
View(head(repayment))

length(unique(lead$Cust.Code))
length(unique(lead$Lead.ID))

length(unique(repayment$CUST.CODE))

length(unique(repayment$LOAN.PROVIDER.RATES))#20
unique(repayment$LOAN.PROVIDER.RATES)

table(lead$Approved.)

#extracting common data from both files
length(intersect(lead$CUST.CODE, repayment$CUST.CODE)) #1159

tmp = intersect(lead$CUST.CODE, repayment$CUST.CODE)

#lead data
lead_CD = subset(lead, lead$CUST.CODE %in% tmp)

#repayment data of borrowers which are present in lead_ file 
repayment_CD = subset(repayment, repayment$CUST.CODE %in% tmp) 

rm(lead, repayment)

# ############################ Concern

# For your reference I have mentioned two CUST.CODE who are not approved but present in repayment data. 9200166808, 9200112854, 9200184530
# table(lead_CD$Approved.)#432 client whos loan is not approved also present in repayment file.
# not_approved_cl = as.character(lead_CD$CUST.CODE[lead_CD$Approved.== 0])
# 
# #details of not approved client in repayment file
# not_approved = subset(repayment, repayment$CUST.CODE %in% not_approved_cl) 

#Objective and understanding
# I have used only repayment data to build the clustering model for predicting the amount of loan to be disbursed to each new lead. 
# I understood that firm doesnt follow the traditional method of EMI's to recover the amount,
# -but it recovers fixed percentage on each transations done after the loan is given.

################################ Feature engineering

#TXN.DATE - Extracted month from here to apply the logic ahead.
repayment_CD$month = format(as.Date(repayment_CD$TXN.DATE, '%m/%d/%Y'), '%m')

#This feature gives the total amount recovered from each transaction. 
repayment_CD$Amt_recovered = repayment_CD$TXNAMT*(repayment_CD$LOAN.PROVIDER.RATES/100)

#This gives the total Amount recovered per month by the each borrower.
sum_amt = setNames(aggregate(Amt_recovered ~ CUST.CODE+month, data = repayment_CD, sum, na.action = na.omit),c('CUST.CODE', 'month','Sum_Amt_recovered'))
repayment_CD = merge(repayment_CD, sum_amt, by = c('CUST.CODE','month'))
rm(sum_amt)
# head(repayment_CD)

#This gives monthly transaction
no_monthly_trx = setNames(aggregate(TXNAMT~ CUST.CODE+month, data = repayment_CD, length), c('CUST.CODE', 'month','no_monthly_trx'))
repayment_CD = merge(repayment_CD, no_monthly_trx, by = c('CUST.CODE','month'))
rm(no_monthly_trx)

#Master data set
master = repayment_CD[,.(Avg_recovery_amt = round(mean(Sum_Amt_recovered, na.rm = T)),
                         Avg_int_rate  = round(mean(LOAN.PROVIDER.RATES, na.rm = T),2),
                         Total_txn_amt = sum(TXNAMT, na.rm = T),
                         uniq_months = length(unique(month)),
                         Avg_monthly_trs = round(mean(no_monthly_trx, na.rm = T))
), by = CUST.CODE]

master$loan_yearly = master$Avg_recovery_amt*12

#'Avg_recovery_amt'-mean of monthly recovered amount from the borrower. 
#'Avg_int_rate' - Average interest rate applied.
#'Total_txn_amt' - Total transaction amount by each borrow.
#'uniq_months' - Number of unique months
#'Avg_monthly_trs'- Average txn per month.

#'loan_yearly'** - Average recovery amount per month is multiplied by 12 to find the approximate amount of loan can be given for 1 year tenure.

#MV analysis
sum(is.na(master))
sapply(master,function(x)sum(is.na(x)))
#There is are missing values.

#treating outliers-Applying winsorizing
plot(master$Avg_recovery_amt) 
tmp = quantile(master$Avg_recovery_amt, probs = seq(0,1,.01),names = T) #Found that only 1% clients have large recovery amount.
master$Avg_recovery_amt[master$Avg_recovery_amt > tmp[["98%"]]] = tmp[["98%"]] 
#As this is income column this is possible that few clients will have large value and removing them completely may loose useful pattern, So I treated them as much as high value.

#Avg_int_rate
plot(master$Avg_int_rate)
tmp = quantile(master$Avg_int_rate, probs = seq(0,1,.05),names = T)
master$Avg_int_rate[master$Avg_int_rate>tmp[["95%"]]] = tmp[["95%"]]

#Avg_monthly_trs
plot(master$Avg_monthly_trs)
tmp = quantile(master$Avg_monthly_trs, probs = seq(0,1,.01),names = T)
master$Avg_monthly_trs[master$Avg_monthly_trs > tmp[["99%"]]] = tmp[['99%']]

#Total_txn_amt
plot(master$Total_txn_amt)
tmp = quantile(master$Total_txn_amt, probs = seq(0,1,.01),names = T)
master$Total_txn_amt[master$Total_txn_amt > tmp[["94%"]]] = tmp[['94%']]


plot(master$loan_yearly)
tmp = quantile(master$loan_yearly, probs = seq(0,1,.01),names = T)
master$loan_yearly[master$loan_yearly > tmp[["99%"]]] = tmp[['99%']]

######################### Applying cluster analysis
#As we dont have the target variable I choose unsupervised ML algorithm.

master1 = scale(master[,-1], center = T, scale=T)

master1 = as.data.frame(master1)
summary(master1)

## Finding the optimal value of cluster
r_sq<- rnorm(20)

for (number in 1:20){
  clus <- kmeans(master1, centers = number, nstart = 50)
  r_sq[number]<- clus$betweenss/clus$totss
}

plot(r_sq)

## Based on r_sq plot plot I selected for K = 8
clus8 <- kmeans(master1,  centers = 8, iter.max = 50, nstart = 50)

## Appending the clusters to master data
master_final <-setnames(cbind(master,clus8$cluster),c(names(master),'Clusters'))

########### plotting the clusters
bivariate_numeric_numeric_categorical_plot = function(var1,var2, var_name)
{
  ggplot(master_final,aes(x=var1,y=var2,color=factor(Clusters)))+geom_point() + ggtitle(var_name)
  
}
bivariate_numeric_numeric_categorical_plot(master_final$loan_yearly,master_final$Avg_int_rate, "Interest Vs Yearly loan")

#Aggregating loan based on clusters  
#Approximate loan which can be given to particular cluster person
master_final = master_final[,Mean_loan_for_Prospect := mean(loan_yearly, na.rm = T), by = Clusters]

#Standard deviation: This much amount can be vary from the mean loan.
master_final = master_final[,SD_loan_for_Prospect:= sd(loan_yearly, na.rm = T),by = Clusters]

#plotting the cluster using clus plot- it uses principlpe component analysis internally and plot all the clusters using two PCA
library(cluster) 
clusplot(master1, clus8$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)
#Based on plot 2 PCA explaining the 74.51% of variance of the data.

#save csv file
write.csv(master_final, file = 'master_final.csv',row.names = F)

