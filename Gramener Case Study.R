library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(corrplot)

loan_data <- read.csv("loan.csv", stringsAsFactors =  FALSE, header =  TRUE)

## understanding the attributes 
str(loan_data)
class(loan_data)

############ DATA CLEANING ####################

## checking if there is any duplicate rows in the primary key
loan_data$id<- unique(loan_data$id)

## removing columns where all the rows have NA values 
loan_data <- loan_data[, colSums(is.na(loan_data)) != nrow(loan_data)]

## checking the number of enteries with loan status as current 
length(which(loan_data$loan_status == "Current"))

## as the number of enteries for current loan status is very less compared to total number of enteries, 
## so we will exlcude current loan status for our analysis as it will have least impact on overall analysis.

loan_current_excluded <- loan_data[(which(loan_data$loan_status != "Current")), ]

##further cleaning the data and removing columns which are not useful for analysis. 
loan_current_excluded <- subset(loan_current_excluded,select=-c(emp_title,issue_d,url,desc,title,zip_code,addr_state,initial_list_status,
                                                    out_prncp,out_prncp_inv,total_rec_late_fee,recoveries,collection_recovery_fee,
                                                    last_pymnt_d,last_pymnt_amnt,next_pymnt_d,last_credit_pull_d,
                                                    collections_12_mths_ex_med,mths_since_last_record,pymnt_plan,
                                                    application_type,delinq_amnt,acc_now_delinq,policy_code,
                                                    tax_liens,chargeoff_within_12_mths,revol_util,earliest_cr_line))

### removing the % symbol from interest rate column and converting it into numeric
loan_current_excluded$int_rate <- str_replace_all(loan_current_excluded$int_rate, "%","")
loan_current_excluded$int_rate <- as.numeric(loan_current_excluded$int_rate)
typeof(loan_current_excluded$int_rate)

##### creating a csv file for tableau ########
write.csv(loan_current_excluded, file = "Loan Clean Data.csv")

################# CORRELATIONS  ##################

## making a duplicate data frame which we will use for finding correlatons 
loan_correlation_data<- loan_current_excluded

## converting the columns into numeric values to find correlations. 
loan_correlation_data$loan_status<-as.numeric(loan_correlation_data$loan_status)
typeof(loan_correlation_data$loan_status)
loan_correlation_data$term<-as.numeric(loan_correlation_data$term)
loan_correlation_data$grade<-as.numeric(loan_correlation_data$grade)
loan_correlation_data$sub_grade<-as.numeric(loan_correlation_data$sub_grade)
loan_correlation_data$home_ownership<-as.numeric(loan_correlation_data$home_ownership)
loan_correlation_data$verification_status<-as.numeric(loan_correlation_data$verification_status)
loan_correlation_data$purpose<-as.numeric(loan_correlation_data$purpose)
loan_correlation_data$emp_length<-as.numeric(loan_correlation_data$emp_length)

### including an extra column NA to remove question marks from correlations plot
corr_plot_loan <- cor(cbind(loan_correlation_data, NA))
corrplot(corr_plot_loan, method = "color", type = "upper", na.label = NA, title = "Correlation plot")

## Plotting a graph between number of loans taken vs interest rates on which loan was taken
hist(loan_current_excluded$int_rate, xlab = "Interest Rate", 
     main = "Frequency of loan taken vs interest rate", col = "pink")


################  BIVARIATE aNALYSIS ##########################
### Plotting a graph between loan status and other variables. 
## This will give us basic idea of how the loan status varies with different variable provided to us for analysis. 

#Loan status vs loan term 
ggplot(loan_current_excluded,aes(term,fill=loan_current_excluded$loan_status))+geom_bar(position = 'fill')

#Loan status vs grade 
ggplot(loan_current_excluded,aes(grade,fill=loan_current_excluded$loan_status))+geom_bar(position = 'fill')

#Loan status vs sub grade 
ggplot(loan_current_excluded,aes(sub_grade,fill=loan_current_excluded$loan_status))+geom_bar(position = 'fill')

#Loan status vs employee length  
ggplot(loan_current_excluded,aes(emp_length,fill=loan_current_excluded$loan_status))+geom_bar(position = 'fill')

#Loan status vs int_rate_bucket 
ggplot(loan_current_excluded,aes(int_rate_Bucket,fill=loan_current_excluded$loan_status))+geom_bar(position = 'fill')

#Loan status vs home_ownership  
ggplot(loan_current_excluded,aes(home_ownership,fill=loan_current_excluded$loan_status))+geom_bar(position = 'fill')

#Loan status vs verification status 
ggplot(loan_current_excluded,aes(verification_status,fill=loan_current_excluded$loan_status))+geom_bar(position = 'fill')

#Loan status vs purpose 
ggplot(loan_current_excluded,aes(purpose,fill=loan_current_excluded$loan_status))+geom_bar(position = 'fill')

#Loan status vs delinq_2yrs 
ggplot(loan_current_excluded,aes(as.factor(delinq_2yrs),fill=loan_current_excluded$loan_status))+geom_bar(position = 'fill')

#Loan status vs inq_last_6mths 
ggplot(loan_current_excluded,aes(as.factor(inq_last_6mths),fill=loan_current_excluded$loan_status))+geom_bar(position = 'fill')

#Loan status vs mths_since_last_delinq 
ggplot(loan_current_excluded,aes(as.factor(mths_since_last_delinq),fill=loan_current_excluded$loan_status))+geom_bar(position = 'fill')

#Loan status vs open_acc 
ggplot(loan_current_excluded,aes(as.factor(open_acc),fill=loan_current_excluded$loan_status))+geom_bar(position = 'fill')

#Loan status vs pub_rec 
ggplot(loan_current_excluded,aes(as.factor(pub_rec),fill=loan_current_excluded$loan_status))+geom_bar(position = 'fill')

#Loan status vs pub_rec_bankruptcies 
ggplot(loan_current_excluded,aes(as.factor(pub_rec_bankruptcies),fill=loan_current_excluded$loan_status))+geom_bar(position = 'fill')

#Loan status vs total_acc 
ggplot(loan_current_excluded,aes(as.factor(total_acc),fill=loan_current_excluded$loan_status))+geom_bar(position = 'fill')

#Loan status vs home_ownership
ggplot(loan_current_excluded,aes(loan_amnt_bucket,fill=loan_current_excluded$loan_status))+geom_bar(position = 'fill')

#Loan status vs total_acc 
ggplot(loan_current_excluded,aes(dti_bucket,fill=loan_current_excluded$loan_status))+geom_bar(position = 'fill')

#Loan status vs annual income bucket significant
ggplot(loan_current_excluded,aes(annual_inc_bucket,fill=loan_current_excluded$loan_status))+geom_bar(position = 'fill')

#Loan status vs delequency significant
ggplot(loan_current_excluded,aes(delinquicy,fill=loan_current_excluded$loan_status))+geom_bar(position = 'fill')

#Loan status vs months delequency significant
ggplot(loan_current_excluded,aes(mnth_delinq,fill=loan_current_excluded$loan_status))+geom_bar(position = 'fill')

#Loan status vs open account buckets significant
ggplot(loan_current_excluded,aes(open_acc_bucket,fill=loan_current_excluded$loan_status))+geom_bar(position = 'fill')


#########################  Multivariate Analysis ###########################

### using quartile and summary funtion to create slots for different variables. 
#### We are creating buckets for the ease of plotting the graphs, we tried to plot the graphs without buckets
#### however, the visulisation was not clear so we have created buckets

### slots for interest rate (high medium low)
hist(loan_current_excluded$int_rate)
summary(loan_current_excluded$int_rate)
loan_current_excluded[which(loan_current_excluded$int_rate<8.94),'int_rate_Bucket']<-'Low'
loan_current_excluded[which(loan_current_excluded$int_rate>14.38),'int_rate_Bucket']<-'High'
loan_current_excluded[which(loan_current_excluded$int_rate>=8.94 & loan_current_excluded$int_rate<14.38),'int_rate_Bucket']<-'Medium'

### Slots based on loan amount 
hist(loan_current_excluded$loan_amnt)
summary(loan_current_excluded$loan_amnt)
loan_current_excluded[which(loan_current_excluded$loan_amnt<=5300),'loan_amnt_bucket']<-'Low'
loan_current_excluded[which(loan_current_excluded$loan_amnt>=15000),'loan_amnt_bucket']<-'High'
loan_current_excluded[which(loan_current_excluded$loan_amnt>5300 & loan_current_excluded$loan_amnt<15000),'loan_amnt_bucket']<-'Medium'


##### Slots based on DTI
hist(loan_current_excluded$dti)
summary(loan_current_excluded$dti)
loan_current_excluded[which(loan_current_excluded$dti<=8.13),'dti_bucket']<-'Low'
loan_current_excluded[which(loan_current_excluded$dti>18.56),'dti_bucket']<-'High'
loan_current_excluded[which(loan_current_excluded$dti>8.13 & loan_current_excluded$dti<=18.56),'dti_bucket']<-'Medium'

### slots based on salaries of the loan borrowers 
summary(loan_current_excluded$annual_inc)
quantile(loan_current_excluded$annual_inc,seq(0,1,0.1))
loan_current_excluded[which(loan_current_excluded$annual_inc<=40000),'annual_inc_bucket']<-'Low'
loan_current_excluded[which(loan_current_excluded$annual_inc>82000),'annual_inc_bucket']<-'High'
loan_current_excluded[which(loan_current_excluded$annual_inc>40000 & loan_current_excluded$annual_inc<=82000),'annual_inc_bucket']<-'Medium'

#Bucketing delinq_2yrs
quantile(loan_current_excluded$delinq_2yrs,seq(0,1,0.01))
loan_current_excluded[which(loan_current_excluded$delinq_2yrs>0 & loan_current_excluded$delinq_2yrs<=3),'delinquicy']<-'1-3'
loan_current_excluded[which(loan_current_excluded$delinq_2yrs>3 & loan_current_excluded$delinq_2yrs<=7),'delinquicy']<-'4-7'
loan_current_excluded[which(loan_current_excluded$delinq_2yrs>7),'delinquicy']<-'7+'
loan_current_excluded[which(loan_current_excluded$delinq_2yrs<=0),'delinquicy']<-'0'


#Bucketing number of open accounts into high,medium and low
quantile(loan_current_excluded$open_acc,seq(0,1,0.1))
summary(loan_current_excluded$open_acc)
loan_current_excluded[which(loan_current_excluded$open_acc>=6 & loan_current_excluded$open_acc<=12),'open_acc_bucket']<-'Medium'
loan_current_excluded[which(loan_current_excluded$open_acc>12),'open_acc_bucket']<-'High'
loan_current_excluded[which(loan_current_excluded$open_acc<6),'open_acc_bucket']<-'Low'

######################   MULTIVARIATE ANALYSIS #############################

#creating subset of low annual income people
loan_annual_inc_low<-loan_current_excluded[which(loan_current_excluded$annual_inc_bucket=="Low"),]
#Interest rate vs loan status for low annual income people
ggplot(loan_annual_inc_low,aes(int_rate_Bucket,fill=loan_annual_inc_low$loan_status))+geom_bar(position = 'fill')

#creating subset of high interest rate
loan_low_inc_high_int<-loan_annual_inc_low[which(loan_annual_inc_low$int_rate_Bucket=="High"),]
#Loan amount vs loan status for people with low income and high interest rate
ggplot(loan_low_inc_high_int,aes(loan_amnt_bucket,fill=loan_low_inc_high_int$loan_status))+geom_bar(position = 'fill')

#creating subset of high loan amount people
loan_low_inc_high_int_high_loan<-loan_low_inc_high_int[which(loan_low_inc_high_int$loan_amnt_bucket=="High"),]
#Loan term vs loan status for people with low income and high interest rate and high loan amount
ggplot(loan_low_inc_high_int_high_loan,aes(term,fill=loan_low_inc_high_int_high_loan$loan_status))+geom_bar(position = 'fill')

#creating subset for loan term 60 months   
loan_low_inc_high_int_high_loan_60_mnth<-loan_low_inc_high_int_high_loan[which(loan_low_inc_high_int_high_loan$term==" 60 months"),]
#Dti bucket vs loan status for people with low income and high interest rate and high loan amount and loan term=60 months
ggplot(loan_low_inc_high_int_high_loan_60_mnth,aes(dti_bucket,fill=loan_low_inc_high_int_high_loan_60_mnth$loan_status))+geom_bar(position = 'fill')

#creating subset for loan grade=G  
loan_grade_g<-loan_current_excluded[which(loan_current_excluded$grade=="G"),]
#Annual income vs loan status for people with loan grade G
ggplot(loan_grade_g,aes(annual_inc_bucket,fill=loan_grade_g$loan_status))+geom_bar(position = 'fill')

#loan term vs loan status for people with loan grade G
ggplot(loan_grade_g,aes(term,fill=loan_grade_g$loan_status))+geom_bar(position = 'fill')

#Open accounts bucket vs loan status for people with loan grade G
ggplot(loan_grade_g,aes(open_acc_bucket,fill=loan_grade_g$loan_status))+geom_bar(position = 'fill')

#Loan grade vs loan status for people with low annual income
ggplot(loan_annual_inc_low,aes(grade,fill=loan_annual_inc_low$loan_status))+geom_bar(position = 'fill')

#creating subset of people with loan grade G from low income people
loan_annual_inc_low_grade_g<-loan_annual_inc_low[which(loan_annual_inc_low$grade=="G"),]
#Loan term vs loan status for people with low annual income and loan grade G
ggplot(loan_annual_inc_low_grade_g,aes(term,fill=loan_annual_inc_low_grade_g$loan_status))+geom_bar(position = 'fill')

#creating subset of people with loan term=60 months
loan_annual_inc_low_grade_g_60_mnth<-loan_annual_inc_low_grade_g[which(loan_annual_inc_low_grade_g$term==" 60 months"),]
#Dti bucket vs loan status for people with low annual income and loan grade G and loan term 60 months
ggplot(loan_annual_inc_low_grade_g_60_mnth,aes(dti_bucket,fill=loan_annual_inc_low_grade_g_60_mnth$loan_status))+geom_bar(position = 'fill')

#################################################################################################

write.csv(loan_current_excluded, file = "Loan Output File.csv")
