#############################HR Aanalytics###################
### Data Understanding

setwd("D:\\R work\\HR_Analytics")

# Install and Load the required packages
library(InformationValue)
library(ROCR)
library(GGally)
library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)
library(e1071)
library(Rcpp)
library(e1071)
library(caret)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

# Loading 5 files
general_data     <- read.csv("general_data.csv", stringsAsFactors = F)
emp_survey_data  <- read.csv("employee_survey_data.csv", stringsAsFactors = F)
mangr_survey_data<- read.csv("manager_survey_data.csv", stringsAsFactors = F)
in_time_data     <- read.csv("in_time.csv", stringsAsFactors = F)
out_time_data    <- read.csv("out_time.csv", stringsAsFactors = F)

str(general_data)      # 4410 obs. of  24 variables
str(emp_survey_data)   # 4410 obs. of  4 variables
str(mangr_survey_data) # 4410 obs. of  3 variables
str(in_time_data)      # 4410 obs. of  262 variables
str(out_time_data)     # 4410 obs. of  262 variables

# To Collate check unique id in all tables
length(unique(general_data$EmployeeID))       # 4410 EmployeeID is unique
length(unique(emp_survey_data$EmployeeID))    # 4410 EmployeeID is unique
length(unique(mangr_survey_data$EmployeeID))  # 4410 EmployeeID is unique

in_time_new            <- in_time_data
in_time_new            <-  gather(in_time_new, LogIn_Date, LogIn_DateTime, X2015.01.01 :X2015.12.31)
in_time_new$LogIn_Date <- str_replace(in_time_new$LogIn_Date, "X", "")

out_time_new             <- out_time_data
out_time_new             <-  gather(out_time_new, LogOut_Date, LogOut_DateTime, X2015.01.01 :X2015.12.31)
out_time_new$LogOut_Date <- str_replace(out_time_new$LogOut_Date, "X", "")

LogIn_LogOut <- data.frame(cbind(in_time_new$X,in_time_new$LogIn_DateTime,out_time_new$LogOut_DateTime))

LogIn_LogOut$X2 <- parse_date_time(LogIn_LogOut$X2, c("Ymd_HMS"), tz="")
LogIn_LogOut$X3 <- parse_date_time(LogIn_LogOut$X3, c("Ymd_HMS"), tz="")
LogIn_LogOut$X4 <- difftime(LogIn_LogOut$X3, LogIn_LogOut$X2, units = c("hours"))
WorkAvg <- aggregate(LogIn_LogOut$X4, by = list(LogIn_LogOut$X1), mean, na.rm=TRUE)
colnames(WorkAvg) <- c("EmployeeID", "AverageWorkingHours") 
WorkAvg$AverageWorkingHours <- round(WorkAvg$AverageWorkingHours,2)


# Collate the data together in one single file
setdiff(general_data$EmployeeID,WorkAvg$EmployeeID)            # integer(0) Identical EmployeeID across these datasets
setdiff(general_data$EmployeeID,emp_survey_data$EmployeeID)    # integer(0) Identical EmployeeID across these datasets
setdiff(general_data$EmployeeID, mangr_survey_data$EmployeeID) # integer(0) Identical EmployeeID across these datasets

HR_Analysis <- merge(general_data, WorkAvg, by="EmployeeID")
HR_Analysis <- merge(HR_Analysis, emp_survey_data, by="EmployeeID")
HR_Analysis <- merge(HR_Analysis, mangr_survey_data, by="EmployeeID")

sum(is.na(HR_Analysis))
#Less percentage NA so remove NA
HR_Analysis <- na.omit(HR_Analysis) 

str(HR_Analysis)

#Delete the columns having same values

table(HR_Analysis$Over18)
HR_Analysis$Over18<-NULL
table(HR_Analysis$StandardHours)
HR_Analysis$StandardHours<- NULL
table(HR_Analysis$EmployeeCount)
HR_Analysis$EmployeeCount<-NULL

#Converting numericals
HR_Analysis$Education               <-  as.character(HR_Analysis$Education)
HR_Analysis$EnvironmentSatisfaction <-  as.character(HR_Analysis$EnvironmentSatisfaction)
HR_Analysis$JobLevel                <-  as.character(HR_Analysis$JobLevel)
HR_Analysis$JobInvolvement          <-  as.character(HR_Analysis$JobInvolvement)
HR_Analysis$JobSatisfaction         <-  as.character(HR_Analysis$JobSatisfaction)
HR_Analysis$PerformanceRating       <-  as.character(HR_Analysis$PerformanceRating)
HR_Analysis$StockOptionLevel        <-  as.character(HR_Analysis$StockOptionLevel)
HR_Analysis$WorkLifeBalance         <-  as.character(HR_Analysis$WorkLifeBalance)

#Convert to numerics
HR_Analysis$AverageWorkingHours <- as.numeric(HR_Analysis$AverageWorkingHours )
class(HR_Analysis$AverageWorkingHours)

#Remove EmployeeID
HR_Analysis$EmployeeID <-  NULL

# Vector for the categorical values 
catvarnames <- names(Filter(is.character, HR_Analysis))
sapply(HR_Analysis[catvarnames], table)

# Vector for the Continuous variables 
contvarnames <- names(Filter(is.numeric, HR_Analysis))
sapply(HR_Analysis[contvarnames], summary)

# Character to factor convertion
HR_Analysis <- HR_Analysis %>% mutate_if(is.character,as.factor)

str(HR_Analysis)


# Barcharts for categorical features with stacked Employee information
bar_theme1<- theme(axis.text.y = element_text(angle = 90, hjust = 0, vjust = 0.5), 
                   legend.position="none")

plot_grid(ggplot(HR_Analysis, aes(x=factor(EnvironmentSatisfaction),fill=Attrition))+ geom_bar(), 
          ggplot(HR_Analysis, aes(x=factor(JobSatisfaction),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(HR_Analysis, aes(x=factor(WorkLifeBalance),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(HR_Analysis, aes(x=factor(JobInvolvement),fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")   
plot_grid(ggplot(HR_Analysis, aes(x=factor(PerformanceRating),fill=Attrition))+ geom_bar(), 
          ggplot(HR_Analysis, aes(x=factor(BusinessTravel),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(HR_Analysis, aes(x=factor(Department),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(HR_Analysis, aes(x=factor(Education),fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")  
plot_grid(ggplot(HR_Analysis, aes(x=factor(EducationField),fill=Attrition))+ geom_bar(), 
          ggplot(HR_Analysis, aes(x=factor(Gender),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(HR_Analysis, aes(x=factor(JobRole),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(HR_Analysis, aes(x=factor(MaritalStatus),fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")  

box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                  axis.ticks=element_blank(), axis.text=element_blank())

box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")

plot_grid(ggplot(HR_Analysis, aes(Age))+ geom_histogram(binwidth = 10),
          ggplot(HR_Analysis, aes(x="",y=Age))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(HR_Analysis, aes(AverageWorkingHours))+ geom_histogram(binwidth = 20),
          ggplot(HR_Analysis, aes(x="",y=AverageWorkingHours))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(HR_Analysis, aes(DistanceFromHome))+ geom_histogram(),
          ggplot(HR_Analysis, aes(x="",y=DistanceFromHome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 

plot_grid(ggplot(HR_Analysis, aes(MonthlyIncome))+ geom_histogram(),
          ggplot(HR_Analysis, aes(x="",y=MonthlyIncome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 

plot_grid(ggplot(HR_Analysis, aes(NumCompaniesWorked))+ geom_histogram(),
          ggplot(HR_Analysis, aes(x="",y=NumCompaniesWorked))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 

plot_grid(ggplot(HR_Analysis, aes(PercentSalaryHike))+ geom_histogram(),
          ggplot(HR_Analysis, aes(x="",y=PercentSalaryHike))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 

plot_grid(ggplot(HR_Analysis, aes(TrainingTimesLastYear))+ geom_histogram(),
          ggplot(HR_Analysis, aes(x="",y=TrainingTimesLastYear))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 

plot_grid(ggplot(HR_Analysis, aes(YearsAtCompany))+ geom_histogram(),
          ggplot(HR_Analysis, aes(x="",y=YearsAtCompany))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 

plot_grid(ggplot(HR_Analysis, aes(YearsSinceLastPromotion))+ geom_histogram(),
          ggplot(HR_Analysis, aes(x="",y=YearsSinceLastPromotion))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 

# Boxplots of numeric variables relative to HR_Analysis status
plot_grid(ggplot(HR_Analysis, aes(x=Attrition,y=Age, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(HR_Analysis, aes(x=Attrition,y=MonthlyIncome, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(HR_Analysis, aes(x=Attrition,y=PercentSalaryHike, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)


HR_Analysis$extra_working_hrs <- ifelse(HR_Analysis$AverageWorkingHours > 8,"Yes","No")
HR_Analysis_1     <- HR_Analysis %>% group_by(Attrition, extra_working_hrs ) %>% summarise(countb = n())
HR_Analysis$JobTenure <- ifelse(HR_Analysis$NumCompaniesWorked!=0,
                              round(HR_Analysis$TotalWorkingYears/HR_Analysis$NumCompaniesWorked,2),0)


summary(HR_Analysis)

# Correlation between numeric variables
ggpairs(HR_Analysis[, c("Age","MonthlyIncome","PercentSalaryHike")])


# Data Preparation
# De-Duplication not needed
# Bringing the variables in the correct format
HR_Analysis$Attrition <- ifelse(HR_Analysis$Attrition=="Yes",1,0)

# Outlier treatment and imputing missing value
#Boxplot showed no outlier, Nevertheless confirming it also with percentiles
sapply(HR_Analysis[,c("Age","MonthlyIncome","PercentSalaryHike")], 
       function(x) quantile(x,seq(0,1,.01),na.rm = T)) #no outlier

# Missing value
sapply(HR_Analysis, function(x) sum(is.na(x))) 

# Feature standardisation
# Normalising continuous features 
HR_Analysis$Age<- scale(HR_Analysis$Age) 
HR_Analysis$MonthlyIncome<- scale(HR_Analysis$MonthlyIncome) 
HR_Analysis$PercentSalaryHike<- scale(HR_Analysis$PercentSalaryHike) 

# converting target variable telecom from No/Yes character to factorwith levels 0/1 
HR_Analysis$AverageWorkingHours <- ifelse(HR_Analysis$AverageWorkingHours>8,1,0)

# Checking AverageWorkingHours rate of prospect employee
Avghrswork <- sum(HR_Analysis$AverageWorkingHours)/nrow(HR_Analysis)
Avghrswork # 29.65% AverageWorkingHours rate. 



##################################################################################################################
# splitting the data between train and test
#################################################################################################################
set.seed(100)

indices = sample.split(HR_Analysis$Attrition, SplitRatio = 0.7)

train = HR_Analysis[indices,]

test = HR_Analysis[!(indices),]

########################################################################
# Logistic Regression: 

#Initial model
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1)


# Stepwise selection
model_2<- stepAIC(model_1, direction="both")
summary(model_2)

# Removing multicollinearity through VIF check
vif(model_2)

model_3 <-glm(formula = Attrition ~ Age + BusinessTravel + Department + DistanceFromHome + 
                Education + EducationField + Gender + JobLevel + JobRole + 
                MaritalStatus + MonthlyIncome + NumCompaniesWorked + PercentSalaryHike + 
                StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + 
                YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + 
                AverageWorkingHours + EnvironmentSatisfaction + JobSatisfaction + 
                WorkLifeBalance + JobInvolvement + PerformanceRating, family = "binomial", data = train)
summary(model_3)
vif(model_3)

#Remove Education
model_4 <-glm(formula = Attrition ~ Age + BusinessTravel + Department + DistanceFromHome + 
               EducationField + Gender + JobLevel + JobRole + 
                MaritalStatus + MonthlyIncome + NumCompaniesWorked + PercentSalaryHike + 
                StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + 
                YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + 
                AverageWorkingHours + EnvironmentSatisfaction + JobSatisfaction + 
                WorkLifeBalance + JobInvolvement + PerformanceRating, family = "binomial", data = train)
summary(model_4)
vif(model_4)

#Remove Department
model_5 <-glm(formula = Attrition ~ Age + BusinessTravel  + DistanceFromHome + 
                EducationField + Gender + JobLevel + JobRole + 
                MaritalStatus + MonthlyIncome + NumCompaniesWorked + PercentSalaryHike + 
                StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + 
                YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + 
                AverageWorkingHours + EnvironmentSatisfaction + JobSatisfaction + 
                WorkLifeBalance + JobInvolvement + PerformanceRating, family = "binomial", data = train)
summary(model_5)
vif(model_5)

#Remove YearsAtCompany
model_6 <-glm(formula = Attrition ~ Age + BusinessTravel  + DistanceFromHome + 
                EducationField + Gender + JobLevel + JobRole + 
                MaritalStatus + MonthlyIncome + NumCompaniesWorked + PercentSalaryHike + 
                StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                AverageWorkingHours + EnvironmentSatisfaction + JobSatisfaction + 
                WorkLifeBalance + JobInvolvement + PerformanceRating, family = "binomial", data = train)
summary(model_6)
vif(model_6)

#Remove PercentSalaryHike 
model_7 <-glm(formula = Attrition ~ Age + BusinessTravel  + DistanceFromHome + 
                EducationField + Gender + JobLevel + JobRole + 
                MaritalStatus + MonthlyIncome + NumCompaniesWorked + 
                StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                AverageWorkingHours + EnvironmentSatisfaction + JobSatisfaction + 
                WorkLifeBalance + JobInvolvement + PerformanceRating, family = "binomial", data = train)
summary(model_7)
vif(model_7)


#Remove PercentSalaryHike 
model_7 <-glm(formula = Attrition ~ Age + BusinessTravel  + DistanceFromHome + 
                EducationField + Gender + JobLevel + JobRole + 
                MaritalStatus + MonthlyIncome + NumCompaniesWorked + 
                StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                AverageWorkingHours + EnvironmentSatisfaction + JobSatisfaction + 
                WorkLifeBalance + JobInvolvement + PerformanceRating, family = "binomial", data = train)
summary(model_7)
vif(model_7)

#Remove MonthlyIncome 
model_8 <-glm(formula = Attrition ~ Age + BusinessTravel  + DistanceFromHome + 
                EducationField + Gender + JobLevel + JobRole + MaritalStatus +
                NumCompaniesWorked + StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                AverageWorkingHours + EnvironmentSatisfaction + JobSatisfaction + 
                WorkLifeBalance + JobInvolvement + PerformanceRating, family = "binomial", data = train)
summary(model_8)
vif(model_8)


#Remove Gender 
model_9 <-glm(formula = Attrition ~ Age + BusinessTravel  + DistanceFromHome + 
                EducationField +  JobLevel + JobRole + MaritalStatus +
                NumCompaniesWorked + StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + 
                AverageWorkingHours + EnvironmentSatisfaction + JobSatisfaction + 
                WorkLifeBalance + JobInvolvement + PerformanceRating, family = "binomial", data = train)
summary(model_9)
vif(model_9)

#Remove JobLevel 
model_10 <-glm(formula = Attrition ~ Age + BusinessTravel + EducationField + JobLevel + 
                 JobRole + MaritalStatus + NumCompaniesWorked + StockOptionLevel + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + AverageWorkingHours + EnvironmentSatisfaction + 
                 JobSatisfaction + WorkLifeBalance + JobInvolvement + PerformanceRating
               , family = "binomial", data = train)
summary(model_10)
vif(model_10)


#Remove NumCompaniesWorked 
model_11 <-glm(formula = Attrition ~ Age + BusinessTravel + EducationField +JobLevel +   
                 JobRole + MaritalStatus + NumCompaniesWorked + StockOptionLevel + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + AverageWorkingHours + EnvironmentSatisfaction + 
                 JobSatisfaction + WorkLifeBalance + JobInvolvement + PerformanceRating
               , family = "binomial", data = train)
summary(model_11)
vif(model_11)


#Final
model_12 <-glm(formula = Attrition ~ NumCompaniesWorked + YearsSinceLastPromotion + 
                 JobRole + MaritalStatus +  StockOptionLevel +  EducationField + 
                 TotalWorkingYears + TrainingTimesLastYear + BusinessTravel +
                 YearsWithCurrManager + AverageWorkingHours + EnvironmentSatisfaction + 
                 JobSatisfaction + WorkLifeBalance + JobInvolvement + PerformanceRating, 
               family = "binomial", data = train)
summary(model_12)
vif(model_12)

HR_model<- model_12

#######################################################################
# Model Valuation
# Test Data 

#predicted probabilities of Attrition 1 for test data

Prediction_test = predict(HR_model, type = "response", 
                    newdata = test[,-1])


# Let's see the summary 
summary(Prediction_test)

test$prob <- Prediction_test
View(test)

# Let's use the probability cutoff of 50%.
test_pred_Attrition <- factor(ifelse(Prediction_test >= 0.50, "Yes", "No"))
test_actual_Attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))

table(test_actual_Attrition,test_pred_Attrition)

#######################################################################
test_pred_Attrition <- factor(ifelse(Prediction_test >= 0.40, "Yes", "No"))

test_conf <- confusionMatrix(test_pred_Attrition, test_actual_Attrition, positive = "Yes")
test_conf

#########################################################################################
# Let's Choose the cutoff value. 
# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_churn <- factor(ifelse(Prediction_test >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(test_pred_Attrition, test_actual_Attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Summary of test probability
summary(Prediction_test)
s = seq(.01,.80,length=100)
OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<1)]
cutoff

# Let's choose a cutoff value of 0.15 for final model
test_KS_stat <- factor(ifelse(Prediction_test >=0.15, "Yes", "No"))


conf_final <- confusionMatrix(test_pred_Attrition, test_actual_Attrition, positive = "Yes")
acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]
acc #Accuracy 75%
sens
spec

View(test)

##################################################################################################
# KS -statistic - Test Data

test_KS_stat <- ifelse(test_KS_stat=="Yes",1,0)
test_actual_Attrition <- ifelse(test_actual_Attrition=="Yes",1,0)

#on testing  data
test_01 <- prediction(test_KS_stat, test_actual_Attrition)

performance_check <- performance(test_01, "tpr", "fpr")

ks_table_test <- attr(performance_check, "y.values")[[1]] - 
  (attr(performance_check, "x.values")[[1]])

max(ks_table_test)

####################################################################
# Lift & Gain Chart 

# plotting the lift chart

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Attrition_decile = lift(test_actual_Attrition, Prediction_test, groups = 10)


plot(Attrition_decile$Cumlift, type="l", lwd=2, col="red",
     xlim = c(0,10),
     ylim = c(0,4),
     main = "Lift Chart",
     xlab = "Decile",
     ylab = "Lift")
abline(h=1, col="blue")
axis(1, 1:10)
abline(h=0:10, v=0:10, lty=3)

ks_plot(test_actual_Attrition, test_KS_stat) # Gain chart plot
