# Linear Regression Assignment: Car Price Prediction 
# Submitted by: Aditya Vikram Singh
# Date of Submission: 5th August 2018

install.packages("tidyr")
install.packages("dplyr")
install.packages("MASS")
install.packages("car")

library(tidyr)
library(dplyr)
library(MASS) 
library(car)  
library(ggplot2)

setwd("C:/Users/adisin/Desktop/Predictive Analysis/Assignment")

car_data <- read.csv("CarPrice_Assignment.csv")

View(car_data)
str(car_data)

summary(car_data)

colnames(car_data) <- tolower(colnames(car_data))

# check for mising values and duplicate values rows:
car_data[which(duplicated(car_data)), ]

sum(is.na(car_data))

# Separating company and model from car name
car_data <- separate(car_data, carname, c('company', 'model'), 
                     sep = '[[:blank:]]', extra = 'merge', fill = 'right')
# exclude model from analysis
car_data <- car_data[, -c(4)] 

levels(factor(car_data$company))

### correcting misspelt car names

company <- function(name){
  Correct_name <- name
  if(name=='maxda'){
    Correct_name = 'mazda'
  } else if (name=='Nissan') {
    Correct_name = 'nissan'
  } else if (name == 'porcshce') {
    Correct_name = 'porsche'
  } else if (name == 'toyouta') {
    Correct_name = 'toyota'
  } else if (name %in% c('vokswagen', 'vw')){
    Correct_name = 'volkswagen'
  } else if (name=='alfa-romero'){
    Correct_name = 'alfa-romeo'
  }
  return(Correct_name)
}

car_data$company <- sapply(car_data$company, company)

levels(factor(car_data$company))

# Converting into numeric format
levels(factor(car_data$doornumber))

levels(car_data$doornumber) <- c(4, 2)

car_data$doornumber <- as.numeric(levels(car_data$doornumber))[car_data$doornumber]

levels(factor(car_data$cylindernumber))

levels(car_data$cylindernumber) <- c(8, 5, 4, 6, 3, 12, 2)
car_data$cylindernumber <- as.numeric(levels(car_data$cylindernumber))[car_data$cylindernumber]


## dummy variable creation

summary(car_data[which(sapply(car_data, class)=='factor')])

# we have 7 factors types for which we need dummy variables
# Variable with 2 levels: fueltype, aspiration, enginelocation. There are two methods in which we can 
## convert these 8 categorical attributes in numerical format. First is by creating dummy variable separately 
# and secon is my creating a function and using that function with our data to convert values in numerical format.

cols_2 <- c('fueltype', 'aspiration', 'enginelocation')

# helper function for encoding 2 level types giving weightage to popular level type
encode_cols <- function(x){ # popular level types: gas, turbo, front
  if(table(x)[1] > table(x)[2]){
    levels(x) <- c(1,0)  
  } else {
    levels(x) <- c(0,1)
  }
  x <- as.numeric(levels(x))[x]
}
car_data[cols_2] <- sapply(car_data[cols_2], encode_cols)


sapply(car_data[cols_2],table)

# 3-lvl: company, drivewheel
car_data_duplicate <- car_data

# factor columns
factor_cols <- which(sapply(car_data_duplicate, is.factor))
factor_cols

#cars1$company <- NULL # Since car origin is derived from car company 
fact_cols <- c('carbody', 'enginetype', 'fuelsystem') ## MISSED STEPS
car_data_duplicate[,fact_cols] <- sapply(car_data_duplicate[,fact_cols], as.character) ## MISSED STEPS
car_data_duplicate$fuelsystem <- ifelse(car_data_duplicate$fuelsystem %in% c('spdi','4bbl','mfi','spfi'),
                           'others', car_data_duplicate$fuelsystem)
car_data_duplicate$enginetype <- ifelse(car_data_duplicate$enginetype %in% c('dohcv','rotor'), 
                           'others', car_data_duplicate$enginetype)
car_data_duplicate$carbody <- ifelse(car_data_duplicate$carbody %in% c('convertible','hardtop'), 
                        'others', car_data_duplicate$carbody)
car_data_duplicate[,fact_cols] <- sapply(car_data_duplicate[,fact_cols], as.factor)

View(car_data_duplicate)

levels(factor(car_data_duplicate$drivewheel))

dummy_drivewheel <- model.matrix(~drivewheel, data=car_data_duplicate)
dummy_drivewheel <- dummy_drivewheel[,-1]
car_data_duplicate <- cbind(car_data_duplicate[,-which(colnames(car_data_duplicate)=='drivewheel')],dummy_drivewheel)

dummy_carbody <- model.matrix(~carbody, data=car_data_duplicate)
dummy_carbody <- dummy_carbody[,-1]
car_data_duplicate <- cbind(car_data_duplicate[,-which(colnames(car_data_duplicate)=='carbody')],dummy_carbody)

dummy_fuelsystem <- model.matrix(~fuelsystem, data=car_data_duplicate)
dummy_fuelsystem <- dummy_fuelsystem[,-1]
car_data_duplicate <- cbind(car_data_duplicate[,-which(colnames(car_data_duplicate)=='fuelsystem')],dummy_fuelsystem)


dummy_enginetype <- model.matrix(~enginetype, data=car_data_duplicate)
dummy_enginetype <- dummy_enginetype[,-1]
car_data_duplicate <- cbind(car_data_duplicate[,-which(colnames(car_data_duplicate)=='enginetype')],dummy_enginetype)


dummy_company <- model.matrix(~company, data=car_data_duplicate)
dummy_company <- dummy_company[,-1]
car_data_duplicate <- cbind(car_data_duplicate[,-which(colnames(car_data_duplicate)=='company')],dummy_company)

#Modeling 

set.seed(1000)

# sample size
ntrain <- sample(1:nrow(car_data_duplicate), 0.7 * nrow(car_data_duplicate))
# creating training and testing data sets
training_data_indices <- car_data_duplicate$car_id[ntrain]
testing_data_indices <- car_data_duplicate$car_id[-ntrain]
training_data <- car_data_duplicate[ntrain,]
testing_data <- car_data_duplicate[-ntrain,]

View(testing_data)
View(training_data)

# model building: model1 - consisting of all variables
model <- lm(price ~ ., data = training_data) #R-squared:  0.8798
summary(model)
## Using StepAIC to remove irrelevant/insignificnt variables
step <- stepAIC(model, direction = 'both')
step

## creating second model based on stepAIC selection
levels(factor(training_data))
model2 <- lm(price ~ fueltype + aspiration + enginelocation + wheelbase + 
               carlength + carwidth + carheight + curbweight + cylindernumber + 
               enginesize + boreratio + stroke + compressionratio + horsepower + 
               peakrpm + drivewheelrwd + carbodyothers + carbodysedan + 
               carbodywagon + fuelsystemmpfi + fuelsystemothers + enginetypeohc + 
               enginetypeohcf + companyaudi + companybmw + companybuick + 
               companydodge + companyhonda + companyjaguar + companymitsubishi + 
               companypeugeot + companyplymouth + companyporsche + companysaab + 
               companyvolvo, data = training_data) 
summary(model2) #Adjusted R-squared:  0.9528 

## check for multicollinearity in order to determine which variable to drop
vif(model2)

# removing fueltype as it has high VIF and high p value
model3 <- lm(price ~ aspiration + enginelocation + wheelbase + 
               carlength + carwidth + carheight + curbweight + cylindernumber + 
               enginesize + boreratio + stroke + compressionratio + horsepower + 
               peakrpm + drivewheelrwd + carbodyothers + carbodysedan + 
               carbodywagon + fuelsystemmpfi + fuelsystemothers + enginetypeohc + 
               enginetypeohcf + companyaudi + companybmw + companybuick + 
               companydodge + companyhonda + companyjaguar + companymitsubishi + 
               companypeugeot + companyplymouth + companyporsche + companysaab + 
               companyvolvo, data = training_data) #Adjusted R-squared:  0.9517

summary(model3)
vif(model3)
# removing horsepower: vif = 29.648429 and p-value=0.159407 > 0.05
model4 <- lm(price ~ aspiration + enginelocation + wheelbase + 
               carlength + carwidth + carheight + curbweight + cylindernumber + 
               enginesize + boreratio + stroke + compressionratio +  
               peakrpm + drivewheelrwd + carbodyothers + carbodysedan + 
               carbodywagon + fuelsystemmpfi + fuelsystemothers + enginetypeohc + 
               enginetypeohcf + companyaudi + companybmw + companybuick + 
               companydodge + companyhonda + companyjaguar + companymitsubishi + 
               companypeugeot + companyplymouth + companyporsche + companysaab + 
               companyvolvo, data = training_data)  #Adjusted R-squared:  0.9512 
summary(model4)
vif(model4)

# remove carlenght:: vif = 20.10 p-value= 0.22774 > 0.05
model5 <-lm(price ~ aspiration + enginelocation + wheelbase + 
               carwidth + carheight + curbweight + cylindernumber + 
              enginesize + boreratio + stroke + compressionratio +  
              peakrpm + drivewheelrwd + carbodyothers + carbodysedan + 
              carbodywagon + fuelsystemmpfi + fuelsystemothers + enginetypeohc + 
              enginetypeohcf + companyaudi + companybmw + companybuick + 
              companydodge + companyhonda + companyjaguar + companymitsubishi + 
              companypeugeot + companyplymouth + companyporsche + companysaab + 
              companyvolvo, data = training_data)  #Adjusted R-squared:  0.951
summary(model5)
vif(model5)

# remove enginesize:  vif = 94.044 p-value=0.15 > 0.05
model6 <-lm(price ~ aspiration + enginelocation + wheelbase + 
              carwidth + carheight + curbweight + cylindernumber + 
               boreratio + stroke + compressionratio +  
              peakrpm + drivewheelrwd + carbodyothers + carbodysedan + 
              carbodywagon + fuelsystemmpfi + fuelsystemothers + enginetypeohc + 
              enginetypeohcf + companyaudi + companybmw + companybuick + 
              companydodge + companyhonda + companyjaguar + companymitsubishi + 
              companypeugeot + companyplymouth + companyporsche + companysaab + 
              companyvolvo, data = training_data) #Adjusted R-squared:  0.931
summary(model6)
vif(model6)

# remove wheelbase : vif = 12.855792, p-value=0.210844 > 0.05
model7 <-lm(price ~ aspiration + enginelocation + 
              carwidth + carheight + curbweight + cylindernumber + 
              boreratio + stroke + compressionratio +  
              peakrpm + drivewheelrwd + carbodyothers + carbodysedan + 
              carbodywagon + fuelsystemmpfi + fuelsystemothers + enginetypeohc + 
              enginetypeohcf + companyaudi + companybmw + companybuick + 
              companydodge + companyhonda + companyjaguar + companymitsubishi + 
              companypeugeot + companyplymouth + companyporsche + companysaab + 
              companyvolvo, data = training_data) #Adjusted R-squared:  0.9306 
summary(model7)
vif(model7)

# remove curbweight: VIF  = 18.50886
model8 <-lm(price ~ aspiration + enginelocation + 
              carwidth + carheight  + cylindernumber + 
              boreratio + stroke + compressionratio +  
              peakrpm + drivewheelrwd + carbodyothers + carbodysedan + 
              carbodywagon + fuelsystemmpfi + fuelsystemothers + enginetypeohc + 
              enginetypeohcf + companyaudi + companybmw + companybuick + 
              companydodge + companyhonda + companyjaguar + companymitsubishi + 
              companypeugeot + companyplymouth + companyporsche + companysaab + 
              companyvolvo, data = training_data) #Adjusted R-squared:  0.924
summary(model8)
vif(model8)

# remove boreratio : vif = 7.038731, p-value=0.197259 
model9 <-lm(price ~ aspiration + enginelocation + 
              carwidth + carheight  + cylindernumber + 
                stroke + compressionratio +  
              peakrpm + drivewheelrwd + carbodyothers + carbodysedan + 
              carbodywagon + fuelsystemmpfi + fuelsystemothers + enginetypeohc + 
              enginetypeohcf + companyaudi + companybmw + companybuick + 
              companydodge + companyhonda + companyjaguar + companymitsubishi + 
              companypeugeot + companyplymouth + companyporsche + companysaab + 
              companyvolvo, data = training_data) #Adjusted R-squared: 0.9235 
summary(model9)
vif(model9)

# remove enginelocation: VIF =  6.512390  p-value=0.003725
model10 <-lm(price ~ aspiration  + carwidth + carheight  + cylindernumber + 
               stroke + compressionratio +  
               peakrpm + drivewheelrwd + carbodyothers + carbodysedan + 
               carbodywagon + fuelsystemmpfi + fuelsystemothers + enginetypeohc + 
               enginetypeohcf + companyaudi + companybmw + companybuick + 
               companydodge + companyhonda + companyjaguar + companymitsubishi + 
               companypeugeot + companyplymouth + companyporsche + companysaab + 
               companyvolvo, data = training_data) #Adjusted R-squared: 0.9183 
summary(model10)
vif(model10)

# remove carbodywagon:  p-value=0.91177 ; vif:2.578639
model11 <-lm(price ~ aspiration  + carwidth + carheight  + cylindernumber + 
               stroke + compressionratio +  
               peakrpm + drivewheelrwd + carbodyothers + carbodysedan + 
                fuelsystemmpfi + fuelsystemothers + enginetypeohc + 
               enginetypeohcf + companyaudi + companybmw + companybuick + 
               companydodge + companyhonda + companyjaguar + companymitsubishi + 
               companypeugeot + companyplymouth + companyporsche + companysaab + 
               companyvolvo, data = training_data) #Adjusted R-squared: 0.919 
summary(model11)
vif(model11)

# remove carwidth:  p-value= 2.58e-08 *** and vif:4.901702
model12 <-lm(price ~ aspiration  + carheight  + cylindernumber + 
               stroke + compressionratio +  
               peakrpm + drivewheelrwd + carbodyothers + carbodysedan + 
               fuelsystemmpfi + fuelsystemothers + enginetypeohc + 
               enginetypeohcf + companyaudi + companybmw + companybuick + 
               companydodge + companyhonda + companyjaguar + companymitsubishi + 
               companypeugeot + companyplymouth + companyporsche + companysaab + 
               companyvolvo, data = training_data) #Adjusted R-squared: 0.895 
summary(model12)
vif(model12)

# remove peakrpm:  p-value= 0.689569 and vif:2.549455
model13 <-lm(price ~ aspiration  + carheight  + cylindernumber + 
               stroke + compressionratio +  drivewheelrwd + carbodyothers + carbodysedan + 
               fuelsystemmpfi + fuelsystemothers + enginetypeohc + 
               enginetypeohcf + companyaudi + companybmw + companybuick + 
               companydodge + companyhonda + companyjaguar + companymitsubishi + 
               companypeugeot + companyplymouth + companyporsche + companysaab + 
               companyvolvo, data = training_data) #Adjusted R-squared:  0.8958 
summary(model13)
vif(model13)
#Removing companyhonda: p-value: 0.683876 and vif:1.379061
model14 <-lm(price ~ aspiration  + carheight  + cylindernumber + 
               stroke + compressionratio +  drivewheelrwd + carbodyothers + carbodysedan + 
               fuelsystemmpfi + fuelsystemothers + enginetypeohc + 
               enginetypeohcf + companyaudi + companybmw + companybuick + 
               companydodge  + companyjaguar + companymitsubishi + 
               companypeugeot + companyplymouth + companyporsche + companysaab + 
               companyvolvo, data = training_data) #Adjusted R-squared:  0.8965 
summary(model14)
vif(model14)

# removing enginetypeohcf:  p-value= 0.545499 and vif:3.005517
model15 <-lm(price ~ aspiration  + carheight  + cylindernumber + 
               stroke + compressionratio +  drivewheelrwd + carbodyothers + carbodysedan + 
               fuelsystemmpfi + fuelsystemothers + enginetypeohc + companyaudi + companybmw + companybuick + 
               companydodge +  companyjaguar + companymitsubishi + 
               companypeugeot + companyplymouth + companyporsche + companysaab + 
               companyvolvo, data = training_data) #Adjusted R-squared:  0.897 
summary(model15)
vif(model15)


# remove fuelsystemmpfi:  p-value= 0.474477 and vif:2.787078
model16 <-lm(price ~ aspiration  + carheight  + cylindernumber + 
               stroke + compressionratio +  drivewheelrwd + carbodyothers + carbodysedan + 
                + fuelsystemothers + companyaudi + companybmw + companybuick + 
               companydodge +  companyjaguar + companymitsubishi + 
               companypeugeot + companyplymouth + companyporsche + companysaab + 
               companyvolvo, data = training_data) #Adjusted R-squared:  0.8896 
summary(model16)
vif(model16)

# remove carheight:  p-value= 0.416777 and vif:2.265271
model17 <-lm(price ~ aspiration  + cylindernumber + stroke + compressionratio +  drivewheelrwd + carbodyothers + carbodysedan + 
               + fuelsystemothers  + companyaudi + companybmw + companybuick + 
               companydodge +  companyjaguar + companymitsubishi + 
               companypeugeot + companyplymouth + companyporsche + companysaab + 
               companyvolvo, data = training_data) #Adjusted R-squared:  0.8898 
summary(model17)
vif(model17)


# remove carbodysedan:  p-value= 0.357283 and vif:1.390033
model18 <-lm(price ~ aspiration    + cylindernumber + 
               stroke + compressionratio +  drivewheelrwd + carbodyothers  + 
               + fuelsystemothers  + companyaudi + companybmw + companybuick + 
               companydodge +  companyjaguar + companymitsubishi + 
               companypeugeot + companyplymouth + companyporsche + companysaab + 
               companyvolvo, data = training_data) #Adjusted R-squared:  0.8899 
summary(model18)
vif(model18)

# remove companydodge:  p-value= 0.207090 and vif:1.136164
model19 <-lm(price ~ aspiration    + cylindernumber + 
               stroke + compressionratio +  drivewheelrwd + carbodyothers  + 
               + fuelsystemothers   + companyaudi + companybmw + companybuick + 
                 companyjaguar + companymitsubishi + 
               companypeugeot + companyplymouth + companyporsche + companysaab + 
               companyvolvo, data = training_data) #Adjusted R-squared:  0.8894 
summary(model19)
vif(model19)

# remove compressionratio:  p-value= 0.230620 and vif:1.752865
model20 <-lm(price ~ aspiration    + cylindernumber + 
               stroke +  drivewheelrwd + carbodyothers  + 
               + fuelsystemothers   + companyaudi + companybmw + companybuick + 
               companyjaguar + companymitsubishi + 
               companypeugeot + companyplymouth + companyporsche + companysaab + 
               companyvolvo, data = training_data) #Adjusted R-squared:  0.889 
summary(model20)
vif(model20)

# remove stroke :  p-value= 0.144409 and vif:1.434614
model21 <-lm(price ~ aspiration    + cylindernumber + drivewheelrwd + carbodyothers  + 
               + fuelsystemothers   + companyaudi + companybmw + companybuick + 
               companyjaguar + companymitsubishi + companypeugeot + companyplymouth + companyporsche + companysaab + 
               companyvolvo, data = training_data) #Adjusted R-squared:  0.888 
summary(model21)
vif(model21)

# remove companyplymouth :  p-value= 0.107907 and vif:1.065109
model22 <-lm(price ~ aspiration    + cylindernumber + drivewheelrwd + carbodyothers  + 
               + fuelsystemothers   + companyaudi + companybmw + companybuick + 
               companyjaguar + companymitsubishi + companypeugeot + companyporsche + companysaab + 
               companyvolvo, data = training_data) #Adjusted R-squared:  0.8866 
summary(model22)
vif(model22)

# remove companymitsubishi :  p-value= 0.16451 and vif:1.608707
model23 <-lm(price ~ aspiration    + cylindernumber + drivewheelrwd + carbodyothers  + 
               + fuelsystemothers   + companyaudi + companybmw + companybuick + 
               companyjaguar  + companypeugeot + companyporsche + companysaab + 
               companyvolvo, data = training_data) #Adjusted R-squared:  0.8857 
summary(model23)
vif(model23)

# remove fuelsystemothers :  p-value= 0.012787 and vif:1.404309
model24 <-lm(price ~ aspiration    + cylindernumber + drivewheelrwd + carbodyothers  + 
                + companyaudi + companybmw + companybuick + 
               companyjaguar  + companypeugeot + companyporsche + companysaab + 
               companyvolvo, data = training_data) #Adjusted R-squared:  0.881 
summary(model24)
vif(model24)

# remove companypeugeot :  p-value= 0.057778 and vif:1.570906
model25 <-lm(price ~ aspiration    + cylindernumber + drivewheelrwd + carbodyothers  + 
               + companyaudi + companybmw + companybuick + 
               companyjaguar  + companyporsche + companysaab + 
               companyvolvo, data = training_data) #Adjusted R-squared:  0.8786 
summary(model25)
vif(model25)


# remove carbodyothers :  p-value= 0.026675 and vif:1.639247
model26 <-lm(price ~ aspiration    + cylindernumber + drivewheelrwd  + 
               + companyaudi + companybmw + companybuick + 
               companyjaguar  + companyporsche + companysaab + 
               companyvolvo, data = training_data) #Adjusted R-squared:  0.8749 
summary(model26)
vif(model26)

# remove companyvolvo :  p-value= 0.016141 and vif:1.331426
model27 <-lm(price ~ aspiration    + cylindernumber + drivewheelrwd  + 
               + companyaudi + companybmw + companybuick + 
               companyjaguar  + companyporsche + companysaab , data = training_data) #Adjusted R-squared:  0.8702 
summary(model27)
vif(model27)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-8488.9 -1606.8  -144.6  1804.0 11721.1 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)     -1355.6     1618.7  -0.837 0.403837    
#aspiration      -2628.0      668.0  -3.934 0.000134 ***
#  cylindernumber   3006.6      371.5   8.092 3.25e-13 ***
#  drivewheelrwd    5781.8      603.9   9.573  < 2e-16 ***
#  companyaudi      6460.0     1299.0   4.973 2.00e-06 ***
#  companybmw       9756.3     1430.2   6.822 2.88e-10 ***
#  companybuick    10749.1     1500.5   7.164 4.84e-11 ***
#  companyjaguar   14062.5     2265.5   6.207 6.39e-09 ***
#  companyporsche  13066.2     1647.9   7.929 7.93e-13 ***
#  companysaab      6514.7     1751.1   3.720 0.000292 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 2968 on 133 degrees of freedom
#Multiple R-squared:  0.8785,	Adjusted R-squared:  0.8702 
#F-statistic: 106.8 on 9 and 133 DF,  p-value: < 2.2e-16
############################################################################################################

## Using model27 for predicting prices  

predicted_price <- predict(model27, testing_data[,-which(colnames(testing_data)=='price')])
testing_data$test_price <- predicted_price

# Now, we need to test the r square between actual and predicted sales. 
r <- cor(testing_data$price,testing_data$test_price)
rsquared <- cor(testing_data$price,testing_data$test_price)^2
rsquared #0.8426566

write.csv(testing_data, file = "Testing_data_final.csv")

#Graph

plot(testing_data$car_id, testing_data$price, type = "l", col = "black")
lines(testing_data$car_id, testing_data$test_price, col= "red")

