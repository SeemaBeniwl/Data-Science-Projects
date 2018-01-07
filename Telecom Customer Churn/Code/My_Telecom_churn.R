####### @ SEEMA BENIWAL ##############
# The libraries to install
library(stats)
library(caTools)
library(Amelia)
library(dplyr)
# install.packages("dplyr")



"dplyr"# Load the data

telecomDF <- read.csv("C://Users/seema/Desktop/customer_churn_analysis-master/customer_churn_analysis-master/WA_Fn-UseC_-Telco-Customer-Churn.csv")

# Data Description and enginering.
View(telecomDF)
summary(telecomDF)
str(telecomDF)

# See the missing values
# check for the NA values 
any(is.na(telecomDF))
telecomDF(is.na(telecomDF))
# visualize the missing values using the missing map from the Amelia package
missmap(telecomDF,col=c("yellow","red"))






# Making the variables as factors.

telecomDF$SeniorCitizen_fact <- as.factor(telecomDF$SeniorCitizen)

# Make buckets on tenuer

group_tenure <- function(tenure){
  if (tenure >= 0 && tenure <= 6){
    return('0-6 Month')
  }else if(tenure > 6 && tenure <= 12){
    return('6-12 Month')
  }else if (tenure > 12 && tenure <= 24){
    return('12-24 Month')
  }else if (tenure > 24 && tenure <=36){
    return('24-36 Month')
  }else if (tenure > 36 && tenure <=48){
    return('36-48 Month')
  }else if (tenure > 48 && tenure <= 62){
    return('48-62 Month')
  }else if (tenure > 62){
    return('> 62 Month')
  }
}

# apply group_tenure function on each row of dataframe
telecomDF$tenure_interval <- sapply(telecomDF$tenure,group_tenure)
telecomDF$tenure_interval <- as.factor(telecomDF$tenure_interval)

# Ignore the variables with more levels while predicting the model
# Columns "customerID" and "tenure" having more levels
telecomDF <- select(telecomDF,-customerID,-tenure)

# The value of the following columns affecting the model and introducing the NA value for "No phone service" and  and "No internet service" need to cleanup the data for these columns MultipleLine,OnlineSecurity,OnlineBackup,DeviceProtection,TechSupport,StreamingTV,StreamingMovies
telecomDF$MultipleLines <- as.character(telecomDF$MultipleLines)
telecomDF$OnlineSecurity <- as.character(telecomDF$OnlineSecurity)
telecomDF$OnlineBackup <- as.character(telecomDF$OnlineBackup)
telecomDF$DeviceProtection <- as.character(telecomDF$DeviceProtection)
telecomDF$TechSupport <- as.character(telecomDF$TechSupport)
telecomDF$StreamingTV <- as.character(telecomDF$StreamingTV)
telecomDF$StreamingMovies <- as.character(telecomDF$StreamingMovies)

# convert factor variables into character variables before changing the values
telecomDF$MultipleLines[telecomDF$MultipleLines=="No phone service"] <- "No"
telecomDF$OnlineSecurity[telecomDF$OnlineSecurity=="No internet service"] <- "No"
telecomDF$OnlineBackup[telecomDF$OnlineBackup=="No internet service"] <- "No"
telecomDF$DeviceProtection[telecomDF$DeviceProtection=="No internet service"] <- "No"
telecomDF$TechSupport[telecomDF$TechSupport=="No internet service"] <- "No"
telecomDF$StreamingTV[telecomDF$StreamingTV=="No internet service"] <- "No"
telecomDF$StreamingMovies[telecomDF$StreamingMovies=="No internet service"] <- "No"

# converting character variables into factor variables
telecomDF$MultipleLines <- as.factor(telecomDF$MultipleLines)
telecomDF$OnlineSecurity <- as.factor(telecomDF$OnlineSecurity)
telecomDF$OnlineBackup <- as.factor(telecomDF$OnlineBackup)
telecomDF$DeviceProtection <- as.factor(telecomDF$DeviceProtection)
telecomDF$TechSupport <- as.factor(telecomDF$TechSupport)
telecomDF$StreamingTV <- as.factor(telecomDF$StreamingTV)
telecomDF$StreamingMovies <- as.factor(telecomDF$StreamingMovies)

# check the number of NA rows if it is relatively small in number then ignore those rows from the analysis
telecomDF <- na.omit(telecomDF)

# set the seed it will output same output when ever the model is executed
set.seed(123)

# sample the input data with 70% for training and 30% for testing
sample <- sample.split(telecomDF$Churn,SplitRatio=0.70)
trainData <- subset(telecomDF,sample==TRUE)
testData <- subset(telecomDF,sample==FALSE)

# logistic regression model on top training the data
telecomDFModel <- glm(Churn ~ .,family=binomial(link="logit"),data=trainData)
print(summary(telecomDModel))

# test the model with test dataset
test.predictions <- predict(telecomDFModel,newdata=testData,type="response")

# if the prediction probability is greater than 0.5 then those 
# customers are classified as churned customer less than 0.5 are classified as not churning customer
fitted.results <- ifelse(test.predictions > 0.5,1,0)
testData$Churn <- as.character(testData$Churn)
testData$Churn[testData$Churn=="No"] <- "0"
testData$Churn[testData$Churn=="Yes"] <- "1"

# calculating the misclassfication rate
misClasificationError <- mean(fitted.results!=testData$Churn)
print(misClasificationError)

# calculating the accuracy rate
accuracyRate <- 1-misClasificationError
print(accuracyRate)

# confusion matrix
table(testData$Churn,test.predictions > 0.5)

# cbinding actual results with the predicted results
results <- cbind(fitted.results,testData$Churn)
colnames(results) <- c("predicted","actual")
results <- as.data.frame(results)
print(results)
