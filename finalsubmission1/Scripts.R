#####################################################################
####################### Auto Insurance Data Case Study ###################
#####################################################################

# Step-1
# Not allowing R to use scientific notation till specified number
options(scipen=1000000)

# Step-2
#Loading the raw Data
AIData=read.csv('/Users/pieu/Desktop/AutoInsuranceData.csv',stringsAsFactors=T)
str(AIData)

# Step-3
# Exploring the dataset and data cleaning
head(AIData)
str(AIData)
summary(AIData)

# Finding unique values for each column
# Defining the function to find unique values
FunctionFindUnique=function(inpColumn){
	return(length(unique(inpColumn)))
}

# Applying the function on every column using sapply
# Understand categorical/continuous columns
sapply(AIData, FunctionFindUnique)

# Removing useless columns
AIData[, c("Customer", "State","Effective.To.Date", "Location.Code", "Months.Since.Last.Claim", "Months.Since.Policy.Inception",
)] = NULL
head(AIData)

# Changing categorical numerical variables to factors

AIData$Number.of.Open.Complaints=as.factor(AIData$Number.of.Open.Complaints)
class(AIData$Number.of.Open.Complaints)

AIData$Number.of.Policies=as.factor(AIData$Number.of.Policies)
class(AIData$Number.of.Policies)

quantile(AIData$Income)

# Step-4
# Identify the Problem Statement, What are you trying to solve?
To predict the Customer Lifetime value

# Step-5
# Identify the Target variable, What value will be predicted?
Customer Lifetime Value

# Step-6
# Whether it is a Regression problem or Classification?
Regression

# Step-7
# Explore each "Potential" predictor for distribution and Quality

# Exploring MULTIPLE CONTINUOUS features
ColsForHist=c("Customer.Lifetime.Value", "Monthly.Premium.Auto","Total.Claim.Amount")

#Splitting the plot window into four parts
par(mfrow=c(2,2))

# library to generate professional colors
library(RColorBrewer)
install.packages("RColorBrewer")

# looping to create the histograms for each column
for (contCol in ColsForHist){
	hist(AIData[,c(contCol)], main=paste('Histogram of:', contCol), 
	col=brewer.pal(8,"Paired"))
}

############################################################
# Exploring MULTIPLE CATEGORICAL features
ColsForBar=c("Number.of.Policies","Coverage","Vehicle.Class","Number.of.Open.Complaints","Renew.Offer.Type","Response","Education","Gender","EmploymentStatus")

#Splitting the plot window into nine parts
par(mfrow=c(3,3))

# looping to create the Bar-Plots for each column
for (catCol in ColsForBar){
	barplot(table(AIData[,c(catCol)]), main=paste('Barplot of:', catCol), 
	col=brewer.pal(8,"Paired"))
}

# Step-8
# Visual Relationship between predictors and target variable
# Continuous Vs Continuous ---- Scatter Plot
# Continuous Vs Categorical --- Box Plot

# Continuous Vs Continuous --- Scatter plot

# For multiple columns at once
ContinuousCols = c("Customer.Lifetime.Value", "Monthly.Premium.Auto","Total.Claim.Amount")

plot(AIData[, ContinuousCols], col='blue')

# Step-9
# Strength of Relationship between predictor and target variable
# Continuous Vs Continuous ---- Correlation test
# Continuous Vs Categorical---- ANOVA test

# Continuous Vs Continuous : Correlation analysis
# use = "complete.obs" means use only those rows which are complete(No Missing values)
# Correlation for multiple columns at once
cor(AIData[, ContinuousCols], use = "complete.obs")

CorrData=cor(AIData[, ContinuousCols], use = "complete.obs")
CorrData

# Final columns which has high correlation with the target variable
names(CorrData[,'Customer.Lifetime.Value'][abs(CorrData[,'Customer.Lifetime.Value'])>0.2])

############################################################
# Continuous Vs Categorical Visual analysis: Boxplot
boxplot(Customer.Lifetime.Value ~ Number.of.Policies, data = AIData, col=brewer.pal(8,"Paired"))

boxplot(Customer.Lifetime.Value ~ Coverage, data = AIData, col=brewer.pal(8,"Paired"))

boxplot(Customer.Lifetime.Value ~ Vehicle.Class, data = AIData, col=brewer.pal(8,"Paired"))

boxplot(Customer.Lifetime.Value ~ Number.of.Open.Complaints, data = AIData, col=brewer.pal(8,"Paired"))

boxplot(Customer.Lifetime.Value ~ Renew.Offer.Type, data = AIData, col=brewer.pal(8,"Paired"))

boxplot(Customer.Lifetime.Value ~ Response, data = AIData, col=brewer.pal(8,"Paired"))

boxplot(Customer.Lifetime.Value ~ Education, data = AIData, col=brewer.pal(8,"Paired"))

boxplot(Customer.Lifetime.Value ~ Gender, data = AIData, col=brewer.pal(8,"Paired"))

boxplot(Customer.Lifetime.Value ~ EmploymentStatus, data = AIData, col=brewer.pal(8,"Paired"))


# Continuous Vs Categorical correlation strength: ANOVA
# Analysis of Variance(ANOVA)
# F-Statistic is Mean Sq error/residual MeanSq error
# H0: Variables are NOT correlated
# Small P-Value--> Variables are correlated(H0 is rejected)
# Large P-Value--> Variables are NOT correlated (H0 is accepted)

# Using a for-loop to perform ANOVA on multiple columns
ColsForANOVA=c("Number.of.Policies","Coverage","Vehicle.Class","Number.of.Open.Complaints","Renew.Offer.Type","Response","Education","Gender","EmploymentStatus")
for (catCol in ColsForANOVA){
	anovaData= AIData[, c("Customer.Lifetime.Value", catCol)]
	print(summary(aov(Customer.Lifetime.Value ~., data= anovaData)))
}

#########################################################################
# Step-10
# Checking and treating missing values

# Checking missing values
colSums(is.na(AIData))

# No Missing values, hence proceeding ahead

# Step-11
Treating Outliers

AIData$Customer.Lifetime.Value=log(AIData$Customer.Lifetime.Value)

AIData$Monthly.Premium.Auto=log(AIData$Monthly.Premium.Auto)

AIData$Total.Claim.Amount=log(AIData$Total.Claim.Amount)

summary(AIData)


#########################################################################
#########################################################################
#########################################################################

# Step-12
# Generating the Data frame for machine learning
InputData=AIData
TargetVariableName=c('Customer.Lifetime.Value')

# Choosing multiple Predictors which may have relation with Target Variable
# Based on the exploratory data analysis
BestPredictorName= c("Monthly.Premium.Auto","Total.Claim.Amount","Number.of.Policies","Coverage","Vehicle.Class","Number.of.Open.Complaints","Renew.Offer.Type","Response","Education","Gender","EmploymentStatus","Total.Claim.Amount")

# Extracting Target and predictor variables from data to create a generic dataset
TargetVariable=InputData[, c(TargetVariableName)]
str(TargetVariable)

# Selecting all other columns as Predictors apart from target variable

PredictorVariable=InputData[, BestPredictorName]
str(PredictorVariable)

DataForML=data.frame(TargetVariable,PredictorVariable)
str(DataForML)
head(DataForML)

#########################################################################
# Sampling | Splitting data into 70% for training 30% for testing
TrainingSampleIndex=sample(1:nrow(DataForML), size=0.7 * nrow(DataForML) )
DataForMLTrain=DataForML[TrainingSampleIndex, ]
DataForMLTest=DataForML[-TrainingSampleIndex, ]
dim(DataForMLTrain)
dim(DataForMLTest)


########################################################################
# Creating Predictive models on training data to check the accuracy of each algorithm
###### Linear Regression #######
startTime=Sys.time()
Model_Reg=lm(TargetVariable~.,data=DataForMLTrain)
summary(Model_Reg)
endTime=Sys.time()
endTime-startTime

# Checking Accuracy of model on Testing data
DataForMLTest$Pred_LM=predict(Model_Reg, DataForMLTest)
head(DataForMLTest)

# Calculating the Absolute Percentage Error for each prediction
LM_APE= 100 *(abs(DataForMLTest$Pred_LM - DataForMLTest$TargetVariable)/DataForMLTest$TargetVariable)
print(paste('### Mean Accuracy of Linear Regression Model is: ', 100 - mean(LM_APE)))
print(paste('### Median Accuracy of Linear Regression Model is: ', 100 - median(LM_APE)))

#Step-13-Other tests

vif(Model_Reg)



install.packages('faraway')
library(faraway)

install.packages("nortest")
library(nortest)

ad.test(DataForMLTest)

ad.test(Model_Reg)

hist(Model_Reg)

shapiro.test(Model_Reg)

install.packages('car')

library(car)

durbinWatsonTest(lm(TargetVariable~.,data=DataForMLTrain)
)

install.packages('lmtest')
library(lmtest)

install.packages('plm')
library(plm)

bptest(TargetVariable~.,data=DataForMLTrain,studentize=F)

plot(AIData$Monthly.Premium.Auto,AIData$Customer.Lifetime.Value,main="Regression Data",xlab="Any Variable",ylab="Customer.Lifetime.Value",col="blue")

abline(lm(AIData$Customer.Lifetime.Value~AIData$Monthly.Premium.Auto),col="red")

