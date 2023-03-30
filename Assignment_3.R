# Set RStudio system language be English
Sys.setenv(LANG = "en")

# Load packages
library(dplyr)
library(ggplot2)
library(psych)
library(corrplot)
library(VIM)
library(gridExtra)
library(car)
library(knitr)
library(gmodels)
library(readxl)
library(readr)
library(reshape2)
library(tidyverse)
library(r2r)
library(ltm)
library(caret)
library(randomForest)
library(gam)
library(mlbench)
library(mlr)
library(rpart)
library(rpart.plot)
library(e1071)
library(glmnet) 
library(MASS)
library(yardstick)
library(gmodels)
library(scales)

### Descriptive statistics

dat <- read_csv("C:/Users/Administrator/Downloads/BankChurners.csv")
describe(dat[,1:23])
summary(dat)
head(dat, 10)

# Select all columns (excluding 'CLIENTNUM') to form a dataframe
dat <- dat %>% dplyr::select(Attrition_Flag, Gender, Education_Level, Marital_Status, Income_Category, Card_Category, Customer_Age, Dependent_count, Months_on_book, Total_Relationship_Count, Months_Inactive_12_mon, Contacts_Count_12_mon, Credit_Limit, Total_Revolving_Bal, Avg_Open_To_Buy, Total_Amt_Chng_Q4_Q1, Total_Trans_Amt, Total_Trans_Ct, Total_Ct_Chng_Q4_Q1, Avg_Utilization_Ratio, Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_1, Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_2)

# Select all 18 features to form a dataframe
dat_18features <- dat %>% dplyr::select(Attrition_Flag, Gender, Education_Level, Marital_Status, Income_Category, Card_Category, Customer_Age, Dependent_count, Months_on_book, Total_Relationship_Count, Months_Inactive_12_mon, Contacts_Count_12_mon, Credit_Limit, Total_Revolving_Bal, Avg_Open_To_Buy, Total_Trans_Amt, Total_Trans_Ct, Avg_Utilization_Ratio)

# calculating the product of dimensions of dataframe 
totalcells = prod(dim(dat))
print("Total number of cells ")
print(totalcells)

# calculating the number of cells with na
missingcells = sum(is.na(dat))
print("Missing value cells")
print(missingcells)

# calculating percentage of missing values
percentage = (missingcells * 100 )/(totalcells)
print("Percentage of missing values' cells")
print (percentage)

matrixplot(dat, interactive=FALSE) # without sorting
dat_comp <- dat[complete.cases(dat),]

# Plot frequency histogram of all numeric features of 18 features
hist(dat_comp$Customer_Age)
hist(dat_comp$Dependent_count)
hist(dat_comp$Months_on_book)
hist(dat_comp$Total_Relationship_Count)
hist(dat_comp$Months_Inactive_12_mon)
hist(dat_comp$Contacts_Count_12_mon)
hist(dat_comp$Credit_Limit)
hist(dat_comp$Total_Revolving_Bal)
hist(dat_comp$Avg_Open_To_Buy)
hist(dat_comp$Total_Trans_Amt)
hist(dat_comp$Total_Trans_Ct)
hist(dat_comp$Avg_Utilization_Ratio)

# Relationship between 'Martial_Status' and 'Customer_Age'
ggplot(dat_comp, aes(x=Marital_Status, y=Customer_Age, fill=Marital_Status)) + 
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none")

# Relationship between 'Attrition_Flag' and 'Customer_Age'
ggplot(dat_comp, aes(x=Attrition_Flag, y=Customer_Age, fill=Attrition_Flag)) + 
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none")

# Show frequency of all non-numeric features of 18 features
barplot(table(dat_comp$Attrition_Flag))
barplot(table(dat_comp$Gender))
barplot(table(dat_comp$Marital_Status))
barplot(table(dat_comp$Card_Category))

#Cutomer statues by Education_Level 
Attrited = subset(dat, dat$Attrition_Flag == "Attrited Customer")
Existing = subset(dat, dat$Attrition_Flag == "Existing Customer")
dat$Education_Level= factor(dat$Education_Level, ordered=TRUE, levels=c("Uneducated","High School","College","Graduate","Post-Graduate","Doctorate", "Unknown"))
ggplot(dat, aes(y=Education_Level))+geom_bar(aes(fill =Attrition_Flag),position = "dodge")+theme_minimal()+ theme_classic()  +xlab("Frequency") + ylab("Education Level") + ggtitle("Customer Status by Education Level" )+  labs(fill = "Customer Status")+ scale_fill_brewer(palette="Greens")

#Cutomer statues by Income_Level
dat$Income_Category= factor(dat$Income_Category, ordered=TRUE, levels=c("Less than $40K","$40K - $60K","$60K - $80K","$80K - $120K","$120K +"))
ggplot(dat, aes(y=Income_Category))+geom_bar(aes(fill =Attrition_Flag),position = "dodge")+theme_minimal()+ theme_classic()  +xlab("Frequency") + ylab("Income Level") + ggtitle("Customer Status by Income Level" )+  labs(fill = "Customer Status")+ scale_fill_brewer(palette="Greens")

## Correlation of all numeric variables among 18 features
dat_comp %>% 
  dplyr::select(Customer_Age, Dependent_count, Months_on_book, Total_Relationship_Count, Months_Inactive_12_mon, Contacts_Count_12_mon, Credit_Limit, Total_Revolving_Bal, Avg_Open_To_Buy, Total_Trans_Amt, Total_Trans_Ct, Avg_Utilization_Ratio) %>%
  cor() %>%
  round(3) %>%
  corrplot(method = "color", addCoef.col="white", type = "upper", 
           title="Correlation of all numeric variables among 18 features",
           mar=c(0,0,2,0),
           tl.cex=0.5, number.cex = 0.4)

## Scatter plot between related numeric variables among 18 features
x<-dat_comp %>%   dplyr::select(Customer_Age, Dependent_count, Months_on_book, Total_Relationship_Count, Months_Inactive_12_mon, Contacts_Count_12_mon, Credit_Limit, Total_Revolving_Bal, Avg_Open_To_Buy, Total_Trans_Amt, Total_Trans_Ct, Avg_Utilization_Ratio) %>%
  cor() 
round(x,3)

# Removing numeric variables which are highly correlated ('Months_on_book', 'Total_Trans_Amt', 'Avg_Utilization_Ratio') since they can create bad models, but also add 'Attrition_Flag' target for upcoming machine learning
keeps <- c("Attrition_Flag", "Customer_Age", "Dependent_count", "Total_Relationship_Count", "Months_Inactive_12_mon", "Contacts_Count_12_mon", "Credit_Limit", "Total_Revolving_Bal", "Avg_Open_To_Buy", "Total_Trans_Ct")
dat_comp1<-dat_comp[keeps]
dim(dat_comp1)

# Convert 'Attrition_Flag' character strings into integers
#dat_comp1$Attrition_Flag <- as.character(dat_comp1$Attrition_Flag)
#dat_comp1$Attrition_Flag[which(dat_comp1$Attrition_Flag=="Attrited Customer")] <- "1"
#dat_comp1$Attrition_Flag[which(dat_comp1$Attrition_Flag=="Existing Customer")] <- "0"
#dat_comp1$Attrition_Flag <- as.numeric(dat_comp1$Attrition_Flag)
#table(dat_comp1$Attrition_Flag)

#trainIndex <- createDataPartition(dat_comp1$Attrition_Flag, p = .75,list=FALSE)
#training <- dat_comp1[trainIndex,]
#testing <- dat_comp1[-trainIndex,]

#Trial Test
# Convert 'Attrition_Flag' character strings into integers
dat_18features$Attrition_Flag <- as.character(dat_18features$Attrition_Flag)
dat_18features$Attrition_Flag[which(dat_18features$Attrition_Flag=="Attrited Customer")] <- "1"
dat_18features$Attrition_Flag[which(dat_18features$Attrition_Flag=="Existing Customer")] <- "0"
dat_18features$Attrition_Flag <- as.numeric(dat_18features$Attrition_Flag)
table(dat_18features$Attrition_Flag)

trainIndex <- createDataPartition(dat_18features$Attrition_Flag, p = .75,list=FALSE)
training <- dat_18features[trainIndex,]
testing <- dat_18features[-trainIndex,]

# Logistic Regression
model <- glm(Attrition_Flag~.,data = training,family = 'binomial')
summary(model)

# Chi-squared test
anova(model, test= "Chisq")

# Feature Importance
importance <-varImp(model,scale = FALSE)
importance %>% arrange(desc(Overall))

# Decision Tree
mt = rpart(Attrition_Flag ~., data = training, method = "class")
plotcp(mt)
mt_prune = prune(mt,cp=0.036)

rpart.plot(mt,
           type = 1,
           extra=100,
           branch.lty=3,
           box.palette = "RdYlGn",
           tweak = 1.6,)

printcp(mt_prune)
dt_imp <- mt_prune$variable.importance
df <- as.data.frame(dt_imp)
df

tree.p = predict(mt_prune, testing, type = "class")
cmt = confusionMatrix(tree.p, as.factor(testing$Attrition_Flag), positive ="1")
cmt

#SVM
# Convert 'Gender' character strings into integers
dat_18features$Gender <- as.character(dat_18features$Gender)
dat_18features$Gender[which(dat_18features$Gender=="F")] <- "0"
dat_18features$Gender[which(dat_18features$Gender=="M")] <- "1"
dat_18features$Gender <- as.numeric(dat_18features$Gender)

# Convert 'Education_Level' character strings into integers
dat_18features$Education_Level <- as.character(dat_18features$Education_Level)
dat_18features$Education_Level[which(dat_18features$Education_Level=="Unknown")] <- "0"
dat_18features$Education_Level[which(dat_18features$Education_Level=="Uneducated")] <- "1"
dat_18features$Education_Level[which(dat_18features$Education_Level=="High School")] <- "2"
dat_18features$Education_Level[which(dat_18features$Education_Level=="College")] <- "3"
dat_18features$Education_Level[which(dat_18features$Education_Level=="Graduate")] <- "4"
dat_18features$Education_Level <- as.numeric(dat_18features$Education_Level)

# Convert 'Marital_Status' character strings into integers
dat_18features$Marital_Status <- as.character(dat_18features$Marital_Status)
dat_18features$Marital_Status[which(dat_18features$Marital_Status=="Married")] <- "0"
dat_18features$Marital_Status[which(dat_18features$Marital_Status=="Single")] <- "1"
dat_18features$Marital_Status[which(dat_18features$Marital_Status=="Unknown")] <- "2"
dat_18features$Marital_Status[which(dat_18features$Marital_Status=="Divorced")] <- "3"
dat_18features$Marital_Status <- as.numeric(dat_18features$Marital_Status)

# Convert 'Income_Category' character strings into integers
dat_18features$Income_Category <- as.character(dat_18features$Income_Category)
dat_18features$Income_Category[which(dat_18features$Income_Category=="Unknown")] <- "0"
dat_18features$Income_Category[which(dat_18features$Income_Category=="Less than $40K")] <- "1"
dat_18features$Income_Category[which(dat_18features$Income_Category=="$40K - $60K")] <- "2"
dat_18features$Income_Category[which(dat_18features$Income_Category=="$60K - $80K")] <- "3"
dat_18features$Income_Category[which(dat_18features$Income_Category=="$80K - $120K")] <- "4"
dat_18features$Income_Category[which(dat_18features$Income_Category=="$120K +")] <- "5"
dat_18features$Income_Category <- as.numeric(dat_18features$Income_Category)

# Convert 'Card_Category' character strings into integers
dat_18features$Card_Category <- as.character(dat_18features$Card_Category)
dat_18features$Card_Category[which(dat_18features$Card_Category=="Blue")] <- "0"
dat_18features$Card_Category[which(dat_18features$Card_Category=="Silver")] <- "1"
dat_18features$Card_Category[which(dat_18features$Card_Category=="Gold")] <- "2"
dat_18features$Card_Category[which(dat_18features$Card_Category=="Platinum")] <- "3"
dat_18features$Card_Category <- as.numeric(dat_18features$Card_Category)

# Split training data and testing data for SVM
trainIndex <- createDataPartition(dat_18features$Total_Trans_Amt, p = .75,list=FALSE)
training <- dat_18features[trainIndex,]
testing <- dat_18features[-trainIndex,]

# compare the distribution of the training and testing data sets
CrossTable(training$Attrition_Flag)
CrossTable(testing$Attrition_Flag)

## SVM
svmL.fit = svm(Attrition_Flag ~ ., data = training, kernel = "linear")
svmL_predication = predict(svmL.fit, testing)
confusionMatrix(svmL_predication, as.factor(testing$Attrition_Flag))
prediction_matrix['SVML'] = svmL_predication
