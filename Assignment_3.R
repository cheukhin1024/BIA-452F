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

### Descriptive statistics

dat <- read_csv("C:/Users/Administrator/Downloads/BankChurners.csv")
describe(dat[,1:23])
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
dat_comp1$Attrition_Flag <- as.character(dat_comp1$Attrition_Flag)
dat_comp1$Attrition_Flag[which(dat_comp1$Attrition_Flag=="Attrited Customer")] <- "1"
dat_comp1$Attrition_Flag[which(dat_comp1$Attrition_Flag=="Existing Customer")] <- "0"
dat_comp1$Attrition_Flag <- as.numeric(dat_comp1$Attrition_Flag)
table(dat_comp1$Attrition_Flag)

trainIndex <- createDataPartition(dat_comp1$Attrition_Flag, p = .75,list=FALSE)
training <- dat_comp1[trainIndex,]
testing <- dat_comp1[-trainIndex,]

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











# Convert 'Card_Category' character strings into integers
dat_comp$Card_Category <- as.character(dat_comp$Card_Category)
dat_comp$Card_Category[which(dat_comp$Card_Category=="Blue")] <- "0"
dat_comp$Card_Category[which(dat_comp$Card_Category=="Silver")] <- "1"
dat_comp$Card_Category[which(dat_comp$Card_Category=="Gold")] <- "2"
dat_comp$Card_Category[which(dat_comp$Card_Category=="Platinum")] <- "3"
dat_comp$Card_Category <- as.numeric(dat_comp$Card_Category)

# Convert 'Marital_Status' character strings into integers
dat_comp$Marital_Status <- as.character(dat_comp$Marital_Status)
dat_comp$Marital_Status[which(dat_comp$Marital_Status=="Married")] <- "0"
dat_comp$Marital_Status[which(dat_comp$Marital_Status=="Single")] <- "1"
dat_comp$Marital_Status[which(dat_comp$Marital_Status=="Unknown")] <- "2"
dat_comp$Marital_Status[which(dat_comp$Marital_Status=="Divorced")] <- "3"
dat_comp$Marital_Status <- as.numeric(dat_comp$Marital_Status)

# Convert 'Gender' character strings into integers
dat_comp$Gender <- as.character(dat_comp$Gender)
dat_comp$Gender[which(dat_comp$Gender=="F")] <- "0"
dat_comp$Gender[which(dat_comp$Gender=="M")] <- "1"
dat_comp$Gender <- as.numeric(dat_comp$Gender)

# Convert 'Income_Category' character strings into integers
dat_comp$Income_Category <- as.character(dat_comp$Income_Category)
dat_comp$Income_Category[which(dat_comp$Income_Category=="Unknown")] <- "0"
dat_comp$Income_Category[which(dat_comp$Income_Category=="Less than $40K")] <- "1"
dat_comp$Income_Category[which(dat_comp$Income_Category=="$40K - $60K")] <- "2"
dat_comp$Income_Category[which(dat_comp$Income_Category=="$60K - $80K")] <- "3"
dat_comp$Income_Category[which(dat_comp$Income_Category=="$80K - $120K")] <- "4"
dat_comp$Income_Category[which(dat_comp$Income_Category=="$120K +")] <- "5"
dat_comp$Income_Category <- as.numeric(dat_comp$Income_Category)

# Convert 'Education_Level' character strings into integers
dat_comp$Education_Level <- as.character(dat_comp$Education_Level)
dat_comp$Education_Level[which(dat_comp$Education_Level=="Unknown")] <- "0"
dat_comp$Education_Level[which(dat_comp$Education_Level=="Uneducated")] <- "1"
dat_comp$Education_Level[which(dat_comp$Education_Level=="High School")] <- "2"
dat_comp$Education_Level[which(dat_comp$Education_Level=="College")] <- "3"
dat_comp$Education_Level[which(dat_comp$Education_Level=="Graduate")] <- "4"
dat_comp$Education_Level <- as.numeric(dat_comp$Education_Level)

# Show top 10 rows
head(dat_comp, 10)

## Correlation analysis of 18 features (all 23 columns but excluding 'CLIENTNUM', Total_Trans_Amt', 'Total_Ct_Chng_Q4', 'Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_1', 'Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_2')between binary variables and continuous variables:
dat_comp %>% 
  dplyr::select(Attrition_Flag, Gender, Education_Level, Marital_Status, Income_Category, Card_Category, Customer_Age, Dependent_count, Months_on_book, Total_Relationship_Count, Months_Inactive_12_mon, Contacts_Count_12_mon, Credit_Limit, Total_Revolving_Bal, Avg_Open_To_Buy, Total_Trans_Amt, Total_Trans_Ct, Avg_Utilization_Ratio) %>%
  cor() %>%
  round(3) %>%
  corrplot(method = "color", addCoef.col="white", type = "upper", 
           title="Correlation Matrix Among Factors",
           mar=c(0,0,2,0),
           tl.cex=0.5, number.cex = 0.4)

## Scatter plot between related variables
x<-dat_comp %>% dplyr::select(Attrition_Flag, Gender, Education_Level, Marital_Status, Income_Category, Card_Category, Customer_Age, Dependent_count, Months_on_book, Total_Relationship_Count, Months_Inactive_12_mon, Contacts_Count_12_mon, Credit_Limit, Total_Revolving_Bal, Avg_Open_To_Buy, Total_Trans_Amt, Total_Trans_Ct, Avg_Utilization_Ratio) %>%
  cor() 
round(x,3)

# Form a new dataframe by choosing those 18 features
dat_features <- dat_comp %>% 
  dplyr::select(Attrition_Flag, Gender, Education_Level, Marital_Status, Income_Category, Card_Category, Customer_Age, Dependent_count, Months_on_book, Total_Relationship_Count, Months_Inactive_12_mon, Contacts_Count_12_mon, Credit_Limit, Total_Revolving_Bal, Avg_Open_To_Buy, Total_Trans_Amt, Total_Trans_Ct, Avg_Utilization_Ratio)

# Show top 10 rows
head(dat_features, 10)

# calculating the number of cells with na of the new data frame
dat_features_missingcells = sum(is.na(dat_features))
print("Missing value cells in dat_features")
print(dat_features_missingcells)
which(is.na(dat_features))

#Artificial Neural Networks
df$Attrition_Flag <- NULL
df<-normalizeFeatures(dat_comp, method="range")
summary(df)
# split data into training and test sets
set.seed(800)
index <- 1:nrow(df)
test_set_index <- sample(index, trunc(length(index)/3))
test_set <- df[test_set_index,]
train_set <- df[-test_set_index,]

# create a neural network classification task
nn_task <- makeClassifTask(id = "nn", data = train_set,
                           target = "Attrition_Flag")
nn_task
# produce summary of the task data
summary(getTaskData(nn_task))
# check parameter sets of learner
getParamSet(makeLearner("classif.nnet"))
getHyperPars(makeLearner("classif.nnet"))
# create a neural network learner - 3 neurons
nn_lrn <- makeLearner("classif.nnet", size=3, maxit=10000L)
nn_lrn
# train the neural network classifier
nn_mod <- train(nn_lrn, nn_task)
nn_mod$learner.model
nn_mod$features
nn_mod$time 
