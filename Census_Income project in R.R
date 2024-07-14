#Data Science Project

#Final Project on "Census Income" Dataset
#In this project, you are going to work on the The "Census Income" data set from the UCI
#Machine Learning Repository that contains the income information for over 48,000 individuals
#taken from the 1994 US census..


#Problem Statement:
#  In this project, initially you need to preprocess the data and then develop an understanding of
#different features of the data by performing exploratory analysis and creating
#visualizations.Further, after having sufficient knowledge about the attributes you will perform a
#predictive task of classification to predict whether an individual makes over 50K a year or
#less,by using different Machine Learning Algorithms.

# 1.Data preprocessing
# a) Replace all the missing values with NA

census.income_ <- read.csv("C:/Users/Lenovo/Downloads/census-income_.csv",stringsAsFactors = T)
View(census.income_)

sum(is.na(census.income_))

str(census.income_)


#To convert all question mark data set to NA

census.income_$workclass[28]
census.income_[census.income_==" ?"] <- NA

View(census.income_)

census.income_$workclass[28]

#missing NA Values in data set
sum(is.na(census.income_))

#b) Remove all the rows that contain NA Values

census.income_ <- na.omit(census.income_)  #it removes all incomplete values of data object

#32561 --- Before using na.omit function
#30162 rows after using na.omit function

sum(is.na(census.income_))  # NA Value count is 0

#Remove all white spaces in the data set

library(stringr)
library(dplyr)

census.income_ <- census.income_%>%
  mutate_if(is.character,str_trim)

summary(census.income_)

#2.Data Manipulation:
#  Your task is to perform data manipulation to analyze the data set using various functions from
#the dplyr package.
#Questions:
 # a) Extract the "education" column and store it in "census_ed" .

census.income_$ed <- census.income_$education
View(census.income_$ed)
head(census.income_$ed)

#Extract all the columns from "age" to "relationship" and store it in "census_seq".
census_seq <- census.income_%>%
  select(age:relationship)
View(census_seq)

# Extract the column number "5", "8", "11" and store it in "census_col".
census_col <- census.income_[, c(5,8,11)]
View(census_col)
head(census_col)

# Extract all the male employees who work in state-gov and store it in "male_gov".
table(census.income_$sex)
table(census.income_$workclass)
male_gov <- census.income_%>% filter(sex=="Male" & workclass=="State_gov")
View(male_gov)
head(male_gov)     

# Not getting output

#e) Extract all the 39 year olds who either have a bachelor's degree or who are native of
#United States and store the result in "census_us".

table(census.income_$native.country)
table(census.income_$education)
census_us <- census.income_%>% filter(age==39&(education=="Bachelors" | native.country=="United-States"))

# Extract 200 random rows from the "census" data frame and store it in "census_200".
census_200 <- sample_n(census.income_,200)
View(census_200)


#g) Get the count of different levels of the "workclass" column.
countclass <- count(census.income_, workclass)
View(countclass)

#h) Calculate the mean of "capital.gain" column grouped according to "workclass".
census.income_%>% group_by(workclass)%>% 
  summarise(mean_capital_gain=mean(capital.gain))

#3.Data Visualization:

library(ggplot2)

 # a) Build a bar-plot for the "relationship" column and fill the bars according to the "race"
#column.
ggplot(census.income_, aes(x= relationship, fill=race))+
  geom_bar()
#Set x-axis label to 'Categories of Relationships'
#ii. Set y-axis label to 'Count of Categories'

ggplot(census.income_, aes(x= relationship, fill=race))+
  geom_bar()+
  labs(x="categories of Relationships", y="Count of Categories")

# Fill the bars according to "sex"

ggplot(census.income_, aes(x= relationship, fill=sex))+
  geom_bar()+
  labs(x="categories of Relationships", y="Count of Categories")

#Set the position of the bars to "dodge"

ggplot(census.income_, aes(x= relationship, fill=sex))+
  geom_bar(position = "dodge")+
  labs(x="categories of Relationships", y="Count of Categories")


#Set the title of plot to be 'Distribution of Relationships by Sex"

ggplot(census.income_, aes(x= relationship, fill=sex))+
  geom_bar(position = "dodge")+
  labs(x="categories of Relationships", y="Count of Categories", title = "distribution of relationships by sex")

#Build a Histogram for the "age" column with number of bins equal to 50.
 ggplot(census.income_,aes(x=age))+geom_histogram(bins = 50)
 
 
 #i) Fill the bars of the histogram according to yearly income column i.e., "X"
  ggplot(census.income_,aes(x=age,fill=X))+geom_histogram(bins = 50)

 #ii)Set the title of the plot to "Distribution of Age".
 ggplot(census.income_,aes(x=age,fill=X))+geom_histogram(bins = 50)+
   labs(title = "Distribution of Age")
 
 #iii)Set the legend title to "Yearly income".
 ggplot(census.income_,aes(x=age,fill=X))+geom_histogram(bins = 50)+
   labs(title = "Distribution of Age",fill="yearly income")+theme_bw()
 
 #iv) Set the theme of the plot to black and white.
 ggplot(census.income_,aes(x=age,fill=X))+geom_histogram(bins = 50)+
   labs(title = "Distribution of Age")+theme_bw()
 
 #c)Build a scatter-plot between "capital.gain" and "hours.per.week". Map "capital.gain" on the x-
  #axis and "hours.per.week" on the y-axis.
 ggplot(census.income_,aes(x=capital.gain, y=hours.per.week))+
   geom_point()
 
 #i)Set the transparency of the points to 40% and size as 2.
 ggplot(census.income_,aes(x=capital.gain, y=hours.per.week))+
   geom_point(alpha=0.6,size=2)
 
 #ii)Set the color of the points according to the "X" (yearly income) column.
 ggplot(census.income_,aes(x=capital.gain, y=hours.per.week,col=X))+
   geom_point(alpha=0.6,size=2)

 #iii)Set the x-axis label to "Capital Gain", y-axis label to "Hours per Week", title
  #to "Capital Gain vs Hours per Week by Income", and legend label to "Yearly income
 ggplot(census.income_,aes(x=capital.gain, y=hours.per.week,fill=X))+
   geom_point(alpha=0.6,size=2)+
   labs(X="Capital gain", y="Hours per week", title = "capital gain vs hours per week y income",fill= "Yearly income")
 
 #d) Build a box-plot between "education" and "age" column.Map "education" on the x-axis and
#"age" on the y-axis.
 ggplot(census.income_,aes(x=education,y=age))+
   geom_boxplot()
#i) Fill the box-plots according to the "sex" column.
 
 ggplot(census.income_,aes(x=education,y=age,fill=sex))+
   geom_boxplot()
 
 #ii)Set the title to "Box-Plot of age by Education and Sex".
 
 ggplot(census.income_,aes(x=education,y=age,fill=sex))+
   geom_boxplot()+
   labs(title = "Box-Plot of age by Education and Sex")
 
 #4. Linear Regression:
 #   a) Build a simple linear regression model as follows:
 #  i)Divide the dataset into training and test sets in 70:30 ratio.
 
library(caTools)
 set.seed(111)
split_data <- sample.split(census.income_$hours.per.week, SplitRatio = 0.70) 
train <- subset(census.income_,split_data==T)
test<- subset(census.income_,split_data==F)

#ii)Build a linear model on the train set where the dependent variable is
#"hours.per.week" and independent variable is "education.num".
  str(census.income_)
  
  lr_model <- lm(hours.per.week~education.num,data = train)
  summary(lr_model)
  
 #  iii)Predict the values on the train set and find the error in prediction.
  predict_result <- predict(lr_model, newdata = test)
  head(predict_result)
  final_data <- cbind(Actual= test$hours.per.week, Predicted=predict_result)
  head(final_data)
  class(final_data)
  final_data <- as.data.frame(final_data)
  class(final_data)
  error <-final_data$Actual-final_data$Predicted
  View(error)
  head(error)
  censusD <- cbind( final_data,error)
  View(censusD)
  head(censusD)
  
# iv)Find the root-mean-square error (RMSE)
  sqrt(mean((censusD$error)^2))

 # 5.Logistic Regression:
  #  a)Build a simple logistic regression model as follows:
  #i)Divide the dataset into training and test sets in 65:35 ratio.  
  library(caTools)
  set.seed(111)
  split_data1 <- sample.split(census.income_$X, SplitRatio = 0.65) 
  train1 <- subset(census.income_,split_data1==T)
  test1<- subset(census.income_,split_data1==F)
  nrow(train1)
 nrow(test1)  
# ii)Build a logistic regression model where the dependent variable is "X"(yearly
 #income) and independent variable is "occupation". 
  log_mod <-glm(X~occupation, data = train1,family = "binomial")
  summary(log_mod)
  
  
 # iii)Predict the values on the test set.
  predict_value<- predict(log_mod,newdata= test1,type = "response")
  head(predict_value)
  range(predict_value)
  #iv)Plot accuracy vs cut-off and pick an ideal value for cut-off.  
  install.packages("ROCR")
 library(ROCR)  #To decide accuracy
  predict_log_roc <- prediction(predict_value,test1$X)
 predict_log_roc  
 acc <- performance(predict_log_roc,"acc") 
 plot(acc) #check for which value accuracy got constant
 
 lm.pred<- ifelse(predict_value>0.45, ">50k", "<50k")
 lm.pred
 #v)Build a confusion matrix and find the accuracy.
 tab <- table(lm.pred,test1$X)
 tab 
 accuracy <- sum(diag(tab))/sum(tab)
 accuracy
 # vi)Plot the ROC curve and find the auc(Area Under Curve).
 roc <- performance(predict_log_roc, "tpr","fpr")
 plot(roc)
 auc <- performance(predict_log_roc,"auc")
 auc
 auc<-auc@y.values[[1]] 
 auc 

# b)Build a multiple logistic regression model as follows:
#  i)Divide the dataset into training and test sets in 80:20 ratio.
 
 library(caTools)
 set.seed(111)
 split_data2 <- sample.split(census.income_$X, SplitRatio = 0.80) 
 train2 <- subset(census.income_,split_data2==T)
 test2<- subset(census.income_,split_data2==F)
 nrow(train2)
 nrow(test2)  
#ii)Build a logistic regression model where the dependent variable is "X"(yearlyincome) and independent variables are "age", "workclass", and "education".
 log_mod1 <-glm(X~age+workclass+education, data = train2,family = "binomial")
 summary(log_mod1)
 
 #iii)Predict the values on the test set.
 predict_value1<- predict(log_mod1,newdata= test2,type = "response")
 head(predict_value1)
 range(predict_value1)
 
#iv)Plot accuracy vs cut-off and pick an ideal value for cut-off.
library(ROCR) #To decide accuracy
predictt_log_roc <- prediction(predict_value1, test2$X)
predictt_log_roc
acc <- performance(predictt_log_roc,"acc")
plot(acc)
lm.pred <- ifelse(predict_value1>0.47, ">50k", "<50k")
lm.pred


#v)Build a confusion matrix and find the accuracy.

tab <- table(lm.pred,test2$X)
tab 
accuracy <- sum(diag(tab))/sum(tab)
accuracy

 #vi)Plot the ROC curve and calculate the auc(Area Under Curve).

 roc<- performance(predictt_log_roc, "tpr","fpr")
 plot(roc)
 auc<-performance(predictt_log_roc,"auc")
 auc
 auc<- auc@y.values[[1]]
auc

#6.Decision Tree:
 # a)Build a decision tree model as follows:
#  i) Divide the dataset into training and test sets in 70:30 ratio.

library(caTools)
set.seed(111)
split_data1 <- sample.split(census.income_$X, SplitRatio = 0.70) 
train1 <- subset(census.income_,split_data1==T)
test1<- subset(census.income_,split_data1==F)
nrow(train1)
nrow(test1)  
#ii)Build a decision tree model where the dependent variable is "X"(Yearly Income)
#and the rest of the variables as independent variables.

install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

census_model <- rpart(formula = X~.,data = train1,method = "class")
#iii)Plot the decision tree.
rpart.plot(x=census_model, type = 5, extra = 0)
#iv)Predict the values on the test set.
 class_prediction <- predict(census_model, 
         newdata = test1,
         type = "class")
 
  
#v)Build a confusion matrix and calculate the accuracy.

 tab <- table(class_prediction,test1$X)
 tab
 
 sum(diag(tab))/sum(tab)
  
  
 # 7.Random Forest:
 #   a)Build a random forest model as follows:
 # i)Divide the dataset into training and test sets in 80:20 ratio.
 library(caTools)
 set.seed(111)
 split_data1 <- sample.split(census.income_$X, SplitRatio = 0.80) 
 train1 <- subset(census.income_,split_data1==T)
 test1<- subset(census.income_,split_data1==F)
 nrow(train1)
 nrow(test1) 
 
 
 #ii)Build a random forest model where the dependent variable is "X"(Yearly Income) and the rest of the variables as independent variables and number of trees as 300.
library(randomForest)
 census_model <- randomForest(formula= X~. , 
                              data=train1,
                              ntree=300)
  
 
 #iii)Predict values on the test set
 
 census_prediction <- predict(census_model, 
                              newdata = test1,
                              type = "class")
 #iv)Build a confusion matrix and calculate the accuracy
 tab <- table(census_prediction, test1$X)
 tab
 sum(diag(tab))/sum(tab)
