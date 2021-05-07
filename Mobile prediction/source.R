#Step 1: install and load headers
library(Hmisc)
library(dplyr)
library(caret)
library(caTools)
library(e1071)
library(yardstick)
library(ggplot2)
library(Metrics)
library(rpart)
library(rpart.plot)
library(randomForest)
#Step 2: Load the data-set
#train data-set
df_train=data.frame(read.csv("train.csv"))
# Step 3: Check for info and describe
sapply(df_train, typeof)
apply(df_train,2,mean)
apply(df_train,2,min)
apply(df_train,2,max)
apply(df_train,2,sum)
apply(df_train,2,median)
#Step 4:Converting to its appropriate data type
df_train$wifi <- as.factor(df_train$wifi) # converting to categorical
df_train$dual_sim = as.factor(df_train$dual_sim) # converting to categorical
df_train$four_g = as.factor(df_train$fdual_sim)  # converting to categorical
df_train$three_g = as.factor(df_train$three_g)  # converting to categorical
df_train$touch_screen = as.factor(df_train$touch_screen) # converting to categorical
df_train$n_cores= as.factor(df_train$n_cores) # converting to categorical
df_train$price_range = as.factor(df_train$price_range)  # converting to categorical
str(df_train)
#Step 5: check for null values
sum(is.na(df_train))
#Step 6:expolarty data analysis
cat_list=list("wifi","dual_sim","touch_screen","four_g","three_g","touch_screen")
  plot(df_train$wifi)
  plot(df_train$dual_sim)
  plot(df_train$touch_screen)
  plot(df_train$four_g)
  plot(df_train$three_g)
  plot(df_train$touch_screen)
#Step 7: Check for corealtion 
  res2 <- rcorr(as.matrix(df_train))
  res2
#Step 8:Repeat all the above thinks for test data as well
  df_test=data.frame(read.csv("test.csv"))
  #remove Id

  # Check for info and describe
  sapply(df_test, typeof)
  apply(df_test,2,mean)
  apply(df_test,2,min)
  apply(df_test,2,max)
  apply(df_test,2,sum)
  apply(df_test,2,median)
  #Converting to its appropriate data type
  df_test$wifi <- as.factor(df_test$wifi) # converting to categorical
  df_test$dual_sim = as.factor(df_test$dual_sim) # converting to categorical
  df_test$four_g = as.factor(df_test$fdual_sim)  # converting to categorical
  df_test$three_g = as.factor(df_test$three_g)  # converting to categorical
  df_test$touch_screen = as.factor(df_test$touch_screen) # converting to categorical
  df_test$n_cores= as.factor(df_test$n_cores) # converting to categorical
  df_test$price_range = as.factor(df_test$price_range)  # converting to categorical
  str(df_test)
  # check for null values
  sum(is.na(df_test))
  #expolarty data analysis
  cat_list=list("wifi","dual_sim","touch_screen","four_g","three_g","touch_screen")
  plot(df_test$wifi)
  plot(df_test$dual_sim)
  plot(df_test$touch_screen)
  plot(df_test$four_g)
  plot(df_test$three_g)
  plot(df_test$touch_screen)
  # Check for correltion 
  res_test <- rcorr(as.matrix(df_test))
  
  res_test
#Step 10: Split the data into train and test
set.seed(0)
split=sample.split(df_train,SplitRatio=0.8)
trainc=subset(df_train,split==TRUE)
testc=subset(df_train,split==FALSE)
#Step 11: Fitting the model
#SVR
classifier = svm(formula = price_range ~ .,data = trainc,type = 'C-classification',kernel = 'linear')
#Predicated values of SVR
y_pred = predict(classifier, testc)
y_pred
cm = table(testc[, 21], y_pred)
cm
#accuracy of SVR 
results_svm=accuracy(y_pred,testc[, 21])
#predicted values of Decison Tree
classtree=rpart(formula = price_range ~ .,data = trainc,method="class",control = rpart.control(maxdepth = 3))
#predcit
y_pred_tree=predict(classtree, testc,type="class")
#accuracy of Tree
results_tree=accuracy(y_pred_tree,testc[, 21])
#Random Forrest
classifier_RF = randomForest(formula = price_range ~ .,data = trainc, ntree = 500)
y_pred_forrest= predict(classifier_RF, testc)
results_forrst=accuracy(y_pred_forrest,testc[, 21])
#Step 12: Compare the results
#barplot to show the results
H <- c(results_forrst,results_tree,results_svm)
M <- c("Forrest","Tree","SVR")
# Give the chart file a name
png(file = "model_Score.png")
# Plot the bar chart 
barplot(H,names.arg=M,xlab="Models",ylab="Accuracy",col="blue",main="ACCURACY",border="red")
# Save the file
dev.off()

df_test<-  df_test[setdiff(colnames( df_test), c('id'))]
pred_test_RF=predict(classifier_RF, df_test)
pred_test_tree=predict(classtree,df_test,type="class")
pred_test_svr=predict(classifier,df_test)
df_test['price_range'] = pred_test_RF