#final project 

#no ICA 

#classification and prediction on EEG data one subject

#subject 1 reading in data 
library(data.table)
level_05_S1 <- fread("~/Desktop/Dr. Huang/Data/LIFE stats project/csv/Ll01/Ll01_level_050.csv")
level_075_S1 <- fread("~/Desktop/Dr. Huang/Data/LIFE stats project/csv/Ll01/Ll01_level_075.csv")
level_100_S1 <- fread("~/Desktop/Dr. Huang/Data/LIFE stats project/csv/Ll01/Ll01_level_100.csv") 
level_125_S1<- fread("~/Desktop/Dr. Huang/Data/LIFE stats project/csv/Ll01/Ll01_level_125.csv") 
level_selfpace_S1 <- fread("~/Desktop/Dr. Huang/Data/LIFE stats project/csv/Ll01/Ll01_level_selfpaced.csv") 

S1_05 <- level_05_S1[,50001:100000]
S1_075 <- level_075_S1[,50001:100000]
S1_100 <- level_100_S1[,50001:100000]
S1_125 <- level_125_S1[,50001:100000]
S1_selfpace <- level_selfpace_S1[,50001:100000] 

S1 <- rbind(S1_05, S1_075)
S1 <- rbind(S1, S1_100)
S1 <- rbind(S1, S1_125)
S1 <- rbind(S1, S1_selfpace)
S1 <- t(S1)
S1 = as.data.frame(S1);

smp_size <- floor(0.0005 * nrow(S1))
set.seed(999)
train_ind <- sample(seq_len(nrow(S1)), size = smp_size)
train <- S1[train_ind, ]

train = as.data.frame(train);
train = t(train)

Speeds = as.factor(c(rep("speed1",128),rep("speed2",128),rep("speed3",128),rep("speed4",128),rep("speed5",128)))

train = cbind(train, Speeds)



#80% split 
set.seed(999)
data1 = sort(sample(nrow(train), nrow(train)*.8))

#creating training data set by selecting the output row values
training<-train[data1,]
testing<-train[-data1,]

training <- as.data.frame(training)
testing <- as.data.frame(testing)
training[["Speeds"]] = factor(training[["Speeds"]])
testing[["Speeds"]] = factor(testing[["Speeds"]])


#naive bayes model 
library(naivebayes)

model.nb.training <- naive_bayes(Speeds ~.,data=training)
nb.pred.testing <- predict(model.nb.training, testing[,1:25], type="class")#Get the fitted labels
table(nb.pred.testing,testing[,26]) 

#bagging model 
library(randomForest)
set.seed(1)
model.bagging.training  <- randomForest(Speeds ~.,data= training)# This is a bagging tree model.
bagging.pred.testing  = predict(model.bagging.training, testing[,1:25], type="class")#Get the fitted labels
table(bagging.pred.testing, testing[,26]) 

#boosting model 

library(rpart)
library(adabag)
model.boosting.training <- boosting(Speeds ~.,data=training, boos  = TRUE, mfinal=100)# This is a boosting tree model.

boosting.pred.testing =predict(model.boosting.training ,newdata = testing[,1:26], n.trees=600) 
summary(boosting.pred.testing) 
boosting.pred.testing$confusion


#random forest model 

model.RF.training <- randomForest(Speeds ~.,data=training, mtry = 10)# This is a Random Forest (RF) tree model. p = 5 and sqrt(5) = 2.2,   
RF.pred.testing = predict(model.RF.training, testing[,1:25], type="class")#Get the fitted labels
table(RF.pred.testing, testing[,26]) 

#logistic regression

model.mlr.train <- glm(Speeds ~.,family=binomial(link='logit'),data=training)

library(nnet) 

training.multinom <- multinom(Speeds ~., data= training, maxit = 1000) # Fit a multinomial logistic regression model to the predictor

testing.multinom.pred <- predict(training.multinom, testing[,1:25] ,type="class") # Use the training set to predict the digits

table(testing.multinom.pred,testing[,26]) # Display predictions in a confusion matrix

#support vector machine linear 

library(ISLR) 
library(e1071) 

svm_linear.model <- svm(Speeds ~ ., data= training , kernel ="linear",cost=.1)

svm_linear.pred <- predict(svm_linear.model, testing[,1:25])

table(svm_linear.pred, testing[,26]) 

#support vector machine RBF 

library(ISLR) 
library(e1071) 

svm_RBF.model <- svm(Speeds ~ ., data= training , kernel ="radial",cost=.5,gamma=.1,scale=TRUE)

svm_RBF.pred <- predict(svm_RBF.model, testing[,1:25])

table(svm_RBF.pred, testing[,26]) 

