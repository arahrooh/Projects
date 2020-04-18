library(readxl)
har <- read_excel("~/Google Drive/Graduate College Docs/PhD/Fall 2019/Data Mining 1/Project 2/har.xlsx")
head(har)
har[is.na(har)] <- 0
smp_size <- floor(0.80 * nrow(har))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(har)), size = smp_size)

train <- har[train_ind, ]
testing <- har[-train_ind, ]

train = as.data.frame(train);
testing = as.data.frame(testing);

train[["class"]] = factor(train[["class"]])
testing[["class"]] = factor(testing[["class"]])

library(naivebayes)

model.nb.train <- naive_bayes(class ~.,data=train[,5:17])

nb.pred.testing <- predict(model.nb.train, testing[,5:16], type="class")#Get the fitted labels

table(testing[,17],nb.pred.testing) 

######Multinomial Logistic Regression

model.mlr.train <- glm(class ~.,family=binomial(link='logit'),data=train[,5:17])

library(nnet) # Import neural network package, which has the "multinom" package

train[,1:16] <- scale(train[,5:16]) # Scale the "total" predictor (so that it has mean 0 and standard deviation 1)

train.multinom <- multinom(class ~., data=train[,5:17], maxit = 1000) # Fit a multinomial logistic regression model to the predictor

testing.multinom.pred <- predict(train.multinom, testing[,5:16] ,type="class") # Use the training set to predict the digits

table(testing[,17],testing.multinom.pred) # Display predictions in a confusion matrix



library(ISLR) 
library(e1071) 

svm_linear.model <- svm(class ~ ., data=train[,5:17] , kernel ="linear",cost=.01)

svm_linear.pred <- predict(svm_linear.model, testing[,5:16])

table(testing[,17],svm_linear.pred) 

library(caret)

naivebayes <- confusionMatrix(testing[,17],nb.pred.testing)

svm_matrix <- confusionMatrix(testing[,17], svm_linear.pred)

mlr_matrix <- confusionMatrix(testing[,17], testing.multinom.pred)

library(pROC)
multiclass.roc(testing[,17], nb.pred.testing, plot = TRUE)




