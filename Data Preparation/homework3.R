library(readr)
music <- read.table("YearPredictionMSD.txt", sep=",")
#data is read in 
#now we apply a transformation for all the predictors 

#v1 is the target variable
predictors <- music[,2:91]
target <- music[,1]
target <- as.data.frame(target)

#transformation 1
#scale and center transformation 
scaling <- scale(predictors, center = TRUE, scale = TRUE)
scaling <- as.data.frame(scaling)
head(scaling)

#transformation 2
#square transformation 
square <- predictors^2
head(square)

#transformation 3
#square root transformation 
#does not work since we have negative values 
square_root <- sqrt(predictors)
head(square_root)

#transformation 4
#log transformation 
#does not work since we have values outside the log domain
logging <- log(predictors)
head(logging)

#transformation 5 
#no transformation 
#I will just take the raw data and apply a regression model in part 3


#running PCA on untransformed data and providing visualizations
pca <- prcomp(predictors)


p.variance <- pca$sdev^2 / sum(pca$sdev^2)
barplot(100*p.variance[1:7],las = 3, ylim = c(0,60), 
        ylab = '% Variance Explained', main = "Histogram of Top 7 Principal Components", 
        col = rainbow(20), xlab = 'Principal Components (1-7)')



#######
#Regression Analysis
#####

#train set : first 463715 rows
#test set: last 51630 rows


# Function that returns Root Mean Squared Error
rmse <- function(error)
{
  sqrt(mean(error^2))
}

#running regression for untransformed predictors
untransformed_test <- music[463716:515345,]

model1 <- lm(V1 ~., data = untransformed_test)
summary(model1)
rmse_model1 <- rmse(model1$residuals)
#9.479

#running regression for transformed predictors using square transformations
transformed_test <- cbind(target, square)
transformed_test <- transformed_test[463716:515345,]

model2 <- lm(target ~., data = transformed_test)
summary(model2)
rmse_model2 <- rmse(model2$residuals)
#10.23


#running regression for principal components 

library(pls)

train_music <- music[1:463715,]
y_test <- music[463716:515345,1]
test_music <- music[463716:515345, 2:91]

pcr_model <- pcr(V1 ~., data = train_music, scale = TRUE, validation = "CV")
validationplot(pcr_model, main = "Root Mean Squared Error Prediction Plot")
pcr_pred <- predict(pcr_model, test_music, ncomp = 4)
sqrt(mean((pcr_pred - y_test)^2))
#10.66
