library(data.table)
maize_data1 <- fread("~/Google Drive/Graduate College Docs/PhD/Fall 2019/Data Mining 1/Project 3/maize_data.csv")

#maize_data1[is.na(maize_data1)] <- 0

maize_data1 = na.omit(maize_data1)

maize_data1 <- maize_data1[complete.cases(maize_data1), ]

predictors <- maize_data1[,10:15]

#10,15 without v1, geno, pop

#select subset then merge predictors 

response <- maize_data1[,7394]

predictors2 <- predictors[,1:2]
predictors3 <- predictors[,4]
predictors4 <- predictors[,6]

predictors2 <- cbind(predictors2, predictors3)
predictors2 <- cbind(predictors2, predictors4)

maize_data <- cbind(predictors2, response)


#use m7 m8 m10 m12 

#variable selection
maize_data[["DtoA"]] = factor(maize_data[["DtoA"]])

model1 <- glm(DtoA ~. , family = binomial(link = "logit"), na.action(na.omit),data = maize_data)


par(mfrow=c(2,2))
plot(model1)
summary(model1)


#model1 <- glm(DtoA ~. , family = binomial(link = "logit"), na.action(na.omit),data = maize_data)

#linear regression model
maize_data$DtoA <- as.numeric(maize_data$DtoA)

model2 <- lm(DtoA ~. , na.action(na.omit), data = maize_data)

par(mfrow=c(2,2))
plot(model2)
summary(model2)





