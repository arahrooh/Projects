library(quadprog)
library(Matrix)

# Load in the data

Housing.df = read.csv("BostonHousing.csv")

head(Housing.df )

dim(Housing.df)

housing.y= Housing.df[,14]

housing.y[housing.y==0]=-1

housing.x = Housing.df[,c(1,3,5:12)]


X = housing.x

y = as.numeric(housing.y)

svm.fit = function(X, y, C=10, gamma) {
  n.samples = nrow(X)
  n.features = ncol(X)
  gamma = -0.5/gamma^2
  K = matrix(rep(0, n.samples*n.samples), nrow=n.samples)
  for (i in 1:n.samples){
    for (j in 1:n.samples){
      K[i,j] = exp(gamma*(unlist(X[i,]) - unlist(X[j,]))%*%(unlist(X[i,])-unlist(X[j,])))}}
  Dmat = outer(y,y) * K
  Dmat = as.matrix(nearPD(Dmat)$mat) 
  dvec = rep(1, n.samples)
  Amat = rbind(y, diag(n.samples), -1*diag(n.samples))
  bvec = c(0, rep(0, n.samples), rep(-C, n.samples))
  res = solve.QP(Dmat,dvec,t(Amat),bvec=bvec, meq=1)
  a = res$solution 
  bomega = apply(a*y*X,2,sum)
  return(bomega)
}

standardize = function(z) (z-mean(z))/sd(z)

for(j in 1:dim(housing.x)[2]) X[,j] = standardize(housing.x[,j])

X = cbind(1,housing.x)

y = housing.y

housing.svm.betas = svm.fit(X,y,C=10, gamma = 0.5)

y_pred = sign(as.matrix(X)%*%matrix(housing.svm.betas,(dim(housing.x)[2]+1),1))

table(y,pred=y_pred)
library(e1071)
svm_tune <- tune(svm.fit, X, y, ranges=list(C=10^(-1:2), gamma=c(.5,1,2)))
print(svm_tune)

#After tuning:
scale_house <- as.matrix(scale(Housing.df, center=T,scale=F))

post_tune <- svm(formula= MEDV~., data = scale_house, kernel="radial", cost=10, gamma=0.5)
summary(post_tune)

y_pred = sign(as.matrix(X)%*%matrix(svm(formula= MEDV~., data = scale_house, kernel="radial", cost=10, gamma=0.5))
table(y,pred=y_pred)
tuner <- (tune(svm.fit, train.x=X, train.y=y, ranges=list(C=10^(-1:2))))
              