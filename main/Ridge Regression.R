library(glmnet)
# data prepping for glmnet
data  = mtcars
head(data)
data = na.omit(data)

y = data$mpg
x = as.matrix(data[,-1])


model = glm(y ~ x)
summary(model)
par(mfrow = c(2,2))
plot(model)
par(mfrow = c(1,1))

fit2 = glm.fit(x,y)
summary(fit2)

y_predicted <- predict.glm(fit2)

# Sum of Squares Total and Error
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)

# R squared
rsq <- 1 - sse / sst
rsq
plot(y - y_predicted)
abline(0,0)



#finding optimal lambda for shrinkage penalty using cross validation
cv_fit <- cv.glmnet(x, y, alpha = 0, lambda = NULL)
plot(cv_fit)
opt_lambda <- cv_fit$lambda.min
opt_lambda

# get all fitted models
fit <- cv_fit$glmnet.fit
summary(fit)



y_predicted2 <- predict.glmnet(fit, s = -15, newx = x)

# Sum of Squares Total and Error
sst2 <- sum((y - mean(y))^2)
sse2 <- sum((y_predicted2 - y)^2)

# R squared
rsq2 <- 1 - sse2 / sst2
rsq2

par(mfrow = c(1,2))
plot(y - y_predicted)
abline(0,0)
plot(y - y_predicted2)
abline(0,0)
var(y - y_predicted)
var(y - y_predicted2)





#############
# clear variables, close windows
rm(list=ls(all=TRUE))
graphics.off()

library(glmnet)
library(ACSWR)
#data("chemicaldata")


data("cardata")
cardata

carc = cardata

str(carc)

#data = cardata[,c(2,3,6,7,8,9,10,11,12,13)]
#data = na.omit(data) # there was nothing missing in any of the selected predictors

carc$R78        = as.numeric(as.character(carc$R78))
carc$R77        = as.numeric(as.character(carc$R77))
carc$C.EU       = as.numeric(carc$C=="Europe")
carc$C          = as.numeric(carc$C=="US")
names(carc)[13] = "C.US"

y     = as.numeric(carc$P)
x     = as.matrix(carc[,-1])

# delete missing values
keep  =! is.na(apply(x,1,sum))
y     = y[keep]
x     = x[keep,]
labs  = colnames(x)

res   = glmnet(x,y)

# cross validation for ridge (alpha = 0)
cv.res    = cv.glmnet(x,y, alpha = 0)
cv.lambda = cv.res$lambda.min # leave-one-out CV
plot(cv.res)
cv.lambda
layout(matrix(c(1,2), 2, 1),heights=c(0.6,0.4))

# CV
plot(cv.res,ylab="binomial deviance",xlab=expression(log(lambda)),xlim=c(-11.5,-1.8))

res2 = glmnet(x,y,family="binomial",lambda=cv.lambda)

#                s0
# M    -0.050983693
# R78   .          
# R77   .          
# H    -0.221051891
# R     0.106341322
# Tr    .          
# W     0.002442590
# L     .          
# T     .          
# D     0.006945569
# G     .          
# C.US -3.987223235
# C.EU  2.095450692

data=data.frame(y,x)

# general linear model: logit
summary(glm(y~.,data=data,family="binomial"))

# Warning messages:
# 1: glm.fit: algorithm did not converge 
# 2: glm.fit: fitted probabilities numerically 0 or 1 occurred 

summary(glm(y~M+H+R+W+D+C.US+C.EU,family="binomial",data=data))


###################
################
##################
##############

# alpha represents the elastic net parameter
#alpha=1 is the lasso penalty, and alpha=0 the ridge penalty.
cv_fit <- cv.glmnet(x, y, alpha = 0, lambda = NULL)
plot(cv_fit)
opt_lambda <- cv_fit$lambda.min
opt_lambda

# get all fitted models
fit <- cv_fit$glmnet.fit
summary(fit)


y_predicted <- predict.glmnet(fit, s = 0.0000001, newx = x)

# Sum of Squares Total and Error
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)

# R squared
rsq <- 1 - sse / sst
rsq

par(mfrow = c(1,1))
plot(y - y_predicted)
abline(0,0)

