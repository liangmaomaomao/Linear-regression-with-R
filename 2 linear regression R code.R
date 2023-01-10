## problem 1
# (1)/(2)
library(faraway)
data(uswages)
uswages[1:5,]
which(uswages$exper<0)
uswages=uswages[which(uswages$exper>=0),]
lm<-lm(wage~educ+exper,uswages)
summary(lm)
# (3)
which.max(lm$residuals)
# (4)
mean(lm$residuals)
median(residuals(lm))
hist(lm$residuals)
help("residuals")
# (5)
a=6
b=3
predict(lm,newdata = data.frame(educ= a, exper= b))-predict(lm,newdata = data.frame(educ= a, exper= b-1))
# (6)
cor(lm$residuals,lm$fitted.values)
par(mfrow=c(2,2))
plot(lm)


## problem3
x <- 1:20 
y <- x+rnorm (20)
lm<-lm(y~x)
summary(lm)
z<-model.matrix(~x)
B=solve(t(z)%*%z)%*%t(z)%*%y

lm1<-lm(y~x+I(x^2))
summary(lm1)
z1<-model.matrix(~x+I(x^2))
B=solve(t(z1)%*%z1)%*%t(z1)%*%y
B

lm2<-lm(y~x+I(x^2)+I(x^3))
summary(lm2)
z2<-model.matrix(~x+I(x^2)+I(x^3))
B=solve(t(z2)%*%z2)%*%t(z2)%*%y
B

lm3<-lm(y~x+I(x^2)+I(x^3)+I(x^4))
summary(lm3)
z3<-model.matrix(~x+I(x^2)+I(x^3)+I(x^4))
B=solve(t(z3)%*%z3)%*%t(z3)%*%y
B

n=5
lmn<-lm(y~poly(x,n,raw=TRUE))
summary(lmn)
zn<-model.matrix(~poly(x,n,raw=TRUE))
B=solve(t(zn)%*%zn)%*%t(zn)%*%y
B