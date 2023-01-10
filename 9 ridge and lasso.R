#(1)
#a
library(MASS)
library(faraway)
data(seatpos)
dim(seatpos)
modridge=lm.ridge(hipcenter~.,lambda = seq(0,100,0.5),data = seatpos)
matplot(modridge$lambda,t(modridge$coef),type = "l")
select(modridge)
which.min(modridge$GCV)
rmse <- function(x,y) sqrt(mean((x-y)^2))
yfit = cbind(1,as.matrix(seatpos[,-9])) %*% coef(modridge)[46,]
rmse(yfit, seatpos$hipcenter)
DFTest <- data.frame( HtShoes=181.080, Ht=178.560, Seated=91.440, Arm=35.640, Thigh=40.950, Leg=38.7,Age=64.800, Weight=263.700)
ypre = cbind(1,as.matrix(DFTest)) %*%coef(modridge)[46,]
ypre

#b
modlm <- lm(hipcenter ~.,seatpos)
summary(modlm)
round(cor(seatpos[,-9]),2)
coef(modridge)[46,]
summary(modridge)
#(2)
data(fat)
dim(fat)
test = seq(10,252,by=10)
fattrain<-fat[-test,]
fattest<-fat[test,]
#(a)
g1 <-lm(siri~.-brozek -density, fattrain)
summary(g1)
rmse(g1$fit, fattrain$siri)
pred1 <- predict(g1, fattest)
rmse(pred1,fattest$siri)
#(b)
gridge=lm.ridge(siri ~.-brozek -density,lambda=seq(0,10,1e-4),data = fattrain)
matplot(gridge$lambda,t(gridge$coef),type = "l")
select(gridge)
which.min(gridge$GCV)
yfit = cbind(1,as.matrix(fattrain[,4:18])) %*% coef(gridge)[468,]
rmse(yfit, fattrain$siri)
ypred = cbind(1,as.matrix(fattest[,4:18])) %*% coef(gridge)[468,]
rmse(ypred,fattest$siri)
#c
install.packages("lars", repos = "http://cran.us.r-project.org")
library("lars")
set.seed(123)
lmod <- lars(as.matrix(fattrain[,4:18]),fattrain$siri)
plot(lmod)
cvlmod <- cv.lars(as.matrix(fattrain[,4:18]),fattrain$siri)
cvlmod$index[which.min(cvlmod$cv)]
predict(lmod,s=0.77778,type="coef",mode="fraction")$coef
predlars <- predict(lmod,fattest[,4:18],s=0.77778,mode="fraction")
fitlars <- predict(lmod,fattrain[,4:18],s=0.77778,mode="fraction")
rmse(fattrain$siri, fitlars$fit)
rmse(fattest$siri, predlars$fit)


