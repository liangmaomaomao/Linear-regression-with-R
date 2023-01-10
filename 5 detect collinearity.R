#(1)
#a
library(faraway)
data(divusa)
lm<-lm(divorce~unemployed+femlab+marriage+birth+military,divusa)
summary(lm)
X <- model.matrix(lm)[, -1]
e <- eigen(t(X) %*% X)
e$val
round(sqrt(e$val[1]/e$val), 3)
#b
round(vif(X), 3)
#c
lm1<-lm(divorce~femlab+marriage+birth,divusa)
X1<- model.matrix(lm1)[, -1]
e <- eigen(t(X1) %*% X1)
round(sqrt(e$val[1]/e$val), 3)
round(vif(X1), 3)
#summary(lm1)


#(2)
#a
data(longley)
lmm<-lm(Employed~.,longley)
summary(lmm)
XX <- model.matrix(lmm)[, -1]
ee <- eigen(t(XX) %*% XX)
ee$val
round(sqrt(ee$val[1]/ee$val), 3)
#b
round(cor(longley[,-7]),2)
#c
round(vif(XX), 3)
