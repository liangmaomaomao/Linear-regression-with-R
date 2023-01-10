#(1)
#a
data(sat)
olslm<-lm(total ~ takers + ratio + salary + expend, data=sat)
summary(olslm)
#b
install.packages('quantreg')
library(quantreg)
lad <- rq(total ~ takers + ratio + salary + expend, data=sat)
summary(lad)
#c
library(MASS)
huberlm <- rlm(total ~ takers + ratio + salary + expend, data=sat)
summary(huberlm)

#(2)
#a
data("cheddar")
lm<-lm(taste ~ Acetic + H2S + Lactic, data=cheddar)
summary(lm)
cheddar$time <-1:nrow(cheddar)
plot(residuals(lm) ~ time, cheddar, main ="Residuals versus time for simple linear model")
#b
library(nlme)
glm <- gls( taste ~ Acetic+H2S + Lactic,  correlation=corAR1(form=~time),  data=cheddar)
summary(glm)
intervals(glm)
#c
lm.fit <- lm(taste ~ ., data=cheddar)
summary(lm.fit)
