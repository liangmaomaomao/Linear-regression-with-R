# Question 1
library(faraway)
data(teengamb)
teengamb$sex<-factor(teengamb$sex)
levels(teengamb$sex) <- c("male","female")
summary(teengamb)
ggplot(teengamb,aes(status,gamble,colour = sex))+geom_point()+theme(legend.position = "bottom")
## comment: For males, if their family status is low, children gamble amount would relative high. However, females gamble amount remain low no matter their family status. 
ggplot(teengamb,aes(income,gamble,colour = sex))+geom_point()
## comment: For males, There is positive relation between children's gamble and income. However, females gamble amount remain low no matter their income.
ggplot(teengamb,aes(verbal,fill=sex))+geom_bar(stat = 'count', position = 'stack')+scale_fill_brewer(palette= 'Accent')+scale_x_continuous(breaks = seq(1,10,1))
teengamb$verbal=factor(teengamb$verbal)
ggplot(teengamb,aes(verbal,gamble))+geom_boxplot()
## it is hard to say there exists relation between verbal and gamble
lmtn<-lm(gamble~sex+income+status,teengamb)
summary(lmtn)
lmtn<-lm(gamble~sex+income,teengamb)
summary(lmtn)

# Question 2
mean(teengamb$income[teengamb$sex=="male"])
mean(teengamb$income[teengamb$sex=="female"])
median(teengamb$income[teengamb$sex=="male"])
median(teengamb$income[teengamb$sex=="female"])
mean(teengamb$gamble[teengamb$sex=="male"])
mean(teengamb$gamble[teengamb$sex=="female"])
median(teengamb$gamble[teengamb$sex=="male"])
median(teengamb$gamble[teengamb$sex=="female"])

## comment: Income data: for males and females, the median income is lower than mean income no matter the sex.
##          Gamble data: for males and females, the median gamble is lower than mean gamble no matter the sex.
## because there are some samples that is too large, which means outlying data.

# Question 3
unique(teengamb$verbal)
length(unique(teengamb$verbal))
# 9
boxplot(teengamb$verbal)
# class 1

# Question 4
ggplot(teengamb,aes(x=sex,y=gamble))+geom_boxplot()
t.test(teengamb$gamble~teengamb$sex)
## comment: Females generally don't like to gamble. However, males are more likely to gamble, and the variance between individuals is more large. From the boxplot, the quantile range of female is really small, and the median gamble of female is at a low level. But the males' quantile range is larger, and the median of males' gamble is also higher.  

ggplot(teengamb,aes(x=sex,y=status))+geom_boxplot()
t.test(teengamb$status~teengamb$sex)
## comment: Females status is relative lower than the males, and the variance of female status is also lower than males, which is consistant to the gamble situation. I think the sample is a little biased, because female and males families tend to have equal status in reality.

ggplot(teengamb,aes(sex,verbal))+geom_boxplot()
teengamb$verbal<-as.character(teengamb$verbal)
t.test(teengamb$verbal~teengamb$sex)
## comment: There is no much mean difference in verbal for different sex.

ggplot(teengamb,aes(sex,income))+geom_boxplot()
t.test(teengamb$income~teengamb$sex)
## comment: There is no much difference in income for different sex. 

