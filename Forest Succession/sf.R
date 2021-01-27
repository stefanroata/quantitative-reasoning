sf<-read.csv("forest.succession.csv",stringsAsFactors = F)
str(sf)
unique(sf$plot)
unique(sf$slope.position)
head(sf,13)
range(sf$age)
hist(sf$biomass,xlim=c(0,200),ylim=c(0,100))
#right-skewed

hist(sf$age)
#right-skewed, unimodal, most trees are between 5 to 10 years of age

summary(sf)
hist(log(sf$recruitment),xlim=c(2,7))

lm.sf<-lm(diversity~age, data=sf)

predict(lm.sf,newdata=data.frame(age=c(40,45,50,55)))
predict(lm.sf,newdata=data.frame(age=150))


hist(sf$age, main="Histogram of age",xlab="Age",col="green",ylim=c(0,140))
hist(sf$biomass, main="Histogram of biomass",xlab="Biomass",col="red",xlim=c(0,200),ylim=c(0,100))
plot(biomass~age, data=sf, col="orange",main="Plot of Biomass as a Function of Age")
reg<-lm(biomass~age, data=sf)
abline(reg,col="blue",lwd=3)
coef(reg)
#(Intercept)         age 
#25.830692    3.089637 
plot(residuals(reg)~predict(reg),main="Plot of residuals",col="darkmagenta")
qqnorm(residuals(reg))
cor(sf$biomass,sf$age)^2
#0.493464

predict(reg,newdata=data.frame(age=c(20,50,100,150)))

#1         2         3         4 
#87.62343 180.31254 334.79439 489.27624 
        

#ACTIVITY #2
sf.sum<-aggregate(biomass~age,data=sf, mean)
cor(sf.sum$age,sf.sum$biomass)
#0.9462366
cor(sf.sum$age,sf.sum$biomass)^2
#0.895

plot(biomass~age, data=sf, col="orange",main="Plot of Biomass as a Function of Age")
abline(reg,col="orange",lwd=3)
points(sf.sum$biomass~sf.sum$age,col="blue",pch=11)
#pch is points changes the shape of the points
reg2<-lm(sf.sum$biomass~sf.sum$age)
coef(reg2)
abline(reg2,col="blue")
#(Intercept)  sf.sum$age 
#27.267744    2.935907 
#the slope is smaller for the mean data
#ACTIVITY #3
hist(sf$recruitment,col="red")
hist(log(sf$recruitment),
     xlim=c(2,7), 
     ylim=c(0,140),
     col="red",
     main="Histogram of logarithm of Recruitment")
plot(log(recruitment)~age, 
     data=sf,
     xlab="Age in years",
     ylab="log of Recruitment",
     main="Log of Recruitment as a function of age",
     col="darkred")
cor(log(sf$recruitment),sf$age)
#-0.6611187
cor(log(sf$recruitment),sf$age)^2
#0.4370779
lm3<-lm(log(recruitment)~age, data=sf)
coef(lm3)
abline(lm3, lwd=3, col="darkred")
exp(predict(lm3,newdata=data.frame(age=c(0,50,100,150))))


reg<-lm(diversity~age, data=sf)
plot(diversity~age,
     data=sf,
     col="orange",
     xlab="Age",
     ylab="Diversity",
     main="Plot of Diversity vs Age")
abline(reg,lwd=3)
cor(sf$age,sf$diversity)^2
#0.4338494
coef(reg)
#(Intercept)         age 
#41.042428    1.087128 

sf$age1<-log(sf$age)

#x transformation
reg2<-lm(diversity~age1, data=sf)
plot(diversity~age1,
     data=sf,
     col="orange",
     xlab=" log of Age",
     ylab="Diversity",
     main="Plot of Diversity vs Age")
abline(reg2,lwd=3)
cor(sf$age1,sf$diversity)^2
#0.4750515
plot(residuals(reg2)~predict(reg2))
qqnorm(residuals(reg2))
coef(reg2)
predict(reg2,newdata=data.frame(age1=log(seq(0,35,5)))) #cannot log 0
#store predicted values for this regression
pred1<-predict(reg2,newdata=data.frame(age1=log(seq(0,35,5))))
#question 2d


#y transformation, squaring the values
sf$diversity1<-sf$diversity^2
reg3<-lm(diversity1~age, data=sf)
plot(diversity1~age,
     data=sf,
     col="orange",
     xlab="Age",
     ylab="Diversity^2",
     main="Plot of Diversity vs Age")
abline(reg3,lwd=3)
cor(sf$age,sf$diversity1)^2
#0.4358095
plot(residuals(reg3)~predict(reg3))
qqnorm(residuals(reg3))
coef(reg3)
sqrt(predict(reg3, newdata=data.frame(age=seq(0,35,5))))

plot(diversity~exp(age1), data=sf) #we come back to the original data

us<-sf[sf$slope.position=="US",]
ls<-sf[sf$slope.position=="LS",]
par(mfrow=c(1,2))
plot(diversity~age,
     data=us,
     col="orange",
     xlab="Age",
     ylab="Diversity",
     main="Plot of Diversity vs Age in Upper Slope")
plot(diversity~age,
     data=ls,
     col="darkmagenta",
     xlab="Age",
     ylab="Diversity",
     main="Plot of Diversity vs Age in Lower Slope")
#both look like they're thickening

us$age.log<-log(us$age)
ls$age.log<-log(ls$age)

plot(diversity~age.log,
     data=us,
     col="orange",
     xlab="Age",
     ylab="Diversity",
     main="Plot of Diversity vs Age in Upper Slope")
plot(diversity~age.log,
     data=ls,
     col="darkmagenta",
     xlab="Age",
     ylab="Diversity",
     main="Plot of Diversity vs Age in Lower Slope")

#fit the models

ls.lm<-lm(diversity~age.log,data=ls)
us.lm<-lm(diversity~age.log,data=us)
plot(residuals(ls.lm)~predict(ls.lm))
qqnorm(residuals(ls.lm))


plot(residuals(us.lm)~predict(us.lm))
qqnorm(residuals(us.lm))

par(mfrow=c(1,1))
plot(diversity~age.log,
     data=us,
     col="orange",
     xlab="log of Age",
     ylab="Diversity",
     main="Plot of Diversity vs Age")
points(diversity~age.log, data=ls, col="blue")
abline(us.lm, col="orange")
abline(ls.lm, col="blue")






