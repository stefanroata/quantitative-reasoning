geyser<-read.csv("Old_Faithful_geyser_data.csv",stringsAsFactors = F)

summary(geyser)
#average eruption time is 3.5 minutes

str(geyser)
hist(geyser$eruptions)
hist(geyser$waiting)
head(geyser)

#fitting a linear model; see if a longer waiting time is predictive of a longer eruption

plot(geyser$eruptions~geyser$waiting, xlab="Waiting Time",ylab="Eruption Time",main="Eruption Time as a Function of Waiting Time")
reg<-lm(geyser$eruptions~geyser$waiting)
abline(reg, col="red",lwd=4)
coef(reg) #the coefficients
#(Intercept)  geyser$waiting  
#-1.60463         0.07162 

cor(geyser$eruptions,geyser$waiting)^2
#r^2 equals 0.761, pretty strong

plot(residuals(reg)~predict(reg),main="Residual Plot")
hist(residuals(reg))

#second linear model without the outlier!!!
geyser2<-geyser[geyser$waiting<100,]
plot(geyser2$eruptions~geyser2$waiting, xlab="Waiting Time",ylab="Eruption Time",main="Eruption Time as a Function of Waiting Time")
reg2<-lm(geyser2$eruptions~geyser2$waiting)
#(Intercept)  geyser2$waiting  
#-1.87402          0.07563  
# WITHOUT THE OUTLIER
abline(reg2, col="red",lwd=4)
cor(geyser2$eruptions,geyser2$waiting)^2
#R^2 is 0.811 without the outlier

#conditions to check it the plot of residuals is normal
hist(residuals(reg2))
qqnorm(residuals(reg))
qqnorm(residuals(reg2))


#what  happens when you flip x and y

plot(geyser2$waiting~geyser2$eruption, ylab="Waiting Time",xlab="Eruption Time",main="Waiting Time as a Function of Eruption")
reg3<-lm(geyser2$waiting~geyser2$eruption)
abline(reg3, col="darkmagenta",lwd=4)
coef(reg3)
#correlation is evidently the same
cor(geyser2$waiting,geyser2$eruption)^2
plot(residuals(reg3))
hist(residuals(reg3))
qqnorm(residuals(reg3))


#waiting time above 70

geyser3<-geyser[geyser$waiting>70,]

plot(geyser3$eruptions~geyser3$waiting,ylab="Eruption Time", xlab="Waiting Time",
     main="Plot of eruptions with waiting time above 70")
reg4<-lm(geyser3$eruptions~geyser3$waiting)
abline(reg4, col="darkmagenta",lwd=4)
coef(reg4)
cor(geyser3$eruptions,geyser3$waiting)^2

reg5<-lm(eruptions~waiting, data=geyser3[geyser3$waiting<105,]) #without the outlier
abline(reg5, col="red",lwd=4)
