
mydata <-data.frame(x=rnorm(100), y1=rnorm(100))
mydata$y2<-3*mydata$x+2
mydata$y3<-mydata$x+mydata$y1

cor(mydata$x,mydata$y1)
#-0.05580942

cor(mydata$x,mydata$y2)
#1
cor(mydata$x,mydata$y3)
#0.6491422

myresults<-data.frame(y1=numeric(),y2=numeric(),y3=numeric())

for(i in 1:100){
  mydata <-data.frame(x=rnorm(100), y1=rnorm(100))
  mydata$y2<-3*mydata$x+2
  mydata$y3<-mydata$x+mydata$y1
  
  #actually store the results into myresults
  myresults[i,1]<-cor(mydata$x,mydata$y1)
  myresults[i,2]<-cor(mydata$x,mydata$y2)
  myresults[i,3]<-cor(mydata$x,mydata$y3)
}

hist(myresults[,1])
hist(myresults[,2])
hist(myresults[,3])

par(mfrow=c(1,3))
plot(mydata$y1~mydata$x)
plot(mydata$y2~mydata$x)
plot(mydata$y3~mydata$x)

par(mfrow=c(1,1))

reg1<-lm(mydata$y1~mydata$x)
abline(reg1, col="red",lwd=2, lty="dashed")

reg2<-lm(mydata$y2~mydata$x)
abline(reg2, col="red",lwd=2, lty="dashed")

reg3<-lm(mydata$y3~mydata$x)
abline(reg3, col="red",lwd=2, lty="dashed")

mydata$x1<-rnorm(100,10,2)       
mydata$x2<-seq(1,100,1)       
mydata$y4<-mydata$x1+mydata$y1
mydata$y5<-mydata$x2+mydata$y1
cor(mydata$y4,mydata$x1)
cor(mydata$y5,mydata$x2)
cor(mydata$y3,mydata$x)

#the correlations differ

cor(mydata$y3,mydata$x1)
cor(mydata$y3,mydata$x2)
cor(mydata$y3,mydata$x)

mydata$y6<-rnorm(100,10,0.5)+mydata$x
mydata$y7<-rnorm(100,10,5)+mydata$x
cor(mydata$y6,mydata$x)
cor(mydata$y7,mydata$x)
cor(mydata$y3,mydata$x)

mydata$y8<-mydata$x1*mydata$x1+mydata$y3
cor(mydata$y8,mydata$x)

#Are the residuals normally distributed? x and y8

model<-lm(mydata$y8~mydata$x)

hist(model$residuals)
qqnorm(model$residuals)
