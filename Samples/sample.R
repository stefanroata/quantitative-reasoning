sample(x=1:3, size=2)
sample(x=3, size=2)
mtcars[sample(x=nrow(mtcars), size=2), ]


#dice rolls

sample(1:6, size=1, replace=T)
sample(6, size=1, replace=F)

die.rolls<-sample(1:6, size=20, replace=T)
die.rolls

dice<-sample(1:6, size=3, replace=T)
mean(dice)

trials<-20
numeric(4)
res<-numeric(trials)

for(i in 1:trials){
  dice<-sample(1:6, size=3, replace=T)
  res[i]<-mean(dice)
  
}

res

table(res)

trials<-20
d<-5
res<-matrix(NA, trials, d)

for(i in 1:trials){
  res[i,]<-sample(1:10, 5, replace=F)
}

res[2,]
res[,5]

#in-class code

sample(1:10, size=5, replace=T)
sum(sample(1:10, size=5, replace=T))

#sample size=5
#sample statistic=the sum
#population=the infinite set attained if we were to keep repeating the sample an infinite number of times
#population parameter= the mean(miu)
#you would want to simulate your code x number of times (100? 1000?)


sum(sample(1:6, size=2, replace=T))
res<-numeric(100)
for(i in 1:100){
  res[i]<-sum(sample(1:6, size=2, replace=T))
}
hist(res, col="red", main="Histogram of the results of the simulation", 
     xlab="The sum of the two dice")





res<-numeric(100000)
for(i in 1:100000){
  res[i]<-sum(sample(1:6, size=2, replace=T))
}
hist(res, col="red", main="Histogram of the results of the simulation", 
     xlab="The sum of the two dice")
barplot(table(res)/length(res), col="purple", main="Barplot of the frequencies of the sum of the dice", ylim=c(0,0.2))
grid()
mean(res)
sd(res)

pop<-read.csv("population.csv")

mean(sample(x=pop$height, size=50))
hist(sample(x=pop$height, size=50))

SRS<-pop[sample(x=pop$height, size=50),]

mean(pop$height)

mean(SRS$height)

mean(pop$height)-mean(SRS$height)

height.res<-numeric(10000)
height.sds<-numeric(10000)
for(i in 1:10000){
  
  SRS<-pop[sample(x=nrow(pop), size=50),]
  height.res[i]<-mean(SRS$height)
  height.sds[i]<-sd(SRS$height)
}
hist(height.res)
hist(height.sds)
qqnorm(height.res)
qqline(height.res)



height.res<-numeric(10000)
height.sds<-numeric(10000)
for(i in 1:10000){
  
  SRS<-pop[sample(x=nrow(pop), size=3),]
  height.res[i]<-mean(SRS$height)
  height.sds[i]<-sd(SRS$height)
}
hist(height.res)
hist(height.sds)
qqnorm(height.res)
qqline(height.res)






height.res<-numeric(100000)
height.sds<-numeric(100000)
for(i in 1:100000){
  
  SRS<-pop[sample(x=nrow(pop), size=75),]
  height.res[i]<-mean(SRS$height)
  height.sds[i]<-sd(SRS$height)
}
hist(height.res)
hist(height.sds)
qqnorm(height.res)
qqline(height.res)
mean(height.res)
sd(height.res)






