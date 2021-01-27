#surival rate class-wise
table(titanic$class[titanic$survived=="TRUE"])/table(titanic$class)*100

#age distribution in a histogram
max(titanic$age)#doesn't work because some passengers don't have an age
max(titanic$age, na.rm=T)

hist(titanic$age, breaks=seq(0,80,10))

summary(titanic$age)
hist(titanic$age[titanic$survived==FALSE], 
     labels=TRUE,col="lightblue", 
     xlab="Age of Titanic victims",
     main="Titanic victims by age",
     breaks=seq(0,80,10)
     )


summary(titanic)
IQR(titanic$age, na.rm=T)
q1<-quantile(titanic$age, 0.25, na.rm=T)
q3<-quantile(titanic$age, 0.75, na.rm=T)
q3-q1
titanic$total_price<-titanic$ticket_pound+titanic$ticket_shilling/20+titanic$ticket_penny/240
boxplot(titanic$total_price~titanic$survived)

hist(titanic$total_price)#skewed to the right
#we try to fix the boxes by unstretching them with log10
titanic$total_price<-log10(titanic$total_price)
boxplot(titanic$total_price~titanic$survived)

