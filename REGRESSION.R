
df<-data.frame(age=rnorm(100), income=NA)

df$income<-df$age*3.5+7
plot(age~income, data=df)
cor(df$age, df$income)

reg<-lm(age~income, data=df)
coef(reg)
summary(reg)
b<-reg$coefficients[2]
plot(residuals(reg)~predict(reg))
hist(residuals(reg))
qqnorm(residuals(reg))
predict(reg, newdata=data.frame(income=c(5,10, 15, 20)))
5*0.2857143 -2

trials<-1000
results<-numeric(trials)
for(i in 1:trials){
  reg_new<-lm(sample(age)~income, data=df)
  results[i]<-reg_new$coefficients[2]
}
hist(results,xlim=c(-1,1))

abline(v=b, col="red")
results2<-numeric(trials)

for(i in 1:trials){
  results2[i]<-cor(sample(df$age), df$income)
}
hist(results2)
