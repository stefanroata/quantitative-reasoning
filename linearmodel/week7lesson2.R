
#24 students each rolling a die 10 times, the chance of rolling a 1 is 1/6
rbinom(24,10,1/6)

#30 students each flipping a fair coin 20 times and counting the number of heads

rbinom(30,20,1/2)

#simulate 100 students rolling a fair die 40 times and
#counted how many times they rolled a one or a two
#prob=2/6=1/3

rbinom(100, 40, 1/3)

x<-rnorm(1000, 10, 3)
z<-rnorm(1000)

z[sample(1:1000, 3)]<-c(6,-9,12)
y=x+z
plot(y~x)
reg<-lm(y~x)
abline(reg)

x1<-x[!(x>15 & y<25)]
y1<-y[!(x>15 & y<25)]
plot(y1~x1)
reg<-lm(y1~x1)
abline(reg)
