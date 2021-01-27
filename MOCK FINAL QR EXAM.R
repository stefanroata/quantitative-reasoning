
#PRELIMINARY QUESTION: When doing random trials to test if a certain relaionship is significant or not,
#should we focus on correlation or the slope of the linear model?
#I mean, what are we storing in the results vector: correlations or slopes of random linear models?

# sampling, experimental design, significance (practical and statistical) ; relative and absolute risk


advert<-read.csv("advert.csv", stringsAsFactors = F)


#Challenge #1
summary(advert$tv)
#Min. 1st Qu.  Median    Mean   3rd Qu.    Max. 
#4.820   8.018  11.075  11.978  15.905  29.410 
sd(advert$tv)
#5.067981
IQR(advert$tv)
#7.8875

#the same applies to radio, newspaper, biscuits, age, income

#QUESTION: Are there any other summary statistics that I need to compute?

#Challenge #2, histograms

hist(advert$tv,
     xlim=c(0,30),
     col="green",
     main="Histogram of Money Spent on TV Advertising",
     xlab="Money Spent on TV Advertising (in units of 1000 USD)")

hist(advert$radio,
     col="yellow",
     ylim=c(0,70),
     main="Histogram of Money Spent on Radio Advertising",
     xlab="Money Spent on Radio Advertising (in units of 1000 USD)")

#Challenge #3, biscuit sales and age

plot(biscuits~age,
     data=advert,
     main="Plot of Biscuit Sales vs. Age",
     col="darkblue",
     xlab="Age",
     ylab="Biscuits Sales (in units of 1000 USD")


plot(sqrt(biscuits)~age,
     data=advert,
     main="Plot of Biscuit Sales vs. Age",
     col="darkblue",
     xlab="Age",
     ylab="Biscuits Sales (in units of 1000 USD")
# virtually no difference with square root

plot(log(biscuits)~age,
     data=advert,
     main="Plot of Biscuit Sales vs. Age",
     col="darkblue",
     xlab="Age",
     ylab="Biscuits Sales (in units of 1000 USD")
#some difference?

cor(log(advert$biscuits), advert$age)
#0.4250855 - correlation even lower, not the right way


plot((1/biscuits)~age,
     data=advert,
     main="Plot of Biscuit Sales vs. Age",
     col="darkblue",
     xlab="Age",
     ylab="Biscuits Sales (in units of 1000 USD")
cor(1/advert$biscuits, advert$age)
#-0.3136056, I messed it up


plot((-1)/sqrt(biscuits)~age,
     data=advert,
     main="Plot of Biscuit Sales vs. Age",
     col="darkblue",
     xlab="Age",
     ylab="Biscuits Sales (in units of 1000 USD")



plot(log(biscuits)~age^2,
     data=advert,
     main="Plot of Biscuit Sales vs. Age",
     col="darkblue",
     xlab="Age",
     ylab="Biscuits Sales (in units of 1000 USD")

cor(log(advert$biscuits), log(advert$age))


hist(advert$biscuits)
hist(advert$age)
#Challenge #4, linear model

#both variables (age and income) are quantitative
#there are no high outliers (?) -NO
#the spread seems consistent (the plot does not thicken)
#the shape is straight enough (?)

r<-cor(advert$biscuits, advert$age)
# 0.4634294 - there is moderately strong correlation
r^2
# 0.2147669 - r squared is low

reg1<-lm(biscuits~age, data=advert)
abline(reg1, lwd=2, col="red")

#Check the plot of residuals

plot(residuals(reg1)~predict(reg1),
     main="Plot of Residuals",
     ylab="Residual Value",
     xlab="Predicted Value")
abline(h=0, col="magenta")
#THERE IS A BEND!!!! LINEAR MODEL NOT A GOOD FIT FOR THESE DATA!!
#also, they were supposed to be equally distributed around the residual value of 0, but they are not

hist(residuals(reg1))
#histogram skewed to the right
qqnorm(residuals(reg1))
#NOT NORMAL
#RESIDUALS DO NOT HAVE TO BE NORMALLY DISTRIBUTED!!!!!!

trials<-1000
results<-numeric(trials)


#Challenge #5
#do the same for markets in the south
advert_south<-advert[advert$region=="S",]

plot(biscuits~age,
     data=advert_south,
     main="Plot of Biscuit Sales vs. Age",
     col="darkblue",
     xlab="Age",
     ylab="Biscuits Sales (in units of 1000 USD")

cor(advert_south$biscuits, advert_south$age)
#0.4611655 - almost the same as the overall correlation
cor(advert_south$biscuits, advert_south$age)^2
#0.2126737 - again, r squared is low

reg2<-lm(biscuits~age, data=advert_south)
abline(reg2, lwd=2, col="red")

plot(residuals(reg2)~predict(reg2),
     main="Plot of Residuals",
     ylab="Residual Value",
     xlab="Predicted Value")
abline(h=0, col="magenta")
#still a bend, so linear model is not a good fit for these data, either

hist(residuals(reg2))
#Clearly skewed to the right, not a normal distribution
qqnorm(residuals(reg2))
#NOT NORMAL

#Challenge #6
#Is there a difference in income between the north and the south?

aggregate(income~region, data=advert, mean)
#region   income
#1      N 56.82233
#2      S 57.78175

#The difference is 57.78175-56.82233=0.95942, which is less than 1, which I think is negligible
#Let's explore in other ways

aggregate(income~region, data=advert, median)

#region income
#1      N  55.44
#2      S  54.66

#the difference in the medians is less than 1, also
boxplot(income~region,
        data=advert,
        col=c("yellow", "green"),
        main="Boxplots of Income vs Region")
#Here, the box for North seems bigger, so a bigger IQR (both a bigger first and third quartile)
#Nevertheless, the lower whisker for the north is way lower than in the south
#We cannot really conclude that one region has a higher income than the other

#QUESTION: Do we also need to test for significance using random trials?

#EXAMPLE

#initial difference between median incomes was 0.95942
trials<-1000
results<-numeric(trials)

for(i in 1:trials){
  df<-aggregate(income~sample(region), data=advert, median)
  results[i]<-df$income[2]-df$income[1]
}
hist(results)
abline(v=0.95942, col="red", lwd=2)
#it's not statistically significant, many values are above our perceived difference

sum(results>0.95942)/trials*100
#44.5 % of random values are bigger than our difference, not statistically significant


# Challenge #7

sum(advert$internet>advert$tv & advert$internet>advert$radio & advert$internet>advert$newspaper) / nrow(advert) *100
# 62% of companies spend more on internet ads than on any other type of ads

#Challenge #8
#what proportion of companies spend less on tv ads than on any other type of ads?

sum(advert$tv<advert$internet & advert$tv<advert$radio & advert$tv<advert$newspaper)/nrow(advert)*100
#13%

#Challenge #9
#Is there a relationship between spending on radio ads and spending on newspaper ads?

plot(radio~newspaper,
     data=advert,
     main="Radio Spending vs Newspaper Spending",
     col="darkgreen")
#it does not seem that it would be a relationship; 
#data look like a cloud with no distinguishable shape, whether linear or not

cor(advert$radio, advert$newspaper)
#0.1219434 - very low
cor(advert$radio, advert$newspaper)^2
#0.01 - extremely low, no point in fitting a linear model!!

#Challenge 10

plot(age~income,
     data=advert,
     main="Age vs Income",
     col="darkgreen")
#there seems to be a linear relationship
cor(advert$age, advert$income)
#0.5180211 - moderately strong correlation
cor(advert$age, advert$income)^2
# 0.2683458 - r squared kinda low

#we can try to plot a regression
#assumptions
#both variables are quantitative
#relationship seems straight enough
#no obvious high outliers
#plot does not thicken; consistent spread of points

reg3<-lm(age~income, data=advert)
abline(reg3, col="red", lwd=2)
coef(reg3)
#(Intercept)      income 
#26.306651    0.311609 

#let's now check the residuals
plot(residuals(reg3)~predict(reg3),
     main="Residual Plot")
#Perfect, no shape, no bends, no curves, no outliers in the residual plot
hist(residuals(reg3))
#seems like a nearly normal distribution
qqnorm(residuals(reg3))
#well, here it does not look normal anymore

#Nevertheless, there seems to be a linear relationship between age and income

#We can test that relationship with random trials

#initial slope = 0.311609
trials<-1000
results2<-numeric(trials)

for(i in 1:trials){
  results2[i]<-lm(sample(age)~income, data=advert)$coefficients[2]
}
#results2 is a numeric vector containing the slopes we obtained for each trial
#you normally sample the predictor!!!!!!!!!!!!!!
hist(results2)
max(results2)
# the maximum value of results2 is 0.1727992, which is lower than our slope of 0.311, 
# which means our slope is statistically significant and not due to randomness alone

hist(results2, xlim=c(-0.2, 0.4))
abline(v=0.311609, col="magenta")

