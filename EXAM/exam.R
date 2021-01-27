advert<-read.csv("advert.csv")

#Challenge 1a

cor(advert$age, advert$newspaper)
# 0.5318722 -moderately strong correlation

cor(advert$age, advert$newspaper)^2
# 0.282888 - R^2 

plot(newspaper~age,
     data=advert,
     main="Plot of Age vs Newspaper Spending",
     col="darkblue",
     ylab="Newspaper Spending (in units of 1000 USD)",
     xlab="Age of the Market")

reg1<-lm(newspaper~age, data=advert)
abline(reg1, lwd=2, col="red")

# 1b

hist(residuals(reg1),
     col="green",
     main="Histogram of Residuals",
     xlab="Residual Values")

# The histogram of the residuals looks depicts a nearly normal, unimodal distribution

plot(residuals(reg1)~predict(reg1),
     col="darkorchid4",
     main="Plot of Residuals vs Fitted Values",
     xlab="Fitted Values",
     ylab="Residual Values")

# The residual plot shows no pattern for the residuals, no bends, no shape, roughly equal amount of scatter throughout
# All in all, the residual plot and the histogram show that the linear model might be a good fit for the data.

# 1c

# Assumptions
# 1. Both variables are quantitative: age is measured in years, newspaper is measured in units of 1000 USD
# 2. Straight enough condition: the plot looks straight enough
# 3. No outliers: there are no obvious high outliers in the plot
# 4. Does the plot thicken? : No, the plot does not thicken, the spread of values is consistent both in the original and the residual plot
# Extra: the residual plot shows no pattern for the residuals, no bends, no shape, roughly equal amount of scatter throughout

#1D
predict(reg1, newdata=data.frame(age=73))
#1 
#8.516606 
# A market with a median age of 73 would spend 8.516606*1000 USD = 8516.606 dollars

# Challenge 2

# a
plot(biscuits~tv, data=advert)
reg2<-lm(biscuits~tv, data=advert)
reg2$coefficients[2]
#tv 
#-0.02977018 
# the answer is -0.02977018

#b
lm(biscuits~radio, data=advert)$coefficients[2]
#radio 
#-9.152503 

#c

cor(advert$biscuits, advert$tv)^2
# the model account for 1.400048e-06 *100= 0.0001400048 % of the variance

cor(advert$biscuits, advert$radio)^2

#the model account for 0.00749494*100=0.74 % of the variance

# Challenge #3

#a
south<-advert[advert$region=="S",]
sum(south$biscuits>mean(advert$biscuits))/nrow(south)*100
#46.39175 percent of southern markets spend more than the mean expenditure of all 200 markets

#b
aggregate(biscuits~region, data=advert, mean)
# region biscuits
# 1      N 293.9891
# 2      S 316.2360

#initial difference is 316.2360 - 293.9891 = 22.2469
# test if 22 is significant
trials<-500
results<-numeric(trials)
for(i in 1:trials){
  df<-aggregate(biscuits~sample(region), data=advert, mean)
  results[i]<-df$biscuits[2]-df$biscuits[1]
}

hist(results,
     main="Histogram of Random Trial Results",
     xlab="Differences")

abline(v=22.2469, col="magenta")

#the answer is to the boss' question is NO
