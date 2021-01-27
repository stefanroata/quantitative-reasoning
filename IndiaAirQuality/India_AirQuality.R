dim(india)
str(india)
head(india, 20)
tail(india)

#Challenge 1

summary(india$Delhi)
sd(india$Delhi)

#Challenge 2

table(is.na(india$Hyderabad))
sum(is.na(india$Hyderabad))
#there are 74 missing PM2.5 values for Hyderabad

#Challenge 3

#a. boxplot organizing values by the time of the day
boxplot(Delhi~Part, data=india,
        xlab="Part of the Day",
        ylab="PM2.5 Measurement",
        col="firebrick2",
        main="Measurements of PM2.5 in Delhi by Time of the Day")

#b. boxplot organizing values by hour
boxplot(Delhi~Time, data=india,
        xlab="Time Expressed in Hours",
        ylab="PM2.5 Measurement",
        col="tomato2",
        main="Measurements of PM2.5 in Delhi by Time of the Day")

#Challenge 4

hist(india$Delhi,
     main="PM2.5 Measurements in Delhi",
     xlab="PM2.5 Value",
     col="yellow")
hist(india$Delhi, 
     breaks=seq(0,140,7.5),
     main="PM2.5 Measurements in Delhi",
     xlab="PM2.5 Value",
     col="yellow"
     )

#Challenge 5

sum(india$Delhi<0)
#there are no values below 0 for the Delhi PM2.5 measurement

#In-class activity
summary(india$Mumbai)
sd(india$Mumbai, na.rm=T)
IQR(india$Mumbai, na.rm=T)
table(is.na(india$Mumbai))
sum(is.na(india$Mumbai))
quantile(india$Mumbai, c(.05,.95), na.rm=T)


#adjust the y axis limits so that it is consistent with the values of New Delhi


par(mfrow=c(2,1)) #plots two graphs one over the other!!!!

plot(india$Mumbai,
     type="l",
     main="PM2.5 Pollution in Mumbai",
     xlab="Hourly PM2.5, June 2016",
     ylab="PM2.5",
     col="tomato2",
     ylim=c(-25, 400))
plot(india$Kolkata,
     type="l",
     main="PM2.5 Pollution in Kolkata",
     xlab="Hourly PM2.5, June 2016",
     ylab="PM2.5",
     col="lightgreen",
     ylim=c(-25, 400))

lines(india$Chennai, col="red")
lines(india$Mumbai, col="blue")
#overlap two graphs; you have to have an initial plot,though

#another function called matplot to overlap graphs!!!

matplot(india[,c(2:6)],
        type="l")
        legend("topleft", legend=names(india)[c(2:6)], col=1:5, lty=1.5)
        

#side by side boxplots comparing Delhi and Mumbai in a fair way
boxplot(india$Delhi, india$Mumbai, col=c("lightgreen", "red"),
        names=c("Delhi","Mumbai"))

#OR

boxplot(india[,c(2,3)])
boxplot(india[,c(2:6)])
boxplot(log(india[,c(2:6)]))

#What is the most and least polluted hour of the day?

boxplot(india$Mumbai~india$Time)
        