
ge<-read.csv("ge2013.csv", stringsAsFactors = F)

head(ge)
length(unique(ge$DISTNO))
length(unique(ge$STATE))

#BN = Barisan Nasional - governing in 2013
#PR = Pakatan Rakyat

ge$check<-ge$VOTESBN/(ge$VOTESBN+ge$VOTESPR)*100

#check for NAs in the pctBN

which(is.na(ge$pctBN))
#22

ge[22,]

hist(ge$VOTESBN, 
     main="Histogram of Votes for Barisan National", 
     col="green",
     xlim=c(2000, 60000), 
     ylim=c(0,40),
     xlab="Number of Votes",
     border="darkmagenta")

#Hypohtesis = malay percentage and BN vote share related

plot(ge$malay, ge$pctBN,
     xlab="Malay %",
     ylab="BN Vote %",
     main="Malay Voting Pattern")

#predict the number of BN votes as a function of Chinese population

#steps: create a plot of the two variables
#Check conditions for linear regression
#Find the equation for your regression
#Check plot of residuals;
#post plot + equation on canvas

plot(ge$chinese, ge$pctBN,
     col="blue",
     xlab="Chinese %",
     ylab="BN Vote %",
     main="Chinese Voting Pattern")
reg<-lm(ge$pctBN~ge$chinese)
abline(reg,lwd=3,col="blue")
coef(reg)
cor(ge$pctBN,ge$chinese, use="complete.obs")^2

plot(residuals(reg)~predict(reg),main="Residual Plot for Chinese",col="red")

#Calculate the standard deviation of the residuals
sd(residuals(reg))
sd(reg$residuals) #the same thing as above

ge18<-read.csv("ge2018.csv",stringsAsFactors = F)
ge18$pctBN<-(ge18$VOTESBN/ge18$TURNOUT)*100
hist(ge18$VOTESBN, 
     main="Histogram of Votes for Barisan National", 
     col="green",
     xlim=c(0, 50000), 
     ylim=c(0,70),
     xlab="Number of Votes",
     border="darkmagenta")

plot(ge18$BUMI~ge18$pctBN, col="darkmagenta",
     main="Plot of Votes for BN from BUMI Population",
     lwd=2)

#The correlations between different ethnic 
cor(ge18$pctBN,ge18$BUMI,use="complete.obs")^2
cor(ge18$pctBN,ge18$CHINESE,use="complete.obs")^2
cor(ge18$pctBN,ge18$INDIAN,use="complete.obs")^2
cor(ge18$pctBN,ge18$OTHER,use="complete.obs")^2


hist(ge18$VOTESPH, 
     main="Histogram of Votes for Pakatan Harapan", 
     col="green",
     xlim=c(0, 130000), 
     ylim=c(0,60),
     xlab="Number of Votes",
     border="darkmagenta")
plot(ge18$CHINESE~ge18$VOTESPH, col="darkmagenta",
     main="Plot of Votes for PH for Chinese Population",
     lwd=2)

cor(ge18$VOTESPH,ge18$BUMI,use="complete.obs")^2
cor(ge18$VOTESPH,ge18$CHINESE,use="complete.obs")^2
cor(ge18$VOTESPH,ge18$INDIAN,use="complete.obs")^2
cor(ge18$VOTESPH,ge18$OTHER,use="complete.obs")^2
