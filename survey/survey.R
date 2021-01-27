survey<-read.csv("survey.csv", stringsAsFactors = F)
hist(survey$height)

qqplot(survey$height, mean=mean(survey$height), sd=sd(survey$height))
hist(survey$facebook)

m<-mean(survey$youtube, na.rm=T)
s<-sd(survey$youtube, na.rm=T)

range(survey$youtube, na.rm=T)

hist(survey$youtube)
qqnorm(survey$youtube, mean=m, sd=s)

range(survey$height)

hist(survey$height, freq=FALSE, main="Histogram of heights in cm",
     xlab="Heights in cm", breaks=seq(145, 195, 1))
curve(dnorm(x, mean=170, sd=10), from=140, to=200, add=T, col="red")

hist(survey$height[survey$gender=="Female"], 
     freq=FALSE, main="Histogram of heights in cm", breaks=seq(145, 195, 1))

#compute the correlation

cor(survey$height, survey$shoe)
cor(survey$shoe, survey$height)

#Properties of correlation
survey<-read.csv("survey.csv", stringsAsFactors = F)
range(survey$shoe)
range(survey$height)

plot(survey$height~survey$shoe,
     xlab="Shoe size in cm",
     ylab="Height")

survey<-survey[survey$shoe<35,]
plot(survey$height~survey$shoe,
     xlab="Shoe size in cm",
     ylab="Height")
cor(survey$height, survey$shoe)

#add numbers to height or shoe
survey<-survey[survey$shoe<35,]
survey$height<-survey$height-10
survey$shoe<-survey$shoe+100
plot(survey$height~survey$shoe,
     xlab="Shoe size in cm",
     ylab="Height",
     col="red",
     main="Modified plot of height and shoe size")
cor(survey$height, survey$shoe)
plot((survey$height[survey$shoe<=35]-10),(survey$shoe[survey$shoe<=35]+100))
cor(survey$height[survey$shoe<=35]-10, survey$shoe[survey$shoe<=35]+100)



plot((survey$height[survey$shoe<=35]*10),(survey$shoe[survey$shoe<=35]/100))
cor(survey$height[survey$shoe<=35]*10, survey$shoe[survey$shoe<=35]/100)


plot((log10(survey$height[survey$shoe<=35])),log10(survey$shoe[survey$shoe<=35]))
cor(log10(survey$height[survey$shoe<=35]),log10(survey$shoe[survey$shoe<=35]))

#Is correlation transitive?

x<-rnorm(100)
y<-rnorm(100)
z<-x+y
cor(x,z)
cor(y,z)
cor(x,y)









