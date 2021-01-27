midterm<-rbinom(24,18,0.25)
score<-midterm/18
hist(score)

#final exam

final<-rbinom(24,18,0.25)
exams<-cbind(midterm, final)

plot(final~midterm)
abline(lm(final~midterm))
cor(final, midterm)
summary(midterm)
summary(final)

#OR exams<-data.frame(cbind(midterm, final))

week<-read.csv("Stocks18Week1_no_split.csv", stringsAsFactors = F)
week$change<-(week$X5.Jan.18-week$X1.Jan.18)/week$X1.Jan.18*100
summary(week$change)
hist(week$change)
improvement<-week[week$change>=25,]
improvement$Symbol
losers<-week[week$change<(-10),]

year<-read.csv("Stocks18_no_split.csv", stringsAsFactors = F)
year$change<-(year$X31.Dec.18-year$X1.Jan.18)/year$X1.Jan.18*100
year$change[year$Symbol=="WTI"]
WTI<-t(year[year$Symbol=="WTI",])

plot(WTI[2:262],type="l")
plot(year$change~week$change)

#Which company is the outlier?
year$Symbol[year$change>500]
#'I'
plot(t(year[year$Symbol=="I",2:262]),type="l")

plot(year$change~week$change,ylim=c(-100,300))
cor(year$change,week$change)
abline(lm(year$change~week$change), col="red", lwd=4)
