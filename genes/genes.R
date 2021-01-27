genes<-read.csv("GeneArray.csv", stringsAsFactors = F)


str(genes)
head(genes)
unique(genes$type)
dim(genes)
summary(genes$feature1)
#min=0.9122
#1st quartile=0.9464 etc
sd(genes$feature1)
IQR(genes$feature1)
range(genes$feature1)

sum(is.na(genes$feature1))
#OR
table(is.na(genes$feature1))

quantile(genes$feature1, 0.75)-quantile(genes$feature1, 0.25)

#What are the frequencies of the categories in type? What is the most common category?
table(genes$type)

#proportions
table(genes$type)/nrow(genes)*100
#you can copy the table in comments

#Produce two histograms of feature 1- one for bscl2 and one for rfnotDL in the same image
par(mfrow=c(1,2))

hist(genes$feature1[genes$type=="bscl2"],
     xlim=c(0.93,0.99),
     ylim=c(0,30))
hist(genes$feature1[genes$type=="rfnotDL"],
     xlim=c(0.93,0.99),
     ylim=c(0,30))

par(mfrow=c(1,1))
#resets the your plot pane to 1 by 1
#you can use xlim or ylim OR breaks=seq(0.9,1,0.01)


#Produce side by side boxplots of feature1 by type

boxplot(feature1~type,data=genes,
        col=cm.colors(5),
        main="Boxplot of feature1 by type",
        xlab="Type")
# for color col=cm.colors(5)
#or col=rainbow(5)


#Calculate z-score for feature1 and 2, append to the data frame
#plot histogram side by side to compare them


genes$zfeature1<-scale(genes$feature1)
genes$zfeature2<-scale(genes$feature2)
par(mfrow=c(1,2))
hist(genes$zfeature1, breaks=seq(-6,6,1))
hist(genes$zfeature2, breaks=seq(-6,6,1))
