forbes<-forbes[forbes$Pay>0,]
hist(log(forbes$Pay, 10), breaks=seq(4, 8.6, 0.2),
     col="green",
     xlab="log10 of Compensation in US$",
     main="Forbes 500 CEO Compensation")

boxplot(log10(Pay)~Age, data=forbes,
        col=c("orange", "lightblue"),
        xlab="Age",
        ylab="log10 of Compensation in US$",
        main="Forbes 500 CEO Compensation")
