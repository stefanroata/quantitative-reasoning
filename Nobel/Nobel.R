Nobel<-read.csv("nobel.csv", stringsAsFactors = F)
#Y=number of Nobels per 10 million population


plot(Y~X, data=Nobel,
     xlab="Mystery",
     ylab="Number of Nobels per 10 million population",
     main="Nobel")
cor(Nobel$X, Nobel$Y)
zx<-scale(Nobel$X)
zy<-scale(Nobel$Y)
cor(zx,zy)
