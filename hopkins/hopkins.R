plot(hopkins$Wind~hopkins$Day,
     col="red",
     cex=0.75, #sets the dimension of the points in the plot; 1 is default
     xlab="Day of the Year",
     ylab="Average Wind Speed (mph)",
     main="Hopkins Forest Wind Speed")
lines(lowess(hopkins$Wind), col="blue")

boxplot(Wind~Quarter, 
        data=hopkins,
        col="yellow",
        main="Hopkins Forest Wind Speed by Quarter",
        xlab="Quarter of the Year",
        ylab="Average Wind Speed (mph)")
