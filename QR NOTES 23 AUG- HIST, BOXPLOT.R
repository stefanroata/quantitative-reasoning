View(iris)
table(iris$Species)
mean(iris$Sepal.Width)
median(iris$Sepal.Width)
min(iris$Sepal.Width)
max(iris$Sepal.Width)
range(iris$Sepal.Width)
sd(iris$Sepal.Width) #standard deviation
IQR(iris$Sepal.Width)
summary(iris$Sepal.Width)
boxplot(iris$Sepal.Width)
boxplot(iris$Sepal.Width,
        col="lightgreen",
        ylab="Sepal width in cm",
        main="Anderson's Iris data")


boxplot(iris$Sepal.Width ~ iris$Species,#split the data by Species; ~=as a function of
        col="lightgreen",
        ylab="Sepal width in cm",
        main="Anderson's Iris data")

boxplot(Sepal.Width ~ Species,
        data=iris,
        col="lightgreen",
        #ylab="Sepal width in cm", #this is oprtional, just for labelling axes
        main="Anderson's Iris data")



#HISTOGRAMS
#SUPPOSE WE WANT MORE DETAIL ABOUT VERSICOLOR SEPAL WIDTH
sep_width_vers<-iris$Sepal.Width[iris$Species=="versicolor"]
length(sep_width_vers)
hist(sep_width_vers,
     col="lightgreen",
     main="Sepal width of iris versicolor",
     xlab="Width in cm"
     )
#observation: the order of col, main, xlab does NOT matter

#we see the breaks(in the x-axis) have a step of 0.2
#if we want another step, or even 0.1, we type:
hist(sep_width_vers,
     col="lightgreen",
     main="Sepal width of iris versicolor",
     xlab="Width in cm",
     breaks=seq(2.0, 3.4, 0.1)
)
