#dnorm is the function that draws the bell-curve

dnorm(600, mean=500, sd=100)
curve(dnorm(x, mean=500, sd=100), from=200, to=800)

#pnorm = the area under the bell curve ---> the PERCENTILE!!!
#IMI ARATA PERCENTILA daca ii dau scorul

#my percentile if I scored 600 on the SAT
pnorm(600, mean=500, sd=100)
pnorm(710, mean=500, sd=100)

#the percentage of students scoring between 450 and 600
pnorm(600, mean=500, sd=100) - pnorm(450, mean=500, sd=100)

#qnorm --> daca ii zic o percentila, sa imi zica scorul echivalent

qnorm(0.9, mean=500, sd=100)

qnorm(.98, mean=500, sd=100)

#rnorm --> imi da un set the n numere random care au mean-ul si sd-ul date de mine

rnorm(10, mean=500, sd=100)

#we want to test if the numbers are indeed normally distributed

r<-rnorm(10000, mean=500, sd=100)
hist(r)

#if we want to associate with dnorm, we have to set the area of all bins to be equal to 1

hist(r, freq=F)

curve(dnorm(x, mean=500, sd=100),
      from=200,
      to=800,
      add=TRUE,
      col="red")

#qqnorm draws the normal probability plot

qqnorm(r)



pnorm(-1.5)
pnorm(-2)
pnorm(-1)
qnorm(.02)
