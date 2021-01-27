sg_pop$fem_prop<-sg_pop$female/(sg_pop$male+sg_pop$female)
summary(sg_pop$fem_prop)
hist(sg_pop$fem_prop)
sd(sg_pop$fem_prop)

pnorm(0.45, mean=mean(sg_pop$fem_prop), sd=sd(sg_pop$fem_prop))

#the percentage of zones that have less than 45 percent female population
sum(sg_pop$fem_prop<0.45)/nrow(sg_pop)

#the percentage of those above 0.55

1-pnorm(0.55, mean=mean(sg_pop$fem_prop), sd=sd(sg_pop$fem_prop))
#8.85

sum(sg_pop$fem_prop>0.55)/nrow(sg_pop)
#2.59

hist(sg_pop$fem_prop,breaks=seq(0.33,0.73,0.01),freq=F)
curve(dnorm(x, mean=mean(sg_pop$fem_prop), sd=sd(sg_pop$fem_prop)),
      add=T,
      col="red")
qqnorm(sg_pop$fem_prop)

#trim the outliers and then replot
sg_trim<-sg_pop[sg_pop$fem_prop>=0.4 & sg_pop$fem_prop<=0.6,]
hist(sg_trim$fem_prop,breaks=seq(0.4,0.6,0.01),freq=F)
curve(dnorm(x, mean=mean(sg_trim$fem_prop), sd=sd(sg_trim$fem_prop)),
      add=T,
      col="red")
