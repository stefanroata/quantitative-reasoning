
sample(x=c("Elm","Saga", "Cendana"), size=1)


sample(x=c("Elm","Saga", "Cendana"), size=4)
#error, only 3 elements and the program tries to fetch 4 componenents
#by default, replace=F, so we should modify that in order for the program
#not to consider the elements unique

sample(x=c("Elm","Saga", "Cendana"), size=4,replace=T)


#IF WE WANT CONSISTENT RESULTS IN ALL TRIALS, WE USED set.seed

set.seed(20191029)
sample(x=c("Elm","Saga", "Cendana"), size=4,replace=T)

#using unequal probablities

sample(x = c("Cendana", "Elm", "Saga"), size = 1, replace = TRUE, prob = c(0, 1, 0))
#of course, Elm has a probability of 1, so Elm will always be the result of this command

sample(x = c("Cendana", "Elm", "Saga"), size = 20, replace = TRUE, prob = c(0.8, 0, 0.2))


load(url("http://www.openintro.org/stat/data/kobe.RData"))

str(kobe)
head(kobe, 20)
tail(kobe, 20)
table(kobe$vs)
table(kobe$game)

calc_streak(1:4)

calc_streak(c("A", "H", "H", "B", "H", "A"))


outcomes<-c("heads","tails")
sample(outcomes, size=1, replace=T)


#flip one hundred times

sample(outcomes, size=100, replace=T)
table(sample(outcomes, size=100, replace=T))/length(sample(outcomes, size=100, replace=T))

set.seed(19930215)
sample(outcomes, size=100, replace=T)
table(sample(outcomes, size=100, replace=T))/length(sample(outcomes, size=100, replace=T))



#repeat this simulation with an unfair coin which lands on heads 20 percent of the time

unfair_coin<-sample(outcomes, size=100, replace=T,prob=c(0.2,0.8))
table(unfair_coin)

#KOBE ACTIVITY
barplot(table(kobe$basket, kobe$game), legend=T)

#could be basket versus time?

kobe$basket[1:9]

table(calc_streak(kobe$basket))
kobe_streak<-calc_streak(kobe$basket)
#look at the distribution of his streak lengths

barplot(table(kobe_streak), ylim=c(0,40))


#compute the relative frequencies 
barplot(table(kobe_streak)/length(kobe_streak))
table(kobe_streak)/length(kobe_streak)

sample(x=c("H","M"), size=1000, replace=T)
independent<- sample(x=c("H","M"), size=1000, replace=T)
table(calc_streak(independent))

hit_prop<-mean(kobe$basket=="H")

independent1<- sample(x=c("H","M"), size=133, replace=T,prob=c(hit_prop, 1-hit_prop))
table(calc_streak(independent1))

#independent shooter
par(mfrow=c(1,2))
barplot(table(calc_streak(independent1)), main="Indepedent shooter",
        col="darkblue", ylim=c(0,60), xlab="Streak lengths")
barplot(table(calc_streak(kobe$basket)), col="red", main="Kobe", ylim=c(0,60),
        xlab="Streak lengths")

df <-data.frame(max.streak=matrix(NA, nrow=6, ncol=1))
for(i in 1:6){
  df$max.streak[i] <- max(calc_streak(sample(x=c("H","M"), size=133, replace=T,prob=c(hit_prop, 1-hit_prop))))
  
}

