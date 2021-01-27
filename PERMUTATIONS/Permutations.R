df<-data.frame(
  sleep=c(7,8,9,6),
  college=c("Saga","Cendana","Cendana","Saga")
)

aggregate(sleep~college, data=df, mean)

#we want to permute the data
#we will use SAMPLE

set.seed(123456789)
sample(c("a","b","c"))

#create a new data frame with only college as the permuted variable

df_p<-df
df_p$college<-sample(df$college)

aggregate(sleep~college, data=df_p, mean)

#simulating permutations in R
data(faithful)
cor(faithful$eruptions,faithful$waiting)
#0.90

trials<-10000
responses<-numeric(trials)

cor(sample(faithful$eruptions), faithful$waiting)
#0.04

for(i in 1:trials)
  responses[i]<-cor(sample(faithful$eruptions), faithful$waiting)

hist(responses, xlab="Correlation")


#IN-CLASS PREP

titanic<-read.csv("titanic.csv", stringsAsFactors = F)

trials<-20
d<-5
res<-matrix(NA, trials, d)

for(i in 1:trials)
  res[i,]<-sample(1:10, 5, replace=F)

res[2,]
res[,5]

#IN-CLASS WORK

#Coin flipping simulation: Is Tim right?

p_val<-seq(0.1, 0.9, by=0.1)
p_dim<-length(p_val)
#number of coin tosses
n<-10
#number of trials
trials<-10000
outcomes<-c("H","T")

results<-matrix(NA, trials, p_dim)


for(i in 1:p_dim){
  for(j in 1:trials){
    sim_coin<-sample(outcomes, size=n, replace=T, prob=c(p_val[i], 1-p_val[i]))
    results[j,i]<-sum(sim_coin=="H")/n
  }
}

par(mfrow=c(3,3))
for(i in 1:p_dim){
  p<-hist(results[,i], probability = T, breaks=seq(0,1,by=0.1),
          xlim=c(0,1),
          main=paste("p=",p_val[i]),
          xaxt="n")

}

#TITANIC DATA
titanic<-read.csv("titanic.csv", stringsAsFactors = F)

freq_tables<-function(cat_data){
  tab<-table(cat_data) #create a table from the data that's been read in
  print(tab)
  print(tab/length(cat_data))
}

freq_tables(titanic$gender)
freq_tables(titanic$class)
freq_tables(titanic$survived)


freq_tables(titanic$age>=18)

table(titanic$survived, titanic$gender)
table(titanic$survived, titanic$gender) / nrow(titanic)*100

freq_tables(titanic$survived[titanic$gender=="Male"])*100
freq_tables(titanic$survived[titanic$gender=="Female"])*100

#simulation solution

response<-function(dying_people){
  death_rate_males<- 1- mean(dying_people$survived[dying_people$gender=="Male"])
  death_rate_females<- 1- mean(dying_people$survived[dying_people$gender=="Female"])
  death_rate_males/death_rate_females #finding the ratio of death rate between males and females
}


response(titanic)

permute<-function(){
  tmp<-titanic
  #permute gender
  tmp$gender<-sample(tmp$gender)
  
  response(tmp)
}

permute() #is our observed difference of 3 actually statistacally significant??

trials<-10000
#

results<-numeric(trials)

for(i in 1:trials){
  results[i]<-permute()
}

par(mfrow=c(1,1))
hist(results)


titanic$total_price<-titanic$ticket_pound + titanic$ticket_shilling/20 +titanic$ticket_penny/240

tit<-titanic[titanic$total_price!=0  & titanic$gender=="Male" & titanic$class=="2nd",]
model<-lm(log(tit$total_price)~tit$age, data=tit)

plot(log(tit$total_price)~tit$age, col="blue")
abline(model, lwd=3)
coef(model)
slope<-model$coefficients[2]
#is the slope significant

trials<-10000
results<-numeric(trials)

for(i in 1:trials){
  results[i]<-lm(log(total_price)~sample(age), data=tit)$coefficients[2]
  
}
hist(results)
mean(results<=slope | results>=-slope)
#p=0.06