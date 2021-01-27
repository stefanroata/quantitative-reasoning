
SGPOP<-read.csv("SGPOP.csv", stringsAsFactors = F)

str(SGPOP)
unique(SGPOP$ETHNIC)
table(SGPOP$ETHNIC)
table(SGPOP$SEX)/nrow(SGPOP)*100

unique(SGPOP$AGE)
table(SGPOP$AGE)
table(is.na(SGPOP$BMI))

table(is.na(SGPOP$NRIC))
#all have an NRIC

head(SGPOP)
tail(SGPOP)

boxplot(SGPOP$BMI)
#many outliers, and a very high one at the top
summary(SGPOP$BMI)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  13.0    17.9    20.4    21.0    23.4    57.0  734362 
hist(SGPOP$BMI)
hist(log(SGPOP$BMI))

mean(SGPOP$BMI, na.rm=T)

table(SGPOP$ETHNIC)/nrow(SGPOP)*100

smp<-SGPOP[sample(x=nrow(SGPOP), size=100),]

table(smp$ETHNIC)/nrow(smp)*100
#proportions of ethnicity from sample
barplot(table(smp$ETHNIC)/nrow(smp)*100,
        ylim=c(0,100),
        col="orange")

#simple random sample, size=100, randomly selected people and you want it to reflect the ethnic proportions of the Singapore population
#number of trials=250





#MATRIX SOLUTION
srs<-matrix(NA, 250, 4)

for(i in 1:250){
  smp<-SGPOP[sample(x=nrow(SGPOP), size=100),]
  temp<-table(smp$ETHNIC)
 srs[i,1]<-temp[1]
 srs[i,2]<-temp[2]
 srs[i,3]<-temp[3]
 srs[i,4]<-temp[4]
}
colnames(srs)<-c("Chinese", "Indian", "Malay", "Other")
par(mfrow=c(2,2))
hist(srs[,1],
     main="Chinese")
hist(srs[,2],
     main="Indian")
hist(srs[,3],
     main="Malay")
hist(srs[,4],
     main="Other")

srs<-as.data.frame(srs) #FORCE THE MATRIX AS A DATAFRAME

mean(srs$Chinese)
mean(srs$Indian)
mean(srs$Malay)
mean(srs$Other, na.rm=T)

summary(SGPOP$BMI)

sp<-sample(SGPOP$BMI, size=100)
mean(sp, na.rm=T)
#OR
the100<-SGPOP[sample(x=nrow(SGPOP), size=100),]
mean(the100$BMI, na.rm=T)

#BOTH ARE CORRECT

#250 trials of BMI samples
results<-numeric(250)
for(i in 1:250){
  sp<-sample(SGPOP$BMI, size=100)
  results[i]<-mean(sp, na.rm=T)
}
par(mfrow=c(1,1))
hist(results)
sum(results<mean(SGPOP$BMI, na.rm=T))/250

