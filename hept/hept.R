hept<-read.csv("hept.csv", stringsAsFactors = F)
str(hept)
hept$z_score_hurdles<--(hept$hurdles-mean(hept$hurdles))/sd(hept$hurdles)
hept$z_score_hj<-(hept$hj-mean(hept$hj))/sd(hept$hj)
hept$z_score_lj<-scale(hept$lj)
hept$z_score_sp<-scale(hept$sp)
hept$z_score_run200<--scale(hept$run200)
hept$z_score_run800<--scale(hept$run800)
hept$z_score_jt<--scale(hept$jt)


hept$our_total_score<-hept$z_score_hurdles+hept$z_score_hj+hept$z_score_lj+hept$z_score_sp+hept$z_score_run200+hept$z_score_run800+hept$z_score_jt
#hept$pts_total<-hept$pts_hurdles+hept$pts_hj+hept$pts_lj+hept$pts_sp+hept$pts_run200+hept$pts_run800+hept$pts_jt
hept$run800<-!is.na(hept$run800)
table(is.na(hept$run800))
plot(hept$our_total_score~hept$pts_total,)
plot(hept$pts_run800~hept$pts_total,
     main="800m Run Compared to Total Points",
     col="tomato2",
     xlab="Total Points",
     ylab="Scores on the 800m Run")
abline(lm(hept$pts_run800~hept$pts_total),col="tomato2") #line of best fit

sd(hept$pts_hj)
range(hept$pts_hj)
all_sd<-c(sd(hept$pts_hj), sd(hept$pts_lj), sd(hept$pts_hurdles), sd(hept$pts_run200), sd(hept$pts_run800, na.rm=T), sd(hept$pts_sp), sd(hept$pts_jt) )

#difference between Thiam and Ennis-Hill's score

pts_dif<-hept[1, seq(6,18,2)]-hept[2, seq(6,18,2)]
pts_dif<-t(pts_dif)

#SD on the x axis, and pts_dif on y

plot(pts_dif~all_sd,
     main="The Difference Between the Two Highest Scores vs. The Standard Deviations for All Events",
     xlab="Standard Deviations for All Events",
     ylab="Difference Between the Two Highest Scores",
     col="tomato3",
     xlim=c(0,200),
     ylim=c(-200, 200))
abline(lm(pts_dif~all_sd), col="tomato3",lty=c(1,2), lwd=c(1, 4))
#the standard deviation
hept[hept$athlete=="Jessica Ennis-Hill",19] #-1.980581
hept[hept$athlete=="Nafissatou Thiam", 20] #2.263759
#Nafissatou in hj > Ennis-Hill in hurdles
hept$new<-scale(hept$hurdles) #calculates the z-score for every value of the data




#HOMEWORK PLOT
plot(pts_dif~all_sd,
     main="The Difference Between the Two Highest Scores vs. The Standard Deviations for All Events",
     xlab="Standard Deviations for All Events",
     ylab="Difference Between the Two Highest Scores",
     col="tomato3",
     xlim=c(0,200),
     ylim=c(-200, 200))
abline(lm(pts_dif~all_sd), col="tomato3",lty=c(1,2), lwd=c(1, 4))
