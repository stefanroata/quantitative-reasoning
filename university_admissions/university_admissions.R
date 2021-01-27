x<-read.csv("AdmissionsData.csv", stringsAsFactors = FALSE)
table(x$Department)
table(x$Admitted)
table(x$Department, x$Sex)

#overall acceptance rate for men
sum(x$Sex=="M" & x$Admitted=="Yes")/sum(x$Sex=="M")*100

#overall acceptance rate for women
sum(x$Sex=="F" & x$Admitted=="Yes")/sum(x$Sex=="F")*100

#ALTERNATIVE SOLUTION TO A)

admits<-table(x$Sex,x$Admitted,x$Department)
admits<-table(x$Department,x$Admitted)
mosaicplot(admits, col = hcl(c(10, 120)),main="Rate of admission across all departments")

#Mathematics

math<-x[x$Department=="Mathematics", ]
sum(math$Sex=="M" & math$Admitted=='Yes')/sum(math$Sex=='M')*100
sum(math$Sex=="F" & math$Admitted=='Yes')/sum(math$Sex=='F')*100

#admitted<-table(x[x$Admitted=="Yes", ]$Sex, x[x$Admitted=="Yes", ]$Department)/table(x$Sex, x$Department)*100
#mosaicplot(admitted,main="Rate of admission for male and female across all departments")


sum(table(x[x$Sex=="M"&x$Admitted=="Yes"&x$Department=="Biology", ]))/sum(table(x[x$Sex=='M'&x$Department=="Biology", ]))
#THIS IS THE SCRIPT FOR THE PERCENTAGE OF MEN. VERY IMPORTANT
#What fraction of men and women applied to each department?

#bio
sum(x$Department=="Biology"&x$Sex=="M")/sum(x$Sex=="M")*100
sum(x$Department=="Biology"&x$Sex=="F")/sum(x$Sex=="F")*100

#english
sum(x$Department=="English"&x$Sex=="M")/sum(x$Sex=="M")*100
sum(x$Department=="English"&x$Sex=="F")/sum(x$Sex=="F")*100

#History
sum(x$Department=="History"&x$Sex=="M")/sum(x$Sex=="M")*100
sum(x$Department=="History"&x$Sex=="F")/sum(x$Sex=="F")*100

#Mathematics
sum(x$Department=="Mathematics"&x$Sex=="M")/sum(x$Sex=="M")*100
sum(x$Department=="Mathematics"&x$Sex=="F")/sum(x$Sex=="F")*100

#Philosophy
sum(x$Department=="Philosophy"&x$Sex=="M")/sum(x$Sex=="M")*100
sum(x$Department=="Philosophy"&x$Sex=="F")/sum(x$Sex=="F")*100

#Psychology
sum(x$Department=="Psychology"&x$Sex=="M")/sum(x$Sex=="M")*100
sum(x$Department=="Psychology"&x$Sex=="F")/sum(x$Sex=="F")*100
