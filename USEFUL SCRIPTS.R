z<-c(TRUE, TRUE, FALSE, FALSE, TRUE)
player<-c("Rachel", "Tatyana", "Noah", "Quentin", "Aisha")
player[z] #shows Rachel, Tatyana and Aisha, the values corresponding to TRUE

score<-c(5, 6, 1, 6, 3)
score==6
player[score==6] #shows the elements in the vector where score is 6
#score is not part of player, so no dollar sign

player[score<5]

player[score>2 & score<5]

titanic[titanic$age==40 & titanic$class=="Crew", ]
nrow(titanic[titanic$age==40 & titanic$class=="Crew", ])
sum(titanic$age==40 & titanic$class=="Crew") 
#shows 26 because "titanic$age==40 & titanic$class=="Crew" is a logical vector!!!!!
sum(titanic$class=="3rd")

#tables

table(titanic$class)#counts
table(titanic$class)/nrow(titanic)*100 #one-way table
table(titanic$class, titanic$survived) #two-way table
table(titanic$class, titanic$survived)/nrow(titanic)*100

#barplots - we need tables here

one_way<-table(titanic$class)
barplot(one_way)
two_way<-table(titanic$class, titanic$survived)
barplot(two_way) #we can't exactly see what this means, but...
barplot(two_way, legend.text = TRUE)
#if we want another type of barchart, a clearer one, we should transpose the rows and the columns of the table
t(two_way)
barplot(t(two_way), legend.text=TRUE)
#the legend is not positioned correctly, so we try to fix it
barplot(t(two_way), legend.text=TRUE, args.legend = list(x="topleft"))
