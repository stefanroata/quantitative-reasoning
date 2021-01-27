res1<-0
n1<-sample(1:100, size=1)
if(n1>50){
  res1<-5
}

n2<-sample(1:100, size=1)
if(n2%%3==0){
  res2<-10
  
} else{
  res2<-20
}

n3<-sample(1:100, size=1)

if(n3%%10==0){
  res3<-1
} else if (n3 %% 5 ==0){
  res3<-2
} else {
  res3<-3
}

#FUNCTIONS

check_10_5<-function(n3){
  if(n3%%10==0){
    res<-1
  } else if (n3 %% 5 ==0){
    res<-2
  } else {
    res<-3
  }
  res
}

check_10_5(75)
check_10_5(80)


cards<-sample(1:100, size=10, replace=F)
score<-0

for(c in cards){
  if(c %% 6 == 0){
    score<-score*3
  } else if (c %% 3 == 0){
    score<-score*2
  } else {
    score<-score+c
  }
    
}

#iteration in a vector

values<-c("a","b","c","d")
for(v in values){
  print(v)
}


game_10<-function(){
  cards<-sample(1:100, size=10, replace=F)
  result<-0
  for(c in cards){
    if(c %% 6 == 0){
      result<-result*3
    } else if (c %% 3 == 0){
      result<-result*2
    } else {
      result<-result+c
    }
    
  }
  result
}
 game_10()

results_10<-numeric(10000) 
for(i in 1:10000){
  results_10[i]<-game_10()
}
hist(log(results_10),
     xlab="log(Results)",
     main="Histogram of the results of the game",
     col="green")


#parameters, global and local variables

x<-50
foo<-function(y,z){
  if(z<y){
    r<-z+x
  } else{
    r<-z+y
  }
  r<-r+z
  r
}

foo(3,5)
foo(5,3)
