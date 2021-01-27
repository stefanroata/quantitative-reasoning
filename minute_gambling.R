
boxes<-sample(x=1:100, size=100, replace=F)
pris_number<-sample(x=1:100, size=1)

boxes_to_open<-sample(x=1:100, size=50, replace=F)
pris_found<-FALSE

for(i in boxes_to_open){
 
 if(!pris_found){
   number<-boxes[i]
   if(number==pris_number){
     pris_found=T
   }
 }
}

simple_strategy<-function(pris_number, boxes){
  boxes_to_open<-sample(x=1:100, size=50, replace=F)
  pris_found<-FALSE
  
  for(i in boxes_to_open){
    
    if(!pris_found){
      number<-boxes[i]
      if(number==pris_number){
        pris_found=T
      }
    }
  }
  pris_found
}


#call the function

simple_strategy(pris_number, boxes)

#testing the function

test_boxes<-sample(1:100, size=100, replace=F)
test_pris_number<-sample(x=1:100, size=1)

simple_strategy(test_pris_number, test_boxes)


trial_num<-1000

succ_prisoners_simple<-numeric(trial_num)

for(i in 1:trial_num){
  boxes<-sample(x=1:100, size=100, replace=F)
  pris_results<-numeric(100)
  for(j in 1:100){
    pris_results[j]<- simple_strategy(j, boxes)
  }
  succ_prisoners_simple[i]<-sum(pris_results)
}

succ_prisoners_simple
summary(succ_prisoners_simple)


new_strategy<-function(pris_number, boxes){
  pris_found<-FALSE
  cur_box<-pris_number
  for(i in 1:50){
    if(!pris_found){
      number_in_box<-boxes[cur_box]
      if(number_in_box==pris_number){
        pris_found<-T
      }
    } else{
      cur_box<-number_in_box
    }
  }
  pris_found
}

#how good is this strategy compared to our simplified strategy?
