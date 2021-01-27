low_income<-primary_education[primary_education$income=="low", ]
sum(low_income$completion_female_percent*low_income$females_at_age_of_last_grade)/sum(low_income$females_at_age_of_last_grade)      
