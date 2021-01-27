setwd("~/QR")
country_info <- read.csv("country_info.csv", stringsAsFactors = F)

#try to calculate the population of Europe

europe <- country_info[country_info$continent=="Europe",]
dim(europe)
sum(europe$population)

oceania <- country_info[country_info$continent=="Oceania", ]
sum(oceania$population)

sum(country_info[country_info$continent=="Oceania", 3]) #the sum of population in Oceania
