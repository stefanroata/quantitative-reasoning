b <- sum(country_info$population)
a <- sum(country_info$population*country_info$access_to_electricity_percent/100)
a/b*100

#In short

sum(country_info$population*country_info$access_to_electricity_percent)/sum(country_info$population)

aggregate(population~continent, data=country_info, sum)
