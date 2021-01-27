dim(uesi)
head(uesi, 15)
tail(uesi, 15)
str(uesi)
table(uesi$continent) 
#I think we should alter the file and erase V1, V2... V21 from the first row; it is also tampering with the data
table(uesi$country)
uesi[uesi$country=="Romania",]
unique(uesi$country)
median(uesi$income_mean, na.rm=T)
table(is.na(uesi$income_mean))
summary(uesi)


sum(uesi$population_total[uesi$continent!="Asia"])
sum(uesi$nbhd_num>100)
length(uesi$city[uesi$nbhd_num>100]) #same result
sum(uesi$PUBTRANS.UESI>85 & uesi$TREECAP.UESI>85, na.rm=T) #how many cities...
unique(uesi[uesi$PUBTRANS.UESI>85 & uesi$TREECAP.UESI>85, ]$city) #what cities
unique(uesi$city[uesi$PUBTRANS.UESI>85 & uesi$TREECAP.UESI>85]) #BETTER VERSION!!!!


#How many cities got perfect scores in PM25.UESI, UHI.UESI and TREECAP.UESI
sum(uesi$PM25.UESI==100 & uesi$UHI.UESI==100 & uesi$TREECAP.UESI==100) #2 cities

table(uesi$PM25.UESI<50, useNA="always")
#What are the cities that have 100 on all three?
unique(uesi$city[uesi$PM25.UESI==100 & uesi$UHI.UESI==100 & uesi$TREECAP.UESI==100])

#use the which()- the index of the row where the element matches the conditions

uesi$city[which(uesi$PM25.UESI==100 & uesi$UHI.UESI==100 & uesi$TREECAP.UESI==100)]

which.min(uesi$PM25.UESI)

#comparing Singapore's performance with other cities
#how many cities are better than Singapore in treecap.uesi?

sum(uesi$TREECAP.UESI > uesi$TREECAP.UESI[uesi$city=="singapore"])
#how many cities are better than singapore in either treecap.uesi and pm25.uesi?
sum(uesi$TREECAP.UESI > uesi$TREECAP.UESI[uesi$city=="singapore"] | uesi$PM25.UESI>uesi$PM25.UESI[uesi$city=="singapore"])

uesi$PM25_mean[uesi$city=="singapore"] 

#how many cities?
nrow(uesi)
length(uesi$city)

boxplot(uesi$PM25.UESI ~ uesi$continent,
        main="How do continents compare on PM25?",
        col="lightblue")
#BETTER AIR QUALITY IN THE USA AND CANADA!!!!!!!!!!!!!!


