str(lung_cancer)
unique(lung_cancer$Country)
head(lung_cancer, 10)
dim(lung_cancer)
unique(lung_cancer$AgeClass)
uk <- lung_cancer[lung_cancer$Country=="UK",]
uk_total_cases <- sum(uk$Cases)
uk_total_population <- sum(uk$Population)
uk_incidence_rate <- (uk_total_cases/uk_total_population)*100000
aggregate(Cases~Country, data=lung_cancer, sum)
total <- aggregate(cbind(Cases, Population)~Country, data=lung_cancer, sum)
total$incidence <- (total$Cases/total$Population)*100000

barplot(total$incidence,
        names.arg=total$Country,
        main="Overall lung cancer rates",
        ylab="Incidence per 100.000 cases",
        col="red",
        las=2)
#las=2 rotates the country names by 90 degrees
grid() #adds a faint grid

#total$Cases <- log10(total$Cases)
#barplot(total$Cases,
#        names.arg=total$Country)

#Are older people more likely to get lung cancer?

singapore<-lung_cancer[lung_cancer$Country=="Singapore",]
singapore$incidence<-(singapore$Cases/singapore$Population)*100000
ethiopia<-lung_cancer[lung_cancer$Country=="Ethiopia",]
ethiopia$incidence<-ethiopia$Cases/ethiopia$Population*100000
barplot(ethiopia$incidence,
        names.arg=ethiopia$AgeClass,
        ylab="Incidence",
        main="Incidence of lung cancer by age group in Ethiopia",
        col="darkblue")

barplot(singapore$incidence,
        names.arg=singapore$AgeClass,
        ylab="Incidence",
        main="Incidence of lung cancer by age group in Singapore",
        col="red",
        las=2)

age_structure <- aggregate(Population ~ AgeClass, data=lung_cancer, sum)
singapore$ages=(singapore$Population/sum(singapore$Population))*100
barplot(singapore$ages,
        names.arg=singapore$AgeClass,
        main="Percentage of people with lung cancer Singapore by global age groups",
        col="lightblue",
        las=2)

ethiopia$ages=(ethiopia$Population/sum(ethiopia$Population))*100
barplot(ethiopia$ages,
        names.arg=ethiopia$AgeClass,
        main="Percentage of people with lung cancer Ethiopia by global age groups",
        col="lightblue",
        las=2)


uk_population<-lung_cancer$Population[lung_cancer$Country=="UK"]
lung_cancer$Population_UK<-uk_population
lung_cancer$Cases_if_uk <- (lung_cancer$Population_UK/lung_cancer$Population)*lung_cancer$Cases

adjusted<-aggregate(Cases_if_uk~Country, data=lung_cancer, sum)
sum(uk_population)
lung_cancer$incidence <- lung_cancer$Cases_if_uk / sum(uk_population)*100000
barplot(lung_cancer$incidence[lung_cancer$Country=="Singapore"],
        names.arg=singapore$AgeClass,
        main="Adjusted incidence in Singapore (with UK as benchmark)",
        col="red",
        las=2)
