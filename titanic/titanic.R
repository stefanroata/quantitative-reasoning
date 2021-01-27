titanic <- read.csv("~/QR/titanic/titanic.csv", stringsAsFactors=FALSE)
nrow(titanic)
ncol(titanic)
dim(titanic)
head(titanic)
tail(titanic)
str(titanic)
names(titanic)


second_class <- titanic[titanic$class=="2nd", ]

second_class$ticket_shilling/20

length(second_class$ticket_shilling)

second_class$ticket_total <- second_class$ticket_pound + second_class$ticket_shilling/20 + second_class$ticket_penny/240
#COMENTARIU
#REMOVE COLUMNS
#second_class <- second_class[, -8]
#OR
#second_class$ticket_total <- NULL

#the sum of a numeric column
sum(second_class$ticket_total)
