#import missingdata.xlsx in R
dt<-missingdata
is.na(dt)
dt$Age[dt$Age==25]= 30

#Removing all missing variables
na.omit(dt)
