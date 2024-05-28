#rm(list=ls())

install.packages("tidyverse") # data manipulation, makes codes more readable
install.packages("learnr") # makes it easy to turn any R Markdown document into an interactive tutorial
install.packages("readxl") # supports importing data in R (excel files)
install.packages("readr") # support data exporting functions
install.packages("dplyr") # supports in data manipulation and operations


library(tidyverse)
library(learnr) 
library(dplyr)
library(readxl)
library(readr)

getwd()
## setting directory

## dataframe and importing data
# Option 1: with xlsx file: file->import Dataset-> From Excel....

#-------------------------------------------------------
setwd("D:/TRAINING AUGUST2023/DAY1_31.7.23")

#-------------------------------------------------
# Option 2: using read.csv() command

read.csv("lab2_data.csv")

diamond<-read.csv("lab2_data.csv")


diamond # view the dataset

head(diamond) # gives the first 6 rows

str(diamond) # describes the structure of data, numeric (has dec points), integer (no decimal points), character
# get to know the scale of measurement for each. whether categorized or not

head(diamond, 3) # display the first 3 rows of the data.frame

class(diamond) # nature of dataset

summary(diamond) # gives the general summary of the dataset

summary(diamond$weight)

mean(diamond$weight)

#Let R know that quality, color and clarity are categorical

# using as.factor()

diamond$quality <-as.factor(diamond$quality)

diamond$color<-as.factor(diamond$color)

str(diamond)

diamond$quality<-as.factor(diamond$quality)

diamond$color<-as.factor(diamond$color)

table(diamond$quality)

prop.table(table(diamond$quality))

prop.table(table(diamond$quality))*100

qual<- table(diamond$quality) # creat an object
qual

nrow(diamond) # displays the number of rows of the data.frame (no. of observations)

ncol(diamond) # no.of variables/no.of columns




------------------------------------------------------------------------------------------------------

## exporting data

#### library(readr)

write.csv(diamond, file = "diamond.csv") # check new diamond.csv
#written in the working folder

#--------------------------------------------------------------------------------
#Data manipulation 

#DM1: creating new variables
str(diamond)

class(diamond$length)
class(diamond$width)
diamond$sum <- diamond$length + diamond$width

diamond$mean <- (diamond$length + diamond$width)/2

#DM1: Recording a variable
summary(diamond$price)
# using mean as the threshold
help(ifelse)
diamond$pricecat <- ifelse(diamond$price > 4534, c("high"), c("low")) 

?ifelse

############################################################################################
#DM2: RENAMING 

# we can change column names by using the rename() function from the R package dplyr
# we could rename the column "weight" to WEIGHT in the dataset

library(dplyr)

diamond=rename(diamond, WEIGHT = weight) # renaming weight to WEIGHT

colnames(diamond) # see the changed name


#-------------------------------------------------------------------
#DM3: Subsetting data

str(diamond)

#Considering only fair quality diamond
diamondfair<-subset(diamond,quality=="Fair")


#subset a column
diamond$weight



#DM4: REMOVE A COLUMN

#same data set

diamond <- subset(diamond, select = -c(weight, quality))

# or create a new data set

diamond3<-subset(diamond, select = -c(weight, quality))

#-------------------------------------------------------------------------------
#DM6: Level of measurements

#Let R know that quality, color and clarity are categorical
diamond$quality<-as.factor(diamond$quality)

diamond$color<-as.factor(diamond$color)


class(diamond$quality)
class(diamond$color)

table(diamond$quality)
table(diamond$color)


#--------------------------------------------------------------------------------
# DM6: dESCRIPTIVE STATISTICS age > 45 & age <= 75

# central tendency: mean, media, mode
#variability: range, interquartile range, variance, standard deviation
#Frequency tables

diamond<- read.csv("lab2_data.csv")

str(diamond)


summary(diamond)

#measure of central tendency and dispersion

#weight is a continuous variable/ scale so we run a summary statistics

min(diamond$weight); max(diamond$weight); mean(diamond$weight); sd(diamond$weight)

range(diamond$weight); var(diamond$weight); median(diamond$weight)

# quality is a categorical variable so we run a frequency table
table(diamond$quality) # categories variable


# color is a categorical variable so we run a frequency table

table(diamond$color)

colorper <-table(diamond$color)

prop.table(colorper)*100

round(prop.table(colorper)*100)
#percentages


#-------------------------------------------------------------------------------------------------------------------------

# Data visualization in R

library(ggplot2)
diamond<- read.csv("lab2_data.csv")
#Univariate analysis
# Scale : Histogram
str(diamond)

hist(diamond$weight)

hist(diamond$price)

hist(diamond$price, breaks=12, col="red")

#categorical, piechart, bar chart
#bar chart
table(diamond$quality)

counts <- table(diamond$quality)# Simple Bar chart
counts
barplot(counts, main="Quality of Diamond",xlab="Quality type") 

library("epiDisplay") # showing data in ascending order
tab1(diamond$quality,sort.group = "decreasing")
# or
tab1(diamond$quality,sort.group = "decreasing", horiz=TRUE)

#pie-chart (not part of the df)
#quality

#Piechart
pie( table(diamond$quality),
     col = c("white", "gray90", "gray60","blue", "black"))

#-------------------------------------------------------------------------------
#for two variables

#boxplot
boxplot(price~quality,data=diamond,main="Ass btn quality and price", xlab="quality", ylab="price")



#MORE IN DATA VISUALISATION

#####################################################
#INBUILT DATASET IN R

library(MASS)
data()

#using mpg dataset
mpg
names(mpg)
str(mpg)
help(mpg)

mpg$trans<-as.factor(mpg$trans); table(mpg$trans)

mpg$drv<-as.factor(mpg$drv); table(mpg$drv)

mpg$fl<-as.factor(mpg$fl); table(mpg$fl)

mpg$class<-as.factor(mpg$class); table(mpg$class)



#Let’s plot the relationship between automobile class (2seater, compact, midsize, minivan,
# pickup, subcompact, suv) 
#and drive type (drv) (front-wheel, rear-wheel, or 4-wheel drive) 
#for the automobiles in the Fuel economy dataset.
  

data(mpg, package="ggplot2")

# stacked bar chart
ggplot(mpg, 
       aes(x = class, 
           fill = drv)) + 
  geom_bar(position = "stack")

#Explanation
#From the chart, we can see for example, that the most common vehicle is the SUV.
#All 2 seater cars are rear wheel drive, while most, but not all SUVs are 4-wheel drive.

###########
#Grouped bar chart

#Grouped bar charts place bars for the second categorical variable side-by-side.
#To create a grouped bar plot use the position = "dodge" option.

library(ggplot2)

# grouped bar plot
ggplot(mpg, 
       aes(x = class, 
           fill = drv)) + 
  geom_bar(position = "dodge")

#Notice that all Minivans are front-wheel drive. 
#By default, zero count bars are dropped and the remaining bars are made wider.
#This may not be the behavior you want. You can modify this using the position = position_dodge(preserve = "single")" option.

library(ggplot2)

# grouped bar plot preserving zero count bars
ggplot(mpg, 
       aes(x = class, 
           fill = drv)) + 
  geom_bar(position = position_dodge(preserve = "single"))



#Segmented bar chart

#A segmented bar plot is a stacked bar plot where each bar represents 100 percent.
#You can create a segmented bar chart using the position = "filled" option.

library(ggplot2)

# bar plot, with each bar representing 100%
ggplot(mpg, 
       aes(x = class, 
           fill = drv)) + 
  geom_bar(position = "fill") +
  labs(y = "Proportion")

#This type of plot is particularly useful if the goal is to compare the percentage
#of a category in one variable across each level of another variable. 
#For example, the proportion of front-wheel drive cars go up as you move 
#from compact, to midsize, to minivan.


#Homework: Find out how to add percentages on the above plots.

####################################
#DESCRIPTIVE BY GROUPS
###################################


group_by(diamond, quality)%>%dplyr::summarise(
  count=n(),
  mean=mean(length, na.rm=TRUE), # na.rm means remove missing values
  sd= sd(length, na.rm=TRUE) # length of diamond
)

#-----------------------------------------------------------------
# to clear console type
Ctrl+L  

## cleaning the environment space
#console


#alternatively
dev.off()
#or
graphics.off()


#Exercise


