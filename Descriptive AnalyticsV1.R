### Course 1 - Statistical Methods for Decision Making ####

## Descriptive Analytics on CardioGood Fitness Data Set

## set working Directory

setwd("C:/Users/rkart/OneDrive/Documents/Great Lakes BABI/Course 1_Statistical Method of Decision Making/In class_Learning Material")

##Import the data set

mydata <- read.csv("CardioGoodFitness.csv", header = TRUE)

View(mydata)

attach(mydata)

## Summary
summary(mydata)

## Get the summary statistics by products
by(mydata, INDICES = Product, FUN = summary)

## Get the summary statistics by Gender
by(mydata, INDICES = Gender, FUN = summary)

## Pattern Recognition

## Histogram for Age
hist(Age, col = "Red")

## Box Plot for Age
boxplot(Age, horizontal = TRUE, col = "Red", main = "Box Plot for Age")

## Box Plot for Age as a function of Gender
boxplot(Age ~ Gender, horizontal = TRUE, col = c("Red", "Blue"), main = "Comparative Box Plot")

## Box Plot for Products
boxplot(Age ~ Product, horizontal = TRUE)

## Creating Cross Tabs (Table)
table(Product, Gender)
table(Product, MaritalStatus)

## Pivot Table in R
library(rpivotTable)

rpivotTable(mydata)

## Standard Deviation of Age
sd(Age)

mean(Age)

## Histogram for miles clocked separtely for Gender
library(lattice)

histogram(~Miles|factor(Gender), data = mydata)

## Histogram for miles clocked separtely for Product
histogram(~Miles|factor(Product), data = mydata)

## Measure correlation between Miles and Usage
cor(Miles, Usage)
