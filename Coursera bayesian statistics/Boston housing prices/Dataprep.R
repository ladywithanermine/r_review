# Load libraries
library(MASS)
library(dplyr)
library(ggplot2)
library(corrplot)
library(DataExplorer)

# Load data
data(Boston)

bdf = rename(Boston, 
             crime_rate=crim, 
             residential_landzone=zn, 
             non_retail_land=indus,
             charles_river=chas, 
             nox_concentration=nox,
             avg_no_of_rooms=rm, 
             owner_age=age,
             dis_employment=dis, 
             access_to_highways=rad,
             tax=tax, 
             black_people=black,
             lower_people=lstat, 
             owner_occupied_homes_price=medv)

# Explore data
head(bdf)
summary(bdf)
glimpse(bdf)

## Missing values
sum(is.na(bdf))

## Duplicated values
sum(duplicated(bdf))

## Some general plots
plot_histogram(bdf)

## Pairplot
pairs(bdf)

## Correlation
corrplot(cor(bdf), method = "number", type = "upper", diag = TRUE)

## Some plots
ggplot(bdf, aes(x = owner_age, y = owner_occupied_homes_price)) +
  geom_point(color = "blue") +
  labs(x = "owner_age", y = "owner_occupied_homes_price (Median Home Value)") +
  ggtitle("Scatterplot of owner_age vs. owner_occupied_homes_price")

## Outliers and their relationship to the crime rate
par(mfrow = c(1, 4))
boxplot(bdf$crime_rate, main='crime_rate',col='Sky Blue')
boxplot(bdf$avg_no_of_rooms, main='resi_land',col='Orange')
boxplot(bdf$residential_landzone, main='resi_land',col='Green')
boxplot(bdf$black_people, main='black_people',col='Red')