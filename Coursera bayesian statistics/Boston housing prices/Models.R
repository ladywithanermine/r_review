# Models
library(MASS)
library(coda)
library(rjags)

## Load data
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
             price=medv)

## Point estimate model (non-informative prior)
lmod = lm(price ~ crime_rate + residential_landzone + non_retail_land + charles_river + 
            nox_concentration + avg_no_of_rooms + owner_age + dis_employment + 
            access_to_highways + tax + black_people + lower_people,
          data=bdf)
summary(lmod)

## Bayesian model

