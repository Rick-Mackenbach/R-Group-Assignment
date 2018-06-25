## Gender and race tests 
##---------Load dependencies------##
library(ggplot2)
library(tidyr)
library(dplyr)
##--------------------------------##



##---------------------------------------------------------------------------##
##---------Load data Set----------##
shootings <- read.csv('cleaned_shootings1', stringsAsFactors = FALSE)
##--------------------------------##




## -- Some pre-cleaning -- ##
## Remove unkown genders
shootings$Gender[shootings$Gender == "Unknown"] <- NA
shootings <- na.omit(shootings)


##---------------------------------------------------------------------------##
##                                                                           ##
##                            PLOTTING                                       ##
##---------------------------------------------------------------------------##
## -- Plot some initial results -- ##
## Basic plots
## How many males and females are responsible for mass shootings?
ggplot(shootings, aes(x = Gender)) + 
  geom_bar() + 
  theme_set(theme_bw())

## Show differences between gender and total victims
ggplot(shootings, aes(x = Gender, y = Total_Victims) ) + 
  geom_bar(stat='identity') + 
  theme_set(theme_bw())

## Show differences between race, gender and total_victims
ggplot(shootings, aes(x = Gender, y = Total_Victims, color = Race) ) + 
  geom_bar(stat='identity',position='dodge')

## Error bars for gender and amount of kills
mean_kills <- shootings %>% group_by(Gender) %>%
  summarize(ymin = min(Total_Victims),
            ymax = max(Total_Victims),
            ymean = mean(Total_Victims))

ggplot(mean_kills, aes(x = Gender, y = ymean)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax))


##---------------------------------------------------------------------------##
##                                                                           ##
##                            ANALYSES                                       ##
##---------------------------------------------------------------------------##
## Remove male/female duo mass-shootings:
shootings$Gender[shootings$Gender == 'Male/Female'] <- NA
shootings <- na.omit(shootings)

## Remove unknown race
shootings$Race[shootings$Race == 'Unknown'] <- NA
shootings <- na.omit(shootings)

## Correlation is not possible with categorical variables
## so we use chisq.test
chi_square_results <- chisq.test(x = shootings$Gender, y = shootings$Race)
chi_square_results




## The chi-square shows significant results P = 0.002
## This means that race and gender are dependent on each other

## Perhaps we could get some more results with linear regression
## Change values to dummy variables
shootings$Gender <- factor(shootings$Gender)
shootings <- shootings %>%
  mutate(Gender = relevel(Gender, ref = "Male"))

## Run LM
summary(lm(Total_Victims ~ Gender, data = shootings))

## We have a significant interaction, which means that there is a significant
## difference between males and females.


## Use linear regression to predict victims based on race
## Use dummy coding
shootings$Race <- factor(shootings$Race)
shootings <- shootings %>%
  mutate(Race = relevel(Race, ref = "White"))

## Run LM
summary(lm(Total_Victims ~ Race, data = shootings))

## No significance here


##---------------------------------------------------------------------------##