## Gender and race tests 
##---------Load dependencies------##
library(ggplot2)
library(tidyr)
library(dplyr)
##--------------------------------##



##---------------------------------------------------------------------------##
##---------Load data Set----------##
shootings <- read.csv('Mass Shootings Dataset Ver 5.csv', stringsAsFactors = FALSE)
##--------------------------------##


## -------------- Remove Unnecessary Columns -----------------------------------

shootings <- subset(shootings, select = -c(1, 2, 5, 9:11, 13, 16, 20, 21))

## -------------- State & City Columns -----------------------------------------

shootings$City <- sapply(strsplit(shootings$Location, ","), "[", 1)
shootings$State <- sapply(strsplit(shootings$Location, ","), "[", 2)

shootings$City[is.na(shootings$City)] <- "Unknown"
shootings$State[is.na(shootings$State)] <- "Unknown"
shootings$State[shootings$City == "Washington D.C."] = "DC"

shootings$State <- factor(shootings$State)
levels(shootings$State) <- c("VA", "AL", "AK", "NM", "AZ", "AR", "CA",
                             "CA", "CO", "CO", "CT", "FL", "GA", "HI",
                             "ID", "IL", "IN", "IA", "KS", "KY", "LA",
                             "PA", "LA", "ME", "MA", "MD", "MI", "MN",
                             "MS", "MO", "MT", "NE", "NV", "NJ", "NM",
                             "NY", "NC", "NV", "OH", "OK", "OR", "PA",
                             "PA", "CA", "PA", "SC", "SD", "TN", "TX",
                             "TX", "TX", "UT", "VT", "VA", "WA", "WA",
                             "WV", "WI", "WY", "DC", "Unknown")

## -------------- Location Type Column -----------------------------------------

names(shootings) <- sub("Open.Close.Location", "Location_Type", names(shootings))
shootings$Location_Type[is.na(shootings$Location_Type)] <- "unknown"
shootings$Location_Type <- factor(shootings$Location_Type)
levels(shootings$Location_Type) <- c("closed", "open", "open+closed", "open+closed", "unknown")

## -------------- Target Column ------------------------------------------------

shootings$Target[!is.na(shootings$Target) & shootings$Target != "random"] = "not random"
shootings$Target[is.na(shootings$Target)] <- "unknown"

## -------------- Cause Column -------------------------------------------------

shooting_causes <- c("unknown", "psycho", "racism", "religious radicalism", "terrorism", "")
shootings$Cause[!(is.na(shootings$Cause)) & !(shootings$Cause %in% shooting_causes)] <- "other"
shootings$Cause[is.na(shootings$Cause)] <- "unknown"

## -------------- Victims Column -----------------------------------------------

names(shootings) <- sub("Total.victims", "Total_Victims", names(shootings))

## -------------- Age Column ---------------------------------------------------

shootings$Age <- as.character(shootings$Age)
shootings$Age[is.na(shootings$Age)] <- "unknown"

## -------------- Employment Status Column -------------------------------------

names(shootings) <- sub("Employeed..Y.N.", "Employment_Status", names(shootings))
shootings$Employment_Status[shootings$Employment_Status == 1] <- "Yes"
shootings$Employment_Status[shootings$Employment_Status == 0] <- "No"
shootings$Employment_Status[is.na(shootings$Employment_Status)] <- "Unknown"

## -------------- Mental Health Issues Column ----------------------------------

names(shootings) <- sub("Mental.Health.Issues", "Mental_Health_Issues",
                        names(shootings), fixed = TRUE)
shootings$Mental_Health_Issues[shootings$Mental_Health_Issues == "unknown"] <- "Unknown"

## -------------- Race Column --------------------------------------------------

shootings$Race[shootings$Race == "Asian American/Some other race" |
                 shootings$Race == "Asian American"] <- "Asian"
shootings$Race[shootings$Race == "Black American or African American/Unknown" |
                 shootings$Race == "black" |
                 shootings$Race == "Black American or African American" |
                 shootings$Race == "Black"] <- "African American"

shootings$Race[shootings$Race == "Native American or Alaska Native"] <- "Native American"

shootings$Race[shootings$Race == "White American or European American/Some other Race" |
                 shootings$Race == "White American or European American" |
                 shootings$Race == "white"] <- "White"

shootings$Race[shootings$Race == "Some other race"] <- "Other"
shootings$Race[is.na(shootings$Race)] <- "Unknown"

## -------------- Gender Column ------------------------------------------------

shootings$Gender[shootings$Gender == "M"] <- "Male"
shootings$Gender[shootings$Gender == "M/F"] <- "Male/Female"
shootings$Gender[is.na(shootings$Gender)] <- "Unknown"

## -------------- Month & Year Columns -----------------------------------------

shootings$Month <- sapply(strsplit(shootings$Date, "/"), "[", 1)
shootings$Year <- sapply(strsplit(shootings$Date, "/"), "[", 3)

shootings$Month_letters <- shootings$Month
shootings$Month_letters <- factor(shootings$Month_letters)
levels(shootings$Month_letters) <- c("Jan", "Oct", "Nov", "Dec", "Feb", "Mar",
                                     "Apr", "May", "Jun", "Jul", "Aug", "Sep")

## -------------- Remove Unnecessary Columns -----------------------------------

shootings <- subset(shootings, select = -c(1, 2))

# -----------------------------------------------




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