## -------------- Load Dataset -------------------------------------------------

shootings <- read.csv("input/shootings.csv", stringsAsFactors = FALSE, header = TRUE, na.strings=c(""," ","NA"))

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
shootings$Mental_Health_Issues[shootings$Mental_Health.Issues == "unknown"] <- "Unknown"

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

























