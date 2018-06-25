## Load the cleaned dataSet
shootings_good_states <- read.csv('State_Clean_Shootings.csv', 
                                  stringsAsFactors = FALSE, header = TRUE)
shootings_good_states$X <- NULL

## Load the laws dataSet
laws <- read.csv('Laws.csv', stringsAsFactors = FALSE)

open_carry_magazines <- select(laws, X, Open.carry.allowed_H, Open.carry.allowed_L,
                               Magazine.size.restriction_H, 
                               Magazine.size.restriction_L)

colnames(open_carry_magazines)[1] <- "State"

shootings_good_states <- left_join(shootings_good_states, open_carry_magazines,
                                   by='State')

## Change values of state lax on open_carry and magazine restrictions
shootings_good_states[14:15][shootings_good_states[14:15] != "Y"] <- 1
shootings_good_states[14:15][shootings_good_states[14:15] == "Y"] <- 0
shootings_good_states[16:17][shootings_good_states[16:17] != "N"] <- 1
shootings_good_states[16:17][shootings_good_states[16:17] == "N"] <- 0

## Change the values to numeric ones
shootings_good_states[,14:17] <- sapply(shootings_good_states[,14:17],as.numeric)
sapply(shootings_good_states, class)

## Add column with score
shootings_good_states[18] <- rowSums(shootings_good_states[,14:17])
colnames(shootings_good_states)[18] <- 'Law_Score'

## Change it to factors
shootings_good_states$State <- factor(shootings_good_states$State)

## Add frequency per state
shootings_nr <- data.frame(table(shootings_good_states$State))
colnames(shootings_nr)[1] <- 'State'
shootings_good_states <- left_join(shootings_good_states, shootings_nr, by='State')

plotting_data <- select(shootings_good_states, State, Freq, Law_Score)
plotting_data <- na.omit(plotting_data)
plotting_data <- unique(plotting_data)
#plotting_data$Law_Score <- factor(plotting_data$Law_Score)

## Add a nice little plot
ggplot(plotting_data, aes(x = Freq, y = Law_Score, color = Law_Score))+
  geom_point(position = "jitter", size = 2) + 
  xlim(0, 35) +
  scale_color_gradient(low = "red", high = "darkred") +
  labs(color = "Open carry\nMag. Restrict.", x = "Mass shootings", y = "Open Carry and Magazine Laws") + 
  ggtitle("Number of Shootings vs Open cary and magazine restrictions") +
  geom_smooth()

rm(plotting_data)
rm(open_carry_magazines)
rm(laws)
rm(shootings_good_states)
rm(shootings_nr)
