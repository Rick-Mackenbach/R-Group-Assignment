## Resulting scores for 'easiness' buying guns per state

## Load the dataset
laws <- read.csv('Laws.csv', stringsAsFactors = FALSE)

## Change all values to integer scores
laws[-1][laws[-1] != 'Y'] <- 1
laws[-1][laws[-1] == 'Y'] <- 0

## Change the values to integers
laws[,2:13] <- sapply(laws[,2:13],as.numeric)
sapply(laws, class)

## Sum the rows and change column name
laws[14] <- rowSums(laws[-1])
colnames(laws)[1] <- 'State'
colnames(laws)[14] <- 'Law Score'

## Remove dirty dataset
cleaned_laws <- laws
rm(laws)

## Save cleaned dataset
# write.csv(cleaned_laws, file = "cleaned_laws.csv", row.names = FALSE)

## Save Law score per state
state_law_scores <- select(cleaned_laws, 'State', 'Law Score')
#write.csv(state_law_scores, file = "state_law_scores", row.names = FALSE)
