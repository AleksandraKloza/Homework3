#Loading "tidyverse" to make R easier
library(tidyverse)
#Set options on scientific notation and the number of digits printed
options(scipen = 5, digits = 3)


#Question 2

#Loading in the Excel data
stroop_data <- read_csv("stroop_standing_data.csv")
view(stroop_data)

#Filtering the data to get rid of missing values
stroop_data <- select(stroop_data, -X11)
view(stroop_data)

#Changing the reaction time to a numeric variable
stroop_data <- mutate(stroop_data, rt = as.numeric(rt))

#Getting rid of false values by making sure all of them are positive
stroop_data <- filter(stroop_data, rt > 0)

#Getting rid of the practice values
stroop_data <- filter(stroop_data, phase != "practice")

#Getting rid of the mistrials by leaving just the ones coded as correct (1) and incorrect (0)
stroop_data <- filter(stroop_data, correct < 2)

#It's a linear model because it's a relationship between "position" (standing or sitting down) and reaction time
stroop_model <- lm(rt~condition, stroop_data)
summary(stroop_model) 
#Significant results; There is a relationship between reaction time and condition - lower reaction time in condition with a start standing up than in condition with a start sitting down

#Plotting the model
stroopplot <- ggplot(stroop_model, aes(x = condition, y = rt)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red") +
  ylab("Reaction time intercept") + xlab("Condition")

#RESULTS:
#In the original study, participants showed a smaller Stroop effect when they performed the task standing than when sitting
#Results from this replication showed a significant lower reaction time in participants who started the task sitting down (condition 1) than standing up (condition 2)
#For the sitting down condition the intercept is equal 819.47, and for the standing up condition it is 841.52 (819.47+22.05), what is also shown on the graph
#In conclusion, the replication of the study showed an opposite result than the original study

#Question 3

#Loading in the Excel data
spotify_data <- read.csv("spotify_cleaned.csv")
view(spotify_data)

#Removing irrelevant data
spotify_data <- select(spotify_data, -X, -Track.Name, -Artist, -Streams, -time_signature)

#Running a PCA
pca <- prcomp(spotify_data) 
summary(pca) #We can see that around 99% of the variance is explained by the first two principal components

#PCA needs to be performed on a symmetric correlation or covariance matrix, so an improvement could be scaling the columns

spotify_data_s <- mutate(spotify_data,
                       tempo = scale(tempo),
                       energy = scale(energy),
                       danceability = scale(danceability),
                       loudness = scale(loudness),
                       valence = scale(valence),
                       acousticness = scale(acousticness)
                       )

pca_s <- prcomp(spotify_data_s)
summary(pca_s)
#After centering the variables the variance changed - it is less spread out now
#The first principal component covers almost 99% of the variance

#Installing packages and plotting the results
screeplot(pca, type = "lines")
install.packages("factoextra")
library("factoextra")
fviz_pca_var(pca)

