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

#Removing trial and stimulus for future plots
stroop_data <- select(stroop_data, -trial)
stroop_data <- select(stroop_data, -stimulus)

#To test the hypothesis I will fit a linear model 
#The measured relationship is between reaction time in different phases (standing vs sitting) in congruent and incongruent trials.
stroop_model_phase <- lm(rt ~ phase + congruency, stroop_data)
summary(stroop_model_phase) 
#Only the sitting down block is significantly related to the reaction time. Standing phase is not significant.As in the previous model, only the reaction times during incongruent trials are impacted.

#Plotting the model after uniting the variables
stroop_data %>%
  unite("phaseandcongruency", phase:congruency, na.rm = TRUE, remove = FALSE) %>%
  glimpse() -> phasecongruency

ggplot(data = phasecongruency, aes(phaseandcongruency)) + geom_boxplot(aes(phaseandcongruency, rt))
#The plot shows the insignificant difference between reaction times in baseline and congruent conditions while both sitting down and standing up

#RESULTS:
# - In the original study, participants showed a smaller Stroop effect when they started the task standing than when sitting
# - Results from this replication showed that reaction time was significantly impacted only during incongruent trials. Sitting down was the only significant phase. It suggests that standing up is not a factor that impacts reaction time at all, and sitting down impacts the reaction time only in incongruent trials. 
# If reaction time increases while sitting down in incongruent trials, the results mean that the Stroop effect will be smaller if participants standing.
# In conclusion, the replication study confirmed the original study's results as the Stroop effect is smaller in the standing phase than the sitting down phase.

#Question 3

#Loading in the Excel data
spotify_data <- read.csv("spotify_cleaned.csv")
view(spotify_data)

#Removing irrelevant data
spotify_data <- select(spotify_data, -X, -Track.Name, -Artist, -Streams, -time_signature)

#Running a PCA
pca <- prcomp(spotify_data) 
summary(pca) 
# We can see that around 99% of the variance is explained by the first two principal components

# PCA needs to be performed on a symmetric correlation or covariance matrix, so an improvement could be scaling the columns
spotify_data_s <- mutate(spotify_data,
                         tempo = scale(tempo),
                         energy = scale(energy),
                         danceability = scale(danceability),
                         loudness = scale(loudness),
                         valence = scale(valence),
                         acousticness = scale(acousticness),
                         key = scale(key),
                         mode = scale(mode),
                         speechiness = scale(speechiness),
                         instrumentalness = scale(instrumentalness),
                         liveness = scale(liveness),
                         duration_sec = scale(duration_sec)
                         )

pca_s <- prcomp(spotify_data_s)
summary(pca_s)

#After centering the variables the variance changed - it is more spread out now

#Plotting the results
library("factoextra")
screeplot(pca_s, type = "lines")
fviz_pca_var(pca_s)

#The graph shows that acousticness is a factor related to popularity in the strongest positive way.
