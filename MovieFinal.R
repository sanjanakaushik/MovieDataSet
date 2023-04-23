library(dplyr)
library(ggplot2)
library(mice)
library(corrplot)
library(boot)
library(MASS)
library(lmridge)

moviesData = read.csv("/Users/lucasmazza/Desktop/IMDB-Movie-Data.csv")


#feature engineering: 

#drop title, description; not relevant 
# print out the column names
print(colnames(moviesData))
#select cols
moviesData <- moviesData[, c("Rank", "Genre","Director","Actors","Year","Runtime..Minutes.","Rating","Votes","Revenue..Millions.","Metascore")]

# extract the first and last names from each list in genre

findGenre <- strsplit(moviesData$Genre, ",")

mainGenre <- sapply(findGenre, function(x) x[1])
moviesData$Genre = mainGenre

#find the 5 most popular genres: 
genre_freq <- table(moviesData$Genre)

# Sort the frequency table in descending order
sorted_freq <- sort(genre_freq, decreasing = TRUE)
sorted_freq
# Get the top 5 most popular genres
top_5_genres <- names(sorted_freq)[1:5]
moviesData <- subset(moviesData, Genre %in% top_5_genres)
nrow(moviesData)
moviesData$Genre <- as.numeric(factor(moviesData$Genre))



#encode the Director: 
moviesData$Director <- as.numeric(factor(moviesData$Director))


#get actor who played main character: 

findNames <- strsplit(moviesData$Actors, ", ")

# extract the first and last names from each list
mainActor <- sapply(findNames, function(x) x[1])
moviesData$Actors = mainActor


#encode the actors: 
moviesData$Actors <- as.numeric(factor(moviesData$Actors))
moviesData[0:10]

#Adjusting column names for runTime and revenue
moviesData = rename(moviesData, Runtime_Minutes = Runtime..Minutes., Revenue_Millions = Revenue..Millions.)
nrow(moviesData)


#export csv: 


#check for NA: 
sum(is.na(moviesData))
#we have about 192 NA values
na_count <- sapply(moviesData, function(x) sum(is.na(x)))
na_count
#Revenue_millions and MetaScore are missing 128 values and 64 respectively
na_rows <- moviesData[!complete.cases(moviesData), ]

# View the resulting rows
na_rows

#explore data to find best way to fit NA (REMOVE NA FIRST TO EXPLORE TRENDS): 
moviesDataNoNA = na.omit(moviesData)
#find trend in metaScore

# Create a histogram of Metascore
ggplot(moviesDataNoNA, aes(x = Metascore)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "white") +
  labs(title = "Histogram of Metascore", x = "Metascore", y = "Frequency")

#approach 1 for metascore
#????the Metascore looks normally distributed, so we can sample from a normal distribution to replace the NA values
# identify missing values in Metascore column
missing <- is.na(moviesData$Metascore)

# calculate mean and standard deviation of non-missing values in Metascore column
mean_val <- mean(moviesData$Metascore[!missing], na.rm = TRUE)
sd_val <- sd(moviesData$Metascore[!missing], na.rm = TRUE)

# generate random values from normal distribution with mean and sd of non-missing values
rand_vals <- rnorm(sum(missing), mean = mean_val, sd = sd_val)

# replace missing values with generated random values
moviesData$Metascore[missing] <- rand_vals

#re-check our distribution: 
ggplot(moviesData, aes(x = Metascore)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "white") +
  labs(title = "Histogram of Metascore", x = "Metascore", y = "Frequency")

#approach 2: remove all NA values. 


#approach 1 for revenue: 
# Create a histogram of Metascore
ggplot(moviesDataNoNA, aes(x = Revenue_Millions)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "white") +
  labs(title = "Revenue_Millions", x = "Metascore", y = "Frequency")
#very right skewed, possibly trying sampling some the already existing data: 
existing_values <- moviesData$Revenue_Millions[!is.na(moviesData$Revenue_Millions)]

# replace the missing values by sampling from the existing values
moviesData$Revenue_Millions[is.na(moviesData$Revenue_Millions)] <- sample(existing_values, sum(is.na(moviesData$Revenue_Millions)), replace = TRUE)

#re-check the distribution:
ggplot(moviesData, aes(x = Revenue_Millions)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "white") +
  labs(title = "Revenue_Millions", x = "Metascore", y = "Frequency")

#check that we have taken care of all NA values: 
sum(is.na(moviesData))
#all NA values have been taken care of. 
nrow(moviesData)
#feature analysis:
write.csv(moviesData, "/Users/lucasmazza/Desktop/Stat512Final/FINALDATASET",row.names = FALSE)


#max votes: 
max_row <- which.max(moviesData$Votes)

# Print the row with the maximum value
print(moviesData[max_row, ])

moviesData

#rank and votes is obviously related 
  #year and votes have a bit of a negatively correlated relationship
  #it seems like the rank is based purely off how many people watched these movies


plot(scatterMovies$Rank, scatterMovies$Rating, xlab = "Rank", ylab = "Rating", main = "Rating vs Votes")

# Add regression line
abline(lm(scatterMovies$Rating ~ scatterMovies$Rank), col = "red")

#Can we see anything from correlation: 
# Compute the correlation matrix
cor_mat <- cor(moviesData)

# Plot the correlation matrix
corrplot(cor_mat, method = "circle")

#votes and revenue are slightly higher correlated 
#Votes and rating are somewhat higher correlated
#Metascore and Rating are highly correlated

#features: 
  #-votes is important since according to IMDB, this is the total aggregate of all people who have
  #left a rating on a movie.

#question 1: 

#model
#Y: the number of votes. 
#X1: rating
#X2: Revenue_Millions
#X3: Genre
#X4: Director
#X5: Main Actor

moviesData
y = moviesData$Votes
x1 = moviesData$Rating
x2 = moviesData$Revenue_Millions
x3 = moviesData$Genre
x4 = moviesData$Director
x5 = moviesData$Actors
x6 = moviesData$Runtime_Minutes

model = lm(y ~ x1 + x2 + x3 + x4 + x5, moviesData)
summary(model)

anova(model)


#Question 1:
#Does the run-time and revenue have a significant impact on the number of votes that a movie recieved . 
#Ho: B2 = B6 = Bnew 
#Reduced model = Y = B0 + Bnew(X2+X6) + error
#Ha: B2 ≠ B6
#Full model = Y = B0 + B2*X2+ B6*X6 + error
modelReduced = lm(y~I(x2 + x6), data = moviesData) 
fullModel = lm(y~x1+x2+x6, data = moviesData)
anova(modelReduced, fullModel)

#conclusion: run time and revenue do not have the same impact on the number of votes a movie received




#question 2: 
#is the impact of rating on number votes the same for the top three most popular genres.
#Ho: 
#Ha: full model
moviesData$Genre
Action <- ifelse(moviesData$Genre %in% c(1), 1,0)
Drama  <- ifelse(moviesData$Genre %in% c(2), 1,0)
Comedy <- ifelse(moviesData$Genre %in% c(3), 1,0)

Reduced = lm(y~x1+x3)
Fullmodel = lm(y ~ x1 + x3 + x1*Action + x1*Drama + x1*Comedy, data = moviesData)
summary(Fullmodel)
#Lower R^2 value, we might be able to help that in some adjustments

  
#model analysis: 
#residuals: 
resNorm = residuals(Fullmodel)

residualPlots(Fullmodel)
plot(fitted(Fullmodel),residuals(Fullmodel), xlab = "Fitted Values", ylab = "Residuals",main = "Residuals vs Fitted Values Plot")
#we can say that the assumption of constant varience has been violated here
shapiro.test(resNorm)

#presence of outliers. 
qqline(resNorm)
qqplot(resNorm)
#non-normal distribution of the residuals

#Robust bootstrapping to address the problem of outliers and deviations from normality: 

#cooks distance: 
dfbetasPlots(Fullmodel)
influencePlot(Fullmodel, id.method = "cook", plot = TRUE)


cooksd <- cooks.distance(Fullmodel)
threshold <- quantile(cooksd, 0.8)
abline(h = threshold, col = "blue")
influential <- which(cooksd > threshold)
influential

boot.Robust <- function(data, indicies,maxit = 100) {

  modelRobust <- rlm(y ~ x1 + x3 + x1*Action + x1*Drama + x1*Comedy + x1*Adventure + x1 * Crime + x1 * Biography+x1*Animation + x1 *Horror + x1*Mystery + 
                     x1*Thriller , moviesData)
  coefficients(modelRobust)
}

Robust.boot <- boot(data = moviesData, statistic = boot.Robust, R = 100, maxit = 100)
Robust.boot



#conclusion
anova(Reduced, Fullmodel)


#question 3: Do Rating, Runtime, and Revenue have a significant effect on the number of votes?
library(car)
model = lm(y~x1+x2+x6, data = moviesData)
resNorm = resNorm[1:5000]
#checking distribution of the residual errors: 


#violation in normality
shapiro.test(resNorm)
qqnorm(resNorm)
qqline(resNorm)
#we have a violation in the distribution of the residuals, does not appear to be normal


