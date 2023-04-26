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
top_10_genres <- names(sorted_freq)[1:10]
moviesData <- subset(moviesData, Genre %in% top_10_genres)
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
#write.csv(moviesData, "/Users/lucasmazza/Desktop/Stat512Final/FINALDATASET",row.names = FALSE)


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



fullModel = lm(y~x2+x6, data = moviesData)

anova(fullModel)
qqPlot(fullModel)

resNorm = residuals(fullModel)
resNorm
residualPlots(fullModel)
bptest(fullModel)

shapiro.test(resNorm)

vif(lm(y~x2+x6, data = moviesData))
vif(lm(y~x6+x2, data = moviesData))
#no multicollinarity issues
dfbetasPlots(fullModel)
influencePlot(fullModel, id.method = "cook", plot = TRUE)

cooksd <- cooks.distance(fullModel)


# Create a bar plot of Cook's distances
plot(cooksd, type = "h", lwd = 2, ylab = "Cook's Distance", main = "Cook's Distance Plot")

# Add a horizontal line at Cook's distance of 0.5
abline(h = 0.5, col = "red")
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
library(car)
resNorm = residuals(Fullmodel)
resNorm
residualPlots(Fullmodel)
plot(fitted(Fullmodel),residuals(Fullmodel), xlab = "Fitted Values", ylab = "Residuals",main = "Residuals vs Fitted Values Plot")
#we can say that the assumption of constant varience has been violated here
shapiro.test(resNorm)

#presence of outliers:
qqline(resNorm)
qqPlot(resNorm)

#VIF: 

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

# Define function for bootstrapping
boot.wls <- function(data, indicies, maxit = 20) {
  data1 <- data[indicies, ]
  y = data1$Votes
  x1 = data1$Rating
  x3 = data1$Genre
 
  # Fit initial model
  Fullmodel <- lm(y ~ x1 + x3 + x1*Action + x1*Drama + x1*Comedy, data = data1)
  
  # Compute weight vector
  wts1 <- 1/fitted(lm(abs(residuals(Fullmodel)) ~ x1 + x3, data = data1))^2
  
  # Fit WLS model using weight vector
  modelWLS <- lm(y ~ x1 + x3 + x1*Action + x1*Drama + x1*Comedy, data = data1, weights = wts1)
  
  return(coef(modelWLS))
}

# Run bootstrap with 100 iterations
WLS.boot <- boot(data = moviesData, statistic = boot.wls, R = 100, maxit = 20)
summary(WLS.boot)
# View results
WLS.boot

wts1 <- 1/fitted(lm(abs(residuals(lm(y~x1 + x3 + x1*Action + x1*Drama + x1*Comedy, data)))~x1 + x3 + x1*Action + x1*Drama + x1*Comedy, data = data))^2

# Fit WLS model using weight vector
modelWLS <- lm(y ~ x1 + x3 + x1*Action + x1*Drama + x1*Comedy, data = data, weights = wts1)

residualPlot(modelWLS)
residualPlot(Fullmodel)
plot(fitted(Fullmodel), rstandard(Fullmodel))
abline(lm(rstandard(Fullmodel) ~ fitted(Fullmodel)), col = "red")

plot(fitted(modelWLS), rstandard(modelWLS))
abline(lm(rstandard(modelWLS) ~ fitted(modelWLS)), col = "red")

qqPlot(modelWLS)
qqPlot(Fullmodel)
#conclusion
anova(Reduced, modelWLS)


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
                   
# question 5
movie.reducedmod<- lm(Votes~Runtime_Minutes, myData1)
anova(movie.reducedmod)

movie.fullmod <- lm(Votes~Runtime_Minutes + Year, myData1)
anova(movie.fullmod)

anova(movie.reducedmod, movie.fullmod)


summary(movie.reducedmod)
summary(movie.fullmod)

dfbetasPlots(movie.fullmod)

resNorm = residuals(movie.fullmod)
resNorm
residualPlots(movie.fullmod)
plot(fitted(movie.fullmod),residuals(movie.fullmod), xlab = "Fitted Values", ylab = "Residuals",main = "Residuals vs Fitted Values Plot")
#we can say that the assumption of constant varience has been violated here
shapiro.test(resNorm)

qqPlot(resNorm)
qqline(resNorm)

#multicollinearity
vif(lm(Votes~Year+Runtime_Minutes, data=myData1))

influencePlot(movie.fullmod, id.method = "cook", plot = TRUE)

cooksd <- cooks.distance(movie.fullmod)


# Create a bar plot of Cook's distances
plot(cooksd, type = "h", lwd = 2, ylab = "Cook's Distance", main = "Cook's Distance Plot")

# Add a horizontal line at Cook's distance of 0.5
abline(h = 0.5, col = "red")

summary(movie.fullmod)

wts1 <- 1/fitted(lm(abs(residuals(movie.fullmod)) ~ Year + Runtime_Minutes, data = myData1))^2 # making weighted model
modelWLS <- lm(Votes ~ Year + Runtime_Minutes, data = myData1, weights = wts1)

summary(modelWLS)


qqPlot(resWeight)
qqline(resWeight)

resWeight <- residuals(modelWLS)
library(lmtest)
bptest(movie.fullmod)


# Define function for bootstrapping
boot.wls <- function(myData1, indicies, maxit = 20) {
  data1 <- myData1[indicies, ]
  y = data1$Votes
  x4 = data1$Year
  x6 = data1$Runtime_Minutes
  
  # Fit initial model
  Fullmodel <- lm(y ~ x4 + x6 , data = data1)
  
  # Compute weight vector
  wts1 <- 1/fitted(lm(abs(residuals(movie.fullmod)) ~ Year + Runtime_Minutes, data = data1))^2
  
  # Fit WLS model using weight vector
  modelWLS <- lm(y ~ Year + Runtime_Minutes , data = data1, weights = wts1)
  
  return(coef(modelWLS))
}

WLS.boot <- boot(data = myData1, statistic = boot.wls, R = 100, maxit = 20)
summary(WLS.boot)
# View results
WLS.boot

resBoot = residuals(WLS.boot)
residualPlot(resBoot)
resBoot

Reduced <- lm(Votes~Runtime_Minutes, data=myData1)
anova(Reduced, modelWLS)


