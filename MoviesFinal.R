library(dplyr)
library(ggplot2)
library(mice)

moviesData = read.csv("/Users/lucasmazza/Desktop/IMDB-Movie-Data.csv")


#feature engineering: 

#drop title, description; not relevant 
# print out the column names
print(colnames(moviesData))
#select cols
moviesData <- moviesData[, c("Rank", "Genre","Director","Actors","Year","Runtime..Minutes.","Rating","Votes","Revenue..Millions.","Metascore")]
#encode the genre
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
moviesData


#export csv: 

#write.csv(moviesData, "/Users/lucasmazza/Desktop/Stat512Final/adjustedMoviesData",row.names = FALSE)


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



#(moviesData, "/Users/lucasmazza/Desktop/Stat512Final/adjustedMoviesData",row.names = FALSE)
