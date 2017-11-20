library(readxl)
library(lubridate)
library(dplyr)
library(ggplot2)

###########################
## Functions Definitions ##
###########################

# Transform dataset
transformDataset <- function(dataset){
  # Transform dataset
  dataset$Type <- as.factor(dataset$Type)
  # Column with corresponding weekdays
  dataset$Weekday <- wday(dataset$Time, label=TRUE)
  # Column with time intervals
  dataset$TimeInterval[hour(dataset$Time) >= 6 & hour(dataset$Time) < 12] <- "6 to 11h59"
  dataset$TimeInterval[hour(dataset$Time) >= 12 & hour(dataset$Time) < 18] <- "12 to 17h59"
  dataset$TimeInterval[hour(dataset$Time) >= 18 & hour(dataset$Time) < 24] <- "18 to 23h59"
  dataset$TimeInterval[hour(dataset$Time) >= 0 & hour(dataset$Time) < 6] <- "00h to 5h59"
  dataset$TimeInterval <- as.factor(dataset$TimeInterval)
  # Columns with day, week number
  # Get start date for each user
  start_date_per_user = aggregate(dataset[, c('Time')], list(dataset$User), min)
  colnames(start_date_per_user) <- c("User", "StartDate")
  # Merge the two dataframes
  dataset <- merge(x = dataset, y = start_date_per_user, by = "User")
  # Find the day number with computing week differences with start date
  dataset$DayNumber <- as.integer(difftime(dataset$Time, dataset$StartDate, units = "days"))
  # Find the week number by looking at the day number
  dataset$WeekNumber <- as.integer(dataset$DayNumber / 7)
  # Observation week is supposed to be week 0, so we change every week number to 0 for observation weeks
  dataset$WeekNumber[dataset$Type == "Observation week"] <- 0
  # Find the last day for each user
  lastDayPerUser <- summarize(group_by(dataset, User), lastDay=max(DayNumber))
  dataset <- merge(dataset, lastDayPerUser)
  
  return(dataset)
}

# Compute total number of cigarettes per user (all modes)
totalCigarettesAllModes <- function(dataset){
  nb_cigarettes = aggregate(dataset$User, by=list(dataset$User), FUN=length)
  colnames(nb_cigarettes) <- c("User", "Total")
  return(nb_cigarettes)
}

# Compute total number of cigarettes per user per mode
totalCigarettesPerMode <- function(dataset){
  nb_cigarettes_per_mode = aggregate(dataset$Type, by=list(dataset$User, dataset$Type), FUN=length)
  colnames(nb_cigarettes_per_mode) <- c("User", "Mode", "Total")
  return(nb_cigarettes_per_mode)
}

# Compute mean and standard deviation of the number of consumed cigarettes per weekday per user
computeMeanSDtotalCigarettesPerWeekday <- function(dataset){
  
  # First we group by user, weekday and week number, to count the number of cigarettes per week day of each week
  byUserPerWeekdayPerWeek <- group_by(dataset, User, Weekday, WeekNumber)
  cigarettes_per_weekday_per_week <- summarize(byUserPerWeekdayPerWeek, count=n())
  # Then we compute the mean and standard deviation of consumed cigarettes per weekday and user
  byUserPerWeekday <- group_by(cigarettes_per_weekday_per_week, User, Weekday)
  mean_sd_cigarettes_per_weekday <- summarize(byUserPerWeekday, mean=mean(count), sd=sd(count))
  
  return(mean_sd_cigarettes_per_weekday)
}

# Compute the number of consumed cigarettes for the last seven days
computeNumberConsumedCigarettesLastSevenDays<- function(dataset){
  
  # Filter to keep the last 7 days for each user
  last_seven_days <- group_by(dataset[userdata$DayNumber > (dataset$lastDay - 7),], User)
  nb_consumed_cigarettes_last_seven_days <- summarize(last_seven_days, count=n())
  
  return(nb_consumed_cigarettes_last_seven_days)
}

# Compute percentage of improvement between two weeks
computePercentageOfImprovementBetweenWeeks <- function(dataset, weekA, weekB){
  
  # Count number of consummed cigarettes for week 0, 1 and 2 without friend mode
  countConsummedCigarettesPerWeek = merge(
    x = summarize(group_by(dataset[dataset$WeekNumber == weekA & dataset$Type != "Friend",], User), countWeekA = n()),
    y = summarize(group_by(dataset[dataset$WeekNumber == weekB & dataset$Type != "Friend",], User), countWeekB = n())
  )
  # Compute the % of difference between the two weeks
  countConsummedCigarettesPerWeek$ImprovementPercent <- ((countConsummedCigarettesPerWeek$countWeekA - countConsummedCigarettesPerWeek$countWeekB) / countConsummedCigarettesPerWeek$countWeekA) * 100
  
  return(countConsummedCigarettesPerWeek)
}

# Compute the mean smoking pattern per weekday per user
computeMeanSmokingPatternPerWeekday <- function(dataset){
  
  # Mean smoking pattern per weekday is the smoking pattern (type and time interval) with the highest cigarettes consumed per weekday
  cigarettesCountPerWeekdayPatternPerUser <- summarize(group_by(dataset, User, Weekday, TimeInterval, Type), count=n())
  # Compute highest count per weekday
  meanSmokingPatternCount <- aggregate(count ~ User + Weekday, cigarettesCountPerWeekdayPatternPerUser, max)
  # Merge to link user and mean smoking patterns
  meanSmokingPatternPerWeekday <- merge(cigarettesCountPerWeekdayPatternPerUser, meanSmokingPatternCount)
  # remove rows with identical user and count
  meanSmokingPatternPerWeekday <- meanSmokingPatternPerWeekday[!duplicated(meanSmokingPatternPerWeekday[,c('User', 'Weekday', 'count')]),]
  # Remove count column
  meanSmokingPatternPerWeekday <- meanSmokingPatternPerWeekday[, c("User", "Weekday", "TimeInterval", "Type")]
  
  return(meanSmokingPatternPerWeekday)
}

# Compute week period with the least or most smoking density per user
computeWeekPeriodWithLeastOrMostSmokingDensity <- function(dataset, most){
  
  # Count number of cigarettes per user per week period
  cigarettesCountPerWeekPeriodPerUser <- summarize(group_by(dataset, User, Weekday, TimeInterval), count=n())
  
  # Compute least and most smoking density per user
  if(most == TRUE){
    leastOrMostSmokingDensityPerUser <- aggregate(count ~ User, cigarettesCountPerWeekPeriodPerUser, max)
  } else {
    leastOrMostSmokingDensityPerUser <- aggregate(count ~ User, cigarettesCountPerWeekPeriodPerUser, min)
  }

  # Merge to link a user to his smoking density week period
  leastOrMostSmokingDensityPerUserPerWeekPeriod <- merge(cigarettesCountPerWeekPeriodPerUser, leastOrMostSmokingDensityPerUser)
  
  # remove rows with identical user and count
  leastOrMostSmokingDensityPerUserPerWeekPeriod <- leastOrMostSmokingDensityPerUserPerWeekPeriod[!duplicated(leastOrMostSmokingDensityPerUserPerWeekPeriod[,c('User','count')]),]
  
  # Remove count column
  leastOrMostSmokingDensityPerUserPerWeekPeriod <- leastOrMostSmokingDensityPerUserPerWeekPeriod[, c("User", "Weekday", "TimeInterval")]

  return(leastOrMostSmokingDensityPerUserPerWeekPeriod[order(leastOrMostSmokingDensityPerUserPerWeekPeriod$User),])
}

# Compute classified time intervals
computeClassifiedTimeIntervals <- function(dataset){
  # smokers tend to smoke at what time interval
  timeIntervalTendencies <- summarize(group_by(userdata, TimeInterval), count=n())
  # Order time intervals
  timeIntervalTendencies <- timeIntervalTendencies[order(-timeIntervalTendencies$count),]
  
  return(timeIntervalTendencies)
}

# Compute classified time intervals per mode
computeClassifiedTimeIntervalsPerMode <- function(dataset){
  # Smokers tend to smoke with which mode in what time interval
  timeIntervalTendenciesPerMode <- summarize(group_by(userdata, Type, TimeInterval), count=n())
  timeIntervalTendenciesPerMode <- timeIntervalTendenciesPerMode[order(timeIntervalTendenciesPerMode$Type, -timeIntervalTendenciesPerMode$count),]
  return(timeIntervalTendenciesPerMode)
}


######################
## Dataset Analysis ##
######################

# Load dataset
userdata <- read_excel("userdata.xlsx")

# Transform the dataset
userdata <- transformDataset(userdata)

### User stats

## Total number of cigarettes
# Total number of cigarettes (all modes)
totalCigarettesAllModes(userdata)
# Total number of cigarettes per mode
totalCigarettesPerMode(userdata)

## Mean and standard deviation of the number of consumed cigarettes per weekday 
# (Mondays, Tuesdays, ...) with the corresponding plot
computeMeanSDtotalCigarettesPerWeekday(userdata)
# Plot for User 1
ggplot(computeMeanSDtotalCigarettesPerWeekday(userdata[userdata$User == 1,]), aes(x=Weekday, y=mean)) +
geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2) +
geom_line() +
geom_point()

## Plot of the number of consumed cigarettes for the last seven days
computeNumberConsumedCigarettesLastSevenDays(userdata)
plot(computeNumberConsumedCigarettesLastSevenDays(userdata))

## Statistics on modes
summary(userdata$Type)

## Percentage of improvement per week for week 1 and week 2
# Percentage of improvement per week for week 1
computePercentageOfImprovementBetweenWeeks(userdata, 0, 1)
# Percentage of improvement per week for week 2
computePercentageOfImprovementBetweenWeeks(userdata, 1, 2)

## Plot of the mean smoking pattern per weekday
computeMeanSmokingPatternPerWeekday(userdata)

## Find the week period with the most and least smoking density
# Week period with the least smoking density
computeWeekPeriodWithLeastOrMostSmokingDensity(userdata, TRUE)
# Week period with the most smoking density
computeWeekPeriodWithLeastOrMostSmokingDensity(userdata, FALSE)


### General all-user stats

## Users classified number of smoking cigarettes intervals, 
## by smoking history time (smokers tend to smoke the most 
## in what time interval, to use the cheat mode in what time interval, etc.)

# Smokers tend to smoke in what time interval
computeClassifiedTimeIntervals(userdata)
# Smokers tend to smoke in what time interval with which mode
computeClassifiedTimeIntervalsPerMode(userdata)