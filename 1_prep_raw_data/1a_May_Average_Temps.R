library(dplyr)
library(lubridate)

#Bonneville

BonData <- read.csv("Data/RawTempNew/Bonneville.csv", stringsAsFactors = FALSE)

BonData$datetime <- as.POSIXct(BonData$Date.Time, format = "%d-%b-%Y %H:%M", tz = "UTC")

BonData$year <- year(BonData$datetime)
BonData$month <- month(BonData$datetime)
colnames(BonData)[2] <- "MeanMayTemp"
BonMayTemp <- BonData %>%
  filter(month == 5) 

View(BonMayTemp)

#Check for outliers where the change in temp from previous hour was 2 degree or more
#should only be the case for May 1st at midnight ever year because jump from end of last year's May is significant
Bonoutlier <- BonMayTemp %>%
  mutate(temp_diff = MeanMayTemp - lag(MeanMayTemp)) %>%
  filter(abs(temp_diff) > 2) 
 
View(Bonoutlier)


#Check For Incorrect Values 
n_below_45 <- sum(BonMayTemp$MeanMayTemp < 45, na.rm = TRUE)
n_above_65 <- sum(BonMayTemp$MeanMayTemp > 65, na.rm = TRUE)
print(n_below_45)
print(n_above_65)

#MeanMayTemp
BonMayAverage <- BonMayTemp %>%
  group_by(year) %>%
  summarise(MeanMayTemp = mean(MeanMayTemp, na.rm = TRUE))

View(BonMayAverage)

#Convert to Celsius
BonMayAverage <- BonMayAverage %>%
  mutate(MeanMayTemp = (MeanMayTemp - 32) * 5/9)
colnames(BonMayAverage)[1] <- "Year"

View(BonMayAverage)
write.csv(BonMayAverage, "Data/RawTempNew/BonMayAverage.csv", row.names = FALSE)  


#Lower Granite 

LWGData <- read.csv("Data/RawTempNew/LowerGranite2.csv", stringsAsFactors = FALSE)

LWGData$datetime <- as.POSIXct(LWGData$Date.Time, format = "%d-%b-%Y %H:%M", tz = "UTC")

LWGData$year <- year(LWGData$datetime)
LWGData$month <- month(LWGData$datetime)
colnames(LWGData)[2] <- "MeanMayTemp"
LWGMayTemp <- LWGData %>%
  filter(month == 5) 
  
View(LWGMayTemp)

LWGoutlier <- LWGMayTemp %>%
  mutate(temp_diff = MeanMayTemp - lag(MeanMayTemp)) %>%
  filter(abs(temp_diff) > 2) 

View(LWGoutlier)

#Check Incorrect Values 
n_below_45 <- sum(LWGMayTemp$MeanMayTemp < 45, na.rm = TRUE)
n_above_65 <- sum(LWGMayTemp$MeanMayTemp > 65, na.rm = TRUE)
print(n_below_45)
print(n_above_65)

#MeanMayTemp
LWGMayAverage <- LWGMayTemp %>%
  group_by(year) %>%
  summarise(MeanMayTemp = mean(MeanMayTemp, na.rm = TRUE))

#Convert To Celsius 
LWGMayAverage <- LWGMayAverage %>%
  mutate(MeanMayTemp = (MeanMayTemp - 32) * 5/9)

colnames(LWGMayAverage)[1] <- "Year"
View(LWGMayAverage)
write.csv(LWGMayAverage, "Data/RawTempNew/LWGMayAverage.csv", row.names = FALSE)  


#McNary

MCNData <- read.csv("Data/RawTempNew/McNary2.csv", stringsAsFactors = FALSE)

MCNData$datetime <- as.POSIXct(MCNData$Date.Time, format = "%d-%b-%Y %H:%M", tz = "UTC")

MCNData$year <- year(MCNData$datetime)
MCNData$month <- month(MCNData$datetime)
colnames(MCNData)[2] <- "MeanMayTemp"
MCNMayTemp <- MCNData %>%
  filter(month == 5) 

View(MCNMayTemp)

MCNoutlier <- MCNMayTemp %>%
  mutate(temp_diff = MeanMayTemp - lag(MeanMayTemp)) %>%
  filter(abs(temp_diff) > 2) 

View(MCNoutlier)

#Check Incorrect Values 
n_below_45 <- sum(MCNMayTemp$MeanMayTemp < 45, na.rm = TRUE)
n_above_65 <- sum(MCNMayTemp$MeanMayTemp > 65, na.rm = TRUE)
print(n_below_45)
print(n_above_65)


#MeanMayTemp
MCNMayAverage <- MCNMayTemp %>%
  group_by(year) %>%
  summarise(MeanMayTemp = mean(MeanMayTemp, na.rm = TRUE))

#Convert To Celsius 
MCNMayAverage <- MCNMayAverage %>%
  mutate(MeanMayTemp = (MeanMayTemp - 32) * 5/9)

colnames(MCNMayAverage)[1] <- "Year"
View(MCNMayAverage)

write.csv(MCNMayAverage, "Data/RawTempNew/MCNMayAverage.csv", row.names = FALSE)  


#RockIsland

RISData <- read.csv("Data/RawTempNew/RockIsland2.csv", stringsAsFactors = FALSE)

RISData$datetime <- as.POSIXct(RISData$Date.Time, format = "%d-%b-%Y %H:%M", tz = "UTC")

RISData$year <- year(RISData$datetime)
RISData$month <- month(RISData$datetime)
colnames(RISData)[2] <- "MeanMayTemp"
RISMayTemp <- RISData %>%
  filter(month == 5) 

View(RISMayTemp)

RISoutlier <- RISMayTemp %>%
  mutate(temp_diff = MeanMayTemp - lag(MeanMayTemp)) %>%
  filter(abs(temp_diff) > 2) 

View(RISoutlier)

#Check Incorrect Values 
n_below_45 <- sum(RISMayTemp$MeanMayTemp < 45, na.rm = TRUE)
n_above_65 <- sum(RISMayTemp$MeanMayTemp > 65, na.rm = TRUE)
print(n_below_45)
print(n_above_65)

#cut incorrect
RISMayTemp <- RISMayTemp %>%
  filter(Date.Time != "26-May-2021 10:00") %>%
  filter(Date.Time != "24-May-2023 10:00") %>%
  filter(Date.Time != "06-May-2009 19:00") %>%
  filter(Date.Time != "01-May-2012 13:00") %>%
  filter(Date.Time != "30-May-2023 13:00") %>%
  filter(Date.Time != "28-May-2024 13:00") 



#MeanMayTemp
RISMayAverage <- RISMayTemp %>%
  group_by(year) %>%
  summarise(MeanMayTemp = mean(MeanMayTemp, na.rm = TRUE))

#Convert To Celsius 
RISMayAverage <- RISMayAverage %>%
  mutate(MeanMayTemp = (MeanMayTemp - 32) * 5/9)

colnames(RISMayAverage)[1] <- "Year"
View(RISMayAverage)
write.csv(RISMayAverage, "Data/RawTempNew/RIGWMayAverage.csv", row.names = FALSE)  





#Bonneville Calculations

#find average of 10 days in 2021

May21Average <- BonMayTemp %>%
  filter(year == 2021) %>%
  summarise(MeanMayTemp21 = mean(MeanMayTemp, na.rm = TRUE)) %>%
  pull(MeanMayTemp21)

print(May21Average)

#May 2021 avg = 57.85077


#find average from may 20-31st in all years, including 2021

AllAverageAfter20 <- BonMayTemp %>%
  filter(day(datetime) >= 20) %>%
  summarise(AverageAfterMay20 = mean(MeanMayTemp, na.rm = TRUE)) %>%
  pull(AverageAfterMay20)
  

print(AllAverageAfter20)
#average after may 20 including 2021 is 57.27334

#find average may 20-31st all years except 2021

Exclude21 <- BonMayTemp %>%
  filter(day(datetime) >= 20) %>%
  filter(year != 2021) %>% 
  summarise(AfterMay20Exclude21 = mean(MeanMayTemp, na.rm = TRUE)) %>%
  pull(AfterMay20Exclude21)

print(Exclude21)
#average after may 20 excluding 2021 is 57.24996
 


