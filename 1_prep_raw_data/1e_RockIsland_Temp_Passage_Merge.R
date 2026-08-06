
library(dplyr)
library(lubridate)


DOYfunction <-function(file_path) {
  rawdata <- read.csv(file_path)
  deleteddata <- rawdata[-c(1:3), ]
  DateFormat <- paste(deleteddata$X50.PassageDate,"/",deleteddata$Year,sep="")
  DateMedians <-as.Date(DateFormat,format = "%m/%d/%Y")
  DOY <- yday(DateMedians)
  DOYandYear <- data.frame(Year = deleteddata$Year, MedianPassage = DOY)
  return(DOYandYear)
}

#Get Median Run Times as DOY
DOYsteelheadRockIsland <- DOYfunction("Data/Dart/Steelhead_Rock_Dart.csv")

DOYcohoRockIsland <- DOYfunction("Data/Dart/Coho_Rock_Dart.csv")

DOYsockeyeRockIsland <- DOYfunction("Data/Dart/Sockeye_Rock_Dart.csv")

DOYsubyearlingRockIsland <- DOYfunction("Data/Dart/SubyearlingChinook_Rock_Dart.csv")

DOYyearlingRockIsland <- DOYfunction("Data/Dart/YearlingChinook_Rock_Dart.csv")



#Temp
YearlyTempMay <- read.csv("Data/RawTempNew/RIGWMayAverage.csv")

#Merge

DOYsteelheadRockIsland$Species <- "Steelhead"
DOYcohoRockIsland$Species <- "Coho"
DOYsockeyeRockIsland$Species <- "Sockeye"
DOYsubyearlingRockIsland$Species <- "Subyearling Chinook"
DOYyearlingRockIsland$Species <- "Yearling Chinook"


all_species <- bind_rows(DOYsteelheadRockIsland, 
                         DOYcohoRockIsland, 
                         DOYsockeyeRockIsland, 
                         DOYsubyearlingRockIsland, 
                         DOYyearlingRockIsland)


all_species$Year <- as.integer(all_species$Year)


RockIsland_Merged_data <- all_species %>%
  left_join(YearlyTempMay, by = "Year") %>%
  arrange(Species, Year)



View(RockIsland_Merged_data)


write.csv(RockIsland_Merged_data, "Data/NewMergedTempData/RockIsland_Merged_New.csv")














































