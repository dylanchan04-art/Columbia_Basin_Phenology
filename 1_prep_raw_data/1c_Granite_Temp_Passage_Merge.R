
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
DOYsteelheadGranite <- DOYfunction("Data/Dart/Steelhead_Granite_Dart.csv")

DOYcohoGranite <- DOYfunction("Data/Dart/Coho_Granite_Dart.csv")

DOYsockeyeGranite <- DOYfunction("Data/Dart/Sockeye_Granite_Dart.csv")

DOYsubyearlingGranite <- DOYfunction("Data/Dart/SubyearlingChinook_Granite_Dart.csv")

DOYyearlingGranite <- DOYfunction("Data/Dart/YearlingChinook_Granite_Dart.csv")


#Temp
YearlyTempMay <- read.csv("Data/RawTempNew/LWGMayAverage.csv")

#Merge

DOYsteelheadGranite$Species <- "Steelhead"
DOYcohoGranite$Species <- "Coho"
DOYsockeyeGranite$Species <- "Sockeye"
DOYsubyearlingGranite$Species <- "Subyearling Chinook"
DOYyearlingGranite$Species <- "Yearling Chinook"

all_species <- bind_rows(DOYsteelheadGranite, 
                         DOYcohoGranite, 
                         DOYsockeyeGranite, 
                         DOYsubyearlingGranite, 
                         DOYyearlingGranite)


all_species$Year <- as.integer(all_species$Year)


LowerGranite_Merged_data <- all_species %>%
  left_join(YearlyTempMay, by = "Year") %>%
  arrange(Species, Year)



View(LowerGranite_Merged_data)


write.csv(LowerGranite_Merged_data, "Data/NewMergedTempData/LowerGranite_Merged_New.csv")



































