
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
DOYsteelheadMcNary <- DOYfunction("Data/Dart/Steelhead_McNary_Dart.csv")

DOYcohoMcNary <- DOYfunction("Data/Dart/Coho_McNary_Dart.csv")

DOYsockeyeMcNary <- DOYfunction("Data/Dart/Sockeye_McNary_Dart.csv")

DOYsubyearlingMcNary <- DOYfunction("Data/Dart/SubyearlingChinook_McNary_Dart.csv")

DOYyearlingMcNary <- DOYfunction("Data/Dart/YearlingChinook_McNary_Dart.csv")


#Temp
YearlyTempMay <- read.csv("Data/RawTempNew/MCNMayAverage.csv")

#Merge


DOYsteelheadMcNary$Species <- "Steelhead"
DOYcohoMcNary$Species <- "Coho"
DOYsockeyeMcNary$Species <- "Sockeye"
DOYsubyearlingMcNary$Species <- "Subyearling Chinook"
DOYyearlingMcNary$Species <- "Yearling Chinook"


all_species <- bind_rows(DOYsteelheadMcNary, 
                         DOYcohoMcNary, 
                         DOYsockeyeMcNary, 
                         DOYsubyearlingMcNary, 
                         DOYyearlingMcNary)


all_species$Year <- as.integer(all_species$Year)


McNary_Merged_data <- all_species %>%
  left_join(YearlyTempMay, by = "Year") %>%
  arrange(Species, Year)



View(McNary_Merged_data)


write.csv(McNary_Merged_data, "Data/NewMergedTempData/McNary_Merged_New.csv")





















