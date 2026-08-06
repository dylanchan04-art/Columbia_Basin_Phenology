
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
DOYsteelheadBonneville <- DOYfunction("Data/Dart/Steelhead_Bonneville_Dart.csv")

DOYcohoBonneville <- DOYfunction("Data/Dart/Coho_Bonneville_Dart.csv")

DOYsockeyeBonneville <- DOYfunction("Data/Dart/Sockeye_Bonneville_Dart.csv")

DOYsubyearlingChinookBonneville <- DOYfunction("Data/Dart/SubyearlingChinook_Bonneville_Dart.csv")

DOYyearlingChinookBonneville <- DOYfunction("Data/Dart/YearlingChinook_Bonneville_Dart.csv")


#Temp
YearlyTempMay <- read.csv("Data/RawTempNew/BonMayAverage.csv")

#Merge


DOYsteelheadBonneville$Species <- "Steelhead"
DOYcohoBonneville$Species <- "Coho"
DOYsockeyeBonneville$Species <- "Sockeye"
DOYsubyearlingChinookBonneville$Species <- "Subyearling Chinook"
DOYyearlingChinookBonneville$Species <- "Yearling Chinook"


all_species <- bind_rows(DOYsteelheadBonneville, 
                         DOYcohoBonneville, 
                         DOYsockeyeBonneville, 
                         DOYsubyearlingChinookBonneville, 
                         DOYyearlingChinookBonneville)


all_species$Year <- as.integer(all_species$Year)


Bonneville_Merged_data <- all_species %>%
  left_join(YearlyTempMay, by = "Year") %>%
  arrange(Species, Year)



#View(Bonneville_Merged_data)


write.csv(Bonneville_Merged_data, "Data/NewMergedTempData/Bonneville_Merged_New.csv")
