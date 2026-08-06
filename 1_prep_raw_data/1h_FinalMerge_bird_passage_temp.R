

year_list <- 2003:2024  

for (year in year_list) {
  
  
  
  data <- get(paste("Caspian_Tern_",year, sep = "")) 
  
  
  filtered <- data[, c("year", "cell", "arr_GAM_mean")]
  
  
  assign(paste("filtered",year, sep = ""), filtered)
}


# Define the list of years and cells
year_list <- 2003:2024
cell_list <- 1:6  # Adjust this based on your actual cell numbers

# Loop through each cell
for (current_cell in cell_list) {
  
  # Initialize an empty data frame to store data across all years for this cell
  all_years_data <- data.frame(year = integer(), cell = integer(), arr_GAM_mean = numeric(), arr_GAM_sd = numeric())
  
  
  # Loop through each year
  for (year in year_list) {
    
    # Get the filtered data frame for the current year
    year_data <- get(paste("filtered",year, sep = ""))
    
    # Correctly filter the data for the current cell
    cell_data <- subset(year_data, cell == current_cell)
    
    # If no data for this cell, add a row with NA values
    if (nrow(cell_data) == 0) {
      cell_data <- data.frame(year = year, cell = current_cell, arr_GAM_mean = NA, arr_GAM_sd = NA
      )
    }
    
    # Add this year's data to the final data frame
    all_years_data <- rbind(all_years_data, cell_data)
  }
  
  # Save the data frame for this cell to a CSV file
  output_file <- paste("NEWcell_", current_cell, "_data.csv", sep = "")
  write.csv(all_years_data, output_file, row.names = FALSE)
}

McNary_Arrival <- read.csv("Data/Arrival_by_cell/cell_1_data.csv")
Bonneville_Arrival <- read.csv("Data/Arrival_by_cell/cell_2_data.csv")
Granite_Arrival <- read.csv("Data/Arrival_by_cell/cell_3_data.csv")
Rock_Arrival <- read.csv("Data/Arrival_by_cell/cell_4_data.csv")
Estuary_Arrival <- read.csv("Data/Arrival_by_cell/cell_6_data.csv")

#Rename from year to Year in arrival data
colnames(McNary_Arrival)[1] <- "Year"
colnames(Bonneville_Arrival)[1] <- "Year"
colnames(Granite_Arrival)[1] <- "Year"
colnames(Rock_Arrival)[1] <- "Year"
colnames(Estuary_Arrival)[1] <- "Year"

McNary_stack <- read.csv("Data/NewMergedTempData/McNary_Merged_New.csv")
Bonneville_stack <- read.csv("Data/NewMergedTempData/Bonneville_Merged_New.csv")
Granite_stack <- read.csv("Data/NewMergedTempData/LowerGranite_Merged_New.csv")
Rock_stack <- read.csv("Data/NewMergedTempData/RockIsland_Merged_New.csv")


McNary_merged <- merge(McNary_stack, McNary_Arrival, by = "Year", all.x = TRUE)
McNary_merged <- McNary_merged[, !names(McNary_merged) %in% "cell"]
McNary_merged$Dam <- "McNary"
McNary_merged <- McNary_merged[, c("Year", "Dam", setdiff(names(McNary_merged), "Year"))]
McNary_merged <- McNary_merged[order(McNary_merged$Species, McNary_merged$Year), ]
McNary_merged <- McNary_merged[, !grepl("^Dam\\.1$", names(McNary_merged))]
McNary_merged <- McNary_merged[, !names(McNary_merged) %in% "X"]
write.csv(McNary_merged,"Data/NEWMerge_with_Arr/McNary_Complete.csv", row.names=FALSE)
View(McNary_merged)



Bonneville_merged <- merge(Bonneville_stack, Bonneville_Arrival, by = "Year", all.x = TRUE)
Bonneville_merged <- Bonneville_merged[, !names(Bonneville_merged) %in% "cell"]
Bonneville_merged$Dam <- "Bonneville"
Bonneville_merged <- Bonneville_merged[, c("Year", "Dam", setdiff(names(Bonneville_merged), "Year"))]
Bonneville_merged <- Bonneville_merged[order(Bonneville_merged$Species, Bonneville_merged$Year), ]
Bonneville_merged <- Bonneville_merged[, !grepl("^Dam\\.1$", names(Bonneville_merged))]
Bonneville_merged <- Bonneville_merged[, !names(Bonneville_merged) %in% "X"]
write.csv(Bonneville_merged,"Data/NEWMerge_with_Arr/Bonneville_Complete.csv", row.names=FALSE)
View(Bonneville_merged)


Granite_merged <- merge(Granite_stack, Granite_Arrival, by = "Year", all.x = TRUE)
Granite_merged <- Granite_merged[, !names(Granite_merged) %in% "cell"]
Granite_merged$Dam <- "Lower Granite"
Granite_merged <- Granite_merged[, c("Year", "Dam", setdiff(names(Granite_merged), "Year"))]
Granite_merged <- Granite_merged[order(Granite_merged$Species, Granite_merged$Year), ]
Granite_merged <- Granite_merged[, !grepl("^Dam\\.1$", names(Granite_merged))]
Granite_merged <- Granite_merged[, !names(Granite_merged) %in% "X"]
write.csv(Granite_merged,"Data/NEWMerge_with_Arr/Granite_Complete.csv", row.names=FALSE)
View(Granite_merged)


Rock_merged <- merge(Rock_stack, Rock_Arrival, by = "Year", all.x = TRUE)
Rock_merged <- Rock_merged[, !names(Rock_merged) %in% "cell"]
Rock_merged$Dam <- "Rock Island"
Rock_merged <- Rock_merged[, c("Year", "Dam", setdiff(names(Rock_merged), "Year"))]
Rock_merged <- Rock_merged[order(Rock_merged$Species, Rock_merged$Year), ]
Rock_merged <- Rock_merged[, !grepl("^Dam\\.1$", names(Rock_merged))]
Rock_merged <- Rock_merged[, !names(Rock_merged) %in% "X"]
write.csv(Rock_merged,"Data/NEWMerge_with_Arr/Rock_Complete.csv", row.names=FALSE)
View(Rock_merged)


estuary_merged <- merge(Bonneville_stack, Estuary_Arrival, by = "Year", all.x = TRUE)
estuary_merged <- estuary_merged[, !names(estuary_merged) %in% "cell"]
estuary_merged$Dam <- "Est_BON"
estuary_merged <- estuary_merged[, c("Year", "Dam", setdiff(names(estuary_merged), "Year"))]
estuary_merged <- estuary_merged[order(estuary_merged$Species, estuary_merged$Year), ]
estuary_merged <- estuary_merged[, !grepl("^Dam\\.1$", names(estuary_merged))]
estuary_merged <- estuary_merged[, !names(estuary_merged) %in% "X"]
write.csv(estuary_merged,"Data/NEWMerge_with_Arr/estuary_Complete.csv", row.names=FALSE)
View(estuary_merged)

#Create combined file WITHOUT ESTUARY DATA** (for temp/passage model only)

csv_files <- c("Data/NEWMerge_with_Arr/Bonneville_Complete.csv", "Data/NEWMerge_with_Arr/Granite_Complete.csv", "Data/NEWMerge_with_Arr/McNary_Complete.csv", 
               "Data/NEWMerge_with_Arr/Rock_Complete.csv")


combined_list <- lapply(csv_files, read.csv)  
df_combined <- do.call(rbind, combined_list)  

View(df_combined)

write.csv(df_combined, "Data/UPDATED_Combined_Dams.csv", row.names = FALSE)  

#Create combined file WITH ESTUARY INCLUDED*** (for passage/arrival model)

csv_files_2 <- c("Data/NEWMerge_with_Arr/Bonneville_Complete.csv", "Data/NEWMerge_with_Arr/Granite_Complete.csv", "Data/NEWMerge_with_Arr/McNary_Complete.csv", 
               "Data/NEWMerge_with_Arr/Rock_Complete.csv", "Data/NEWMerge_with_Arr/estuary_Complete.csv")

combined_list_2 <- lapply(csv_files_2, read.csv)  
df_combined_2 <- do.call(rbind, combined_list_2)  

View(df_combined_2)

write.csv(df_combined_2, "Data/Combined_Dams_WITH_ESTUARY.csv", row.names = FALSE)  
