library(MCMCvis)
library(birdcolors)
library(dplyr)
library(ggplot2)

############
#PLOTS (temperature vs passage date, arrival date vs passage date)
############

### Figure 2 ####
species_list <- c("Coho", "Sockeye", "Steelhead", 
                  "Subyearling Chinook", "Yearling Chinook")

stan_model_1 <- readRDS("Data/Combined_RDS_results/temp_passage.rds")


dam_data <- read.csv("Data/UPDATED_Combined_Dams.csv")

all_data <- dam_data

#Get average median passage date, average temp for each dam
mean_col <- all_data %>% group_by(Dam,Species) %>% 
  summarise(passage_mean = mean(MedianPassage,na.rm=TRUE),
            temp_mean = mean(MeanMayTemp, na.rm=TRUE),
            count=n())


#Merge in mean columns 
all_data <- merge(all_data, mean_col,by=c("Dam","Species"))

#Get how anomalous each passage date and temp is 
all_data$passage_anomaly <- all_data$MedianPassage- all_data$passage_mean
all_data$temp_anomaly <- all_data$MeanMayTemp - all_data$temp_mean

#Create new column for dam, species identity
dam_spec <- all_data %>% select("Species","Dam") %>% unique()
dam_spec$sp_dam <- 1:nrow(dam_spec)
all_data <- merge(all_data,dam_spec,by=c("Species","Dam"))

#get rid of any NA in median passage column
all_data <- all_data[!is.na(all_data$MedianPassage), ]

# Extract beta values
alpha1_samples_all <- MCMCchains(stan_model_1, params = "alpha1")
beta_samples_all <- MCMCchains(stan_model_1, params = "beta")

# Use same palette as other dam plots
col_pal <- bird_colors("Keel-billed Toucan")

#Change orange color to avoid confusion with Caspian tern
col_pal[4] <- bird_colors("Lilac-breasted Roller")[3]

#Now reorder so that steelhead is blue and yearling chinook is green
col_pal <- col_pal[c(3,4,1,5,2)]

col_num <- as.numeric(as.factor(all_data$Species))

plot(NULL,xlim=c(-2,2.5),ylim=c(105,205),
     xlab = "Temperature Anomaly (°C)",
     ylab = "Median Passage Date",)
points(all_data$temp_anomaly,all_data$MedianPassage,pch=19,col=alpha(col_pal[col_num],.95))

count <- 0
for (each_species in unique(all_data$Species)){
  count <- count + 1
  xx <- seq(min(all_data$temp_anomaly),max(all_data$temp_anomaly),by=.1)
  
  yy_sim <- c()
  for (mm in 1:nrow(alpha1_samples_all)){
    yy_sim <- rbind(yy_sim,alpha1_samples_all[mm,count] + beta_samples_all[mm,count] * xx)
  }
  
  median_st_trend <- apply(yy_sim,2,quantile,.5)
  lower_ci <- apply(yy_sim,2,quantile,.025)
  upper_ci <- apply(yy_sim,2,quantile,.975)
  
  points(xx,median_st_trend,type="l",lwd=2,col=col_pal[count])
  polygon(x = c(xx, rev(xx)),
          y = c(lower_ci, rev(upper_ci)),col=alpha(col_pal[count],.25), border = NA)
}

legend("bottomleft",legend=unique(as.factor(all_data$Species)),pch=19,col = col_pal)


### Figure 3 ####
#Similar process as above, but with arrival, passage model

arr_data <- read.csv("Data/New_Compiled_yrs.csv") %>% select(arr_GAM_mean,arr_GAM_sd)
dam_data <- read.csv("Data/Combined_Dams_WITH_ESTUARY.csv")

all_data <- merge(dam_data,arr_data,by="arr_GAM_mean", all.x = TRUE)
#Get data, filter to species of interest
all_data <- all_data %>% 
  filter(Species == "Steelhead")

all_data$bird_data_pa <- as.numeric(!is.na(all_data$arr_GAM_mean))

#Get average median passage date, average arrival date
mean_col <- all_data %>% group_by(Dam,Species) %>% 
  summarise(passage_mean = mean(MedianPassage,na.rm=TRUE),
            arrival_mean = mean(arr_GAM_mean, na.rm=TRUE),
            count=n())

#Merge in mean columns 
all_data <- merge(all_data, mean_col,by=c("Dam","Species"))


# Stan cannot take in data that includes NAs. 
# Add in placeholders for the NAs, use arbitrary large number.

all_data$arr_GAM_mean[is.na(all_data$arr_GAM_mean)] <- 999
all_data$arr_GAM_sd[is.na(all_data$arr_GAM_sd)] <- 999


#Get how anomalous each passage date, arrival date is for each species
all_data$passage_anomaly <- all_data$MedianPassage- all_data$passage_mean
all_data$arrival_anomaly <- all_data$arr_GAM_mean - all_data$arrival_mean

#Create new column for dam, species identity
dam_spec <- all_data %>% select("Species","Dam") %>% unique()
dam_spec$sp_dam <- 1:nrow(dam_spec)
all_data <- merge(all_data,dam_spec,by=c("Species","Dam"))

#get rid of NAs in passage 
all_data <- all_data[!is.na(all_data$MedianPassage), ]

#create separate data frame with only arrival anomaly, steelhead passage anomaly for years where arrival
#data exists 
bird_only_data <- all_data %>% 
  filter(all_data$arr_GAM_mean != 999) %>%
  select(Dam,arr_GAM_mean,arrival_anomaly,arr_GAM_sd)

rel_sp1 <- all_data %>% 
  filter(all_data$arr_GAM_mean != 999 & Species == "Steelhead") %>%
  select(Dam,arrival_anomaly,arr_GAM_sd,passage_anomaly)

bird_only_data_st <- merge(bird_only_data,rel_sp1,by=c("Dam","arrival_anomaly","arr_GAM_sd"))
colnames(bird_only_data_st)[5] <- "Steelhead"

# Use same palette as other dam plots
all_data <- merge(dam_data,arr_data,by="arr_GAM_mean", all.x = TRUE)
#Get data, filter to species of interest
all_data <- all_data %>% 
  filter(Species == "Yearling Chinook")

all_data$bird_data_pa <- as.numeric(!is.na(all_data$arr_GAM_mean))

#Get average median passage date, average arrival date
mean_col <- all_data %>% group_by(Dam,Species) %>% 
  summarise(passage_mean = mean(MedianPassage,na.rm=TRUE),
            arrival_mean = mean(arr_GAM_mean, na.rm=TRUE),
            count=n())

#Merge in mean columns 
all_data <- merge(all_data, mean_col,by=c("Dam","Species"))


# Stan cannot take in data that includes NAs. 
# Add in placeholders for the NAs, use arbitrary large number.

all_data$arr_GAM_mean[is.na(all_data$arr_GAM_mean)] <- 999
all_data$arr_GAM_sd[is.na(all_data$arr_GAM_sd)] <- 999


#Get how anomalous each passage date, arrival date is for each species
all_data$passage_anomaly <- all_data$MedianPassage- all_data$passage_mean
all_data$arrival_anomaly <- all_data$arr_GAM_mean - all_data$arrival_mean

#Create new column for dam, species identity
dam_spec <- all_data %>% select("Species","Dam") %>% unique()
dam_spec$sp_dam <- 1:nrow(dam_spec)
all_data <- merge(all_data,dam_spec,by=c("Species","Dam"))

#get rid of NAs in passage 
all_data <- all_data[!is.na(all_data$MedianPassage), ]

#create separate data frame with only arrival anomaly, steelhead passage anomaly for years where arrival
#data exists 
bird_only_data <- all_data %>% 
  filter(all_data$arr_GAM_mean != 999) %>%
  select(Dam,arr_GAM_mean,arrival_anomaly,arr_GAM_sd)

rel_sp1 <- all_data %>% 
  filter(all_data$arr_GAM_mean != 999 & Species == "Yearling Chinook") %>%
  select(Dam,arrival_anomaly,arr_GAM_sd,passage_anomaly)

bird_only_data_yc <- merge(bird_only_data,rel_sp1,by=c("Dam","arrival_anomaly","arr_GAM_sd"))
colnames(bird_only_data_yc)[5] <- "Yearling Chinook"

col_yearling_chinook <- bird_colors("Keel-billed Toucan")[2]
col_yearling_steelhead <- bird_colors("Keel-billed Toucan")[1]

# Create plots 
yc_model <- readRDS("Data/Combined_RDS_results/Yearling_Arrival_Passage.rds")
eta_chains <- MCMCchains(yc_model,params = "eta")

plot(bird_only_data_yc$`Yearling Chinook`,bird_only_data_yc$arrival_anomaly,pch=19,col=col_yearling_chinook,cex=1.25,
     xlab="Yearling Chinook Passage Anomaly (days)",
     ylab="Tern Arrival Anomaly (days)")

xx <- seq(min(bird_only_data_yc$`Yearling Chinook`),max(bird_only_data_yc$`Yearling Chinook`),by=1)
yy_sim <- c()
for (mm in 1:nrow(eta_chains)){
  yy_sim <- rbind(yy_sim,eta_chains[mm] * xx)
}

median_st_trend <- apply(yy_sim,2,quantile,.5)
lower_ci <- apply(yy_sim,2,quantile,.025)
upper_ci <- apply(yy_sim,2,quantile,.975)

points(xx,median_st_trend,type="l",lwd=4,col=col_yearling_chinook)
polygon(x = c(xx, rev(xx)),
        y = c(lower_ci, rev(upper_ci)),col=alpha(col_yearling_chinook,.25), border = NA)


yc_model <- readRDS("Data/Combined_RDS_results/Steelhead_Arrival_Passage.rds")
eta_chains <- MCMCchains(yc_model,params = "eta")

plot(bird_only_data_st$Steelhead,bird_only_data_st$arrival_anomaly,pch=19,col=col_yearling_steelhead,cex=1.25,
     xlab="Steelhead Passage Anomaly (days)",
     ylab="Tern Arrival Anomaly (days)")

xx <- seq(min(bird_only_data_st$Steelhead),max(bird_only_data_st$Steelhead),by=1)
yy_sim <- c()
for (mm in 1:nrow(eta_chains)){
  yy_sim <- rbind(yy_sim,eta_chains[mm] * xx)
}

median_st_trend <- apply(yy_sim,2,quantile,.5)
lower_ci <- apply(yy_sim,2,quantile,.025)
upper_ci <- apply(yy_sim,2,quantile,.975)

points(xx,median_st_trend,type="l",lwd=4,col=col_yearling_steelhead)
polygon(x = c(xx, rev(xx)),
        y = c(lower_ci, rev(upper_ci)),col=alpha(col_yearling_steelhead,.25), border = NA)


####FIGURE 4#####


#For each dam, create a plot showing three trend lines with data for steelhead, yearling, birds
bird_mdl <- readRDS("bird_trend_mdl.rds")
steelhead_mdl <- readRDS("steelhead_trend_mdl.rds")
yearling_mdl <- readRDS("yearling_trend_mdl.rds")

bird_data <- readRDS("bird_trend_data.rds")
steelhead_data <- readRDS("steelhead_trend_data.rds")
yearling_data <- readRDS("yearling_trend_data.rds")

col_pal <- bird_colors("Keel-billed Toucan")

bird_col <- col_pal[4]
st_col <- col_pal[1]
yr_col <- col_pal[2]

cell_dam_name <- c("McNary","Bonneville","Lower Granite","Rock Island")
for(cell_num in 1:4){
  
  bd_in <- bird_data %>% filter(cell==cell_num)
  st_in <- steelhead_data %>% filter(cell==cell_num)
  yr_in <- yearling_data %>% filter(cell==cell_num)
  
  plot(bd_in$year,bd_in$arr_GAM_mean,main=cell_dam_name[cell_num],
       col=alpha(bird_col,.75),pch=19,xlim=c(2000,2024),ylim=c(80,160),cex=2,
       xlab="Year",ylab="Date",cex.axis=1.2,cex.lab=1.2)
  points(st_in$Year,st_in$MedianPassage,col=alpha(st_col,.75),pch=19,cex=2)
  points(yr_in$Year,yr_in$MedianPassage,col=alpha(yr_col,.75),pch=19,cex=2)
  
  legend("bottomleft",legend=c("Caspian tern","Steelhead","Yearling chinook"),
         pch=19,cex=1.2,col=c(bird_col,st_col,yr_col))
  
  ### Bird Trend Line ####
  bird_intercept_info <- MCMCsummary(bird_mdl,params=paste0("omega[",cell_num,"]"),ISB=FALSE)
  bird_trend_info <- MCMCsummary(bird_mdl,params=paste0("zeta[",cell_num,"]"),ISB=FALSE)
  xx_bird <- min(bd_in$year):max(bd_in$year) - 2017
  xx_bird_og <- xx_bird+2017
  
  bird_chains <- MCMCchains(bird_mdl,params=c(paste0("omega[",cell_num,"]"),paste0("zeta[",cell_num,"]")),ISB=FALSE)
  
  yy_sim <- c()
  for (mm in 1:nrow(bird_chains)){
    yy_sim <- rbind(yy_sim,bird_chains[mm,1] + bird_chains[mm,2] * xx_bird)
  }
  
  median_bird_trend <- apply(yy_sim,2,quantile,.5)
  lower_bird_trend <- apply(yy_sim,2,quantile,.025)
  upper_bird_trend <- apply(yy_sim,2,quantile,.975)
  
  
  points(xx_bird_og,median_bird_trend,type="l",col=alpha(bird_col,.95),lwd=4)
  polygon(x = c(xx_bird_og, rev(xx_bird_og)),
          y = c(lower_bird_trend, rev(upper_bird_trend)),col = alpha(bird_col,.3), border = NA)
  
  ### Steelhead Trend Line ####
  steelhead_intercept_info <- MCMCsummary(steelhead_mdl,params=paste0("omega[",cell_num,"]"),ISB=FALSE)
  steelhead_trend_info <- MCMCsummary(steelhead_mdl,params=paste0("zeta[",cell_num,"]"),ISB=FALSE)
  xx_st <- min(st_in$Year):max(st_in$Year) - 2017
  xx_st_og <- xx_st + 2017
  
  steelhead_chains <- MCMCchains(steelhead_mdl,params=c(paste0("omega[",cell_num,"]"),paste0("zeta[",cell_num,"]")),ISB=FALSE)
  
  yy_sim <- c()
  for (mm in 1:nrow(steelhead_chains)){
    yy_sim <- rbind(yy_sim,steelhead_chains[mm,1] + steelhead_chains[mm,2] * xx_st)
  }
  
  median_st_trend <- apply(yy_sim,2,quantile,.5)
  lower_st_trend <- apply(yy_sim,2,quantile,.025)
  upper_st_trend <- apply(yy_sim,2,quantile,.975)
  
  
  points(xx_st_og,median_st_trend,type="l",col=alpha(st_col,.95),lwd=4)
  polygon(x = c(xx_st_og, rev(xx_st_og)),
          y = c(lower_st_trend, rev(upper_st_trend)),col = alpha(st_col,.3), border = NA)
  
  ### Yearling Trend Line ####
  yearling_intercept_info <- MCMCsummary(yearling_mdl,params=paste0("omega[",cell_num,"]"),ISB=FALSE)
  yearling_trend_info <- MCMCsummary(yearling_mdl,params=paste0("zeta[",cell_num,"]"),ISB=FALSE)
  xx_yr <- min(yr_in$Year):max(yr_in$Year) - 2017
  xx_yr_og <- xx_yr + 2017
  
  yearling_chains <- MCMCchains(yearling_mdl,params=c(paste0("omega[",cell_num,"]"),paste0("zeta[",cell_num,"]")),ISB=FALSE)
  
  yy_sim <- c()
  for (mm in 1:nrow(yearling_chains)){
    yy_sim <- rbind(yy_sim,yearling_chains[mm,1] + yearling_chains[mm,2] * xx_yr)
  }
  
  median_yr_trend <- apply(yy_sim,2,quantile,.5)
  lower_yr_trend <- apply(yy_sim,2,quantile,.025)
  upper_yr_trend <- apply(yy_sim,2,quantile,.975)
  
  
  points(xx_yr_og,median_yr_trend,type="l",col=alpha(yr_col,.95),lwd=4)
  polygon(x = c(xx_yr_og, rev(xx_yr_og)),
          y = c(lower_yr_trend, rev(upper_yr_trend)),col = alpha(yr_col,.3), border = NA)
}

### Make plot for river mouth ####
# Note, fish data is repeated from bonneville
bd_in <- bird_data %>% filter(cell==6)

plot(bd_in$year,bd_in$arr_GAM_mean,main="Columbia River Mouth",
     col=alpha(bird_col,.75),pch=19,xlim=c(2000,2024),ylim=c(80,160),cex=2,
     xlab="Year",ylab="Date",cex.axis=1.2,cex.lab=1.2)
points(st_in$Year,st_in$MedianPassage,col=alpha(st_col,.75),pch=1,cex=2)
points(yr_in$Year,yr_in$MedianPassage,col=alpha(yr_col,.75),pch=1,cex=2)

### Bird Trend Line ####
bird_intercept_info <- MCMCsummary(bird_mdl,params=paste0("omega[",5,"]"),ISB=FALSE)
bird_trend_info <- MCMCsummary(bird_mdl,params=paste0("zeta[",5,"]"),ISB=FALSE)
xx_bird <- min(bd_in$year):max(bd_in$year) - 2017
xx_bird_og <- xx_bird+2017

bird_chains <- MCMCchains(bird_mdl,params=c(paste0("omega[",5,"]"),paste0("zeta[",5,"]")),ISB=FALSE)

yy_sim <- c()
for (mm in 1:nrow(bird_chains)){
  yy_sim <- rbind(yy_sim,bird_chains[mm,1] + bird_chains[mm,2] * xx_bird)
}

median_bird_trend <- apply(yy_sim,2,quantile,.5)
lower_bird_trend <- apply(yy_sim,2,quantile,.025)
upper_bird_trend <- apply(yy_sim,2,quantile,.975)

points(xx_bird_og,median_bird_trend,type="l",col=alpha(bird_col,.95),lwd=4)
polygon(x = c(xx_bird_og, rev(xx_bird_og)),
        y = c(lower_bird_trend, rev(upper_bird_trend)),col = alpha(bird_col,.3), border = NA)

### Steelhead Trend Line ####
steelhead_intercept_info <- MCMCsummary(steelhead_mdl,params=paste0("omega[",cell_num,"]"),ISB=FALSE)
steelhead_trend_info <- MCMCsummary(steelhead_mdl,params=paste0("zeta[",cell_num,"]"),ISB=FALSE)
xx_st <- min(st_in$Year):max(st_in$Year) - 2017
xx_st_og <- xx_st + 2017

steelhead_chains <- MCMCchains(steelhead_mdl,params=c(paste0("omega[",cell_num,"]"),paste0("zeta[",cell_num,"]")),ISB=FALSE)

yy_sim <- c()
for (mm in 1:nrow(steelhead_chains)){
  yy_sim <- rbind(yy_sim,steelhead_chains[mm,1] + steelhead_chains[mm,2] * xx_st)
}

median_st_trend <- apply(yy_sim,2,quantile,.5)
lower_st_trend <- apply(yy_sim,2,quantile,.025)
upper_st_trend <- apply(yy_sim,2,quantile,.975)


points(xx_st_og,median_st_trend,type="l",col=alpha(st_col,.95),lwd=4,lty=2)
polygon(x = c(xx_st_og, rev(xx_st_og)),
        y = c(lower_st_trend, rev(upper_st_trend)),col = alpha(st_col,.3), border = NA)

### Yearling Trend Line ####
yearling_intercept_info <- MCMCsummary(yearling_mdl,params=paste0("omega[",cell_num,"]"),ISB=FALSE)
yearling_trend_info <- MCMCsummary(yearling_mdl,params=paste0("zeta[",cell_num,"]"),ISB=FALSE)
xx_yr <- min(yr_in$Year):max(yr_in$Year) - 2017
xx_yr_og <- xx_yr + 2017

yearling_chains <- MCMCchains(yearling_mdl,params=c(paste0("omega[",cell_num,"]"),paste0("zeta[",cell_num,"]")),ISB=FALSE)

yy_sim <- c()
for (mm in 1:nrow(yearling_chains)){
  yy_sim <- rbind(yy_sim,yearling_chains[mm,1] + yearling_chains[mm,2] * xx_yr)
}

median_yr_trend <- apply(yy_sim,2,quantile,.5)
lower_yr_trend <- apply(yy_sim,2,quantile,.025)
upper_yr_trend <- apply(yy_sim,2,quantile,.975)


points(xx_yr_og,median_yr_trend,type="l",col=alpha(yr_col,.95),lwd=4,lty=2)
polygon(x = c(xx_yr_og, rev(xx_yr_og)),
        y = c(lower_yr_trend, rev(upper_yr_trend)),col = alpha(yr_col,.3), border = NA)
legend("bottomleft",legend=c("Caspian tern","Steelhead","Yearling chinook"),
       pch=19,cex=1.2,col=c(bird_col,st_col,yr_col))
