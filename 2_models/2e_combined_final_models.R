# Template to create integrated model: Temperature - fish - bird data
library(rstan)
library(MCMCvis)
library(shinystan)
library(dplyr)
library(birdcolors)

#### MODEL 1 (FIGURE 2): TEMP/SMOLT PASSAGE MODEL #######

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



data_in <- list(
  N1 = nrow(all_data),
  Nreg = length(unique(all_data$Dam)),
  Nyr = length(unique(all_data$Year)),
  Nsp = length(unique(all_data$Species)),
  ii = as.numeric(as.factor(all_data$Species)),
  Ns_d = max(all_data$sp_dam),
  r = as.numeric(as.factor(all_data$Dam)),
  t = as.numeric(as.factor(all_data$Year)),
  s_d = all_data$sp_dam,
  
  TT = all_data$temp_anomaly, 
  SS1 = all_data$MedianPassage
)

stan_model_1 <- stan(
  file = "Stan Files/V2temp_passage.stan",  
  data = data_in,    #named list of data
  chains = 4,             # number of Markov chains
  warmup = 20000,          # number of warmup iterations per chain
  iter = 22000,            # total number of iterations per chain
  cores = 4,             # number of cores (should use one per chain)
  control = list(adapt_delta = .99)
)

MCMCsummary(stan_model_1, pg0 = TRUE)
MCMCplot(
  stan_model_1,
  params=c("beta"),
  labels = c("Coho","Sockeye","Steelhead","Subyearling Chinook","Yearling Chinook")
)

saveRDS(stan_model_1, "Data/Combined_RDS_Results/temp_passage.rds")


###### MODEL 2 (FIGURE 3): STEELHEAD PASSAGE AND TERN ARRIVAL MODEL ######

arr_data <- read.csv("Data/New_Compiled_yrs.csv") %>% select(arr_GAM_mean,arr_GAM_sd)
dam_data <- read.csv("Data/Combined_Dams_WITH_ESTUARY.csv")

all_data <- merge(dam_data,arr_data,by="arr_GAM_mean", all.x = TRUE)
#Get data, filter to species of interest
all_data <- all_data %>% 
  filter(Species == "Steelhead")


# Add new column to data that indicates whether we have bird data (present or absent)

# 1s yes data,
# 0s no data.

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


bird_only_data <- merge(bird_only_data,rel_sp1,by=c("Dam","arrival_anomaly","arr_GAM_sd"))
colnames(bird_only_data)[5] <- "Steelhead"


data_in <- list(
  #Bird level data
  N2 = nrow(bird_only_data),
  Nreg = length(unique(bird_only_data$Dam)),
  r = as.numeric(as.factor(bird_only_data$Dam)),
  SS2 = bird_only_data$Steelhead,
  BB = bird_only_data$arr_GAM_mean,
  BB_unc = bird_only_data$arr_GAM_sd
)


stan_model_2 <- stan(
  file = "Stan Files/V2arrival_passage.stan",  
  data = data_in,    #named list of data
  chains = 4,             # number of Markov chains
  warmup = 10000,          # number of warmup iterations per chain
  iter = 12000,            # total number of iterations per chain
  cores = 4,             # number of cores (should use one per chain)
  control = list(adapt_delta = .8)
)

MCMCsummary(stan_model_2,excl = c("BB_true"),pg0 = TRUE)

MCMCplot(stan_model_2,params="eta")

saveRDS(stan_model_2, "Data/Combined_RDS_Results/Steelhead_Arrival_Passage.rds")



#### MODEL 3 (FIGURE 3): YEARLING PASSAGE AND TERN ARRIVAL MODEL ######

arr_data <- read.csv("Data/New_Compiled_yrs.csv") %>% select(arr_GAM_mean,arr_GAM_sd)
dam_data <- read.csv("Data/Combined_Dams_WITH_ESTUARY.csv")

all_data <- merge(dam_data,arr_data,by="arr_GAM_mean", all.x = TRUE)
#Get data, filter to yearling chinook this time *****
all_data <- all_data %>% 
  filter(Species == "Yearling Chinook")


# Add new column to data that indicates whether we have bird data (present or absent)

# 1s yes data,
# 0s no data.

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


bird_only_data <- merge(bird_only_data,rel_sp1,by=c("Dam","arrival_anomaly","arr_GAM_sd"))
colnames(bird_only_data)[5] <- "YearlingChinook"


data_in <- list(
  #Bird level data
  N2 = nrow(bird_only_data),
  Nreg = length(unique(bird_only_data$Dam)),
  r = as.numeric(as.factor(bird_only_data$Dam)),
  SS2 = bird_only_data$YearlingChinook,
  BB = bird_only_data$arr_GAM_mean,
  BB_unc = bird_only_data$arr_GAM_sd
)


stan_model_3 <- stan(
  file = "Stan Files/V2arrival_passage.stan",  
  data = data_in,    #named list of data
  chains = 4,             # number of Markov chains
  warmup = 10000,          # number of warmup iterations per chain
  iter = 12000,            # total number of iterations per chain
  cores = 4,             # number of cores (should use one per chain)
  control = list(adapt_delta = .8)
)

MCMCsummary(stan_model_3,excl = c("BB_true"),pg0 = TRUE)

MCMCplot(stan_model_3,params="eta")

saveRDS(stan_model_3, "Data/Combined_RDS_Results/Yearling_Arrival_Passage.rds")


##### MODEL 4 (FIGURE 4): LINEAR TREND ANALYSIS #######

library(rstan)
library(MCMCvis)
library(shinystan)
library(dplyr)
library(ggplot2)

## MODEL 4A: tern arrival/year ##
dam_data <- read.csv("Data/Combined_Dams_WITH_ESTUARY.csv") %>% 
  select(Year,Dam,arr_GAM_mean)

# 1: McNary
# 2: Bonneville
# 3: Lower Granite
# 4: Rock Island
# 5: Puget Sound
# 6: River Mouth
match_help <- data.frame(Dam = c("McNary","Bonneville","Lower Granite","Rock Island","Puget Sound","Est_BON"),
                         cell = c(1,2,3,4,5,6))

arr_data <- read.csv("Data/New_Compiled_yrs.csv") %>% 
  select(cell,year,arr_GAM_mean,arr_GAM_sd) %>%
  filter(cell %in% c(1,2,3,4,6))# %>%
#filter(year >= 2009)

comb_data <- merge(arr_data,dam_data,by="arr_GAM_mean",all.x = TRUE)

arr_data <- merge(match_help,arr_data,by=c("cell"))

arr_data <- arr_data %>% arrange(Dam)

plot(arr_data$year,arr_data$arr_GAM_mean,pch=19,col=as.factor(arr_data$Dam))

arr_data$region <- as.numeric(as.factor(arr_data$cell))

data_in <- list(
  #Bird level data
  N2 = nrow(arr_data),
  Nreg = length(unique(arr_data$region)),
  r = arr_data$region,
  YR = arr_data$year-2017, # New effect of year
  BB = arr_data$arr_GAM_mean,
  BB_unc = arr_data$arr_GAM_sd
)

stan_model_arrival_linear <- stan(
  file = "Stan Files/V3_Trend_bird.stan",  
  data = data_in,    #named list of data
  chains = 4,             # number of Markov chains
  warmup = 20000,          # number of warmup iterations per chain
  iter = 22000,            # total number of iterations per chain
  cores = 4,             # number of cores (should use one per chain)
  control = list(adapt_delta = .99)
)

MCMCsummary(stan_model_arrival_linear,excl = c("BB_true"),pg0 = TRUE)

unique(arr_data$Dam)
MCMCplot(stan_model_arrival_linear,params=c("mu_zeta","zeta"),
         labels=c("Global","McNary","Bonneville","Lower Granite","Rock Island","River Mouth"),
         main = "Bird Trends")

saveRDS(arr_data,"bird_trend_data.rds")
saveRDS(stan_model_arrival_linear,"bird_trend_mdl.rds")

## MODEL 4B: Steelhead/Year ##

steelhead_data <- read.csv("Data/Combined_Dams_WITH_ESTUARY.csv") %>%
  filter(Species == "Steelhead") %>%
  select(Year, Dam, MedianPassage) %>%
  filter(!is.na(MedianPassage)& Dam != "Est_BON")

steelhead_data <- merge(match_help,steelhead_data,by="Dam")

steelhead_data <- steelhead_data %>% arrange(cell,Year)

plot(steelhead_data$Year, steelhead_data$MedianPassage,
     pch=19, col=as.factor(steelhead_data$Dam))

steelhead_data$region <- as.numeric(as.factor(steelhead_data$cell))

data_in <- list(
  N = nrow(steelhead_data),
  Nreg = length(unique(steelhead_data$region)),
  r = steelhead_data$region,
  YR = steelhead_data$Year - 2017,
  Y = steelhead_data$MedianPassage
)

stan_model_steelhead_linear <- stan(
  file = "Stan Files/V3_Trend_fish.stan",
  data = data_in,
  chains = 4,
  warmup = 20000,
  iter = 22000,
  cores = 4,
  control = list(adapt_delta = .99)
)

saveRDS(steelhead_data,"steelhead_trend_data.rds")
saveRDS(stan_model_steelhead_linear,"steelhead_trend_mdl.rds")
MCMCsummary(stan_model_steelhead_linear, pg0 = TRUE)

unique(steelhead_data$cell)
unique(steelhead_data$Dam)
MCMCplot(stan_model_steelhead_linear,params=c("mu_zeta","zeta"),
         labels=c("Global","McNary","Bonneville","Lower Granite","Rock Island"),
         main = "Steelhead Trends")

## MODEL 4C: Yearling/Year ##

Yearling_data <- read.csv("Data/Combined_Dams_WITH_ESTUARY.csv") %>%
  filter(Species == "Yearling Chinook") %>%
  select(Year, Dam, MedianPassage) %>%
  filter(!is.na(MedianPassage)& Dam != "Est_BON")

Yearling_data <- merge(match_help,Yearling_data,by="Dam")

Yearling_data <- Yearling_data %>% arrange(cell,Year)

plot(Yearling_data$Year, Yearling_data$MedianPassage,
     pch=19, col=as.factor(Yearling_data$Dam))

Yearling_data$region <- as.numeric(as.factor(Yearling_data$cell))

data_in <- list(
  N = nrow(Yearling_data),
  Nreg = length(unique(Yearling_data$region)),
  r = as.numeric(as.factor(Yearling_data$region)),
  YR = Yearling_data$Year - 2017,
  Y = Yearling_data$MedianPassage
)

stan_model_yearling_linear <- stan(
  file = "Stan Files/V3_Trend_fish.stan",
  data = data_in,
  chains = 4,
  warmup = 20000,
  iter = 22000,
  cores = 4,
  control = list(adapt_delta = .995)
)

saveRDS(Yearling_data,"yearling_trend_data.rds")
saveRDS(stan_model_yearling_linear,"yearling_trend_mdl.rds")

MCMCsummary(stan_model_yearling_linear, pg0 = TRUE)

unique(Yearling_data$cell)
unique(Yearling_data$Dam)
MCMCplot(stan_model_yearling_linear,params=c("mu_zeta","zeta"),
         labels=c("Global","McNary","Bonneville","Lower Granite","Rock Island"),
         main = "Steelhead Trends")



