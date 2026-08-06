

library(nloptr)
library(raster)
library(lubridate)
library(dggridR)
library(rstan)
library(rstanarm)
library(MCMCvis)
library(dplyr)




spec_list <- c("Caspian Tern") # Species list (Just terns here)
year_list <- 2024:2024 # Which years to run for


subregion_list <- list(
  c(-120.1587, -118.91, 45.54066, 46.42694), # 1- McNary  
  c(-122.50852, -120.85763, 45.3086, 45.99029), # 2- Bonneville 
  c(-118.9097, -116.49219, 46.06, 47.01067), # 3- Lower Granite  
  c(-120.84177, -119.40753, 46.86806, 47.64674), # 4 - Rock Island 
  c(-123.4738, -122.08, 46.99, 48.72257), # 5 - Puget Sound  
  c(-124.05549, -123.21989, 46.01954, 46.53563)  # 6 - Columbia Mouth 
)




for (year in year_list){
  #Create df for record keeping
  print(year)
  #Read in year file - note here where these files are coming from. You'll need to download the files from 
  #Google Drive to your local computer
  f_name <- paste("Data/eBird/PNW_Oct-2024_Y",year,".txt",sep="")
  data_in <- read.delim(f_name,sep = "|",na.strings = "")
  
  
  #only keep needed columns
  data_in <- dplyr::select(data_in,c(COMMON.NAME,LATITUDE,LONGITUDE,OBSERVATION.DATE,
                                     SAMPLING.EVENT.IDENTIFIER,DURATION.MINUTES,GROUP.IDENTIFIER))
  
  #Create a new column to save subregion
  data_in$subregion <- 0
  
  
  
  # Save subregion information to each record
  subregion_index <- 0  
  
  for (each_subregion in subregion_list) {
    subregion_index <- subregion_index + 1 
    
    
    Question1 <- data_in$LONGITUDE > each_subregion[1]
    Question2 <- data_in$LONGITUDE < each_subregion[2]
    Question3 <- data_in$LATITUDE > each_subregion[3]
    Question4 <- data_in$LATITUDE < each_subregion[4]
    
    
    matching <- which((Question1 * Question2 * Question3 * Question4) == 1)
    
    
    data_in$subregion[matching] <- subregion_index
  }
  

  data_in <- filter(data_in, subregion > 0)
  
  
  #Read in unique checklists
  grid_cell_checklist <- unique(dplyr::select(data_in,c("SAMPLING.EVENT.IDENTIFIER","subregion","DURATION.MINUTES")))
  
  #Change effort from minutes to hours
  grid_cell_checklist$DURATION.MINUTES <- grid_cell_checklist$DURATION.MINUTES/60
  colnames(grid_cell_checklist)[c(1,3)] <- c("checklist_num","EH")
  
  #Get unique sampling events by removing duplicates
  sampling_events <- select(data_in,c("SAMPLING.EVENT.IDENTIFIER","GROUP.IDENTIFIER"))
  sampling_events <- sampling_events[!duplicated(sampling_events[,"GROUP.IDENTIFIER"], incomparables = NA),]
  sampling_events <- unique(select(sampling_events,"SAMPLING.EVENT.IDENTIFIER"))
  
  #Add julian days, lat long
  julian_days <- unique(dplyr::select(data_in,c("SAMPLING.EVENT.IDENTIFIER","OBSERVATION.DATE","LATITUDE","LONGITUDE")))
  julian_days$julian_day <- yday(julian_days$OBSERVATION.DATE)
  sampling_events <- merge(sampling_events,julian_days,by="SAMPLING.EVENT.IDENTIFIER")
  
  #Filter to checklists where julian day < 200, rename columns
  sampling_events <- sampling_events %>%
    filter(julian_day < 300) %>%
    dplyr::select(c("SAMPLING.EVENT.IDENTIFIER","julian_day","LATITUDE","LONGITUDE"))
  colnames(sampling_events) <- c("checklist_num","julian_day","LATITUDE","LONGITUDE") 
  
  #Merge together with cell identities
  spec_matrix <- merge(sampling_events,grid_cell_checklist,by="checklist_num")
  
  #Remove Lat-long, no longer needed
  spec_matrix <- dplyr::select(spec_matrix,c("checklist_num","julian_day","subregion","EH"))
  
  #For each species, add columns, and P/NP information for each checklist.
  # This results in a 5-column dataframe, with the last column indicating whether 
  # a caspian tern was reported (1) or not (0)
  for (spec_name in spec_list){
    #Add species columns
    spec_data <- data_in %>% 
      filter(COMMON.NAME == spec_name) %>%
      summarise(checklist_num = unique(SAMPLING.EVENT.IDENTIFIER),p = 1)
    
    spec_matrix <- merge(spec_matrix,spec_data,by="checklist_num", all.x = TRUE)
    spec_matrix$p[which(is.na(spec_matrix$p))] <- 0
    colnames(spec_matrix)[ncol(spec_matrix)] <- spec_name
  }
  
  
  #Save total number of species
  num_species <- length(spec_list)
  
  #Set up stan model conditions
  DELTA <- .95
  TREE_DEPTH <- 10
  ITER <- 1500
  CHAINS <- 4
  
  #Clear memory
  lat_longs <- fff <- julian_days <- sampling_events <- grid_cell_checklist <- data_in <- grid_cell_checklist <- NULL
  
  #This code is in a for loop in case you want to do multiple species
  for (each_spec in 1:num_species){
    arrival_df_all <- c()
    #Counter for cell-species-year combos
    counter <- 0
    #Index for the column where species information is given
    spec_ind <- each_spec + 4
    
    print(paste(spec_list[each_spec],each_spec,sep=" "))
    print(spec_ind)
    
    # For each cell, check:
    # Is species reported on >= 1% of checklists?
    # Are <= 2% of detections from before julian_day 60?
    # Are 20+ days of non-detection present before first detection?
    # Detections on >=20 days
    
    unique_cells <- unique(spec_matrix$subregion)
    GAM_cells <- c()
    low_prob_cells <- c()
    for (each_cell in unique_cells){
      
      #Get all the data from each subregion
      cell_data <- spec_matrix %>% filter(subregion == each_cell) %>% dplyr::select(c(1,2,3,4,spec_ind))
      
      total_detections <- sum(cell_data[,5])
      
      if (total_detections > 0){
        
        num_checklists <- length(unique(cell_data$checklist_num))
        num_terns_observed <- sum(cell_data[, 5])
        
        # Print the results
        print(paste("Year:", year, 
                    "Subregion:", each_cell, 
                    "Checklists:", num_checklists, 
                    "Caspian Terns Observed:", num_terns_observed))
        
        perc_checklists_reported <- sum(cell_data[,5])/nrow(cell_data)
        
        first_detection <- min(cell_data$julian_day[which(cell_data[,5]==1)])
        
        non_detections <- cell_data$julian_day[which(cell_data[,5] == 0)]
        
        num_detection_dates <- length(unique(cell_data$julian_day[which(cell_data[,5]==1)]))
        
        detections_before_60 <- sum(cell_data$julian_day[which(cell_data[,5]==1)] < 60)/total_detections
        
        nd_before_first <- sum(non_detections < first_detection)
        
        if(detections_before_60 <= .02 & nd_before_first >= 20 & num_detection_dates >= 20){
          
          #Add extra if layer to save cells that are eliminated because they have a low overall probability
          if(perc_checklists_reported >= .001){
            GAM_cells <- c(GAM_cells,each_cell)
          } else {
            low_prob_cells <- c(low_prob_cells,each_cell)
          }
        }
      }
    }
    
    #Check how many subregions will be included for a given species
    print(paste("Number of cells: ",length(GAM_cells),sep=""))
    print(paste("Low prob cells removed: ",length(low_prob_cells),sep=""))
    
    #As long as there are subregions to run, create arrival df to save model output
    if (length(GAM_cells) > 0 ){
      hm_mat <- matrix(data = NA, nrow = length(GAM_cells), ncol = ((ITER/2)*CHAINS))
      max_mat <- matrix(data = NA, nrow = length(GAM_cells), ncol = ((ITER/2)*CHAINS))
      colnames(hm_mat) <- paste0('hm_iter_', 1:((ITER/2)*CHAINS))
      colnames(max_mat) <- paste0('max_iter_', 1:((ITER/2)*CHAINS))
      arrival_df <- data.frame(species = spec_list[each_spec], 
                               year = rep(year, each = length(GAM_cells)), 
                               cell = GAM_cells,
                               num_og_records = NA,
                               max_Rhat = NA,
                               min_neff = NA,
                               mlmax = NA,
                               plmax = NA,
                               num_diverge = NA,
                               num_tree = NA,
                               num_BFMI = NA,
                               delta = NA,
                               tree_depth = NA,
                               t_iter = NA,
                               arr_GAM_mean = NA,
                               arr_GAM_sd = NA,
                               hm_mat,
                               max_mat)
      
      #For each cell to run a model in
      for(each_cell in GAM_cells){
        
        
        counter <- counter + 1
        print(paste("Running #",counter," of ",length(GAM_cells), " cells"))
        
        DELTA <- 0.95
        TREE_DEPTH <- 15
        
        #Filter data to sp-cell-yr
        cell_data <- filter(spec_matrix,subregion==each_cell) %>% dplyr::select(c(1,2,3,4,spec_ind))
        colnames(cell_data)[5] <- "detect"
        og_number_rec <- nrow(cell_data)
      
      
        if (nrow(cell_data) > 10000){
          
          print(paste("Thinning from:",og_number_rec,"records"))
          
          targ_records <- 10000
          
          cell_data <- cell_data[sample(1:nrow(cell_data),targ_records),]
          reduction <- (1-(nrow(cell_data)/og_number_rec))*100
          print(paste("Thinned to",nrow(cell_data),"records,","a",round(reduction,2),"% reduction"))
          if ("detect" %in% colnames(cell_data)) {
            print(paste("Total 1s:", sum(cell_data$detect)))
          } else {
            print("Error: 'detect' column not found in cell_data")
          }
        }
        
       
        #Scale effort
        cell_data$shr <- scale(cell_data$EH, scale = FALSE)[,1]
        
        fit2 <- rstanarm::stan_gamm4(detect ~ s(julian_day, k = 30) + shr,
                                     data = cell_data,
                                     family = binomial(link = "logit"),
                                     algorithm = 'sampling',
                                     iter = ITER,
                                     chains = CHAINS,
                                     cores = CHAINS,
                                     adapt_delta = .95,
                                     control = list(max_treedepth = 10))
        
        #Save model output
        num_diverge <- rstan::get_num_divergent(fit2$stanfit)
        num_tree <- rstan::get_num_max_treedepth(fit2$stanfit)
        num_BFMI <- length(rstan::get_low_bfmi_chains(fit2$stanfit))
        max_Rhat <- round(max(summary(fit2)[, 'Rhat']), 2)
        min_neff <- min(summary(fit2)[, 'n_eff'])
        
        #If there are divergences, try, try again
        while (num_diverge > 0 & DELTA <= 0.99){
          DELTA <- DELTA + 0.01
          
          fit2 <- rstanarm::stan_gamm4(detect ~ s(julian_day, k = 30) + shr,
                                       data = cell_data,
                                       family = binomial(link = "logit"),
                                       algorithm = 'sampling',
                                       iter = ITER,
                                       chains = CHAINS,
                                       cores = CHAINS,
                                       adapt_delta = .95,
                                       control = list(max_treedepth = 10))
          num_diverge <- rstan::get_num_divergent(fit2$stanfit)
          num_tree <- rstan::get_num_max_treedepth(fit2$stanfit)
          num_BFMI <- length(rstan::get_low_bfmi_chains(fit2$stanfit))
          
        }
        arrival_df$num_og_records[counter] <- og_number_rec
        arrival_df$num_diverge[counter] <- num_diverge
        arrival_df$num_tree[counter] <- num_tree
        arrival_df$num_BFMI[counter] <- num_BFMI
        arrival_df$delta[counter] <- DELTA
        arrival_df$tree_depth[counter] <- TREE_DEPTH
        arrival_df$t_iter[counter] <- ITER
        arrival_df$max_Rhat[counter] <- max_Rhat
        arrival_df$min_neff[counter] <- min_neff
        
        #generate predict data
        predictDays <- range(cell_data$julian_day)[1]:range(cell_data$julian_day)[2]
        
        newdata <- data.frame(julian_day = predictDays, shr = 0)
        
        #predict response
        dfit <- rstanarm::posterior_linpred(fit2, newdata = newdata, transform = T)
        halfmax_fit <- rep(NA, ((ITER/2)*CHAINS))
        max_fit <- rep(NA, ((ITER/2)*CHAINS))
        tlmax <- rep(NA, ((ITER/2)*CHAINS))
        #day at which probability of occurence is half local maximum value
        for (L in 1:((ITER/2)*CHAINS)){
          rowL <- as.vector(dfit[L,])
          #first detection
          fd <- min(cell_data$julian_day[which(cell_data$detect == 1)])
          #local maximum(s)
          #from: stackoverflow.com/questions/6836409/finding-local-maxima-and-minima
          lmax_idx <- which(diff(sign(diff(rowL))) == -2) + 1
          lmax <- predictDays[lmax_idx]
          #first local max to come after first detection
          flm <- which(lmax > fd)
          if (length(flm) > 0)
          {
            #first local max to come after first detection
            lmax2_idx <- lmax_idx[min(flm)]
            lmax2 <- lmax[min(flm)]
            tlmax[L] <- TRUE
          } else {
            #no local max
            lmax2_idx <- which.max(rowL)
            lmax2 <- predictDays[which.max(rowL)]
            tlmax[L] <- FALSE
          }
          #store local max
          max_fit[L] <- lmax2 
          #local mins before max (global and local mins)
          lmin_idx <- c(which.min(rowL[1:lmax2_idx]), 
                        which(diff(sign(diff(rowL[1:lmax2_idx]))) == 2) + 1)
          lmin <- predictDays[lmin_idx]
          #local min nearest to local max
          lmin2_idx <- lmin_idx[which.min(lmax2 - lmin)]
          lmin2 <- predictDays[lmin2_idx]
          
          #value at local max - value at min (typically 0)
          dmm <- rowL[lmax2_idx] - rowL[lmin2_idx]
          #all positions less than or equal to half diff between max and min + value min
          tlm <- which(rowL <= ((dmm/2) + rowL[lmin2_idx]))
          #which of these come before max and after or at min
          
          vgm <- tlm[which(tlm < lmax2_idx & tlm >= lmin2_idx)]
          #insert halfmax (first day for situations where max is a julian_day = 1)
          if (length(vgm) > 0)
          {
            halfmax_fit[L] <- predictDays[max(vgm)]
          } else {
            halfmax_fit[L] <- predictDays[1]
          }
        }
        
        #number of iterations that had local max
        arrival_df$plmax[counter] <- round(sum(tlmax)/((ITER/2)*CHAINS), 3)
        
        #model fit
        mn_dfit <- apply(dfit, 2, mean)
        LCI_dfit <- apply(dfit, 2, function(x) quantile(x, probs = 0.025))
        UCI_dfit <- apply(dfit, 2, function(x) quantile(x, probs = 0.975))
        
        #check whether local max exists for mean model fit
        mlmax <- sum(diff(sign(diff(mn_dfit))) == -2)
        if (mlmax > 0){
          arrival_df$mlmax[counter] <- TRUE
        } else {
          arrival_df$mlmax[counter] <- FALSE
        }
        
        #estimated halfmax
        mn_hm <- mean(halfmax_fit)
        LCI_hm <- quantile(halfmax_fit, probs = 0.025)
        UCI_hm <- quantile(halfmax_fit, probs = 0.975)
        
        #estimated max
        mn_max <- mean(max_fit)
        LCI_max <- quantile(max_fit, probs = 0.025)
        UCI_max <- quantile(max_fit, probs = 0.975)
        
        #fill df with halfmax iter
        cndf <- colnames(arrival_df)
        hm_iter_ind <- grep('hm_iter', cndf)
        arrival_df[counter, hm_iter_ind] <- halfmax_fit
        
        #fill df with max iter
        max_iter_ind <- grep('max_iter', cndf)
        arrival_df[counter, max_iter_ind] <- max_fit
        
        arrival_df[counter,"arr_GAM_mean"] <- mean(halfmax_fit)
        arrival_df[counter,"arr_GAM_sd"] <- sd(halfmax_fit)
        
        ########################
        #PLOT MODEL FIT AND DATA
        
        pdf(paste0("Data/spec_hm_pdfs/",spec_list[each_spec], '_', year, '_',each_cell, '_arrival.pdf'))
        plot(predictDays, UCI_dfit, type = 'l', col = 'red', lty = 2, lwd = 2,
             ylim = c(0, max(UCI_dfit)),
             main = paste0(spec_list[each_spec], ' - ', year, ' - ', each_cell),
             xlab = 'Julian Day', ylab = 'Probability of occurrence')
        lines(predictDays, LCI_dfit, col = 'red', lty = 2, lwd = 2)
        lines(predictDays, mn_dfit, lwd = 2)
        dd <- cell_data$detect
        dd[which(dd == 1)] <- max(UCI_dfit)
        points(cell_data$julian_day, dd, col = rgb(0,0,0,0.25))
        abline(v = mn_hm, col = rgb(0,0,1,0.5), lwd = 2)
        abline(v = LCI_hm, col = rgb(0,0,1,0.5), lwd = 2, lty = 2)
        abline(v = UCI_hm, col = rgb(0,0,1,0.5), lwd = 2, lty = 2)
        abline(v = mn_max, col = rgb(0,1,0,0.5), lwd = 2)
        abline(v = LCI_max, col = rgb(0,1,0,0.5), lwd = 2, lty = 2)
        abline(v = UCI_max, col = rgb(0,1,0,0.5), lwd = 2, lty = 2)
        legend('topleft',
               legend = c('Model fit', 'CI fit', 'Half max', 'CI HM'),
               col = c('black', 'red', rgb(0,0,1,0.5), rgb(0,0,1,0.5)),
               lty = c(1,2,1,2), lwd = c(2,2,2,2), cex = 1.3)
        dev.off()
      }
      #Add to full record
      arrival_df_all <- rbind(arrival_df_all,arrival_df)
    }
    #arrival_df_all[,1:16]
    spec_name_gsub <- gsub(" ","_",spec_list[each_spec])
    #If any GAMs were succesfull, save
    if (!is.null(nrow(arrival_df_all))){
      f_name <- paste("Data/spec_yr_files/",spec_name_gsub,"_",year,".rds",sep="")
      saveRDS(object = arrival_df_all,f_name)
    }
  }
}




