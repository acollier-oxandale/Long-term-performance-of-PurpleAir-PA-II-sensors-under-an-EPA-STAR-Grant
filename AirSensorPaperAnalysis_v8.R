
###

# Analysis to accompany the manuscript: "AirSensor v1.0: Enhancements to the open-source R package to enable deep 
# understanding of the long-term performance and reliability of PurpleAir sensors"

# Authors: Ashley Collier-Oxandale, Brandon Feenstra, Vasileios Papapostolou, and Andrea Polidori 

# Last edited: 6/30/2021

###

# R version output: 

# platform       x86_64-w64-mingw32          
# arch           x86_64                      
# os             mingw32                     
# system         x86_64, mingw32             
# status                                     
# major          3                           
# minor          5.1                         
# year           2018                        
# month          07                          
# day            02                          
# svn rev        74947                       
# language       R                           
# version.string R version 3.5.1 (2018-07-02)
# nickname       Feather Spray   

###

# To begin, enable the following packages: 

library(AirSensor)     # ver. 1.0.8
library(stringr)       # ver. 1.4.0
library(dplyr)         # ver. 1.0.7
library(openair)       # ver. 2.8-3
library(lubridate)     # ver. 1.7.10
library(Metrics)       # ver. 0.1.4
library(PWFSLSmoke)    # ver. 1.2.111
library(plyr)          # ver. 1.8.6
library(scales)        # ver. 1.1.1
library(geodist)       # ver. 0.0.7
library(classInt)      # ver. 0.4-3
library(RColorBrewer)  # ver. 1.1.-2
library(pracma)        # ver. 2.3.3
library(geosphere)     # ver. 1.5-10

###

# Set archive base for the AirSensor package 
setArchiveBaseUrl("http://data.mazamascience.com/PurpleAir/v1/")


### PART 1 ###########################################################################################

# Identify relevant sensors and create a local archive of data 

# Load several 'pas' data objects in orber to develop a complete list of active and inactive sensors 
pas1 <- pas_load(datestamp = "20191001", archival = TRUE)
pas2 <-pas_load(datestamp = "20201101", archival = TRUE)

# Merge 'pas' data objects and remove duplicate rows
pas <- rbind(pas1, pas2)
pas <- distinct(pas, label, .keep_all = TRUE)

# Filter pas objects for each STAR Grant commmunity and create a 'pas' data object including
# all communities (used for data processing purposes) 
pas_SCAH <-  pas_filter(pas = pas, stringr::str_detect(label, "[Ss][Cc][Aa][Hh]_")); 
pas_SCAH <- distinct(pas_SCAH, deviceDeploymentID, .keep_all = TRUE)

pas_SCAN <-  pas_filter(pas = pas, stringr::str_detect(label, "[Ss][Cc][Aa][Nn]_")); 
pas_SCAN <- distinct(pas_SCAN, deviceDeploymentID, .keep_all = TRUE)

pas_SCAP <-  pas_filter(pas = pas, stringr::str_detect(label, "[Ss][Cc][Aa][Pp]_")); 
pas_SCAP <- distinct(pas_SCAP, deviceDeploymentID, .keep_all = TRUE)

pas_SCEM <-  pas_filter(pas = pas, stringr::str_detect(label, "[Ss][Cc][Ee][Mm]_")); 
pas_SCEM <- distinct(pas_SCEM, deviceDeploymentID, .keep_all = TRUE)

pas_SCIV <-  pas_filter(pas = pas, stringr::str_detect(label, "[Ss][Cc][Ii][Vv]_")); 
pas_SCIV <- distinct(pas_SCIV, deviceDeploymentID, .keep_all = TRUE)

pas_SCNP <-  pas_filter(pas = pas, stringr::str_detect(label, "[Ss][Cc][Nn][Pp]_"));
pas_SCNP <- distinct(pas_SCNP, deviceDeploymentID, .keep_all = TRUE)

pas_SCPR <-  pas_filter(pas = pas, stringr::str_detect(label, "[Ss][Cc][Pp][Rr]_"));
pas_SCPR <- distinct(pas_SCPR, deviceDeploymentID, .keep_all = TRUE)

pas_SCSB <-  pas_filter(pas = pas, stringr::str_detect(label, "[Ss][Cc][Ss][Bb]_"));
pas_SCSB <- distinct(pas_SCSB, deviceDeploymentID, .keep_all = TRUE)

pas_SCSG <-  pas_filter(pas = pas, stringr::str_detect(label, "[Ss][Cc][Ss][Gg]_"));
pas_SCSG <- distinct(pas_SCSG, deviceDeploymentID, .keep_all = TRUE)

pas_SCSH <-  pas_filter(pas = pas, stringr::str_detect(label, "[Ss][Cc][Ss][Hh]_"));
pas_SCSH <- distinct(pas_SCSH, deviceDeploymentID, .keep_all = TRUE)

pas_SCTV <-  pas_filter(pas = pas, stringr::str_detect(label, "[Ss][Cc][Tt][Vv]_"));
pas_SCTV <- distinct(pas_SCTV, deviceDeploymentID, .keep_all = TRUE)

pas_SCUV <-  pas_filter(pas = pas, stringr::str_detect(label, "[Ss][Cc][Uu][Vv]_"));
pas_SCUV <- distinct(pas_SCUV, deviceDeploymentID, .keep_all = TRUE)

pas_RUSD <-  pas_filter(pas = pas, stringr::str_detect(label, "[Rr][Uu][Ss][Dd]_"));
pas_RUSD <- distinct(pas_RUSD, deviceDeploymentID, .keep_all = TRUE)

pas_RIVR <-  pas_filter(pas = pas, stringr::str_detect(label, "[Rr][Ii][Vv][Rr]_"));
pas_RIVR <- distinct(pas_RIVR, deviceDeploymentID, .keep_all = TRUE)

pas_all <- rbind(pas_SCAH, pas_SCAN, pas_SCAP, pas_SCEM, pas_SCIV, pas_SCNP, pas_SCPR, 
                 pas_SCSB, pas_SCSG, pas_SCSH, pas_SCTV, pas_SCUV, pas_RUSD, pas_RIVR)

communityList <- list(pas_SCAH, pas_SCAN, pas_SCAP, pas_SCEM, pas_SCIV, pas_SCNP, pas_SCPR, 
                      pas_SCSB, pas_SCSG, pas_SCSH, pas_SCTV, pas_SCUV, pas_RUSD, pas_RIVR)


# Create an archive of 'pat' data objects, process data into sensor objects, 
# and organize data for analysis  

# Create an archive to save 'pat' data objects in the working directoy
pat_filePath <- path.expand("~/Data/patArchive")
if ( !dir.exists(pat_filePath) ) {
  dir.create(pat_filePath)
}

# Create an archive to save 'sensor' data objects in the working directory
sen_filePath <- path.expand("~/Data/senArchive")
if ( !dir.exists(sen_filePath) ) {
  dir.create(sen_filePath)
}

# Create an archive to save 'soh' data objects in the working directory
soh_filePath <- path.expand("~/Data/sohArchive")
if ( !dir.exists(soh_filePath) ) {
  dir.create(soh_filePath)
}

# Loop through all STAR Grant sensors and create a local archive of data required for the analysis
# (note, it can take some time to access and save the data, but once a local copy is available
# the data may be recalled and used for analysis very quickly) 

for(i in 1:14){
  comm <- communityList[[i]]
  
  for(j in 1:length(comm$ID)){
    
    label <- comm$label[j]; id <- comm$deviceDeploymentID[j]
    temp_pat <- NA
    
    # first attempt to load data from the STAR Grant data archive
    try(temp_pat <- pat_load(id = id, pas = pas, label = label, startdate = 20171001, enddate = 20201101, timezone = "America/Los_Angeles"), silent = TRUE)
    
    # if the data is not available in the STAR Grant archive, load from Thingspeak
    if(is.na(temp_pat) == TRUE) {
      try(temp_pat <- pat_createNew(id = id, pas = pas, label = label, startdate = 20171001, enddate = 20201101, timezone = "America/Los_Angeles"), silent = TRUE)
    }
    
    # if a 'pat' is available for the given sensor, save the 'pat' and process the data
    if(is.na(temp_pat) == FALSE) {
      
      # remove bench test data 
      startDate <- temp_pat$data$datetime[1] + hours(24)
      
      if(startDate < max(temp_pat$data$datetime)){
        temp_pat <- pat_filterDate(temp_pat, startdate = startDate, enddate = "20201101")
      }
      if(startDate >= max(temp_pat$data$datetime)){
        temp_pat <- NA
      }
      
    }
    
    if(is.na(temp_pat) == FALSE) {
      
      filePath_temp <- file.path(pat_filePath, paste(paste("pat_", id, ".rda", sep = "")))
      save(temp_pat, file = filePath_temp)
      
      temp_sensor <- pat_createAirSensor(pat = temp_pat, parameter = "pm25", FUN = PurpleAirQC_hourly_AB_03)
      filePath_temp <- file.path(sen_filePath, paste(paste("sen_", id, ".rda", sep = "")))
      save(temp_sensor, file = filePath_temp)
      
      temp_soh <- pat_dailySoH(pat = temp_pat)
      filePath_temp <- file.path(soh_filePath, paste(paste("soh_", id, ".rda", sep = "")))
      save(temp_soh, file = filePath_temp)
      
    }
    
  }
  
  print(paste("Community", i, "is complete"))
  
}


### PART 2 ###########################################################################################

# Load the data from the local archive and process/organize for analysis and visualization
# including: calculating a percent of data passing QA/QC per day, a cumulative runtime (in days), 
# and an estimated cumulative expsoure or loading (in ug) 

# Remove indoor sensors for West LA Community, leaving only outdoor sensors for analysis 
communityList[[12]] <- communityList[[12]][-c(14:32),] 


for(i in 1:14){
  
  comm <- communityList[[i]]
  soh_list <- list()
  
  for(k in 1:length(comm$ID)){
    
    # load data files 
    label <- comm$label[k]; id <- comm$deviceDeploymentID[k]
    temp_pat <- NA; temp_sensor <- NA; temp_soh <- NA
    
    filePath_temp <- file.path(pat_filePath, paste(paste("pat_", id, ".rda", sep = ""))); 
    try(load(filePath_temp))
    
    filePath_temp <- file.path(sen_filePath, paste(paste("sen_", id, ".rda", sep = ""))); 
    try(load(filePath_temp))
    
    filePath_temp <- file.path(soh_filePath, paste(paste("soh_", id, ".rda", sep = ""))); 
    try(load(filePath_temp))
    
    if(is.na(temp_sensor) == FALSE) {
      
      # add environmental parameters and pm2.5 data to assess percent passing QA/QC
      temp <- temp_pat$data; temp$date <- temp$datetime 
      temp$pm25_raw <- rowMeans(temp[,c('pm25_A', 'pm25_B')], na.rm = TRUE)
      temp <- timeAverage(temp, avg.time = "hour", statistic = "median")
      temp <- temp[,c("date", "pm25_raw", "temperature", "humidity")]
      
      # calculate percent passing QA/QC
      colnames(temp_sensor$data) <- c("date", "pm25") 
      temp_sensor$data <- left_join(temp_sensor$data, temp)
      temp_sensor$data$qaqc_pass <- NA
      ind1 = which(is.na(temp_sensor$data$pm25) == TRUE & is.na(temp_sensor$data$pm25_raw) != TRUE)
      ind2 = which(is.na(temp_sensor$data$pm25) != TRUE & is.na(temp_sensor$data$pm25_raw) != TRUE)
      temp_sensor$data$qaqc_pass[ind1] = 0
      temp_sensor$data$qaqc_pass[ind2] = 1
      temp_sensor_daily = timeAverage(temp_sensor$data, avg.time = "day")
      colnames(temp_sensor_daily) <- c("datetime", "pm25", "pm25_raw", "temperature","humidity", "qaqc_pass") 
      
      # add cumulative runtime 
      temp_sensor_daily$runtime = 1
      ind = which(is.na(temp_sensor_daily$pm25) == TRUE)
      temp_sensor_daily$runtime[ind] = 0
      temp_sensor_daily$runtime <- cumsum(temp_sensor_daily$runtime)
      
      # add estimated cumulative exposure - original
      #  temp_sensor_daily$cmexp = temp_sensor_daily$pm25
      #  ind = which(is.na(temp_sensor_daily$pm25) == TRUE)
      #  temp_sensor_daily$cmexp[ind] = 0
      #  temp_sensor_daily$cmexp <- cumsum(temp_sensor_daily$cmexp)
      
      
      # add estimated cumulative exposure - new
      # factors incorporated into calc: .5 ft3/min flow, 0.02831685 m3/ft3, and 1440 minutes/day
      temp_sensor_daily$cmexp = temp_sensor_daily$pm25
      ind = which(is.na(temp_sensor_daily$pm25) == TRUE)
      temp_sensor_daily$cmexp[ind] = 0
      temp_sensor_daily$cmexp <- cumsum(temp_sensor_daily$cmexp * 20.3881)
      
      # merge with SOH metrics
      timezone = tz(temp_sensor_daily$datetime)
      tz(temp_soh$datetime) <- timezone
      tempdata <- left_join(temp_soh, temp_sensor_daily)
      
      # save data to list
      soh_list[[k]] <- tempdata
      
    }
  }
  
  newText <- substr(communityList[[i]]$label[1], 1, 4)
  newName <- paste("soh_", newText, sep = "")
  assign(newName, soh_list)
  
  print(paste("Community", i, "is complete"))
  
}


# assemble enhanced SOH datasets into single list
sohList <- list(soh_SCAH, soh_SCAN, soh_SCAP, soh_SCEM, soh_SCIV, soh_SCNP, soh_SCPR, 
                soh_SCSB, soh_SCSG, soh_SCSH, soh_Sctv, soh_SCUV, soh_RUSD, soh_RIVR)


### PART 3 ###########################################################################################

### Results and plots for Section 3.1 

# Calculate the min, max, and median number of days passing QA/QC by community, data in Table 3

# Table 3
lengthOpr <- matrix(ncol=14,nrow = 3)

for(i in 1:14){
  
  dataset <- sohList[[i]]
  tempdata <- matrix(ncol = 1, nrow = length(sohList))
  
  for (k in 1:length(dataset)){
    tempdata[k] <- length(which(dataset[[k]]$qaqc_pass >= .85))
  }
  
  ind = which(tempdata == 0); tempdata[ind] = NA
  lengthOpr[1,i] = min(tempdata, na.rm = TRUE)
  lengthOpr[2,i] = max(tempdata, na.rm = TRUE)
  lengthOpr[3,i] = median(tempdata, na.rm = TRUE)
  
}

# Sort the sensors in each community based on perforamnce

# Group 1: for 90% of days, 90% or more of the data passed QA/QC
# Group 2: sensor exhibted QA/QC issues, but more than 90% of the data collected in the final 2 weeks 
#          of operation passed QA/QC indicating that the sensor recovered 
# Group 3: sensor exhibited consistent QA/QC issues and/or exhibited QA/QC issue at the end of it's operation

perfStats <- matrix(ncol=14,nrow = 3)

for(i in 1:14){
  
  dataset <- sohList[[i]]
  tempdata <- data.frame(matrix(ncol=3,nrow=length(dataset)))
  colnames(tempdata) <- c("grp1", "grp2", "grp3")
  tempdata$grp1 = 0; tempdata$grp2 = 0; tempdata$grp3 = 0
  
  for(j in 1:length(dataset)){
    soh <- dataset[[j]]
    if(length(soh) == 28 & length(soh$datetime) >= 14){
      metric1 <- quantile(soh$qaqc_pass, .1, na.rm = TRUE)
      l = length(soh$datetime); metric2 <- mean(soh$qaqc_pass[l-14:l], na.rm = TRUE)
      if(metric1 >= .9 & metric2 >= .9) {tempdata$grp1[j] = 1}
      if(metric1 < .9 & metric2 >= .9) {tempdata$grp2[j] = 1}
      if(metric1 < .9 & metric2 < .9) {tempdata$grp3[j] = 1}
    }
  }
  
  perfStatsbyComm <- colSums(tempdata)
  perfStats[,i] <- perfStatsbyComm
  
}

# barplot of groupings in perfStats

# Figure 4
par(mfrow=c(1,1), mar = c(5,5,3,3))
barplot(perfStats, legend = c("Group 1", "Group 2", "Group 3"),  
        col = c("navyblue", "dodgerblue3", "lightskyblue"), 
        names.arg=c("A","B","C","D","E","F","G","H","I","J","K","L","M","Z"), 
        ylab = "Number of Sensors (#)")

# Examine performance over time

# Create three dataframes: 
# 1 - indicating whether a sensor appeared operational (based on the presence of raw PM2.5 data)
# 2 - indicating whether Channel A and B were both providing more than 90% complete data, per day (SOH metric)
# 3 - indicating whether more than 90% of data passed QA/QC 

tempdates <- seq(as.POSIXct("2017-10-01", tz = "UTC"), as.POSIXct("2020-11-01", tx = "UTC"), by="days")

senFunc <- data.frame(matrix(ncol = 2, nrow = 1128)); senFunc$X1 <- tempdates
totComplete <- data.frame(matrix(ncol = 2, nrow = 1128)); totComplete$X1 <- tempdates
totQAQCPass <- data.frame(matrix(ncol = 2, nrow = 1128)); totQAQCPass$X1 <- tempdates

colnames(senFunc) <- c("date"); colnames(totComplete) <- c("date"); colnames(totQAQCPass) <- c("date")

for(i  in 1:14){
  dataset <- sohList[[i]]
  
  for(k in 1:length(dataset)){
    if(is.null(dataset[[k]]) == FALSE){
      temp1 <- data.frame(matrix(ncol = 2, nrow = length(dataset[[k]]$datetime)))
      temp2 <- data.frame(matrix(ncol = 2, nrow = length(dataset[[k]]$datetime)))
      temp3 <- data.frame(matrix(ncol = 2, nrow = length(dataset[[k]]$datetime)))
      temp1$X1 <- dataset[[k]]$datetime;  temp2$X1 <- dataset[[k]]$datetime; temp3$X1 <- dataset[[k]]$datetime
      temp1$X2 <- NA; temp2$X2 <- NA; temp3$X2 <- NA
      
      ind1 <- which(dataset[[k]]$pm25_A_pctValid >= 90 & dataset[[k]]$pm25_B_pctValid >= 90 & 
                      dataset[[k]]$humidity_pctValid >= 90 & dataset[[k]]$temperature_pctValid >= 90)
      ind2 <- which(dataset[[k]]$qaqc_pass >= .9)
      ind3 <- which(is.na(dataset[[k]]$pm25_raw) == FALSE )
      
      temp1$X2[ind1] = 1; temp2$X2[ind2] = 1; temp3$X2[ind3] = 1
      
      colnames(temp1) <- c("date",communityList[[i]]$label[k]);  colnames(temp2) <- c("date",communityList[[i]]$label[k])
      colnames(temp3) <- c("date",communityList[[i]]$label[k]); 
      
      totComplete <- left_join(totComplete, temp1, by = "date")
      totQAQCPass <- left_join(totQAQCPass, temp2, by = "date")
      senFunc <- left_join(senFunc, temp3, by = "date")
    }
  }
  
  print(paste("Community", i, "is complete"))
  
}

# Plot performance over time, Figures 5a. 5b, and 6

senFunc_sum <- rowSums(senFunc[,2:258], na.rm = TRUE)
totComplete_sum <- rowSums(totComplete[,2:258], na.rm = TRUE)
totQAQCPass_sum <- rowSums(totQAQCPass[,2:258], na.rm = TRUE)

par(mfrow=c(1,3), mar = c(5,5,3,3))

# Figure 5a
plot(senFunc$date, senFunc_sum, xlab = "", ylab = "Number of Sensors (#)")
points(totComplete$date, totComplete_sum, col = "dodgerblue3")
points(totComplete$date, totQAQCPass_sum, col = "skyblue")
legend("bottomright", legend = c("Operational Sensors", "Valid Data", "Data Passing QA/QC"), pch = 20, 
       col = c("black", "dodgerblue4", "skyblue"))

# Figure 5b
plot(senFunc$date, totComplete_sum/senFunc_sum*100, xlab = "", ylab = "Percent of Sensors Providing Valid Data", ylim = c(0,110))

# Figure 5c
plot(senFunc$date, totQAQCPass_sum/senFunc_sum*100, xlab = "", ylab = "Percent of Sensors Providing Data Passing QA/QC", 
     ylim = c(0,110))



### PART 4 ###########################################################################################

### Results and plots for Section 3.2

# Reformat data in order to create boxplots - averge by month, add a sensor number, 
# aggregate to a single table

# Organize data by month

boxdata_month <- sohList[[1]][[1]]; boxdata_month$senNum = 1; boxdata_month$date <- boxdata_month$datetime
boxdata_month <- timeAverage(boxdata_month, avg.time = "month")

for (i in 2:length(sohList[[1]])) {
  if(is.null(sohList[[1]][[i]]) == FALSE){
    temp <- sohList[[1]][[i]]; temp$senNum = i; temp$date <- temp$datetime
    temp <- timeAverage(temp, avg.time = "month")
    boxdata_month <- rbind(boxdata_month, temp)
  }}

x <- max(boxdata_month$senNum)

for(i in 2:14){
  for (j in 1:length(sohList[[i]])) {
    if(is.null(sohList[[i]][[j]]) == FALSE){
      temp <- sohList[[i]][[j]]; temp$senNum = x + j; temp$date <- temp$datetime
      temp <- timeAverage(temp, avg.time = "month")
      boxdata_month <- rbind(boxdata_month, temp)
    }}
  x <- max(boxdata_month$senNum)
}

boxdata_month$date <- as.Date(boxdata_month$date, format="%m-%Y")

# Create boxplots

# Figure 6
par(mfrow=c(1,1), mar = c(5,5,3,3))
temptime <- seq(min(boxdata_month$datetime), max(boxdata_month$datetime), by = "month")
temptime <- as.character.POSIXt(temptime, format = "%b %Y")
temptime2 <- temptime; ind <- seq(2, length(temptime2), 2); temptime2[ind] <- ""
boxplot(boxdata_month$pm25_A_pm25_B_rsquared ~ boxdata_month$date, col = "lightskyblue1", ylim = c(0,1.1), 
ylab = expression("R"^2* " (between Ch A and Ch B)"), las = 2, names = temptime2)
axis.Date(1, at=seq(min(boxdata_month$date), max(boxdata_month$date), "months"), format="%m / %d / %Y", las = 2)
nbGroup <- nlevels(factor(boxdata_month$date))
text(x=c(1:nbGroup), y=1.05, cex = .85, paste(table(boxdata_month$date),sep=""))

# Figure 7a - 7d
par(mfrow=c(2,2), mar = c(5,4,2,2) + .1)
boxplot(boxdata_month$pm25_A_pctValid ~ boxdata_month$date, col = "lightskyblue1", ylim = c(0,100), 
        ylab = "Percent Valid - Ch A (%)",las = 2, names = temptime2)
boxplot(boxdata_month$pm25_A_pm25_B_slope ~ boxdata_month$date, col = "lightskyblue1", ylim = c(-1,3), 
        ylab = "Slope (between Ch A and Ch B)", las = 2, names = temptime2)
boxplot(boxdata_month$pm25_B_pctValid ~ boxdata_month$date, col = "lightskyblue1", ylim = c(0,100), 
        ylab = "Percent Valid - Ch B (%)", las = 2, names = temptime2)
boxplot(boxdata_month$qaqc_pass*100 ~ boxdata_month$date, col = "lightskyblue1", ylim = c(0,100), 
        ylab = "Percent Passing QA/QC (%)", las = 2, names = temptime2)

# Figure S1
par(mfrow=c(2,2), mar = c(4,4,2,2) + .1)
boxplot(boxdata_month$humidity_pctValid ~ boxdata_month$date, col = "lightskyblue1", ylim = c(0,100), 
        ylab = "Percent Valid - Humidity (%)")
boxplot(boxdata_month$temperature_pctValid ~ boxdata_month$date, col = "lightskyblue1", ylim = c(0,100), 
        ylab = "Percent Valid - Temperature (%)")
boxplot(boxdata_month$pm25_A_pctReporting ~ boxdata_month$date, col = "lightskyblue1", ylim = c(0,160), 
        ylab = "Percent Reporting - Ch A (%)")
boxplot(boxdata_month$pm25_A_pctDC ~ boxdata_month$date, col = "lightskyblue1", ylim = c(0,100), 
        ylab = "Percent DC - Ch A (%)")

###

# Organize data by cumulative runtime

boxdata_runtime <- sohList[[1]][[1]]; boxdata_runtime$senNum = 1; 
boxdata_runtime$roundRT <- round_any(boxdata_runtime$runtime, 50)
boxdata_runtime <- aggregate(boxdata_runtime, by = list(boxdata_runtime$roundRT), FUN = "mean")

for (i in 2:length(sohList[[1]])) {
  if(is.null(sohList[[1]][[i]]) == FALSE){
    temp <- sohList[[1]][[i]]; temp$senNum = i
    temp$roundRT <- round_any(temp$runtime, 50); temp <- aggregate(temp, by = list(temp$roundRT), FUN = "mean")
    boxdata_runtime <- rbind(boxdata_runtime, temp)
  }}

x <- max(boxdata_runtime$senNum)

for(i in 2:14){
  for (j in 1:length(sohList[[i]])) {
    if(is.null(sohList[[i]][[j]]) == FALSE){
      temp <- sohList[[i]][[j]]; temp$senNum = x + j;
      temp$roundRT <- round_any(temp$runtime, 50); temp <- aggregate(temp, by = list(temp$roundRT), FUN = "mean")
      boxdata_runtime <- rbind(boxdata_runtime, temp)
    }}
  x <- max(boxdata_runtime$senNum)
}

# Create boxplots

# Figure 8
par(mfrow=c(1,1), mar = c(5,5,3,3))
boxplot(boxdata_runtime$pm25_A_pm25_B_rsquared ~ boxdata_runtime$roundRT, col = "lightskyblue1", ylim = c(0,1.1), 
        ylab = expression("R"^2* " (between Ch A and Ch B)"), xlab = "Runtime (days)")
nbGroup <- nlevels(factor(boxdata_runtime$roundRT))
text(x=c(1:nbGroup), y=1.05, cex = .85, paste(table(boxdata_runtime$roundRT),sep=""))

# Figure 9a - 9d
par(mfrow=c(2,2), mar = c(4,4,1,1) + .1)
boxplot(boxdata_runtime$pm25_A_pctValid ~ boxdata_runtime$roundRT, col = "lightskyblue1", ylim = c(0,100), 
        ylab = "Percent Valid - Ch A (%)", xlab = "Runtime (days)")
boxplot(boxdata_runtime$pm25_A_pm25_B_slope ~ boxdata_runtime$roundRT, col = "lightskyblue1", ylim = c(-1,3), 
        ylab = "Slope (between Ch A and Ch B)", xlab = "Runtime (days)")
boxplot(boxdata_runtime$pm25_B_pctValid ~ boxdata_runtime$roundRT, col = "lightskyblue1", ylim = c(0,100), 
        ylab = "Percent Valid - Ch B (%)", xlab = "Runtime (days)")
boxplot(boxdata_runtime$qaqc_pass*100 ~ boxdata_runtime$roundRT, col = "lightskyblue1", ylim = c(0,100), 
        ylab = "Percent Passing QA/QC (%)", xlab = "Runtime (days)")

# Figure S2
par(mfrow=c(2,2), mar = c(4,4,2,2) + .1)
boxplot(boxdata_runtime$humidity_pctValid ~ boxdata_runtime$roundRT, col = "lightskyblue1", ylim = c(0,100), 
        ylab = "Percent Valid - Humidity (%)",  xlab = "Runtime (days)")
boxplot(boxdata_runtime$temperature_pctValid ~ boxdata_runtime$roundRT, col = "lightskyblue1", ylim = c(0,100), 
        ylab = "Percent Valid - Temperature (%)",  xlab = "Runtime (days)")
boxplot(boxdata_runtime$pm25_A_pctReporting ~ boxdata_runtime$roundRT, col = "lightskyblue1", ylim = c(0,160), 
        ylab = "Percent Reporting - Ch A (%)",  xlab = "Runtime (days)")
boxplot(boxdata_runtime$pm25_A_pctDC ~ boxdata_runtime$roundRT, col = "lightskyblue1", ylim = c(0,100), 
        ylab = "Percent DC - Ch A (%)",  xlab = "Runtime (days)")

###

# Organize data by cumulative exposure

boxdata_cexp <- sohList[[1]][[1]]; boxdata_cexp$senNum = 1; 
boxdata_cexp$roundCE <- round_any(boxdata_cexp$cmexp, 10000) + 10000
boxdata_cexp <- aggregate(boxdata_cexp, by = list(boxdata_cexp$roundCE), FUN = "median")

for (i in 2:length(sohList[[1]])) {
  if(is.null(sohList[[1]][[i]]) == FALSE){
    temp <- sohList[[1]][[i]]; temp$senNum = i
    temp$roundCE <- round_any(temp$cmexp, 10000) + 10000; 
    temp <- aggregate(temp, by = list(temp$roundCE), FUN = "median")
    boxdata_cexp <- rbind(boxdata_cexp, temp)
  }}

x <- max(boxdata_cexp$senNum)

for(i in 2:14){
  for (j in 1:length(sohList[[i]])) {
    if(is.null(sohList[[i]][[j]]) == FALSE){
      temp <- sohList[[i]][[j]]; temp$senNum = x + j;
      temp$roundCE <- round_any(temp$cmexp, 10000) + 10000; 
      temp <- aggregate(temp, by = list(temp$roundCE), FUN = "median")
      boxdata_cexp <- rbind(boxdata_cexp, temp)
    }}
  x <- max(boxdata_cexp$senNum)
}

# Create boxplots

# Figure 10
par(mfrow=c(1,1), mar = c(5,5,3,3))
boxplot(boxdata_cexp$pm25_A_pm25_B_rsquared ~ boxdata_cexp$roundCE, col = "lightskyblue1", ylim = c(0,1.1), 
        ylab = expression("R"^2* " (between Ch A and Ch B)"), xlab = expression("Cumulative Exposure ("*mu*"g)"))
nbGroup <- nlevels(factor(boxdata_cexp$roundCE))
text(x=c(1:nbGroup), y=1.05, cex = .85, paste(table(boxdata_cexp$roundCE),sep=""))

# Figure 11a - 11d
par(mfrow=c(2,2), mar = c(4,4,1,1) + .1)
boxplot(boxdata_cexp$pm25_A_pctValid ~ boxdata_cexp$roundCE, col = "lightskyblue1", ylim = c(0,100), 
        ylab = "Percent Valid - Ch A (%)", xlab = expression("Cumulative Exposure ("*mu*"g)"))
boxplot(boxdata_cexp$pm25_A_pm25_B_slope ~ boxdata_cexp$roundCE, col = "lightskyblue1", ylim = c(-1,3), 
        ylab = "Slope (between Ch A and Ch B)", xlab = expression("Cumulative Exposure ("*mu*"g)"))
boxplot(boxdata_cexp$pm25_B_pctValid ~ boxdata_cexp$roundCE, col = "lightskyblue1", ylim = c(0,100), 
        ylab = "Percent Valid - Ch B (%)", xlab = expression("Cumulative Exposure ("*mu*"g)"))
boxplot(boxdata_cexp$qaqc_pass*100 ~ boxdata_cexp$roundCE, col = "lightskyblue1", ylim = c(0,100), 
        ylab = "Percent Passing QA/QC (%)", xlab = expression("Cumulative Exposure ("*mu*"g)"))

# Figure S3
par(mfrow=c(2,2), mar = c(4,4,2,2) + .1)
boxplot(boxdata_cexp$humidity_pctValid ~ boxdata_cexp$roundCE, col = "lightskyblue1", ylim = c(0,100), 
        ylab = "Percent Valid - Humidity (%)", xlab = expression("Cumulative Exposure ("*mu*"g)"))
boxplot(boxdata_cexp$temperature_pctValid ~ boxdata_cexp$roundCE, col = "lightskyblue1", ylim = c(0,100), 
        ylab = "Percent Valid - Temperature (%)", xlab = expression("Cumulative Exposure ("*mu*"g)"))
boxplot(boxdata_cexp$pm25_A_pctReporting ~ boxdata_cexp$roundCE, col = "lightskyblue1", ylim = c(0,160), 
        ylab = "Percent Reporting - Ch A (%)", xlab = expression("Cumulative Exposure ("*mu*"g)"))
boxplot(boxdata_cexp$pm25_A_pctDC ~ boxdata_cexp$roundCE, col = "lightskyblue1", ylim = c(0,160), 
        ylab = "Percent DC - Ch A (%)", xlab = expression("Cumulative Exposure ("*mu*"g)"))


# To generate similar plots for an individual community, use the code below 
# These are available for each individaul community in the Supplement 
# Figures S4 - S17

## CHOOSE COMMUNITY
#  c = 1
# 
#  # format data
#  boxdata_month <- sohList[[c]][[1]]; boxdata_month$senNum = 1; boxdata_month$date <- boxdata_month$datetime
#  boxdata_month <- timeAverage(boxdata_month, avg.time = "month")
# 
#  for (i in 2:length(sohList[[c]])) {
#    if(is.null(sohList[[c]][[i]]) == FALSE){
#      temp <- sohList[[c]][[i]]; temp$senNum = i; temp$date <- temp$datetime
#      temp <- timeAverage(temp, avg.time = "month")
#      boxdata_month <- rbind(boxdata_month, temp)
#    }}
# 
# boxdata_month <- select(boxdata_month, c("date","pm25_A_pm25_B_rsquared",  "pm25_A_pctValid", "pm25_A_pm25_B_slope", "pm25_B_pctValid", "qaqc_pass" ))
# boxdata_month <- na.exclude(boxdata_month)
#  
# # create boxplots
# 
#  boxdata_month$date <- as.Date(boxdata_month$date, format="%m-%Y")
#  
#  par(mfrow=c(1,1), mar = c(5,5,3,3))
#  boxplot(boxdata_month$pm25_A_pm25_B_rsquared ~ boxdata_month$date, col = "lightskyblue1", ylim = c(0,1.1),
#          ylab = expression("R"^2* " (between Ch A and Ch B)"))
#  nbGroup <- nlevels(factor(boxdata_month$date))
#  text(x=c(1:nbGroup), y=1.05, cex = .85, paste(table(boxdata_month$date),sep=""))
# 
#  par(mfrow=c(2,2), mar = c(4,4,2,2) + .1)
#  boxplot(boxdata_month$pm25_A_pctValid ~ boxdata_month$date, col = "lightskyblue1", ylim = c(0,100),
#          ylab = "Percent Valid - Ch A (%)")
#  boxplot(boxdata_month$pm25_A_pm25_B_slope ~ boxdata_month$date, col = "lightskyblue1", ylim = c(-1,3),
#          ylab = "Slope (between Ch A and Ch B)")
#  boxplot(boxdata_month$pm25_B_pctValid ~ boxdata_month$date, col = "lightskyblue1", ylim = c(0,100),
#          ylab = "Percent Valid - Ch B (%)")
#  boxplot(boxdata_month$qaqc_pass*100 ~ boxdata_month$date, col = "lightskyblue1", ylim = c(0,100),
#          ylab = "Percent Passing QA/QC (%)")


# Additional plots assessing what conditions may be associated with drops in performance 

# Figures 12a - 12d
par(mfrow=c(1,4), mar = c(5,5,1,1) + .1)
smoothScatter(boxdata_month$pm25, boxdata_month$pm25_A_pm25_B_rsquared, xlim = c(0,25), 
              xlab = expression("PM"[2.5]* " ("*mu* "g/m"^3* ")"), cex = 2,  ylab = expression("R"^2* " (between Ch A and Ch B)"))
smoothScatter(boxdata_month$temperature, boxdata_month$pm25_A_pm25_B_rsquared, xlim = c(55,95), 
              xlab = "Temperature (deg F)", cex = 2, ylab = expression("R"^2* " (between Ch A and Ch B)"))
smoothScatter(boxdata_month$humidity, boxdata_month$pm25_A_pm25_B_rsquared, xlim = c(15,85), 
              xlab = "Humidity (%)", cex = 2, ylab = expression("R"^2* " (between Ch A and Ch B)"))

monthIDs <- month(boxdata_month$datetime)
smoothScatter(monthIDs, boxdata_month$pm25_A_pm25_B_rsquared, xlab = "Month (#)", cex = 2,
              ylab = expression("R"^2* " (between Ch A and Ch B)"))



### PART 5 ###########################################################################################

### Results and plots for Section 3.3

# Initial processing involves downloading data from the nearest regulatory monitoring site and 
# pairing the data with the sensor data, the code also calculates several statistics between the 
# sensor and regualtory data using a moving time frame (30 days)

sohList_reg <- sohList # this version will include the regulatory data
monitorIDCheck <- "NA"

# The following code reassigns the closest AMS ID for one sensor (this change is needed based 
# on the availability of data from AMS stations and helps to avoid a failure in the code)
communityList[[5]]$pwfsl_closestMonitorID[1] <- communityList[[5]]$pwfsl_closestMonitorID[2]

# Loops through each community and each sensor

for(i in 1:14){
  
  for(j in 1:length(sohList[[i]])){
    
    if(is.null(sohList[[i]][[j]]) == FALSE) {
      
      id <- communityList[[i]]$deviceDeploymentID[j]
      monitorID <- communityList[[i]]$pwfsl_closestMonitorID[j]
      filePath_temp <- file.path(sen_filePath, paste(paste("sen_", id, ".rda", sep = ""))); load(filePath_temp)
      
      monitorExists <- NA
      try(monitorExists <- monitor_load(20171001, 20171231, monitorIDs = monitorID))
      
      if(strcmp(monitorID, monitorIDCheck) ==  FALSE & is.na(monitorExists) == FALSE){
        
        pwfsl_monitor <- monitor_load(20171001, 20171231, monitorIDs = monitorID)
        pwfsl_data1 <-
          pwfsl_monitor %>%
          PWFSLSmoke::monitor_extractData()
        names(pwfsl_data1) <- c("datetime", "reg_pm25")
        
        pwfsl_monitor <- monitor_load(20180101, 20181231, monitorIDs = monitorID)
        pwfsl_data2 <-
          pwfsl_monitor %>%
          PWFSLSmoke::monitor_extractData()
        names(pwfsl_data2) <- c("datetime", "reg_pm25")
        
        pwfsl_monitor <- monitor_load(20190101, 20191231, monitorIDs = monitorID)
        pwfsl_data3 <-
          pwfsl_monitor %>%
          PWFSLSmoke::monitor_extractData()
        names(pwfsl_data3) <- c("datetime", "reg_pm25")
        
        pwfsl_monitor <- monitor_load(20200101, 20201101, monitorIDs = monitorID)
        pwfsl_data4 <-
          pwfsl_monitor %>%
          PWFSLSmoke::monitor_extractData()
        names(pwfsl_data4) <- c("datetime", "reg_pm25")
        
        pwfsl_data <- rbind(pwfsl_data1, pwfsl_data2, pwfsl_data3, pwfsl_data4)
        
      }
      
      temp_sensor$data <- left_join(temp_sensor$data, pwfsl_data)
      
      tempdata <- temp_sensor$data; colnames(tempdata) <- c("datetime", "pm25", "reg_pm25")
      tempdata$Rsq <- NA; tempdata$slope <- NA; tempdata$intercept <- NA
      tempdata$RMSE <- NA; tempdata$MAE <- NA; tempdata$MBE <- NA; tempdata$MMR <- NA
      
      enddate <- tempdata$datetime[length(tempdata$datetime) - hours(720)]
      ind <- max(which(tempdata$datetime <= enddate))
      
      for(k in 1:ind){
        
        m <- max(which(tempdata$datetime <= (tempdata$datetime[k] + hours(720))))
        mdl <- NULL; rmse <- NULL; mae <- NULL
        tempdata_sub <- tempdata[k:m, 1:3]
        tempdata_sub <- tempdata[k:m, 1:3]
        tempdata_sub <- na.exclude(tempdata_sub)
        
        if(length(tempdata_sub$datetime >= (540))){
          
          mdl <- lm(tempdata_sub$pm25 ~ tempdata_sub$reg_pm25)
          rmse <- rmse(tempdata_sub$reg_pm25, tempdata_sub$pm25)
          mae <- mae(tempdata_sub$reg_pm25, tempdata_sub$pm25)
          mbe <- (sum(tempdata_sub$reg_pm25 - tempdata_sub$pm25))*(1/length(tempdata_sub$datetime))
          mmratio <- abs(mbe)/mae
          
          tempdata$Rsq[k] <- summary(mdl)$r.squared
          tempdata$slope[k] <- mdl$coefficients[[2]]
          tempdata$intercept[k] <- mdl$coefficients[[1]]
          tempdata$RMSE[k] <- rmse
          tempdata$MAE[k] <- mae
          tempdata$MBE[k] <- mbe
          tempdata$MMR[k] <- mmratio
          
        }
      }
      
      tempdata$date <- tempdata$datetime
      tempdata_daily <- timeAverage(tempdata, avg.time = "day"); tempdata_daily$datetime <- tempdata_daily$date
      tempdata_daily$pm25 <- NULL
      sohList_reg[[i]][[j]] <- left_join(sohList_reg[[i]][[j]], tempdata_daily, by = "datetime")
    }
    
    monitorIDCheck <- monitorID
  }
  
  print(paste("Community", i, "is complete"))
}

# Format data for Section 3.3 boxplots

boxdata_reg <- sohList_reg[[14]][[1]]; boxdata_reg$senNum = 1; boxdata_reg$date <- boxdata_reg$datetime
boxdata_reg <- timeAverage(boxdata_reg, avg.time = "month")

for (i in 2:length(sohList_reg[[14]])) {
  if(is.null(sohList_reg[[14]][[i]]) == FALSE){
    temp <- sohList_reg[[14]][[i]]; temp$senNum = i; temp$date <- temp$datetime
    temp <- timeAverage(temp, avg.time = "month")
    boxdata_reg <- rbind(boxdata_reg, temp)
  }}

# Create boxplots 

boxdata_reg$date <- as.Date(boxdata_reg$date, format="%m-%Y")

# Figure 13
par(mfrow=c(1,1), mar = c(5,5,3,3))
temptime <- seq(min(boxdata_month$datetime), max(boxdata_month$datetime), by = "month")
temptime <- as.character.POSIXt(temptime, format = "%b %Y")
temptime2 <- temptime; ind <- seq(2, length(temptime2), 2); temptime2[ind] <- ""
boxplot(boxdata_reg$Rsq ~ boxdata_reg$date, col = "lightskyblue1", ylim = c(0,1.1), 
        ylab = expression("R"^2* " (between Sensor and Regulatory Inst.)"), las = 2, names = temptime2)
nbGroup <- nlevels(factor(boxdata_reg$date))
text(x=c(1:nbGroup), y=1.05, cex = .85, paste(table(boxdata_reg$date),sep=""))

# Figure 14
par(mfrow=c(1,4), mar = c(5,5,1,1) + .1)
smoothScatter(boxdata_reg$pm25, boxdata_reg$pm25_A_pm25_B_rsquared, xlim = c(0,25), 
              xlab = expression("PM"[2.5]* " ("*mu* "g/m"^3* ")"), cex = 2,  ylab = expression("R"^2* " (between Sensor and Regulatory Inst.)"))
smoothScatter(boxdata_reg$temperature, boxdata_reg$pm25_A_pm25_B_rsquared, xlim = c(55,95), 
              xlab = "Temperature (deg F)", cex = 2, ylab = expression("R"^2* " (between Sensor and Regulatory Inst.)"))
smoothScatter(boxdata_reg$humidity, boxdata_reg$pm25_A_pm25_B_rsquared, xlim = c(15,85), 
              xlab = "Humidity (%)", cex = 2, ylab = expression("R"^2* " (between Sensor and Regulatory Inst.)"))

monthIDs <- month(boxdata_reg$datetime)
smoothScatter(monthIDs, boxdata_reg$pm25_A_pm25_B_rsquared, xlab = "Month (#)", cex = 2,
              ylab = expression("R"^2* " (between Sensor and Regulatory Inst.)"))


# Figure 15a - 15f
par(mfrow=c(3,2), mar = c(5,5,2,2) + .1)
boxplot(boxdata_reg$slope ~ boxdata_reg$date, col = "lightskyblue1", ylab = "Slope", las = 2, names = temptime2)
boxplot(boxdata_reg$intercept ~ boxdata_reg$date, col = "lightskyblue1", ylab = "Intercept",  las = 2, names = temptime2)
boxplot(boxdata_reg$RMSE ~ boxdata_reg$date, col = "lightskyblue1",  ylab = expression("RMSE ("*mu* "g/m"^3* ")"),  las = 2, names = temptime2)
boxplot(boxdata_reg$MAE ~ boxdata_reg$date, col = "lightskyblue1",  ylab = expression("MAE ("*mu* "g/m"^3* ")"),  las = 2, names = temptime2)
boxplot(boxdata_reg$MBE ~ boxdata_reg$date, col = "lightskyblue1", ylab = expression("MBE ("*mu* "g/m"^3* ")"),  las = 2, names = temptime2)
boxplot(boxdata_reg$MMR ~ boxdata_reg$date, col = "lightskyblue1", ylab = "MBE/MAE",  las = 2, names = temptime2)

# Figure 16
par(mfrow=c(1,1), mar = c(5,5,3,3))
plot(sohList_reg[[14]][[1]]$MAE, sohList_reg[[14]][[1]]$MMR, xlim = c(0,25), ylim = c(-.05, 1.05),
     ylab = "MBE/MAE Ratio", xlab = expression("MAE ("*mu* "g/m"^3* ")"), col = alpha("dodgerblue2", .5), pch = 16)

for (i in 2:17) {
  points(sohList_reg[[14]][[i]]$MAE, sohList_reg[[14]][[i]]$MMR,col = alpha("dodgerblue2", .5), pch = 16)
}

###

# Create supplemental plots, examining sensor data WRT regualtory data 
# over runtime and cumulative exposure 

# For runtime

boxdata_runtime <- sohList_reg[[14]][[1]]; boxdata_runtime$senNum = 1; 
boxdata_runtime$roundRT <- round_any(boxdata_runtime$runtime, 50)
boxdata_runtime <- aggregate(boxdata_runtime, by = list(boxdata_runtime$roundRT), FUN = "mean")

for (i in 2:length(sohList_reg[[14]])) {
  if(is.null(sohList_reg[[14]][[i]]) == FALSE){
    temp <- sohList_reg[[14]][[i]]; temp$senNum = i
    temp$roundRT <- round_any(temp$runtime, 50); temp <- aggregate(temp, by = list(temp$roundRT), FUN = "mean")
    boxdata_runtime <- rbind(boxdata_runtime, temp)
  }}

boxdata_runtime$date <- as.Date(boxdata_runtime$date, format="%m-%Y")

# Create boxplots

# Figure S18
par(mfrow=c(1,1), mar = c(5,5,3,3))
boxplot(boxdata_runtime$Rsq ~ boxdata_runtime$roundRT, col = "lightskyblue1", ylim = c(0,1.1), 
        ylab = expression("R"^2* " (between Sensor and Regulatory Inst.)"), xlab = "Cumulative Runtime (days)")
nbGroup <- nlevels(factor(boxdata_runtime$roundRT))
text(x=c(1:nbGroup), y=1.05, cex = .85, paste(table(boxdata_runtime$roundRT),sep=""))

# Figure S19
par(mfrow=c(3,2), mar = c(4,4,2,2) + .1)
boxplot(boxdata_runtime$slope ~ boxdata_runtime$roundRT, col = "lightskyblue1", ylab = "Slope", xlab = "Cumulative Runtime (days)")
boxplot(boxdata_runtime$intercept ~ boxdata_runtime$roundRT, col = "lightskyblue1", ylab = "Intercept", xlab = "Cumulative Runtime (days)")
boxplot(boxdata_runtime$RMSE ~ boxdata_runtime$roundRT, col = "lightskyblue1",  ylab = expression("RMSE ("*mu* "g/m"^3* ")"), xlab = "Cumulative Runtime (days)")
boxplot(boxdata_runtime$MAE ~ boxdata_runtime$roundRT, col = "lightskyblue1",  ylab = expression("MAE ("*mu* "g/m"^3* ")"), xlab = "Cumulative Runtime (days)")
boxplot(boxdata_runtime$MBE ~ boxdata_runtime$roundRT, col = "lightskyblue1", ylab = expression("MBE ("*mu* "g/m"^3* ")"), xlab = "Cumulative Runtime (days)")
boxplot(boxdata_runtime$MMR ~ boxdata_runtime$roundRT, col = "lightskyblue1", ylab = "MBE/MAE", xlab = "Cumulative Runtime (days)")

# For cumulative exposure 

boxdata_cexp <- sohList_reg[[14]][[1]]; boxdata_cexp$senNum = 1; 
boxdata_cexp$roundCE <- round_any(boxdata_cexp$cmexp, 10000) + 10000
boxdata_cexp <- aggregate(boxdata_cexp, by = list(boxdata_cexp$roundCE), FUN = "median")

for (i in 2:length(sohList_reg[[14]])) {
  if(is.null(sohList_reg[[14]][[i]]) == FALSE){
    temp <- sohList_reg[[14]][[i]]; temp$senNum = i
    temp$roundCE <- round_any(temp$cmexp, 10000) + 10000; 
    temp <- aggregate(temp, by = list(temp$roundCE), FUN = "median")
    boxdata_cexp <- rbind(boxdata_cexp, temp)
  }}

# Figure S20
par(mfrow=c(1,1), mar = c(5,5,3,3))
boxplot(boxdata_cexp$Rsq ~ boxdata_cexp$roundCE, col = "lightskyblue1", ylim = c(0,1.1), 
        ylab = expression("R"^2* " (between Sensor and Regulatory Inst.)"),  xlab = expression("Cumulative Exposure ("*mu*"g)"))
nbGroup <- nlevels(factor(boxdata_cexp$roundCE))
text(x=c(1:nbGroup), y=1.05, cex = .85, paste(table(boxdata_cexp$roundCE),sep=""))

# Figure S21
par(mfrow=c(3,2), mar = c(4,4,2,2) + .1)
boxplot(boxdata_cexp$slope ~ boxdata_cexp$roundCE, col = "lightskyblue1", ylab = "Slope", xlab = expression("Cumulative Exposure ("*mu*"g)"))
boxplot(boxdata_cexp$intercept ~ boxdata_cexp$roundCE, col = "lightskyblue1", ylab = "Intercept", xlab = expression("Cumulative Exposure ("*mu*"g)"))
boxplot(boxdata_cexp$RMSE ~ boxdata_cexp$roundCE, col = "lightskyblue1",  ylab = expression("RMSE ("*mu* "g/m"^3* ")"), xlab = expression("Cumulative Exposure ("*mu*"g)"))
boxplot(boxdata_cexp$MAE ~ boxdata_cexp$roundCE, col = "lightskyblue1",  ylab = expression("MAE ("*mu* "g/m"^3* ")"), xlab = expression("Cumulative Exposure ("*mu*"g)"))
boxplot(boxdata_cexp$MBE ~ boxdata_cexp$roundCE, col = "lightskyblue1", ylab = expression("MBE ("*mu* "g/m"^3* ")"), xlab = expression("Cumulative Exposure ("*mu*"g)"))
boxplot(boxdata_cexp$MMR ~ boxdata_cexp$roundCE, col = "lightskyblue1", ylab = "MBE/MAE", xlab = expression("Cumulative Exposure ("*mu*"g)"))

###

# Excerpt of previous plots, inlcuded in manuscript 

# Figure 17 - x-axis format***
par(mfrow=c(2,2), mar = c(4,5,1,2) + .1)
boxplot(boxdata_cexp$Rsq ~ boxdata_cexp$roundCE, col = "lightskyblue1", ylab = expression("R"^2),  xlab = expression("Cumulative Exposure ("*mu*"g)"))
boxplot(boxdata_cexp$MAE ~ boxdata_cexp$roundCE, col = "lightskyblue1",  ylab = expression("MAE ("*mu*"g/m"^3* ")"),  xlab = expression("Cumulative Exposure ("*mu*"g)"))
boxplot(boxdata_cexp$MBE ~ boxdata_cexp$roundCE, col = "lightskyblue1", ylab = expression("MBE ("*mu*"g/m"^3* ")"),  xlab = expression("Cumulative Exposure ("*mu*"g)"))
boxplot(boxdata_cexp$MMR ~ boxdata_cexp$roundCE, col = "lightskyblue1", ylab = "MBE/MAE",  xlab = expression("Cumulative Exposure ("*mu*"g)"))

###

# Analysis of whether sensor drift was observed 
# (code selects reference data between 10 and 15 ug/m3 and corresponding sensor data 
# to assess drift over time) 

drift_data <- sohList_reg[[14]][[1]]; drift_data$senNum = 1

for (i in 2:length(sohList_reg[[14]])) {
  if(is.null(sohList_reg[[14]][[i]]) == FALSE){
    temp <- sohList_reg[[14]][[i]]; temp$senNum = i
    drift_data <- rbind(drift_data, temp)
  }}

drift_data <- subset(drift_data, reg_pm25 >= 10 & reg_pm25 <= 15)
drift_data$diff <- (drift_data$pm25 - drift_data$reg_pm25)

# Figure 18
par(mfrow=c(1,1), mar = c(5,5,2,2) + .1)
plot(drift_data$runtime, (drift_data$diff), ylim = c(-15, 35), ylab = expression("Difference between Sensor & Reg. Data ("*mu*"g/m"^3* ")"), xlab = "Cumulative Runtime (days)")
mdl <- lm((drift_data$diff) ~ drift_data$runtime)
abline(mdl, col = "blue", lwd = 2)
mdl$coefficients
legend("topright", legend = c("Aggregate Sensor Data", "Line of Best Fit (slope = -0.000192, intercept = 4.717)"), col = c("black", "blue"), pch = 20)

# Cumulative expsoure by group 

# group 1
boxdata_cexp_g1 <- sohList_reg[[14]][[12]]; boxdata_cexp_g1$senNum = 1; 
boxdata_cexp_g1$roundCE <- round_any(boxdata_cexp_g1$cmexp, 10000) + 10000
boxdata_cexp_g1 <- aggregate(boxdata_cexp_g1, by = list(boxdata_cexp_g1$roundCE), FUN = "median")

for (i in 13:17) {
  if(is.null(sohList_reg[[14]][[i]]) == FALSE){
    temp <- sohList_reg[[14]][[i]]; temp$senNum = i
    temp$roundCE <- round_any(temp$cmexp, 10000) + 10000; 
    temp <- aggregate(temp, by = list(temp$roundCE), FUN = "median")
    boxdata_cexp_g1 <- rbind(boxdata_cexp_g1, temp)
  }}

# group 2
boxdata_cexp_g2 <- sohList_reg[[14]][[2]]; boxdata_cexp_g2$senNum = 1; 
boxdata_cexp_g2$roundCE <- round_any(boxdata_cexp_g2$cmexp, 10000) + 10000
boxdata_cexp_g2 <- aggregate(boxdata_cexp_g2, by = list(boxdata_cexp_g2$roundCE), FUN = "median")

for (i in 3:5) {
  if(is.null(sohList_reg[[14]][[i]]) == FALSE){
    temp <- sohList_reg[[14]][[i]]; temp$senNum = i
    temp$roundCE <- round_any(temp$cmexp, 10000) + 10000; 
    temp <- aggregate(temp, by = list(temp$roundCE), FUN = "median")
    boxdata_cexp_g2 <- rbind(boxdata_cexp_g2, temp)
  }}

# group 3
boxdata_cexp_g3 <- sohList_reg[[14]][[7]]; boxdata_cexp_g3$senNum = 1; 
boxdata_cexp_g3$roundCE <- round_any(boxdata_cexp_g3$cmexp, 10000) + 10000
boxdata_cexp_g3 <- aggregate(boxdata_cexp_g3, by = list(boxdata_cexp_g3$roundCE), FUN = "median")

for (i in 8:9) {
  if(is.null(sohList_reg[[14]][[i]]) == FALSE){
    temp <- sohList_reg[[14]][[i]]; temp$senNum = i
    temp$roundCE <- round_any(temp$cmexp, 10000) + 10000; 
    temp <- aggregate(temp, by = list(temp$roundCE), FUN = "median")
    boxdata_cexp_g3 <- rbind(boxdata_cexp_g3, temp)
  }}

# Figure S22
par(mfrow=c(3,1), mar = c(4,5,1,1) + .1)
boxplot(boxdata_cexp_g1$MBE ~ boxdata_cexp_g1$roundCE, col = "lightskyblue1", ylab = expression("MBE ("*mu* "g/m"^3* ")"), xlab = expression("Cumulative Exposure ("*mu*"g) - Group 1, deployed Oct. 2017, n = 6"), ylim = c(-10,0))
boxplot(boxdata_cexp_g2$MBE ~ boxdata_cexp_g2$roundCE, col = "lightskyblue1", ylab = expression("MBE ("*mu* "g/m"^3* ")"), xlab = expression("Cumulative Exposure ("*mu*"g) - Group 2, deployed Dec. 2017, n = 4"), ylim = c(-10,0))
boxplot(boxdata_cexp_g3$MBE ~ boxdata_cexp_g3$roundCE, col = "lightskyblue1", ylab = expression("MBE ("*mu* "g/m"^3* ")"), xlab = expression("Cumulative Exposure ("*mu*"g) - Group 3, deployed April 2018, n = 3"), ylim = c(-10,0))

# Figure S23
par(mfrow=c(2,2), mar = c(3,5,2,2) + .1)

plot(sohList_reg[[14]][[10]]$datetime, sohList_reg[[14]][[10]]$reg_pm25, xlab = "", ylab = expression("PM"[2.5]* " ("*mu*"g/m"^3* ")"))
points(sohList_reg[[14]][[10]]$datetime, sohList_reg[[14]][[10]]$pm25, col = "blue")
legend("topleft", legend = c("Reg.", "Sens."), pch = 20, col = c("black", "blue"))

plot(sohList_reg[[14]][[11]]$datetime, sohList_reg[[14]][[11]]$reg_pm25, xlab = "", ylab = expression("PM"[2.5]* " ("*mu*"g/m"^3* ")"))
points(sohList_reg[[14]][[11]]$datetime, sohList_reg[[14]][[11]]$pm25, col = "blue")
legend("topleft", legend = c("Reg.", "Sens."), pch = 20, col = c("black", "blue"))

plot(sohList_reg[[14]][[15]]$datetime, sohList_reg[[14]][[15]]$reg_pm25, xlab = "", ylab = expression("PM"[2.5]* " ("*mu*"g/m"^3* ")"))
points(sohList_reg[[14]][[15]]$datetime, sohList_reg[[14]][[15]]$pm25, col = "blue")
legend("topleft", legend = c("Reg.", "Sens."), pch = 20, col = c("black", "blue"))

plot(sohList_reg[[14]][[16]]$datetime, sohList_reg[[14]][[16]]$reg_pm25, xlab = "", ylab = expression("PM"[2.5]* " ("*mu*"g/m"^3* ")"))
points(sohList_reg[[14]][[16]]$datetime, sohList_reg[[14]][[16]]$pm25, col = "blue")
legend("topleft", legend = c("Reg.", "Sens."), pch = 20, col = c("black", "blue"))

# The following examines the similarity in data from sensors co-located but 
# deployed at different times 

# The following assigns groups based on deployment data (visible as minimum datetime) 
# Note, sensors deployed within a week of each other are grouped and the two sensors 
# previously identified that experienced dramatic drift have been exlcuded 

sohList_reg[[14]][[1]]$grp = 2; sohList_reg[[14]][[2]]$grp = 3; sohList_reg[[14]][[3]]$grp = 3; 
sohList_reg[[14]][[4]]$grp = 3; sohList_reg[[14]][[5]]$grp = 3; sohList_reg[[14]][[6]]$grp = 4; 
sohList_reg[[14]][[7]]$grp = 5; sohList_reg[[14]][[8]]$grp = 5; sohList_reg[[14]][[9]]$grp = 5; 
sohList_reg[[14]][[10]]$grp = 0; sohList_reg[[14]][[11]]$grp = 0; sohList_reg[[14]][[12]]$grp = 1; 
sohList_reg[[14]][[13]]$grp = 1; sohList_reg[[14]][[14]]$grp = 1; sohList_reg[[14]][[15]]$grp = 1; 
sohList_reg[[14]][[16]]$grp = 1; sohList_reg[[14]][[17]]$grp = 1

# Calculate pairwise R2 and MAE values between sensors  

pairedStats <- data.frame(matrix(ncol = 6, nrow = 1000))
colnames(pairedStats) <- c("grp1", "grp2", "rsq", "mae", "s1", "s2") 
k = 0

for(i in 1:16){
  for(j in (i+1):17){
    dataset1 <- sohList_reg[[14]][[i]]; dataset2 <- sohList_reg[[14]][[j]]
    tempdata <- full_join(dataset1, dataset2, by = "datetime")
    tempdata <- select(tempdata, c("datetime", "pm25.x", "pm25.y"))
    tempdata <- na.exclude(tempdata)
    
    mdl <- lm(tempdata$pm25.x ~ tempdata$pm25.y)
    mae <- mae(tempdata$pm25.x, tempdata$pm25.y)
    rsq <- summary(mdl)$r.squared
    
    k <- k +1
    
    pairedStats$grp1[k] <- dataset1$grp[1]
    pairedStats$grp2[k] <- dataset2$grp[1]
    pairedStats$rsq[k] <- rsq
    pairedStats$mae[k] <- mae
    pairedStats$s1[k] <- communityList[[14]]$label[i]
    pairedStats$s2[k] <- communityList[[14]]$label[j]
    
  }
  
  print(paste("Sensor", i, "is complete"))
  
}

# Aggregate these pairwise results based on groupings 

# Table 4
grpStats <- data.frame(matrix(ncol = 5, nrow = 15))
colnames(grpStats) <- c("grp1", "grp2", "rsq", "mae", "n")
grpStats$grp1 <- c(1, 2, 3, 4, 5, 1, 1, 1, 1, 2, 2, 2, 3, 3, 4)
grpStats$grp2 <- c(1, 2, 3, 4, 5, 2, 3, 4, 5, 3, 4, 5, 4, 5, 5)

for(i in 1:15){
  x <- grpStats$grp1[i]; y <- grpStats$grp2[i]
  tempdata <- subset(pairedStats, grp1 == x & grp2 == y | grp1 == y & grp2 == x)
  grpStats$rsq[i] <- mean(tempdata$rsq)
  grpStats$mae[i] <- mean(tempdata$mae)
}

# Dataset for withing group comparisons 
dataset1 <- grpStats[1:5,]

# Dataset for between group comparisons 
dataset2 <- grpStats[6:15, ]

# T-test for R2 values
t.test(dataset1$rsq, dataset2$rsq, paired = FALSE)

# T-test for MAE values 
t.test(dataset1$mae, dataset2$mae, paired = FALSE)

# (the t-test results are in Table 5)

# Additonal columns of grouping assignments are deleted to ensure function of future code 
sohList_reg[[14]][[1]]$grp = NULL; sohList_reg[[14]][[2]]$grp = NULL; sohList_reg[[14]][[3]]$grp = NULL; 
sohList_reg[[14]][[4]]$grp = NULL; sohList_reg[[14]][[5]]$grp = NULL; sohList_reg[[14]][[6]]$grp = NULL; 
sohList_reg[[14]][[7]]$grp = NULL; sohList_reg[[14]][[8]]$grp = NULL; sohList_reg[[14]][[9]]$grp = NULL; 
sohList_reg[[14]][[10]]$grp = NULL; sohList_reg[[14]][[11]]$grp = NULL; sohList_reg[[14]][[12]]$grp = NULL; 
sohList_reg[[14]][[13]]$grp = NULL; sohList_reg[[14]][[14]]$grp = NULL; sohList_reg[[14]][[15]]$grp = NULL; 
sohList_reg[[14]][[16]]$grp = NULL; sohList_reg[[14]][[17]]$grp = NULL


# Plots of a subset of sensors with approximately 3 years of data 

par(mfrow=c(1,1), mar = c(5,5,2,2) + .1)

# Figure 19
plot(sohList_reg[[14]][[13]]$datetime, sohList_reg[[14]][[13]]$pm25, type = 'l', 
     col = alpha("olivedrab3",.75), lwd = 1.75, ylab = expression("PM"[2.5]*" 24-Hour Average ("*mu*"g/m"^3* ")"), 
     ylim = c(0,100), xlab = "Date")
lines(sohList_reg[[14]][[15]]$datetime, sohList_reg[[14]][[15]]$pm25, col = alpha("seagreen3",.75), lwd = 1.75)
lines(sohList_reg[[14]][[16]]$datetime, sohList_reg[[14]][[16]]$pm25, col = alpha("royalblue", .75), lwd = 1.75)
lines(sohList_reg[[14]][[13]]$datetime, sohList_reg[[14]][[13]]$reg_pm25, col = alpha("black", .75))
legend("top", legend = c("Sensor (2.9 yrs)","Sensor (3.0 yrs)","Sensor (2.9 yrs)","Regualtory Inst."), 
       col = c("olivedrab3", "seagreen3", "royalblue", "black"), pch = 20)

# Figure 20a
plot(sohList_reg[[14]][[13]]$datetime, sohList_reg[[14]][[13]]$Rsq, pch = 16, 
     col = alpha("olivedrab3",.3), ylab = expression("R"^2*" Between Sensor and Reg. Data"), ylim = c(0,1), xlab = "Date")
mdl <- lm((sohList_reg[[14]][[13]]$Rsq) ~ sohList_reg[[14]][[13]]$datetime); abline(mdl, col = "olivedrab3", lwd = 1.5, lty = 2)
points(sohList_reg[[14]][[15]]$datetime, sohList_reg[[14]][[15]]$Rsq, pch = 16, col = alpha("seagreen3",.3))
mdl <- lm((sohList_reg[[14]][[15]]$Rsq) ~ sohList_reg[[14]][[15]]$datetime); abline(mdl, col = "seagreen3", lwd = 1.5, lty = 2)
points(sohList_reg[[14]][[16]]$datetime, sohList_reg[[14]][[16]]$Rsq, pch = 16, col = alpha("royalblue", .3))
mdl <- lm((sohList_reg[[14]][[16]]$Rsq) ~ sohList_reg[[14]][[16]]$datetime); abline(mdl, col = "royalblue", lwd = 1.5, lty = 2)
legend("bottomleft", legend = c("Sensor (2.9 yrs)","Sensor (3.0 yrs)","Sensor (2.9 yrs)"), 
       col = c("olivedrab3", "seagreen3", "royalblue"), pch = 20)

# Figure 20b
plot(sohList_reg[[14]][[13]]$datetime, sohList_reg[[14]][[13]]$MAE, pch = 16, 
     col = alpha("olivedrab3",.3), ylab = expression("Mean Absolute Error ("*mu*"g/m"^3*")"), ylim = c(2, 16), xlab = "Date")
mdl <- lm((sohList_reg[[14]][[13]]$MAE) ~ sohList_reg[[14]][[13]]$datetime); abline(mdl, col = "olivedrab3", lwd = 1.5, lty = 2)
points(sohList_reg[[14]][[15]]$datetime, sohList_reg[[14]][[15]]$MAE, pch = 16, col = alpha("seagreen3",.3))
mdl <- lm((sohList_reg[[14]][[15]]$MAE) ~ sohList_reg[[14]][[15]]$datetime); abline(mdl, col = "seagreen3", lwd = 1.5, lty = 2)
points(sohList_reg[[14]][[16]]$datetime, sohList_reg[[14]][[16]]$MAE, pch = 16, col = alpha("royalblue", .3))
mdl <- lm((sohList_reg[[14]][[16]]$MAE) ~ sohList_reg[[14]][[16]]$datetime); abline(mdl, col = "royalblue", lwd = 1.5, lty = 2)
legend("topleft", legend = c("Sensor (2.9 yrs)","Sensor (3.0 yrs)","Sensor (2.9 yrs)"), 
       col = c("olivedrab3", "seagreen3", "royalblue"), pch = 20)



### PART 6 ###########################################################################################

### Results and plots for Section 3.4 

# Calculate pairwise statistics for all sensors within each community to get an understanding 
# of the variability

pairedStats2 <- data.frame(matrix(ncol = 6, nrow = 5000))
colnames(pairedStats2) <- c("comm", "s1", "s2", "rsq", "mae", "dist") 

z = 0

for(k in 1:14){
  
  for(i in 1:(length(sohList_reg[[k]])-1)){
    for(j in (i+1):length(sohList_reg[[k]])){
      
      dataset1 <- NA; dataset2 <- NA
      dataset1 <- sohList_reg[[k]][[i]]; dataset2 <- sohList_reg[[k]][[j]]
      
      if(length(dataset1$datetime) >= 30 & length(dataset2$datetime) >= 30) {
        
        tempdata <- full_join(dataset1, dataset2, by = "datetime")
        tempdata <- select(tempdata, c("datetime", "pm25.x", "pm25.y"))
        tempdata <- na.exclude(tempdata)
        
      }
      
      if(length(tempdata$datetime) >= 30) {
        
        mdl <- lm(tempdata$pm25.x ~ tempdata$pm25.y)
        mae <- mae(tempdata$pm25.x, tempdata$pm25.y)
        rsq <- summary(mdl)$r.squared
        
        z <- z +1
        
        long1 <- communityList[[k]]$longitude[i]; lat1 <- communityList[[k]]$latitude[i]
        long2 <- communityList[[k]]$longitude[j]; lat2 <- communityList[[k]]$latitude[j]
        
        pairedStats2$comm[z] <- k
        pairedStats2$s1[z] <- communityList[[k]]$label[i]
        pairedStats2$s2[z] <- communityList[[k]]$label[j]
        pairedStats2$rsq[z] <- rsq
        pairedStats2$mae[z] <- mae
        pairedStats2$dist[z] <- distm(c(long1, lat1), c(long2, lat2))
        
      }
    }
    
  }
  
  print(paste("Community", k, "is complete"))
  
}

data <- data.frame(matrix(ncol = 4, nrow = 14))
colnames(data) <- c("comm", "rsq", "mae", "dist") 

# Aggregate individual sensor pair results to community level
for(i in 1:14){
  temp <- subset(pairedStats2, comm == i)
  data$comm[i] = i
  data$rsq[i] = mean(temp$rsq, na.exclude = TRUE)
  data$mae[i] = mean(temp$mae, na.exclude = TRUE)
  data$dist[i] = mean(temp$dist, na.exclude = TRUE)
}


# Organize and format the data in order to create boxplots

boxdata_reg2 <- sohList_reg[[1]][[1]]; boxdata_reg2$Comm = 1; boxdata_reg2$date <- boxdata_reg2$datetime
boxdata_reg2 <- timeAverage(boxdata_reg2, avg.time = "month")

for (i in 2:length(sohList_reg[[1]])) {
  if(is.null(sohList_reg[[1]][[i]]) == FALSE){
    temp <- sohList_reg[[1]][[i]]; temp$Comm = 1; temp$date <- temp$datetime
    temp <- timeAverage(temp, avg.time = "month")
    boxdata_reg2 <- rbind(boxdata_reg2, temp)
  }}

for(i in 2:14){
  for (j in 1:length(sohList_reg[[i]])) {
    if(is.null(sohList_reg[[i]][[j]]) == FALSE){
      temp <- sohList_reg[[i]][[j]]; temp$Comm = i; temp$date <- temp$datetime
      temp <- timeAverage(temp, avg.time = "month")
      boxdata_reg2 <- rbind(boxdata_reg2, temp)
    }}
}


# Figure 21a, for May 2018

data_month <- subset(boxdata_reg2, datetime >= "2018-05-01" & datetime < "2018-05-31")
data_month$date <- as.Date(data_month$date, format="%m-%Y")
data_month <- data_month %>% select("Comm", "Rsq"); data_month <- na.exclude(data_month)

data_month$Dist <- NA
for(i in 1:length(data_month$Comm)){
  x = data_month$Comm[i]
  data_month$Dist[i] = data$dist[x]
}

nbGroup <- nlevels(factor(data_month$Comm)); pal <- rev(brewer.pal(9, "Blues")); pal <- colorRampPalette(pal)
palData <- classIntervals(data_month$Dist, n = 14, style="equal") 
data_month$colors <- findColours(palData, pal(100)); tempcol <- unique(data_month$colors)

par(mfrow=c(1,1), mar = c(5,5,3,3))
p <- boxplot(data_month$Rsq ~ data_month$Comm, ylim = c(0,1.1), add = FALSE, 
             col = tempcol, xlab = "Community", ylab = expression("R"^2*" with Nearest Regulatory Site"), 
             names = c( "C","D","E","F","G","H","I","J", "L","M","Z"))
text(x=c(1:nbGroup), y=1.05, cex = .85, paste(table(data_month$Comm),sep=""))
title("May 2018")


# Figure 21b, for December 2018

data_month <- subset(boxdata_reg2, datetime >= "2018-12-01" & datetime < "2018-12-31")
data_month$date <- as.Date(data_month$date, format="%m-%Y")
data_month <- data_month %>% select("Comm", "Rsq"); data_month <- na.exclude(data_month)

data_month$Dist <- NA
for(i in 1:length(data_month$Comm)){
  x = data_month$Comm[i]
  data_month$Dist[i] = data$dist[x]
}

nbGroup <- nlevels(factor(data_month$Comm)); pal <- rev(brewer.pal(9, "Blues")); pal <- colorRampPalette(pal)
palData <- classIntervals(data_month$Dist, n = 14, style="equal") 
data_month$colors <- findColours(palData, pal(100)); tempcol <- unique(data_month$colors)

par(mfrow=c(1,1), mar = c(5,5,3,3))
p <- boxplot(data_month$Rsq ~ data_month$Comm, ylim = c(0,1.1), add = FALSE, 
             col = tempcol, xlab = "Community", ylab = expression("R"^2*" with Nearest Regulatory Site"), 
             names = c( "B", "C","D","E","F","G","H","I","J", "K", "L","M","Z"))
text(x=c(1:nbGroup), y=1.05, cex = .85, paste(table(data_month$Comm),sep=""))
title("December 2018")

# Figure 21c, for May 2019

data_month <- subset(boxdata_reg2, datetime >= "2019-05-01" & datetime < "2019-05-31")
data_month$date <- as.Date(data_month$date, format="%m-%Y")
data_month <- data_month %>% select("Comm", "Rsq"); data_month <- na.exclude(data_month)

data_month$Dist <- NA
for(i in 1:length(data_month$Comm)){
  x = data_month$Comm[i]
  data_month$Dist[i] = data$dist[x]
}

nbGroup <- nlevels(factor(data_month$Comm)); pal <- rev(brewer.pal(9, "Blues")); pal <- colorRampPalette(pal)
palData <- classIntervals(data_month$Dist, n = 14, style="equal") 
data_month$colors <- findColours(palData, pal(100)); tempcol <- unique(data_month$colors)

par(mfrow=c(1,1), mar = c(5,5,3,3))
p <- boxplot(data_month$Rsq ~ data_month$Comm, ylim = c(0,1.1), add = FALSE, 
             col = tempcol, xlab = "Community", ylab = expression("R"^2*" with Nearest Regulatory Site"), 
             names = c( "A", "B", "C","D","E","F","G","H","I","J", "K", "L","M","Z"))
text(x=c(1:nbGroup), y=1.05, cex = .85, paste(table(data_month$Comm),sep=""))
title("May 2019")

# Figure 21d, for May 2019

data_month <- subset(boxdata_reg2, datetime >= "2019-12-01" & datetime < "2019-12-31")
data_month$date <- as.Date(data_month$date, format="%m-%Y")
data_month <- data_month %>% select("Comm", "Rsq"); data_month <- na.exclude(data_month)

data_month$Dist <- NA
for(i in 1:length(data_month$Comm)){
  x = data_month$Comm[i]
  data_month$Dist[i] = data$dist[x]
}

nbGroup <- nlevels(factor(data_month$Comm)); pal <- rev(brewer.pal(9, "Blues")); pal <- colorRampPalette(pal)
palData <- classIntervals(data_month$Dist, n = 14, style="equal") 
data_month$colors <- findColours(palData, pal(100)); tempcol <- unique(data_month$colors)

par(mfrow=c(1,1), mar = c(5,5,3,3))
p <- boxplot(data_month$Rsq ~ data_month$Comm, ylim = c(0,1.1), add = FALSE, 
             col = tempcol, xlab = "Community", ylab = expression("R"^2*" with Nearest Regulatory Site"), 
             names = c( "A", "B", "C","D","E","F","G","H","I","J", "K", "L","M","Z"))
text(x=c(1:nbGroup), y=1.05, cex = .85, paste(table(data_month$Comm),sep=""))
title("December 2019")


# Figure S24, for all months 

data_month <- boxdata_reg2
data_month$date <- as.Date(data_month$date, format="%m-%Y")
data_month <- data_month %>% select("Comm", "Rsq"); data_month <- na.exclude(data_month)

data_month$Dist <- NA
for(i in 1:length(data_month$Comm)){
  x = data_month$Comm[i]
  data_month$Dist[i] = data$dist[x]
}

nbGroup <- nlevels(factor(data_month$Comm)); pal <- rev(brewer.pal(9, "Blues")); pal <- colorRampPalette(pal)
palData <- classIntervals(data_month$Dist, n = 14, style="equal") 
data_month$colors <- findColours(palData, pal(100)); tempcol <- unique(data_month$colors)

par(mfrow=c(1,1), mar = c(5,5,3,3))
p <- boxplot(data_month$Rsq ~ data_month$Comm, ylim = c(0,1.02), add = FALSE, 
             col = tempcol, xlab = "Community", ylab = expression("R"^2*" with Nearest Regulatory Site"), 
             names = c( "A", "B", "C","D","E","F","G","H","I","J", "K", "L","M","Z"))
title("All Months")


# Comparing pairwise statistics to average distance between sensors 

par(mfrow=c(1,2), mar = c(5,5,3,3))

# Figure 22a
plot(data$dist, data$rsq, xlab = "Distance (m)", ylab = expression("Average R"^2*" between Sensor Pairs"),
     pch = 20, col = "skyblue", cex = 3, ylim = c(0,1))
points(data$dist[14], data$rsq[14], pch = 20, col = "navyblue", cex = 3)
legend("bottomright", legend = c("Co-located Comm.", "Other Comm."), col = c("navyblue", "skyblue"), pch = 20)

# Figure 22b
plot(data$dist, data$mae, xlab = "Distance (m)", ylab = expression("Average MAE between Sensor Pairs ("*mu*"g/m"^3*")"),
     pch = 20, col = "skyblue", cex = 3, ylim = c(0,10))
points(data$dist[14], data$mae[14], pch = 20, col = "navyblue", cex = 3)
legend("topright", legend = c("Co-located Comm.", "Other Comm."), col = c("navyblue", "skyblue"), pch = 20)



### END ###########################################################################################

