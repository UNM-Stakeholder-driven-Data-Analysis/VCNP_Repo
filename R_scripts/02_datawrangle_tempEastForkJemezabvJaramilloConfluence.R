#### read me ####
# the purpose of this script is to import and clean temperature records from HOBO sensors.
#### load libraries ####
library(lubridate)
library(tidyverse)
library(tsibble)
library(dplyr)

#### load data ####

# import all data from a site folder

# this function imports all the file names for files in a specific site folder
# change "path" argument for each site

LJWB_file_list = list.files(path="C:/Users/Brionna/OneDrive - University of New Mexico/Classes/EPS545_BIO502/VCNP/VCNP_Repo/raw data/Valles_Flume_CSVs_Corrected/WFDCG/East Fork Jemez abv Jaramillo Confluence/", 
                            recursive=F, 
                            full.names=TRUE)
# this imports files into an R list of dataframes (review intro to R tutorial if you don't understand R lists!)
LJWB_HOBO_list = lapply(LJWB_file_list, 
                        read.csv, 
                        stringsAsFactors=F, 
                        header=T, 
                        skip=1, row.names=1)

#### examine data ####

# this is to look at each dataframe and make sure they are structured the same way. 
# all dataframes should be structured like so:
# col 1 = date and time (am/pm)
# col 2 = pressure
# col 3 = temp
# col 4 = battery
# col 5-8 = HOBO notes

# View(LJWB_HOBO_list[[1]]) #need to split date/time
# View(LJWB_HOBO_list[[2]]) #need to split date/time every entry except 12-13 #also note tz 7.00
# View(LJWB_HOBO_list[[3]]) 
# View(LJWB_HOBO_list[[4]]) 
# View(LJWB_HOBO_list[[5]])
# View(LJWB_HOBO_list[[6]])
# View(LJWB_HOBO_list[[7]]) # note tz 7.00
# View(LJWB_HOBO_list[[8]])
# View(LJWB_HOBO_list[[9]])
# View(LJWB_HOBO_list[[10]])
# View(LJWB_HOBO_list[[11]])
# View(LJWB_HOBO_list[[12]])
# View(LJWB_HOBO_list[[13]])

# These look like they are structured the same. Except list 1 and 2 need first two columns split

LJWB_HOBO_list[[1]]$Date.Time..GMT.06.00.1 <- format(strptime(LJWB_HOBO_list[[1]]$Date.Time..GMT.06.00, format="%m/%d/%y %I:%M:%S %p"), format="%H:%M:%S")
LJWB_HOBO_list[[1]]$Date.Time..GMT.06.00 <- as.Date(LJWB_HOBO_list[[1]]$Date.Time..GMT.06.00, format="%m/%d/%y")
LJWB_HOBO_list[[1]] <- LJWB_HOBO_list[[1]] %>% relocate(Date.Time..GMT.06.00.1, .after=Date.Time..GMT.06.00)

LJWB_HOBO_list[[2]]$Date.Time..GMT.07.00.1 <- format(strptime(LJWB_HOBO_list[[2]]$Date.Time..GMT.07.00, format="%m/%d/%y %I:%M:%S %p"), format="%H:%M:%S")
LJWB_HOBO_list[[2]]$Date.Time..GMT.07.00 <- as.Date(LJWB_HOBO_list[[2]]$Date.Time..GMT.07.00, format="%m/%d/%y")
LJWB_HOBO_list[[2]] <- LJWB_HOBO_list[[2]] %>% relocate(Date.Time..GMT.07.00.1, .after=Date.Time..GMT.07.00)

LJWB_HOBO_list[[3]]$Date.Time..GMT.06.00.1 <- format(strptime(LJWB_HOBO_list[[3]]$Date.Time..GMT.06.00, format="%m/%d/%y %I:%M:%S %p"), format="%H:%M:%S")
LJWB_HOBO_list[[3]]$Date.Time..GMT.06.00 <- as.Date(LJWB_HOBO_list[[3]]$Date.Time..GMT.06.00, format="%m/%d/%y")
LJWB_HOBO_list[[3]] <- LJWB_HOBO_list[[3]] %>% relocate(Date.Time..GMT.06.00.1, .after=Date.Time..GMT.06.00)

LJWB_HOBO_list[[4]]$Date.Time..GMT.06.00.1 <- format(strptime(LJWB_HOBO_list[[4]]$Date.Time..GMT.06.00, format="%m/%d/%y %I:%M:%S %p"), format="%H:%M:%S")
LJWB_HOBO_list[[4]]$Date.Time..GMT.06.00 <- as.Date(LJWB_HOBO_list[[4]]$Date.Time..GMT.06.00, format="%m/%d/%y")
LJWB_HOBO_list[[4]] <- LJWB_HOBO_list[[4]] %>% relocate(Date.Time..GMT.06.00.1, .after=Date.Time..GMT.06.00)

LJWB_HOBO_list[[5]]$Date.Time..GMT.06.00.1 <- format(strptime(LJWB_HOBO_list[[5]]$Date.Time..GMT.06.00, format="%m/%d/%y %I:%M:%S %p"), format="%H:%M:%S")
LJWB_HOBO_list[[5]]$Date.Time..GMT.06.00 <- as.Date(LJWB_HOBO_list[[5]]$Date.Time..GMT.06.00, format="%m/%d/%y")
LJWB_HOBO_list[[5]] <- LJWB_HOBO_list[[5]] %>% relocate(Date.Time..GMT.06.00.1, .after=Date.Time..GMT.06.00)

LJWB_HOBO_list[[6]]$Date.Time..GMT.06.00.1 <- format(strptime(LJWB_HOBO_list[[6]]$Date.Time..GMT.06.00, format="%m/%d/%y %I:%M:%S %p"), format="%H:%M:%S")
LJWB_HOBO_list[[6]]$Date.Time..GMT.06.00 <- as.Date(LJWB_HOBO_list[[6]]$Date.Time..GMT.06.00, format="%m/%d/%y")
LJWB_HOBO_list[[6]] <- LJWB_HOBO_list[[1]] %>% relocate(Date.Time..GMT.06.00.1, .after=Date.Time..GMT.06.00)

LJWB_HOBO_list[[7]]$Date.Time..GMT.07.00.1 <- format(strptime(LJWB_HOBO_list[[7]]$Date.Time..GMT.07.00, format="%m/%d/%y %I:%M:%S %p"), format="%H:%M:%S")
LJWB_HOBO_list[[7]]$Date.Time..GMT.07.00 <- as.Date(LJWB_HOBO_list[[7]]$Date.Time..GMT.07.00, format="%m/%d/%y")
LJWB_HOBO_list[[7]] <- LJWB_HOBO_list[[7]] %>% relocate(Date.Time..GMT.07.00.1, .after=Date.Time..GMT.07.00)

LJWB_HOBO_list[[8]]$Date.Time..GMT.06.00.1 <- format(strptime(LJWB_HOBO_list[[8]]$Date.Time..GMT.06.00, format="%m/%d/%y %I:%M:%S %p"), format="%H:%M:%S")
LJWB_HOBO_list[[8]]$Date.Time..GMT.06.00 <- as.Date(LJWB_HOBO_list[[8]]$Date.Time..GMT.06.00, format="%m/%d/%y")
LJWB_HOBO_list[[8]] <- LJWB_HOBO_list[[8]] %>% relocate(Date.Time..GMT.06.00.1, .after=Date.Time..GMT.06.00)

LJWB_HOBO_list[[9]]$Date.Time..GMT.06.00.1 <- format(strptime(LJWB_HOBO_list[[9]]$Date.Time..GMT.06.00, format="%m/%d/%y %I:%M:%S %p"), format="%H:%M:%S")
LJWB_HOBO_list[[9]]$Date.Time..GMT.06.00 <- as.Date(LJWB_HOBO_list[[9]]$Date.Time..GMT.06.00, format="%m/%d/%y")
LJWB_HOBO_list[[9]] <- LJWB_HOBO_list[[9]] %>% relocate(Date.Time..GMT.06.00.1, .after=Date.Time..GMT.06.00)

LJWB_HOBO_list[[10]]$Date.Time..GMT.06.00.1 <- format(strptime(LJWB_HOBO_list[[10]]$Date.Time..GMT.06.00, format="%m/%d/%y %I:%M:%S %p"), format="%H:%M:%S")
LJWB_HOBO_list[[10]]$Date.Time..GMT.06.00 <- as.Date(LJWB_HOBO_list[[10]]$Date.Time..GMT.06.00, format="%m/%d/%y")
LJWB_HOBO_list[[10]] <- LJWB_HOBO_list[[10]] %>% relocate(Date.Time..GMT.06.00.1, .after=Date.Time..GMT.06.00)

LJWB_HOBO_list[[11]]$Date.Time..GMT.06.00.1 <- format(strptime(LJWB_HOBO_list[[11]]$Date.Time..GMT.06.00, format="%m/%d/%y %I:%M:%S %p"), format="%H:%M:%S")
LJWB_HOBO_list[[11]]$Date.Time..GMT.06.00 <- as.Date(LJWB_HOBO_list[[11]]$Date.Time..GMT.06.00, format="%m/%d/%y")
LJWB_HOBO_list[[11]] <- LJWB_HOBO_list[[11]] %>% relocate(Date.Time..GMT.06.00.1, .after=Date.Time..GMT.06.00)


# First, I select only the columns that I'm interested in. 
# this first function selects only columns with "Date", "Time", or "Temp" in the column name
LJWB_HOBO_list_2 = lapply(LJWB_HOBO_list, 
                          dplyr::select,
                          (contains("Date") | contains("Time") | contains("Temp")))
View(LJWB_HOBO_list_2[[3]])

#### format date ####
for(i in 1:length(LJWB_HOBO_list_2)){
  LJWB_HOBO_list_2[[i]]$date =  as.Date(LJWB_HOBO_list_2[[i]][,1], format = "%m/%d/%Y")
}


#### format time and correct time zones ####
# note that some of the time column names say GMT.06.00 and some say GMT.07.00. This indicates that the loggers were set to different time zones at different times. If the data was recorded between March and Nov (exact date varies by year), GMT-6 is the Mountain time zone with the daylight savings time offset included. If it is not between March and Nov, it should be GMT-7. . The code below checks these time zones and corrects the times where needed. 

# this extracts the time zone from the header into a new column and checks to make sure that the time zone is correct 
for(i in 1:length(LJWB_HOBO_list_2)){
  LJWB_HOBO_list_2[[i]]$tz = sub("Date.Time..", "", names(LJWB_HOBO_list_2[[i]])[1])
  LJWB_HOBO_list_2[[i]]$datetime_unk = as.POSIXct(paste(LJWB_HOBO_list_2[[i]]$date, LJWB_HOBO_list_2[[i]][,2]), 
                                                  format="%Y-%m-%d %H:%M:%S",
                                                  tz="America/Denver")
  LJWB_HOBO_list_2[[i]]$dst = dst(LJWB_HOBO_list_2[[i]]$datetime_unk)
  LJWB_HOBO_list_2[[i]]$tz_flag = ifelse(LJWB_HOBO_list_2[[i]]$dst=="FALSE" &
                                           LJWB_HOBO_list_2[[i]]$tz=="GMT.06.00", "check tz!", "OK")
}

# this returns the sum of rows with incorrect time zones. If there is anything but zero in here, there is an issue with time zones that needs to be corrected before proceeding!!
for(i in 1:length(LJWB_HOBO_list_2)){
  z = length(LJWB_HOBO_list_2[[i]]$tz_flag[LJWB_HOBO_list_2[[i]]$tz_flag=="check tz!"])
  print(z)
}
View(LJWB_HOBO_list_2[[1]])
# there are 3 dataframes with incorrect time zones. Here, I add a column to label the appropriate time zone for each observation
for(i in 1:length(LJWB_HOBO_list_2)){
  LJWB_HOBO_list_2[[i]]$tz_corrected = ifelse(LJWB_HOBO_list_2[[i]]$tz_flag=="OK",LJWB_HOBO_list_2[[i]]$tz,"GMT.07.00")
}

# now I'm ready to format date/time and correct for the incorrect time zone
for(i in 1:length(LJWB_HOBO_list_2)){
  LJWB_HOBO_list_2[[i]]$datetime_NM = if_else(LJWB_HOBO_list_2[[i]]$tz_flag=="OK",
                                              LJWB_HOBO_list_2[[i]]$datetime_unk,
                                              LJWB_HOBO_list_2[[i]]$datetime_unk - lubridate::hours(1)
  )
}

# #check ranges and appearance of NAs in date/time for each df
# summary(LJWB_HOBO_list_2[[1]]$datetime_NM)
# summary(LJWB_HOBO_list_2[[2]]$datetime_NM)
# summary(LJWB_HOBO_list_2[[3]]$datetime_NM)
# summary(LJWB_HOBO_list_2[[4]]$datetime_NM)
# summary(LJWB_HOBO_list_2[[5]]$datetime_NM)
# summary(LJWB_HOBO_list_2[[6]]$datetime_NM)
# summary(LJWB_HOBO_list_2[[7]]$datetime_NM)
# summary(LJWB_HOBO_list_2[[8]]$datetime_NM)
# summary(LJWB_HOBO_list_2[[9]]$datetime_NM)
# summary(LJWB_HOBO_list_2[[10]]$datetime_NM)
# summary(LJWB_HOBO_list_2[[11]]$datetime_NM)
# summary(LJWB_HOBO_list_2[[12]]$datetime_NM)
# summary(LJWB_HOBO_list_2[[13]]$datetime_NM)


# note that accounting for time zones when the HOBOs weren't calibrated for changing time zones causes doubling of dates on the fall-side of daylight savings, and NA dates on the spring-side. We will correct for this by removing NA dates and averaging duplicates once we have one dataframe

#### combine dataframes to one ####

# select just newly formatted date/time column and temp column
LJWB_HOBO_list_3 = lapply(LJWB_HOBO_list_2, 
                          dplyr::select,
                          ( matches("datetime_NM") | contains("Temp")))

# rename temp column
LJWB_HOBO_list_3 = lapply(LJWB_HOBO_list_3, setNames, nm = c("datetime_NM", "temp_C"))

# bind rows together
LJWB_HOBO = do.call("rbind",LJWB_HOBO_list_3)

#### make date/time stamps regular ####

# check range of date/time
summary(LJWB_HOBO$datetime_NM)

# round date/times to nearest hour
# this is necessary because data was collected on variable intervals and not always right on the 00 second interval
LJWB_HOBO_hrly = 
  LJWB_HOBO %>%
  mutate(datetime_NM = lubridate::round_date(datetime_NM, "1 hour")) %>%
  group_by(datetime_NM) %>%
  summarise(temp_C = mean(temp_C, na.rm = T))

# remove rows with no date/time
LJWB_HOBO_hrly = LJWB_HOBO_hrly[!is.na(LJWB_HOBO_hrly$datetime_NM),]

# fill in date/times with missing data
LJWB_HOBO_hrly_ts =
  LJWB_HOBO_hrly %>% 
  complete(datetime_NM = seq(min(datetime_NM, na.rm = T), max(datetime_NM, na.rm = T), by = "hour"), 
           fill = list(value = NA)) %>%
  as_tsibble(index = datetime_NM)

#### plot time series ####

# faceted by year, day, month:
LJWB_HOBO_hrly_ts$yr = lubridate::year(LJWB_HOBO_hrly_ts$datetime_NM)
LJWB_HOBO_hrly_ts$day = lubridate::yday(LJWB_HOBO_hrly_ts$datetime_NM)
LJWB_HOBO_hrly_ts$mo = lubridate::month(LJWB_HOBO_hrly_ts$datetime_NM)

# add season (i'm estimating on season here - could be more accutate using dates)
LJWB_HOBO_hrly_ts$season = ifelse(LJWB_HOBO_hrly_ts$mo %in% c(11,12,1,2,3) , "Winter", "Summer")

#export as csv
write.csv(LJWB_HOBO_hrly_ts, "C:/Users/Brionna/OneDrive - University of New Mexico/Classes/EPS545_BIO502/VCNP/VCNP_Repo/processed data/EastForkJemezabvJaramilloConfluence_temp.csv")

# plot 
ggplot(data=LJWB_HOBO_hrly_ts, aes(x=day, y=temp_C))+
  geom_point() + geom_path()+
  facet_wrap(~yr, scales="free_y")+
  theme(legend.title = element_blank()) +
  theme_bw()

# across years:
ggplot(data=LJWB_HOBO_hrly_ts, aes(x=datetime_NM, y=temp_C))+
  geom_point() + geom_path()+
  theme(legend.title = element_blank()) +
  theme_bw()

# across years, colored by seaspm:

# plot
ggplot(data=LJWB_HOBO_hrly_ts, aes(x=datetime_NM, y=temp_C, color=season))+
  geom_point() + 
  theme(legend.title = element_blank()) +
  theme_bw()
