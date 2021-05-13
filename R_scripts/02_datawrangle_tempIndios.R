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

LJWB_file_list = list.files(path="C:/Users/Brionna/OneDrive - University of New Mexico/Classes/EPS545_BIO502/VCNP/VCNP_Repo/raw data/Valles_Flume_CSVs_Corrected/WFDCG/Indios/", 
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
# View(LJWB_HOBO_list[[1]]) #need to fix am/pm and split for ALL files
# View(LJWB_HOBO_list[[2]]) 
# View(LJWB_HOBO_list[[3]])
# View(LJWB_HOBO_list[[4]])
# View(LJWB_HOBO_list[[5]])
# View(LJWB_HOBO_list[[6]])
# View(LJWB_HOBO_list[[7]])
# View(LJWB_HOBO_list[[8]])
# View(LJWB_HOBO_list[[9]])
# View(LJWB_HOBO_list[[10]])
# View(LJWB_HOBO_list[[11]])
# View(LJWB_HOBO_list[[12]])
# View(LJWB_HOBO_list[[13]])
# View(LJWB_HOBO_list[[14]])
# View(LJWB_HOBO_list[[15]])
# View(LJWB_HOBO_list[[16]])
# View(LJWB_HOBO_list[[17]])
# View(LJWB_HOBO_list[[18]])
# View(LJWB_HOBO_list[[19]])

# These look like they are structured the same. 
# If they are not structured the same, you'll want to edit the offending dataframes. Let me know if you run into this. 

##12 hr time for all lists
for(i in 1:length(LJWB_HOBO_list)){
 LJWB_HOBO_list[[i]]$dummy <- LJWB_HOBO_list[[i]][,1]
   LJWB_HOBO_list[[i]][,1] <- format(strptime(LJWB_HOBO_list[[i]][,1], format="%m/%d/%y %I:%M:%S %p"), format="%m/%d/%y %H:%M:%S")
  

  LJWB_HOBO_list[[i]]$time <- format(strptime(LJWB_HOBO_list[[i]]$dummy, format="%m/%d/%y %I:%M:%S %p"), format="%H:%M:%S")
  
  LJWB_HOBO_list[[i]][,1] <- as.Date(LJWB_HOBO_list[[i]][,1], format="%m/%d/%y")
  
  LJWB_HOBO_list[[i]] <- LJWB_HOBO_list[[i]] %>% relocate(time, .after=1)
  }


# once I'm sure they're all structured the same, I can do some manipulations to them while they are still in the list form. This is more efficient than manipulating them one by one
# note that I'm making a new list when I do this so that the orininal dataframes remain unchanged and I can go back to them if I need to?strptime


# First, I select only the columns that I'm interested in. 
# this first function selects only columns with "Date", "Time", or "Temp" in the column name
LJWB_HOBO_list_2 = lapply(LJWB_HOBO_list, 
                          dplyr::select,
                          (contains("Date") | contains("Time") | contains("Temp")))
View(LJWB_HOBO_list_2[[19]])

#### format date ####

# Second, I will format the data column

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
                                           LJWB_HOBO_list_2[[i]]$tz=="GMT.06.00" |LJWB_HOBO_list_2[[i]]$dst=="TRUE" &
                                           LJWB_HOBO_list_2[[i]]$tz=="GMT.07.00", "check tz!", "OK")
}
# this returns the sum of rows with incorrect time zones. If there is anything but zero in here, there is an issue with time zones that needs to be corrected before proceeding!!
for(i in 1:length(LJWB_HOBO_list_2)){
  z = length(LJWB_HOBO_list_2[[i]]$tz_flag[LJWB_HOBO_list_2[[i]]$tz_flag=="check tz!"])
  print(z)
}
View(LJWB_HOBO_list_2[[8]])
# there are several dataframes with incorrect time zones.
# format date/time and correct for the incorrect time zone
for(i in 1:length(LJWB_HOBO_list_2)){
  LJWB_HOBO_list_2[[i]]$datetime_NM = if_else(LJWB_HOBO_list_2[[i]]$tz_flag=="OK",
                                              LJWB_HOBO_list_2[[i]]$datetime_unk,
                                              if_else(LJWB_HOBO_list_2[[i]]$tz_flag=="check tz!" & LJWB_HOBO_list_2[[i]]$tz=="GMT.06.00",
                                                      LJWB_HOBO_list_2[[i]]$datetime_unk - lubridate::hours(1),
                                                      LJWB_HOBO_list_2[[i]]$datetime_unk + lubridate::hours(1)))
}

#check ranges and appearance of NAs in date/time for each df
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
# summary(LJWB_HOBO_list_2[[14]]$datetime_NM)
# summary(LJWB_HOBO_list_2[[15]]$datetime_NM)
# summary(LJWB_HOBO_list_2[[16]]$datetime_NM)
# summary(LJWB_HOBO_list_2[[17]]$datetime_NM)
# summary(LJWB_HOBO_list_2[[18]]$datetime_NM)
# summary(LJWB_HOBO_list_2[[19]]$datetime_NM)

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

# faceted by year:
# add year and doy
LJWB_HOBO_hrly_ts$yr = lubridate::year(LJWB_HOBO_hrly_ts$datetime_NM)
LJWB_HOBO_hrly_ts$day = lubridate::yday(LJWB_HOBO_hrly_ts$datetime_NM)
LJWB_HOBO_hrly_ts$mo = lubridate::month(LJWB_HOBO_hrly_ts$datetime_NM)

# add season (i'm estimating on season here - could be more accutate using dates)
LJWB_HOBO_hrly_ts$season = ifelse(LJWB_HOBO_hrly_ts$mo %in% c(11,12,1,2,3) , "Winter", "Summer")

# zoom in on dates to see where transducer may have been frozen:
ggplot(data=LJWB_HOBO_hrly_ts, aes(x=datetime_NM, y =temp_C))+
  geom_point() + geom_path()+
  theme(legend.title = element_blank()) +
  theme_bw() +
  scale_x_datetime(limits=c(as.POSIXct('2017-11-02 00:00:00', tz="America/Denver"), as.POSIXct('2018-01-26 00:00:00', tz="America/Denver")), date_breaks="14 days") +
  scale_y_continuous(limits=c(0,30))

#need to delete winter data where logger wasn't stopped but appears to be removed from water
LJWB_HOBO_hrly_ts$date <- as.Date(LJWB_HOBO_hrly_ts$datetime_NM)
LJWB_HOBO_hrly_ts_1st <- filter(LJWB_HOBO_hrly_ts, date < as.Date("2013-11-12"))
LJWB_HOBO_hrly_ts_2nd <- filter(LJWB_HOBO_hrly_ts, date>as.Date("2014-04-09"))
LJWB_HOBO_hrly_ts<-rbind(LJWB_HOBO_hrly_ts_1st, LJWB_HOBO_hrly_ts_2nd)
LJWB_HOBO_hrly_ts_3rd <- filter(LJWB_HOBO_hrly_ts, date < as.Date("2017-11-02"))
LJWB_HOBO_hrly_ts_4th <- filter(LJWB_HOBO_hrly_ts, date>as.Date("2018-01-26"))
LJWB_HOBO_hrly_ts<-rbind(LJWB_HOBO_hrly_ts_3rd, LJWB_HOBO_hrly_ts_4th)


#correctfreezedata
tmp <- rle(LJWB_HOBO_hrly_ts$temp_C)
reps <- rep(tmp$lengths > 10,times = tmp$lengths)
LJWB_HOBO_hrly_ts = cbind(LJWB_HOBO_hrly_ts,reps)

freeze_times = data.frame(date_timeNM = LJWB_HOBO_hrly_ts$datetime_NM[LJWB_HOBO_hrly_ts$reps==TRUE])
freeze_times$winter = ifelse(as.Date(freeze_times$date_timeNM)>as.Date("2008-10-01") &
                               as.Date(freeze_times$date_timeNM)<as.Date("2009-05-01"),
                             "winter_0",
                             ifelse(as.Date(freeze_times$date_timeNM)>as.Date("2009-10-01") &
                                      as.Date(freeze_times$date_timeNM)<as.Date("2010-05-01"),
                                    "winter_1",
                                    ifelse(as.Date(freeze_times$date_timeNM)>as.Date("2010-10-01") &
                                             as.Date(freeze_times$date_timeNM)<as.Date("2011-05-01"),
                                           "winter_2",
                                           ifelse(as.Date(freeze_times$date_timeNM)>as.Date("2011-10-01") &
                                                    as.Date(freeze_times$date_timeNM)<as.Date("2012-05-01"),
                                                  "winter_3",
                                                  ifelse(as.Date(freeze_times$date_timeNM)>as.Date("2012-10-01") &
                                                           as.Date(freeze_times$date_timeNM)<as.Date("2013-05-01"),
                                                         "winter_4",
                                                         ifelse(as.Date(freeze_times$date_timeNM)>as.Date("2013-10-01") &
                                                                  as.Date(freeze_times$date_timeNM)<as.Date("2014-05-01"),
                                                                "winter_5",
                                                                ifelse(as.Date(freeze_times$date_timeNM)>as.Date("2014-10-01") &
                                                                         as.Date(freeze_times$date_timeNM)<as.Date("2015-05-01"),
                                                                       "winter_6", 
                                                                       ifelse(as.Date(freeze_times$date_timeNM)>as.Date("2015-10-01") &
                                                                                as.Date(freeze_times$date_timeNM)<as.Date("2016-05-01"),
                                                                              "winter_7",
                                                                              ifelse(as.Date(freeze_times$date_timeNM)>as.Date("2016-10-01") &
                                                                                       as.Date(freeze_times$date_timeNM)<as.Date("2017-05-01"),
                                                                                     "winter_8",
                                                                                     ifelse(as.Date(freeze_times$date_timeNM)>as.Date("2017-10-01") &
                                                                                              as.Date(freeze_times$date_timeNM)<as.Date("2018-05-01"),
                                                                                            "winter_9",
                                                                                            ifelse(as.Date(freeze_times$date_timeNM)>as.Date("2018-10-01") &
                                                                                                     as.Date(freeze_times$date_timeNM)<as.Date("2019-05-01"),
                                                                                                   "winter_10", 
                                                                                                   ifelse(as.Date(freeze_times$date_timeNM)>as.Date("2019-10-01") &
                                                                                                            as.Date(freeze_times$date_timeNM)<as.Date("2020-05-01"),
                                                                                                          "winter_11",NA))))))))))))
     
freeze_times =
  freeze_times %>%
  group_by(winter) %>%
  summarise(min = min(date_timeNM), max=max(date_timeNM))

LJWB_HOBO_hrly_ts$temp_C_c =LJWB_HOBO_hrly_ts$temp_C
winterz = unique(freeze_times$winter)
for (i in winterz) {
  min_frz = freeze_times$min[freeze_times$winter==i]
  max_frz = freeze_times$max[freeze_times$winter==i]
  LJWB_HOBO_hrly_ts$temp_C_c[LJWB_HOBO_hrly_ts$datetime_NM>=min_frz &
                               LJWB_HOBO_hrly_ts$datetime_NM<=max_frz] = NA
}

par(mfrow=c(2,1))
plot(LJWB_HOBO_hrly_ts$temp_C ~ LJWB_HOBO_hrly_ts$datetime_NM)
plot(LJWB_HOBO_hrly_ts$temp_C_c ~ LJWB_HOBO_hrly_ts$datetime_NM)


# zoom in on dates to see where transducer may have been frozen:
ggplot(data=LJWB_HOBO_hrly_ts, aes(x=datetime_NM, y =temp_C))+
  geom_point() + geom_path()+
  theme(legend.title = element_blank()) +
  theme_bw() +
  scale_x_datetime(limits=c(as.POSIXct('2009-12-10 17:00:00', tz="America/Denver"), as.POSIXct('2010-03-27 00:00:00', tz="America/Denver")), labels = date_format("%m/%d"), date_breaks="7 days") + 
  scale_y_continuous(limits=c(0,3))

#no need to filter winter data - stays above 0 and does not appear to freeze

#QAQC temp threshold to identify in-air points is set here to be x1.5 the upper (75%) quantile of temp data on each date. Can edit by changing either quantile probability or what it is divided by. Can also add a lower threshold, although this would not be indicative of in-air in summer data.

dayz= unique(LJWB_HOBO_hrly_ts$date)
daily_temp_stats <- LJWB_HOBO_hrly_ts[LJWB_HOBO_hrly_ts$date %in% dayz,] %>%
  select(date, temp_C_c) %>%
  group_by(date) %>%
  summarize_all(list(Temp.highQ = quantile), probs = 0.75, na.rm = TRUE) %>%
  mutate(Temp.highQ = Temp.highQ * 1.1)

#join threshold to data #
LJWB_HOBO_hrly_ts = left_join(LJWB_HOBO_hrly_ts, daily_temp_stats, by="date")

#replace pts above temp threshold on service dates with NA #
LJWB_HOBO_hrly_ts<-LJWB_HOBO_hrly_ts %>% 
  mutate(temp_C_c = case_when(temp_C_c > Temp.highQ ~ NA_real_, 
                              TRUE ~ as.numeric(temp_C_c)))
#clean up dataframe
LJWB_HOBO_hrly_ts_export = LJWB_HOBO_hrly_ts [, c(1,2,6,9)]
LJWB_HOBO_hrly_ts_export$siteID = "Indios"

#export as csv
write.csv(LJWB_HOBO_hrly_ts_export, "C:/Users/Brionna/OneDrive - University of New Mexico/Classes/EPS545_BIO502/VCNP/VCNP_Repo/processed data/alldata/Indios_temp.csv")

#get monthly averages
dat_monthly =
  LJWB_HOBO_hrly_ts %>%
  na.exclude()%>%
  dplyr::select(yr, mo, temp_C_c) %>%
  group_by(yr, mo) %>%
  summarise(Value.mn = mean(temp_C_c, na.rm = T), 
            Value.sd = sd(temp_C_c, na.rm = T), 
            Value.n = n())
dat_monthly$siteID = "Indios"
# paste month and year back together and convert to date
dat_monthly$date = paste(dat_monthly$yr, dat_monthly$mo, "15", sep="-")
dat_monthly$date = as.Date(dat_monthly$date)
#export as csv
write.csv(dat_monthly, "C:/Users/Brionna/OneDrive - University of New Mexico/Classes/EPS545_BIO502/VCNP/VCNP_Repo/processed data/monthly/Indios_temp_monthly.csv")
          
          #get yearly averages
          dat_yearly =
            LJWB_HOBO_hrly_ts %>%
            na.exclude()%>%
            dplyr::select(yr, temp_C_c) %>%
            group_by(yr) %>%
            summarise(Value.mn = mean(temp_C_c, na.rm = T), 
                      Value.sd = sd(temp_C_c, na.rm = T), 
                      Value.n = n())
          dat_yearly$siteID = "Indios"
          #export as csv
          write.csv(dat_yearly, "C:/Users/Brionna/OneDrive - University of New Mexico/Classes/EPS545_BIO502/VCNP/VCNP_Repo/processed data/yearly/Indios_temp_yr.csv")


# across years, colored by season:
          ggplot(data=LJWB_HOBO_hrly_ts, aes(x=datetime_NM, y=temp_C_c, color=season))+
            geom_point() + 
            theme(legend.title = element_blank()) +
            theme_bw() +
            theme(legend.position="bottom") +
            theme(axis.title=element_text(size=14,face="bold")) +
            theme(axis.text.x=element_text(angle=25)) +
            xlab("year")+
            theme(text = element_text(size = 16)) +
            scale_x_datetime(date_breaks="1 year",date_labels = "%Y") +
            scale_y_continuous(limits=c(-5,30), breaks=c(-5,0,5,10,15,20,25,30))
          
          ggsave(path="QAQC plots", filename="Indios.png", width=6.4, height=5.5, units="in")
