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

LJWB_file_list = list.files(path="C:/Users/Brionna/OneDrive - University of New Mexico/Classes/EPS545_BIO502/VCNP/VCNP_Repo/raw data/Valles_Flume_CSVs_Corrected/Redondo Creek Lower/", 
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

# View(LJWB_HOBO_list[[1]])
# View(LJWB_HOBO_list[[2]])  #need to correct date from 3/1/1980 to 11/2/17
# View(LJWB_HOBO_list[[3]]) #need to correct date from 1/1/70 to 10/16/18
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
# View(LJWB_HOBO_list[[20]])
# View(LJWB_HOBO_list[[21]])
# View(LJWB_HOBO_list[[22]])
# View(LJWB_HOBO_list[[23]])
# View(LJWB_HOBO_list[[24]])
# View(LJWB_HOBO_list[[25]])
# View(LJWB_HOBO_list[[26]])
# View(LJWB_HOBO_list[[27]])
# View(LJWB_HOBO_list[[28]])
# View(LJWB_HOBO_list[[29]])
# View(LJWB_HOBO_list[[30]])
# View(LJWB_HOBO_list[[31]])
# View(LJWB_HOBO_list[[32]])
# View(LJWB_HOBO_list[[33]])
# View(LJWB_HOBO_list[[34]])
# View(LJWB_HOBO_list[[35]])
# View(LJWB_HOBO_list[[36]])
# View(LJWB_HOBO_list[[37]])
# View(LJWB_HOBO_list[[38]])
# View(LJWB_HOBO_list[[39]])
# View(LJWB_HOBO_list[[40]])
# View(LJWB_HOBO_list[[41]])
# View(LJWB_HOBO_list[[42]])
# View(LJWB_HOBO_list[[43]])
# View(LJWB_HOBO_list[[44]])
# View(LJWB_HOBO_list[[45]])
# View(LJWB_HOBO_list[[46]]) #need to correct date from 1/10/1937 to 7/25/12
# View(LJWB_HOBO_list[[47]])  #need to correct date from 1/16/1937 to 7/31/12
# View(LJWB_HOBO_list[[48]])
# View(LJWB_HOBO_list[[49]])
# View(LJWB_HOBO_list[[50]])
# View(LJWB_HOBO_list[[51]])
# View(LJWB_HOBO_list[[52]])
# View(LJWB_HOBO_list[[53]])
# View(LJWB_HOBO_list[[54]])
# View(LJWB_HOBO_list[[55]])
# View(LJWB_HOBO_list[[56]])
# View(LJWB_HOBO_list[[57]])
# View(LJWB_HOBO_list[[58]])
# View(LJWB_HOBO_list[[59]])

# These look like they are structured the same. 
# If they are not structured the same, you'll want to edit the offending dataframes. Let me know if you run into this. 

##need to format 12 hr time for list 3 (will need to come back and correct date also)
LJWB_HOBO_list[[3]]$Date.Time..GMT.06.00.1 <- format(strptime(LJWB_HOBO_list[[3]]$Date.Time..GMT.06.00, format="%m/%d/%y %I:%M:%S %p"), format="%H:%M:%S")
LJWB_HOBO_list[[3]]$Date.Time..GMT.06.00 <- as.Date(LJWB_HOBO_list[[3]]$Date.Time..GMT.06.00, format="%m/%d/%y")

LJWB_HOBO_list[[3]] <- LJWB_HOBO_list[[3]] %>% relocate(Date.Time..GMT.06.00.1, .after=Date.Time..GMT.06.00)

# First, I select only the columns that I'm interested in. 
# this first function selects only columns with "Date", "Time", or "Temp" in the column name
LJWB_HOBO_list_2 = lapply(LJWB_HOBO_list, 
                          dplyr::select,
                          (contains("Date") | contains("Time") | contains("Temp")))
View(LJWB_HOBO_list_2[[30]])

#### format date ####

# Second, I will format the data column

for(i in 1:length(LJWB_HOBO_list_2)){
  LJWB_HOBO_list_2[[i]]$date =  as.Date(LJWB_HOBO_list_2[[i]][,1], format = "%m/%d/%Y")
}

# Third, I will correct dates in df 2
View(LJWB_HOBO_list_2[[2]])
range(LJWB_HOBO_list_2[[2]]$date)
# notice that the dates in this dataframe are in 2037
# To correct, we have to assume that the starting date in the file name "2012-07-31" is the correct starting date, and that dates are sequential. We also have to assume that the time stamps are correct. It'd be nice to check this assumption with someone, but the person who collected data in 2012 is no longer around. 
# I will correct it here, but note this issue and if the diel or seasonal patterns look wrong later on, our assumptions may be incorrect and we should remove these data. 

# calculate correction (# of days difference)
diff_date = as.Date("2017-11-02") - as.Date("1980-03-01")
# apply correction
LJWB_HOBO_list_2[[2]]$date = LJWB_HOBO_list_2[[2]]$date + diff_date
# check new range
range(LJWB_HOBO_list_2[[2]]$date)

# Now do the same for 3
View(LJWB_HOBO_list_2[[3]])
range(LJWB_HOBO_list_2[[3]]$date)
diff_date = as.Date("2018-10-16") - as.Date("1970-01-01")
LJWB_HOBO_list_2[[3]]$date = LJWB_HOBO_list_2[[3]]$date + diff_date
range(LJWB_HOBO_list_2[[3]]$date)
# Now do the same for 46
View(LJWB_HOBO_list_2[[46]])
range(LJWB_HOBO_list_2[[46]]$date)
diff_date = as.Date("2012-07-25") - as.Date("1937-01-10")
LJWB_HOBO_list_2[[46]]$date = LJWB_HOBO_list_2[[46]]$date + diff_date
range(LJWB_HOBO_list_2[[46]]$date)
# Now do the same for 47
View(LJWB_HOBO_list_2[[47]])
range(LJWB_HOBO_list_2[[47]]$date)
diff_date = as.Date("2012-07-31") - as.Date("1937-01-16")
LJWB_HOBO_list_2[[47]]$date = LJWB_HOBO_list_2[[47]]$date + diff_date
range(LJWB_HOBO_list_2[[47]]$date)


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
#summary(LJWB_HOBO_list_2[[1]]$datetime_NM)
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
# summary(LJWB_HOBO_list_2[[20]]$datetime_NM)
# summary(LJWB_HOBO_list_2[[21]]$datetime_NM)
# summary(LJWB_HOBO_list_2[[22]]$datetime_NM)
# summary(LJWB_HOBO_list_2[[23]]$datetime_NM)
# summary(LJWB_HOBO_list_2[[24]]$datetime_NM)
# summary(LJWB_HOBO_list_2[[25]]$datetime_NM)
# summary(LJWB_HOBO_list_2[[26]]$datetime_NM)
# summary(LJWB_HOBO_list_2[[27]]$datetime_NM)
# summary(LJWB_HOBO_list_2[[28]]$datetime_NM)
# summary(LJWB_HOBO_list_2[[29]]$datetime_NM)
# summary(LJWB_HOBO_list_2[[30]]$datetime_NM)
# summary(LJWB_HOBO_list_2[[31]]$datetime_NM)
# summary(LJWB_HOBO_list_2[[32]]$datetime_NM)
# summary(LJWB_HOBO_list_2[[33]]$datetime_NM)
# summary(LJWB_HOBO_list_2[[34]]$datetime_NM)
# summary(LJWB_HOBO_list_2[[35]]$datetime_NM)
# summary(LJWB_HOBO_list_2[[36]]$datetime_NM)
# summary(LJWB_HOBO_list_2[[37]]$datetime_NM)
# summary(LJWB_HOBO_list_2[[38]]$datetime_NM)
# summary(LJWB_HOBO_list_2[[39]]$datetime_NM)
# summary(LJWB_HOBO_list_2[[40]]$datetime_NM)
# summary(LJWB_HOBO_list_2[[41]]$datetime_NM)
# summary(LJWB_HOBO_list_2[[42]]$datetime_NM)
# summary(LJWB_HOBO_list_2[[43]]$datetime_NM)
# summary(LJWB_HOBO_list_2[[44]]$datetime_NM)
# summary(LJWB_HOBO_list_2[[45]]$datetime_NM)
# summary(LJWB_HOBO_list_2[[46]]$datetime_NM)
# summary(LJWB_HOBO_list_2[[47]]$datetime_NM)
# summary(LJWB_HOBO_list_2[[48]]$datetime_NM)
# summary(LJWB_HOBO_list_2[[49]]$datetime_NM)
# summary(LJWB_HOBO_list_2[[50]]$datetime_NM)
# summary(LJWB_HOBO_list_2[[51]]$datetime_NM)
# summary(LJWB_HOBO_list_2[[52]]$datetime_NM)
# summary(LJWB_HOBO_list_2[[53]]$datetime_NM)
# summary(LJWB_HOBO_list_2[[54]]$datetime_NM)
# summary(LJWB_HOBO_list_2[[55]]$datetime_NM)
# summary(LJWB_HOBO_list_2[[56]]$datetime_NM)
# summary(LJWB_HOBO_list_2[[57]]$datetime_NM)
# summary(LJWB_HOBO_list_2[[58]]$datetime_NM)
# summary(LJWB_HOBO_list_2[[59]]$datetime_NM)

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

# add season (i'm estimating on season here - could be more accurate using dates)
LJWB_HOBO_hrly_ts$season = ifelse(LJWB_HOBO_hrly_ts$mo %in% c(11,12,1,2,3) , "Winter", "Summer")

# zoom in on dates to see where transducer may have been frozen:
ggplot(data=LJWB_HOBO_hrly_ts, aes(x=datetime_NM, y =temp_C))+
  geom_point() + geom_path()+
  theme(legend.title = element_blank()) +
  theme_bw() +
  scale_x_datetime(limits=c(as.POSIXct('2012-09-10 17:00:00', tz="America/Denver"), as.POSIXct('2012-09-14 00:00:00', tz="America/Denver")), date_breaks="7 days") +
  scale_y_continuous(limits=c(5,15))

#correct freeze data

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
                                                                                                                "winter_10", NA)))))))))))
freeze_times <- filter(freeze_times, winter!="NA")       
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

#QAQC temp threshold to identify in-air points is set here to be x1.5 the upper (75%) quantile of temp data on each date. Can edit by changing either quantile probability or what it is divided by. Can also add a lower threshold, although this would not be indicative of in-air in summer data.
LJWB_HOBO_hrly_ts$date = lubridate::date(LJWB_HOBO_hrly_ts$datetime_NM)
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
LJWB_HOBO_hrly_ts_export = LJWB_HOBO_hrly_ts [, c(1,2,6,8)]
LJWB_HOBO_hrly_ts_export$siteID = "RedondoCreekLower"

#export as csv
write.csv(LJWB_HOBO_hrly_ts_export, "C:/Users/Brionna/OneDrive - University of New Mexico/Classes/EPS545_BIO502/VCNP/VCNP_Repo/processed data/alldata/RedondoCreekLower_temp.csv")

#get monthly averages
dat_monthly =
  LJWB_HOBO_hrly_ts %>%
  na.exclude()%>%
  dplyr::select(yr, mo, temp_C_c) %>%
  group_by(yr, mo) %>%
  summarise(Value.mn = mean(temp_C_c, na.rm = T), 
            Value.sd = sd(temp_C_c, na.rm = T), 
            Value.n = n())
dat_monthly$siteID = "RedondoCreekLower"
# paste month and year back together and convert to date
dat_monthly$date = paste(dat_monthly$yr, dat_monthly$mo, "15", sep="-")
dat_monthly$date = as.Date(dat_monthly$date)
#export as csv
write.csv(dat_monthly, "C:/Users/Brionna/OneDrive - University of New Mexico/Classes/EPS545_BIO502/VCNP/VCNP_Repo/processed data/monthly/RedondoCreekLower_temp_monthly.csv")
          
          #get yearly averages
          dat_yearly =
            LJWB_HOBO_hrly_ts %>%
            na.exclude()%>%
            dplyr::select(yr, temp_C_c) %>%
            group_by(yr) %>%
            summarise(Value.mn = mean(temp_C_c, na.rm = T), 
                      Value.sd = sd(temp_C_c, na.rm = T), 
                      Value.n = n())
          dat_yearly$siteID = "RedondoCreekLower"
          #export as csv
          write.csv(dat_yearly, "C:/Users/Brionna/OneDrive - University of New Mexico/Classes/EPS545_BIO502/VCNP/VCNP_Repo/processed data/yearly/RedondoCreekLower_temp_yr.csv")
                    
# plot 
ggplot(data=LJWB_HOBO_hrly_ts, aes(x=day, y=temp_C))+
  geom_point() + geom_path()+
  facet_wrap(~yr, scales="free_y")+
  theme(legend.title = element_blank()) +
  theme_bw()

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

ggsave(path="QAQC plots", filename="RedondoCreekLower.png", width=6.4, height=5.5, units="in")
