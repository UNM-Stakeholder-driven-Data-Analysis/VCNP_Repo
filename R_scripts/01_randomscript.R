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
####################################################################################################################
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
LJWB_HOBO_hrly_ts_export = LJWB_HOBO_hrly_ts [, c(1,2,9)]
LJWB_HOBO_hrly_ts_export$siteID = "x"

#export as csv
write.csv(LJWB_HOBO_hrly_ts_export, 

######################################################################################################################
#get monthly averages
dat_monthly =
  LJWB_HOBO_hrly_ts %>%
  na.exclude()%>%
  dplyr::select(yr, mo, temp_C_c) %>%
  group_by(yr, mo) %>%
  summarise(Value.mn = mean(temp_C_c, na.rm = T), 
            Value.sd = sd(temp_C_c, na.rm = T), 
            Value.n = n())
dat_monthly$siteID = "x"
# paste month and year back together and convert to date
dat_monthly$date = paste(dat_monthly$yr, dat_monthly$mo, "15", sep="-")
dat_monthly$date = as.Date(dat_monthly$date)
#export as csv
write.csv(dat_monthly, 

#get yearly averages
dat_yearly =
  LJWB_HOBO_hrly_ts %>%
  na.exclude()%>%
  dplyr::select(yr, temp_C_c) %>%
  group_by(yr) %>%
  summarise(Value.mn = mean(temp_C_c, na.rm = T), 
            Value.sd = sd(temp_C_c, na.rm = T), 
            Value.n = n())
dat_yearly$siteID = "x"
#export as csv
write.csv(dat_yearly, 

####################################################################################################
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

ggsave(path="QAQC plots", filename="x.png", width=6.4, height=5.5, units="in")
##########################################################################################

#will filter out data between  11/28-4/01
LJWB_HOBO_hrly_ts$date <- as.Date(LJWB_HOBO_hrly_ts$datetime_NM)
LJWB_HOBO_hrly_ts$monthday <- format(LJWB_HOBO_hrly_ts$date, "%m%d") 
LJWB_HOBO_hrly_ts<- filter(LJWB_HOBO_hrly_ts, monthday>="0401"& monthday<="1128")

#QAQC temp threshold to identify in-air points is set here to be x1.5 the upper (75%) quantile of temp data on each date. Can edit by changing either quantile probability or what it is divided by. Can also add a lower threshold, although this would not be indicative of in-air in summer data.
dayz= unique(LJWB_HOBO_hrly_ts$date)
daily_temp_stats <- LJWB_HOBO_hrly_ts[LJWB_HOBO_hrly_ts$date %in% dayz,] %>%
 select(date, temp_C) %>%
group_by(date) %>%
summarize_all(list(Temp.highQ = quantile), probs = 0.75, na.rm = TRUE) %>%
  mutate_at(vars(-date), ~(.*1.5))

 join threshold to data #
LJWB_HOBO_hrly_ts = left_join(LJWB_HOBO_hrly_ts, daily_temp_stats, by="datez")

 replace pts above temp theshold on service dates with NA #
LJWB_HOBO_hrly_ts[which(LJWB_HOBO_hrly_ts$temp_C > LJWB_HOBO_hrly_ts$Temp.highQ), c(5:15,17)] = NA