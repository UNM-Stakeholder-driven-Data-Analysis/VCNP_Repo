#### read me ####
# the purpose of this script is to combine all temperature records from HOBO sensors.
#### load libraries ####
library(lubridate)
library(tidyverse)
library(tsibble)
library(dplyr)

#### load data ####

LJWB_file_list = list.files(path="C:/Users/Brionna/OneDrive - University of New Mexico/Classes/EPS545_BIO502/VCNP/VCNP_Repo/processed data", 
                            recursive=F, 
                            full.names=TRUE)
# this imports files into an R list of dataframes (review intro to R tutorial if you don't understand R lists!)
LJWB_HOBO_list = lapply(LJWB_file_list, 
                        read.csv, 
                        stringsAsFactors=F, 
                   )
 View(LJWB_HOBO_list[[3]]) 

# plot 
# ggplot(data=LJWB_HOBO_hrly_ts_1, aes(x=day, y=temp_C))+
#   geom_point() + geom_path()+
#   facet_wrap(~yr, scales="free_y")+
#   theme(legend.title = element_blank()) +
#   theme_bw()
# 
# # across years:
# ggplot(data=LJWB_HOBO_hrly_ts_1, aes(x=datetime_NM, y=temp_C))+
#   geom_point() + geom_path()+
#   theme(legend.title = element_blank()) +
#   theme_bw()

# across years, colored by season:
# plot
for(i in 1:length(LJWB_HOBO_list)){
  p=ggplot(data=LJWB_HOBO_list[[i]], aes(x=datetime_NM, y=temp_C, color=season))+
    geom_point() + 
    facet_wrap()
  theme(legend.title = element_blank()) +
    theme_bw()
  print(p)
}


  
  