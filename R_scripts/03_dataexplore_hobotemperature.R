#### read me ####
# the purpose of this script is to combine all temperature records from HOBO sensors.
#### load libraries ####
library(lubridate)
library(tidyverse)
library(tsibble)
library(dplyr)

#### load data ####

LJWB_file_list = list.files(path="C:/Users/Brionna/OneDrive - University of New Mexico/Classes/EPS545_BIO502/VCNP/VCNP_Repo/processed data/alldata", 
                            recursive=F, 
                            full.names=TRUE)
LJWB_file_list_short = list.files(path="C:/Users/Brionna/OneDrive - University of New Mexico/Classes/EPS545_BIO502/VCNP/VCNP_Repo/processed data/alldata", 
                            recursive=F, 
                            full.names=FALSE) %>%
  str_replace("_temp.csv", "")

#### manipulate_data ####

 View(LJWB_file_list_short[[3]]) 
 p<-tibble(path = LJWB_file_list) %>%
   mutate(file_contents = map(path, read_csv)) %>% 
   unnest(file_contents)
 

 
# plot 
# ggplot(data=LJWB_HOBO_hrly_ts_1, aes(x=day, y=temp_C_c))+
#   geom_point() + geom_path()+
#   facet_wrap(~yr, scales="free_y")+
#   theme(legend.title = element_blank()) +
#   theme_bw()
# 
# # across years:
# ggplot(data=LJWB_HOBO_hrly_ts_1, aes(x=datetime_NM, y=temp_C_c))+
#   geom_point() + geom_path()+
#   theme(legend.title = element_blank()) +
#   theme_bw()

# across years, colored by season:
# plot
 path="C:/Users/Brionna/OneDrive - University of New Mexico/Classes/EPS545_BIO502/VCNP/VCNP_Repo/processed data/alldata"

  ggplot(data=p, aes(x=datetime_NM, y=temp_C_c, color=season))+
    geom_point() +
    facet_wrap(~siteID, ncol=4)+
     theme(legend.title = element_blank()) +
      theme_bw() +
     theme(axis.text.y = element_text(size = 12)) +
     theme(axis.text.x = element_text(size = 12, angle=25)) +
     theme(legend.position="bottom") +
      theme(axis.title=element_text(size=14,face="bold"))+
     scale_x_datetime(date_breaks="3 year",date_labels = "%Y") +
      scale_y_continuous(limits=c(-5,30))

  ggsave(path="QAQC plots", filename="VCNP_Stations.png", width=8.4, height=7.5, units="in")

