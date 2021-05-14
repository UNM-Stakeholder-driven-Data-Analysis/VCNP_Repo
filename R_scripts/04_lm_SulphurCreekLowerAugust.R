#### read me ####
# the purpose of this script is to combine all monthly temperature records from HOBO sensors.
#### load libraries ####
library(lubridate)
library(tidyverse)
library(forecast)
library(tsibble)
library(dplyr)
library(nlme)
library(lme4)
library(car)
library(visreg)
#### load data ####

LJWB_file_list = list.files(path="C:/Users/Brionna/OneDrive - University of New Mexico/Classes/EPS545_BIO502/VCNP/VCNP_Repo/processed data/monthly", 
                            recursive=F, 
                            full.names=TRUE)
LJWB_file_list_short = list.files(path="C:/Users/Brionna/OneDrive - University of New Mexico/Classes/EPS545_BIO502/VCNP/VCNP_Repo/processed data/monthly", 
                                  recursive=F, 
                                  full.names=FALSE) %>%
  str_replace("monthly_temp.csv", "")

#### manipulate_data ####
p<-tibble(path = LJWB_file_list) %>%
  mutate(file_contents = map(path, read_csv)) %>% 
  unnest(file_contents)

SulphurCreekLower_August<-filter(p,siteID=="SulphurCreekLower")
SulphurCreekLower_August<-filter(SulphurCreekLower_August, mo=="8")

#### linear trends ####
mod = lm(Value.mn ~ X1, SulphurCreekLower_August)
summary(mod)
visreg(mod,"X1")
confint(mod, 'X1', level=0.95)

## diagnostics ##
Acf(resid(mod))
forecast::checkresiduals(mod)




