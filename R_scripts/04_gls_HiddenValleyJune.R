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
library(beepr)
library(bbmle)
library(MuMIn)
library(gridExtra)
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
View(LJWB_file_list_short[[3]]) 
p<-tibble(path = LJWB_file_list) %>%
  mutate(file_contents = map(path, read_csv)) %>% 
  unnest(file_contents)

HiddenValley_June<-filter(p,siteID=="EastForkJemezHiddenValley")
HiddenValley_June<-filter(HiddenValley_June,mo=="6")

#### linear trends ####
# add simple time steps to df
HiddenValley_June$t = c(1:nrow(HiddenValley_June))

mod = lm(Value.mn ~ t, HiddenValley_June)
summary(mod)
visreg(mod,"t")
confint(mod, 't', level=0.95)

## diagnostics ##
Acf(resid(mod))
forecast::checkresiduals(mod)


####test and calculate trends - nlme::gls####

# ask auto.arima what it thinks the autocorrelation structure is
auto.arima(HiddenValley_June$Value.mn)

# fit AR(1) regression model with time as a predictor
mod_Ar1 = gls(Value.mn ~ t, data=HiddenValley_June, correlation=corCAR1(), method="ML")

# fit some other candidate structures - not sure if CorARMA is appropriate for unevenly spaced data- but ultimately used the first model that uses CorCAR to account for the uneven spacing
mod_AMRAp1q1 = gls(Value.mn ~ t, data=HiddenValley_June, correlation=corARMA(p=1,q=1), method="ML")
mod_AMRAp2 = gls(Value.mn ~ t, data=HiddenValley_June, correlation=corARMA(p=2), method="ML")

# compare models with AICc (for small dataset) (looking for value less than 2)
bbmle::AICctab(mod_Ar1,mod_AMRAp1q1,mod_AMRAp2)
summary(mod_Ar1)

# intervals() for nlme is equivelant to confint() for lm
intervals(mod_Ar1)

visreg(mod_Ar1,"t")

Acf(resid(mod_Ar1))

# extract and assess residuals
par(mfrow=c(1,3))
Acf(resid(mod_Ar1, type = "normalized"), main="GLS AR(1) model residuals")
plot(resid(mod_Ar1, type = "normalized")~c(1:length(HiddenValley_June$t)), main="GLS AR(1) model residuals"); abline(h=0)
qqnorm(resid(mod_Ar1, type = "normalized"), main="GLS AR(1) model residuals", pch=16, 
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_Ar1, type = "normalized"))$statistic,2))); qqline(resid(mod_Ar1, type = "normalized"))

# exctract parameter estimates for comparison with MARSS
mod_Ar1.phi = coef(mod_Ar1$modelStruct[[1]], unconstrained=FALSE)
ests.gls = c(b=mod_Ar1.phi, alpha=coef(mod_Ar1)[1],
             time=coef(mod_Ar1)[2],
             logLik=logLik(mod_Ar1))

