
###########################################################################
#
# Project: Prevalence of multimorbidity
#
# Purpose: Set up imputed data for analysis
#
# Author: Jacqueline Rudolph
#
# Last Update: 29 Jul 2025
#
###########################################################################

packages <- c("tidyverse", "lubridate")

for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package, repos='http://lib.stat.cmu.edu/R/CRAN')
  }
  library(package, character.only=T)
}

M <- 30


# Read in data ------------------------------------------------------------

mort <- read_csv("../data/alive_mort.csv") %>% 
  select(id, dod)

dat5 <- read_csv("../data/alive_chronic_imputed5.csv")
dat10 <- read_csv("../data/alive_chronic_imputed10.csv")
dat15 <- read_csv("../data/alive_chronic_imputed15.csv")
dat20 <- read_csv("../data/alive_chronic_imputed20.csv")
dat25 <- read_csv("../data/alive_chronic_imputed25.csv")
dat30 <- read_csv("../data/alive_chronic_imputed30.csv")

dat <- bind_rows(dat5, dat10, dat15, dat20, dat25, dat30)


# Set up data -------------------------------------------------------------

setup <- function(r) {
  
  temp <- filter(dat, rep==r)
  
  # Create disease and control indicators
  temp <- temp %>% 
    group_by(id) %>% 
    mutate(# Hypertension
           htn_ind = as.numeric(bpsys>=140 | bpdias>=90 | hbptx6m==1),
           htn = as.numeric(cumsum(htn_ind)>0),
           
           htn_control = ifelse(htn==1, as.numeric(bpsys<140 & bpdias<90), NA),
           htn_control2 = ifelse(htn==1, as.numeric(bpsys<130 & bpdias<80), NA),
      
           # Diabetes
           diab_ind = as.numeric(hba1c>6.5 | diabtx6m==1),
           diab = as.numeric(cumsum(diab_ind)>0),
           
           diab_control = ifelse(diab==1, as.numeric(hba1c<=7), NA),

           # Lung disease
           fev_fvc = maxfev/maxfvc,
           lung_ind = as.numeric(fev_fvc<=0.7 | fev_fvc<=llnfevfvc),
           lung = as.numeric(cumsum(lung_ind)>0),
           lung_ind2 = ifelse(lung==1, 1, ifelse(fev1pp<80, 1, 0)),
           lung2 = as.numeric(cumsum(lung_ind2)>0),
           
           fev1_decline_yr = ifelse(visnum==1, 0, 
                                    ifelse(visnum==2, lag(maxfev) - maxfev, 
                                           lag(maxfev, n=2) - maxfev)),
           fev1_decline60 = ifelse(visnum<=2, as.numeric(fev1_decline_yr<0.03), as.numeric(fev1_decline_yr<0.06)),
           # NOTE: fev1 in L but cutpoint in mL
           wrsbrttx = as.numeric(wrsbrttx==TRUE),
           exacerbation = wrsbrttx + lag(wrsbrttx, default=0),
           lung_control = ifelse(lung==1, as.numeric(fev1_decline60==1 & exacerbation<2), NA),
           lung_control2 = ifelse(lung2==1, as.numeric(fev1_decline60==1 & exacerbation<2), NA),
      
           # Renal disease
           uprt_crt = exp(uprt_crt),
           renal_ind = as.numeric(uprt_crt>200 | gfr<=60),
           renal = as.numeric(cumsum(renal_ind)>0),
           
           renal_decline = ifelse(visnum>2, (lag(gfr, n=2) - gfr)/lag(gfr, n=2), 0),
           renal_control = ifelse(renal==1, as.numeric(renal_decline<0.1), NA),
           renal_control2 = ifelse(renal==1, as.numeric(uprt_crt<=200 & renal_decline<0.1), NA),
           renal_control3 = ifelse(renal==1, as.numeric(renal_decline<0.05), NA),
           
           # Liver disease
           fbscan = exp(fbscan),
           liver_ind = as.numeric(fbscan>=9.3),
           liver = as.numeric(cumsum(liver_ind)>0),
           
           liver_decline = ifelse(visnum>2, (lag(fbscan, n=2) - fbscan)/lag(fbscan, n=2), 0),
           liver_control = ifelse(liver==1, as.numeric(liver_decline>-0.1), NA),
           liver_control2 = ifelse(liver==1, as.numeric(fbscan<9.3), NA),
           liver_control3 = ifelse(liver==1, as.numeric(liver_decline>-0.05), NA),
      
           # Depression
           cesdtot = as.numeric(cesdtot),
           dep_ind = as.numeric(cesdtot>=23 | deptx6m==1),
           dep = as.numeric(cumsum(dep_ind)>0),
           
           dep_control = ifelse(dep==1, as.numeric(cesdtot<23), NA),
           dep_control2 = ifelse(dep==1, as.numeric(cesdtot<16), NA)) %>% 
    select(-c(htn_ind, dep_ind, renal_ind, diab_ind, lung_ind, lung_ind2, liver_ind))
  
  # If person died within 1 year of last visit, create a death record for them
  last <- temp %>% 
    filter(!duplicated(id, fromLast = TRUE)) %>%
    left_join(mort, by = "id") %>%
    mutate(time_difference = as.numeric(difftime(dod, visdate, units = "days")),
           time_difference_years = time_difference / 365.25,
           death = case_when(is.na(time_difference) ~ 0,
                             time_difference_years < 1 ~ 1, 
                             T ~ 0),
           aod = age + time_difference_years,
           age = if_else(death == 1, aod, age+1)) %>% 
    select(-c(time_difference, time_difference_years, aod, dod)) %>% 
    filter(death==1)
  
  dat2 <- bind_rows(temp, last) %>% 
    arrange(id, age) %>% 
    mutate(death = if_else(is.na(death), 0, death)) 
  
  # Flag drop out
  last2 <- dat2 %>% 
    filter(!duplicated(id, fromLast=T)) %>% 
    mutate(drop1 = case_when(death==1 ~ 0,
                             visdate<=ymd("2018-12-31") & age<=69 ~ 1,
                             T ~ 0),
           drop2 = case_when(death==1 ~ 1,
                             visdate<=ymd("2018-12-31") & age<=69 ~ 1,
                             T ~ 0)) %>% 
    select(id, visdate, drop1, drop2)
  
  dat3 <- left_join(dat2, last2, by=c("id", "visdate")) %>% 
    mutate(drop1 = ifelse(is.na(drop1), 0, drop1),
           drop2 = ifelse(is.na(drop2), 0, drop2))
      
  # Prepare for analysis    
  dat3 <- dat3 %>% 
    mutate(n_cond = htn + dep + renal + diab + lung + liver,    
           n_cond2 = htn + dep + renal + diab + lung2 + liver,  # Sensitivity analysis
           
           # Main analysis
           n_cond_control = rowSums(across(c(htn_control, dep_control, renal_control, 
                                             diab_control, lung_control, liver_control)), na.rm=T),
           prop_cond_control = as.numeric(n_cond_control/n_cond),
           most_control = as.numeric(prop_cond_control>=0.5),
           all_control = as.numeric(prop_cond_control==1),
           
           state = case_when(death==1 ~ 4,                     # Dead
                             n_cond<2 ~ 1,                     # No multimorbidity
                             n_cond>=2 & most_control==1 ~ 2,  # Multimorbidity controlled
                             T ~ 3),                           # Multimorbidity not controlled
           
           htn_state = case_when(death==1 ~ 4,
                                 htn==0 ~ 1,
                                 htn==1 & htn_control==1 ~ 2,
                                 T ~ 3),
           diab_state = case_when(death==1 ~ 4,
                                  diab==0 ~ 1,
                                  diab==1 & diab_control==1 ~ 2,
                                  T ~ 3),
           lung_state = case_when(death==1 ~ 4,
                                  lung==0 ~ 1,
                                  lung==1 & lung_control==1 ~ 2,
                                  T ~ 3),
           renal_state = case_when(death==1 ~ 4,
                                   renal==0 ~ 1,
                                   renal==1 & renal_control==1 ~ 2,
                                   T ~ 3),
           liver_state = case_when(death==1 ~ 4,
                                   liver==0 ~ 1,
                                   liver==1 & liver_control==1 ~ 2,
                                   T ~ 3),
           dep_state = case_when(death==1 ~ 4,
                                 dep==0 ~ 1,
                                 dep==1 & dep_control==1 ~ 2,
                                 T ~ 3),
           
           # Sensitivity analysis: require all controlled
           state_all = case_when(death==1 ~ 4,
                              n_cond<2 ~ 1, 
                              n_cond>=2 & all_control==1 ~ 2,
                              T ~ 3),
           
           # Sensitivity analysis: alternative lung disease
           n_cond_control2 = rowSums(across(c(htn_control, dep_control, renal_control, 
                                              diab_control, lung_control2, liver_control)), na.rm=T),
           prop_cond_control2 = as.numeric(n_cond_control2/n_cond2),
           most_control2 = as.numeric(prop_cond_control2>=0.5),
           
           state2 = case_when(death==1 ~ 4,
                              n_cond2<2 ~ 1, 
                              n_cond2>=2 & most_control2==1 ~ 2,
                              T ~ 3),
           
           # Sensitivity analysis: alternative hypertension control
           n_cond_control3 = rowSums(across(c(htn_control2, dep_control, renal_control, 
                                              diab_control, lung_control, liver_control)), na.rm=T),
           prop_cond_control3 = as.numeric(n_cond_control3/n_cond),
           most_control3 = as.numeric(prop_cond_control3>=0.5),
           
           state3 = case_when(death==1 ~ 4,
                              n_cond<2 ~ 1, 
                              n_cond>=2 & most_control3==1 ~ 2,
                              T ~ 3),
           
           # Sensitivity analysis: alternative depression control
           n_cond_control4 = rowSums(across(c(htn_control, dep_control2, renal_control, 
                                              diab_control, lung_control, liver_control)), na.rm=T),
           prop_cond_control4 = as.numeric(n_cond_control4/n_cond),
           most_control4 = as.numeric(prop_cond_control4>=0.5),
           
           state4 = case_when(death==1 ~ 4,
                              n_cond<2 ~ 1, 
                              n_cond>=2 & most_control4==1 ~ 2,
                              T ~ 3),
           
           # Sensitivity analysis: alternative renal disease control
           n_cond_control5 = rowSums(across(c(htn_control, dep_control, renal_control2, 
                                              diab_control, lung_control, liver_control)), na.rm=T),
           prop_cond_control5 = as.numeric(n_cond_control5/n_cond),
           most_control5 = as.numeric(prop_cond_control5>=0.5),
           
           state5 = case_when(death==1 ~ 4,
                              n_cond<2 ~ 1, 
                              n_cond>=2 & most_control5==1 ~ 2,
                              T ~ 3),
           
           # Sensitivity analysis: alternative renal disease control 2
           n_cond_control6 = rowSums(across(c(htn_control, dep_control, renal_control3, 
                                              diab_control, lung_control, liver_control)), na.rm=T),
           prop_cond_control6 = as.numeric(n_cond_control6/n_cond),
           most_control6 = as.numeric(prop_cond_control6>=0.5),
           
           state6 = case_when(death==1 ~ 4,
                              n_cond<2 ~ 1, 
                              n_cond>=2 & most_control6==1 ~ 2,
                              T ~ 3),
           
           # Sensitivity analysis: alternative liver disease control
           n_cond_control7 = rowSums(across(c(htn_control, dep_control, renal_control, 
                                              diab_control, lung_control, liver_control2)), na.rm=T),
           prop_cond_control7 = as.numeric(n_cond_control7/n_cond),
           most_control7 = as.numeric(prop_cond_control7>=0.5),
           
           state7 = case_when(death==1 ~ 4,
                              n_cond<2 ~ 1, 
                              n_cond>=2 & most_control7==1 ~ 2,
                              T ~ 3),
           
           # Sensitivity analysis: alternative liver disease control 2
           n_cond_control8 = rowSums(across(c(htn_control, dep_control, renal_control, 
                                              diab_control, lung_control, liver_control3)), na.rm=T),
           prop_cond_control8 = as.numeric(n_cond_control8/n_cond),
           most_control8 = as.numeric(prop_cond_control8>=0.5),
           
           state8 = case_when(death==1 ~ 4,
                              n_cond<2 ~ 1, 
                              n_cond>=2 & most_control8==1 ~ 2,
                              T ~ 3)) %>%
    group_by(id) %>% 
    mutate(age0 = lag(age),
           state0 = lag(state),
           state0 = ifelse(is.na(state0), state, state0)) %>% 
    filter(age<70) %>% # In case death was after age 70
    ungroup()
  
  # Baseline HIV status for stratification
  first <- filter(dat3, !duplicated(id)) %>% 
    rename(hiv_base = hiv) %>% 
    select(id, hiv_base)
  
  dat3 <- left_join(dat3, first, by="id")
  
  return(dat3)

}

all.dat <- lapply(1:M, function(x){setup(x)})
all.dat <- bind_rows(all.dat)

write_csv(all.dat, "../data/alive_chronic_imputed_setup.csv")

