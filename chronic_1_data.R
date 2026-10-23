
###########################################################################
#
# Project: Prevalence of multimorbidity
#
# Purpose: Set up data
#
# Author: Jacqueline Rudolph
#
# Last Update: 21 May 2024
#
###########################################################################

library("tidyverse")
library("lubridate")


# Read in data ------------------------------------------------------------

base <- read_csv("../data/alive_base.csv") %>% 
  select(id, beduchs, bnevmar)

socdem <- read_csv("../data/alive_socdem.csv") %>% 
  filter(year(visdate) %in% 2009:2019) %>% 
  select(id, visdate, black, age, m0f1, anyhomels, work)

chronic <- read_csv("../data/alive_comorbid.csv") %>% 
  filter(year(visdate) %in% 2009:2019) %>% 
  select(id, visdate, 
         hbptx6m, bpsys, bpdias,
         diabtx6m, hba1c, bmimedh,
         lungtx6m, maxfev, maxfvc, wrsbrttx, fev1pp, llnfevfvc,
         renaltx6m, gfr, uprt_crt,
         fbscan)
  # 2094 participants

mental <- read_csv("../data/alive_mental.csv") %>% 
  filter(year(visdate) %in% 2009:2019) %>% 
  select(id, visdate, deptx6m, emeddep, cesdtot)

drug <- read_csv("../data/alive_drug.csv") %>% 
  filter(year(visdate) %in% 2009:2019) %>% 
  select(id, visdate, alcheavy, alcmoderate, ivstat, anntivwomj, cigyn)

hiv <- read_csv("../data/alive_hiv.csv") %>% 
  filter(year(visdate) %in% 2009:2019) %>% 
  select(id, visdate, hiv, hcvvis)

care <- read_csv(file="../data/alive_care.csv") %>% 
  filter(year(visdate) %in% 2009:2019) %>% 
  select(id, visdate, samedoc)


# Manage data -------------------------------------------------------------

# Grab baseline demographic variables
socdem.base <- socdem %>% 
  filter(!duplicated(id)) %>% 
  select(id, m0f1, black)

# Merge data 
chronic2 <- chronic %>% 
  left_join(base, by="id") %>% 
  left_join(socdem.base, by="id") %>% 
  left_join(mental, by=c("id", "visdate")) %>% 
  left_join(hiv, by=c("id", "visdate")) %>% 
  left_join(select(socdem, id, age, visdate, work, anyhomels), by=c("id", "visdate")) %>% 
  left_join(care, by=c("id", "visdate")) %>% 
  left_join(drug, by=c("id", "visdate")) %>% 
  filter(between(age, 30, 70))
  # 1821 participants
  
# If participant had no visit for >1 year, censor
chronic3 <- chronic2 %>% 
  group_by(id) %>% 
  mutate(yrs_between = (lag(visdate) %--% visdate)/years(1),
         flag = ifelse(is.na(yrs_between), 0, as.numeric(yrs_between>1)),
         cumflag = cumsum(flag)) %>% 
  filter(cumflag==0) %>% 
  # Now count # of visits
  mutate(visnum = row_number()) %>% 
  select(-c(flag, yrs_between, cumflag))
  # 1821 participants

# Inclusion criterium: at least 2 visits
exclude <- chronic3 %>%
  filter(!duplicated(id, fromLast=T),
         visnum<2)

chronic4 <- chronic3 %>%
  filter(!(id %in% exclude$id))
  # 1503 participants


# Output data -------------------------------------------------------------

write_csv(chronic4, "../data/alive_chronic_long.csv")


  