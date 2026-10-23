
###########################################################################
#
# Project: Prevalence of multimorbidity
#
# Purpose: Impute data
#
# Author: Jacqueline Rudolph
#
# Last Update: 10 Sep 2025
#
###########################################################################


packages <- c("tidyverse", "data.table", "lubridate", "tidyselect", "mice", "missForest", "parallel", "doParallel")
for (package in packages) {
  if (!(package %in% installed.packages())) {
    install.packages(package)
  }
  library(package, character.only=T, quietly=T)
}

M1 <- 26  # Imputation start
M2 <- 30 # Imputation end


# Read in data ------------------------------------------------------------

dat <- read_csv("../data/alive_chronic_long.csv") 


# Manage data -------------------------------------------------------------

dat2 <- dat %>% 
  mutate(alc = case_when(is.na(alcheavy) & is.na(alcmoderate) ~ NA_real_,
                         alcheavy==1 ~ 2,
                         alcmoderate==1 ~ 1,
                         T ~ 0),
         
         # Set outliers >99th percentile to missing
         bmimedh = ifelse(bmimedh>47.34, NA, bmimedh),
         uprt_crt = ifelse(uprt_crt>3640, NA, uprt_crt),
         fbscan = ifelse(fbscan>51, NA, fbscan),
         
         # Log transform troublesome continuous variables
         uprt_crt = log(uprt_crt),
         fbscan = log(fbscan),
         
         # Make categorical variables factors
         across(!c(id, visdate, visnum, bpsys, bpdias, hba1c, bmimedh, cesdtot, maxfev, maxfvc,
                   fev1pp, llnfevfvc, gfr, uprt_crt, fbscan, age), ~ as.factor(.x))) %>% 
  select(-c(alcmoderate, alcheavy))


# Impute data -------------------------------------------------------------

set.seed(123 + M1)
registerDoParallel(cores=4)

impute.dat <- function(i) {
  
  # Baseline variables
  base <- dat2 %>% 
    select(id, m0f1, black, beduchs) %>% 
    filter(!duplicated(id))
  
  # Summary comorbidity variables to be included in each model
  summ <- dat2 %>% 
    select(id, bpsys, bpdias, cesdtot, uprt_crt, gfr, hba1c, maxfev, maxfvc, fbscan) %>% 
    group_by(id) %>% 
    summarize(max_bpsys = max(bpsys, na.rm=T),
              max_bpdias = max(bpdias, na.rm=T),
              max_cesdtot = max(cesdtot, na.rm=T),
              max_uprt_crt = max(uprt_crt, na.rm=T),
              max_hba1c = max(hba1c, na.rm=T),
              min_maxfev = min(maxfev, na.rm=T),
              min_maxfvc = min(maxfvc, na.rm=T),
              max_fbscan = max(fbscan, na.rm=T)) %>% 
    mutate(across(everything(), ~ifelse(is.infinite(.x), NA, .x)))
  
  # Hypertension
  htn <- dat2 %>% 
    select(id, visnum, bpsys, bpdias, hbptx6m, 
           age, hiv, hcvvis, bmimedh, alc, cigyn, ivstat, anntivwomj, samedoc, work, anyhomels)
  wide <- htn %>% 
    pivot_wider(id_cols=id, names_from=visnum, values_from=-c(id, visnum))
  wide2 <- right_join(base, wide, by="id") %>% 
    left_join(select(summ, -c(max_bpdias, max_bpsys)), by="id")
  wide2 <- as.data.frame(wide2)
  
  impute <- missForest(select(wide2, -id), verbose=T, ntree=100, parallelize="forests")
  wide.impute <- impute$ximp
  wide.impute2 <- bind_cols(id=wide2$id, wide.impute)
  
  # Depression
  dep <- dat2 %>% 
    select(id, visnum, cesdtot, deptx6m, emeddep, 
           age, hiv, hcvvis, alc, ivstat, anntivwomj, samedoc, work, anyhomels)
  wide <- dep %>% 
    pivot_wider(id_cols=id, names_from=visnum, values_from=-c(id, visnum))
  wide2 <- right_join(base, wide, by="id") %>% 
    left_join(select(summ, -max_cesdtot), by="id")
  wide2 <- as.data.frame(wide2)
  
  impute <- missForest(select(wide2, -id), verbose=T, ntree=100, parallelize="forests")
  wide.impute <- impute$ximp 
  merge <- wide.impute %>% 
    select(vars_select(names(wide.impute), starts_with(c("cesdtot", "deptx6m"))))
  wide.impute2 <- bind_cols(wide.impute2, merge)
  
  # Renal disease
  renal <- dat2 %>% 
    select(id, visnum, uprt_crt, gfr, 
           age, hiv, hcvvis, alc, ivstat, anntivwomj, samedoc, work, anyhomels)
  wide <- renal %>% 
    pivot_wider(id_cols=id, names_from=visnum, values_from=-c(id, visnum))
  wide2 <- right_join(base, wide, by="id") %>% 
    left_join(select(summ, -max_uprt_crt), by="id")
  wide2 <- as.data.frame(wide2)
  
  impute <- missForest(select(wide2, -id), verbose=T, ntree=100, parallelize="forests")
  wide.impute <- impute$ximp
  merge <- wide.impute %>% 
    select(vars_select(names(wide.impute), starts_with(c("uprt_crt", "gfr"))))
  wide.impute2 <- bind_cols(wide.impute2, merge)
  
  # Diabetes
  diab <- dat2 %>% 
    select(id, visnum, hba1c, diabtx6m, 
           age, hiv, hcvvis, bmimedh, alc, ivstat, anntivwomj, samedoc, work, anyhomels)
  wide <- diab %>% 
    pivot_wider(id_cols=id, names_from=visnum, values_from=-c(id, visnum))
  wide2 <- right_join(base, wide, by="id") %>% 
    left_join(select(summ, -max_hba1c), by="id")
  wide2 <- as.data.frame(wide2)
  
  impute <- missForest(select(wide2, -id), verbose=T, ntree=100, parallelize="forests")
  wide.impute <- impute$ximp
  merge <- wide.impute %>% 
    select(vars_select(names(wide.impute), starts_with(c("hba1c", "diabtx6m"))))
  wide.impute2 <- bind_cols(wide.impute2, merge)
  
  # Lung disease
  lung <- dat2 %>% 
    select(id, visnum, maxfev, maxfvc, fev1pp, llnfevfvc, wrsbrttx,
           age, hiv, hcvvis, alc, cigyn, ivstat, anntivwomj, samedoc, work, anyhomels)
  wide <- lung %>% 
    pivot_wider(id_cols=id, names_from=visnum, values_from=-c(id, visnum))
  wide2 <- right_join(base, wide, by="id") %>% 
    left_join(select(summ, -c(min_maxfev, min_maxfvc)), by="id")
  wide2 <- as.data.frame(wide2)
  
  impute <- missForest(select(wide2, -id), verbose=T, ntree=100, parallelize="forests")
  wide.impute <- impute$ximp
  merge <- wide.impute %>% 
    select(vars_select(names(wide.impute), starts_with(c("maxfev", "maxfvc", "fev1pp", "llnfevfvc", "wrsbrttx"))))
  wide.impute2 <- bind_cols(wide.impute2, merge)
  
  # Liver disease
  liver <- dat2 %>% 
    select(id, visnum, fbscan, 
           age, hiv, hcvvis, bmimedh, alc, ivstat, anntivwomj, samedoc, work, anyhomels)
  wide <- liver %>% 
    pivot_wider(id_cols=id, names_from=visnum, values_from=-c(id, visnum))
  wide2 <- right_join(base, wide, by="id") %>% 
    left_join(select(summ, -max_fbscan), by="id")
  wide2 <- as.data.frame(wide2)
  
  impute <- missForest(select(wide2, -id), verbose=T, ntree=100, parallelize="forests")
  wide.impute <- impute$ximp
  merge <- wide.impute %>% 
    select(vars_select(names(wide.impute), starts_with(c("fbscan"))))
  wide.impute2 <- bind_cols(wide.impute2, merge)

  # Make data long
  long.impute <- wide.impute2 %>% 
    select(vars_select(names(wide.impute2), !starts_with(c("max_", "min_")))) %>% 
    pivot_longer(cols=!c(id, m0f1, black, beduchs), 
                 names_pattern="(.*)_(.*)",
                 names_to=c(".value", "visnum")) %>% 
    mutate(visnum=as.numeric(visnum))
  
  # Keep imputed values from observed visits
  dat.imp <- dat2 %>% 
    select(id, visnum, visdate) %>% 
    left_join(long.impute, by=c("id", "visnum")) 
  
  output <- dat.imp %>% 
    mutate(rep = i)
  
  return(output)
}

dat.imp <- lapply(M1:M2, function(x){impute.dat(i=x)})
dat.imp <- bind_rows(dat.imp)


# Output data -------------------------------------------------------------

write_csv(dat.imp, paste0("../data/alive_chronic_imputed", M2, ".csv"))

