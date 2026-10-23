
###########################################################################
#
# Project: Prevalence of multimorbidity
#
# Purpose: Run sensitivity analyses
#
# Author: Jacqueline Rudolph
#
# Last Update: 01 Apr 2026
#
###########################################################################


packages <- c("tidyverse", "lubridate", "survival", "broom", "splines", 
              "scales", "nnet", "zoo", "parallel")

for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package, repos='http://lib.stat.cmu.edu/R/CRAN')
  }
  library(package, character.only=T)
}

nboot <- 500

sens <- "alt-liver2" # all-control, alt-htn, alt-dep, alt-renal, alt-renal2, alt-liver, alt-liver2

M <- 30

for (m in 1:M) {

  
# Read in data ------------------------------------------------------------

dat <- read_csv("../data/alive_chronic_imputed_setup.csv") %>% 
  filter(rep==m) %>% 
  filter(between(age, 30, 70))

if (sens=="all-control") {
  dat$delta <- dat$state_all
} else if (sens=="alt-lung") {
  dat$delta <- dat$state2
} else if (sens=="alt-htn") {
  dat$delta <- dat$state3
} else if (sens=="alt-dep") {
  dat$delta <- dat$state4
} else if (sens=="alt-renal") {
  dat$delta <- dat$state5
} else if (sens=="alt-renal2") {
  dat$delta <- dat$state6
} else if (sens=="alt-liver") {
  dat$delta <- dat$state7
} else if (sens=="alt-liver2") {
  dat$delta <- dat$state8
}

base <- filter(dat, !duplicated(id))


# Function to run analysis ------------------------------------------------

run_models <- function(data, group) {
  
  if (group=="Overall") {
    temp <- data
    formula <- I(drop1==0) ~ bs(age, df=3) + black + m0f1 + beduchs + hiv + hcvvis + 
                             work + samedoc + anyhomels + ivstat + alc + cigyn
    ages <- tibble(age = unique(dat$age)) %>% 
      arrange(age)

  } else if (group=="HIV") {
    temp <- filter(data, hiv_base==1)
    formula <- I(drop1==0) ~ bs(age, df=3) + black + m0f1 + beduchs + hcvvis + 
                             work + samedoc + anyhomels + ivstat + alc + cigyn
    ages <- tibble(age = unique(dat$age[dat$hiv_base==1])) %>% 
      arrange(age)   
    
  } else if (group=="No HIV") {
    temp <- filter(data, hiv_base==0)
    formula <- I(drop1==0) ~ bs(age, df=3) + black + m0f1 + beduchs + hcvvis + 
                             work + samedoc + anyhomels + ivstat + alc + cigyn
    ages <- tibble(age = unique(dat$age[dat$hiv_base==0])) %>% 
      arrange(age)   
    
  } else if (group=="Female") {
    temp <- filter(data, m0f1==1)
    formula <- I(drop1==0) ~ bs(age, df=3) + black + beduchs + hiv + hcvvis + 
                             work + samedoc + anyhomels + ivstat + alc + cigyn
    ages <- tibble(age = unique(dat$age[dat$m0f1==1])) %>% 
      arrange(age)   
    
  } else if (group=="Male") {
    temp <- filter(data, m0f1==0)
    formula <- I(drop1==0) ~ bs(age, df=3) + black + beduchs + hiv + hcvvis + 
                             work + samedoc + anyhomels + ivstat + alc + cigyn
    ages <- tibble(age = unique(dat$age[dat$m0f1==0])) %>% 
      arrange(age)   
    
  } else if (group=="Black") {
    temp <- filter(data, black==1)
    formula <- I(drop1==0) ~ bs(age, df=3) + m0f1 + beduchs + hiv + hcvvis + 
                             work + samedoc + anyhomels + ivstat + alc + cigyn
    ages <- tibble(age = unique(dat$age[dat$black==1])) %>% 
      arrange(age)   
    
  } else if (group=="Other Race") {
    temp <- filter(data, black==0)
    formula <- I(drop1==0) ~ bs(age, df=3) + m0f1 + beduchs + hiv + hcvvis + 
                             work + samedoc + anyhomels + ivstat + alc + cigyn
    ages <- tibble(age = unique(dat$age[dat$black==0])) %>% 
      arrange(age)   
    
  }
  
  
# Censoring weights -------------------------------------------------------
  
  # Death is not included in drop out
  den1 <- glm(formula,
              family=binomial(link="logit"), data=temp)$fitted.values
  num1 <- glm(I(drop1==0) ~ bs(age, df=3),
              family=binomial(link="logit"), data=temp)$fitted.values
  temp$wt1 <- den1/num1
  
  # Death is included in drop out (this made no difference in findings overall)
  # den2 <- glm(I(drop2==0) ~ bs(age, df=3) + black + m0f1 + beduchs + hiv + hcvvis + 
  #               work + samedoc + anyhomels + ivstat + alc + cigyn,
  #             family=binomial(link="logit"), data=temp)$fitted.values
  # num2 <- glm(I(drop2==0) ~ bs(age, df=3),
  #             family=binomial(link="logit"), data=temp)$fitted.values
  # temp$wt2 <- den2/num2
  
  # Cumulative weights
  temp2 <- temp %>% 
    group_by(bid) %>% 
    mutate(ipcw1 = cumprod(wt1))
  
  
# Model multimorbidity ----------------------------------------------------
  
  # Unweighted
  temp3 <- temp2 %>% 
    filter(delta!=4) 
  mod <- multinom(factor(delta) ~ bs(age, knots=seq(40, 70, 5), degree=1), data=temp3)
  
  new.dat <- tibble(age = unique(temp3$age)) %>% 
    arrange(age)
  mod.pred <- predict(mod, newdata=new.dat, type="probs")
  crude <- bind_cols(new.dat, mod.pred) %>% 
    rename(p1_crude = `1`,
           p2_crude = `2`,
           p3_crude = `3`) 
  
  # Weight 1 (death is not drop out)
  mod <- multinom(factor(delta) ~ bs(age, knots=seq(40, 70, 5), degree=1), data=temp3, weights=ipcw1)
  
  new.dat <- tibble(age = unique(temp3$age)) %>% 
    arrange(age)
  mod.pred <- predict(mod, newdata=new.dat, type="probs")
  wt1 <- bind_cols(new.dat, mod.pred) %>% 
    rename(p1_wt1 = `1`,
           p2_wt1 = `2`,
           p3_wt1 = `3`) 
  
  # Weight 2 (death is drop out)
  # mod <- multinom(factor(delta) ~ bs(age, knots=seq(40, 70, 5), degree=1), data=temp3, weights=ipcw2)
  # 
  # new.dat <- tibble(age = unique(temp3$age)) %>% 
  #   arrange(age)
  # mod.pred <- predict(mod, newdata=new.dat, type="probs")
  # wt2 <- bind_cols(new.dat, mod.pred) %>% 
  #   rename(p1_wt2 = `1`,
  #          p2_wt2 = `2`,
  #          p3_wt2 = `3`) 
  
  state.probs <- crude %>% 
    left_join(wt1, by="age") 
  
  
# Model death -------------------------------------------------------------
  
  # Unweighted survival
  surv1 <- tidy(survfit(Surv(age0, age, death) ~ 1, data=temp2)) %>% 
    rename(age = time,
           surv_crude = estimate) %>% 
    select(age, surv_crude)
  
  # Weighted survival
  surv2 <- tidy(survfit(Surv(age0, age, death) ~ 1, data=temp2, weights=wt1)) %>% 
    rename(age = time,
           surv_wt1 = estimate) %>% 
    select(age, surv_wt1)
  
  surv <- left_join(surv1, surv2, by="age")
  
  
# Combine results ---------------------------------------------------------
  
  # Merge by age
  res <- ages %>% 
    left_join(state.probs, by="age") %>% 
    left_join(surv, by="age")
  res$surv_crude[1] <- 1
  res$surv_wt1[1] <- 1
  
  res2 <- res %>% 
    na.locf(na.rm=F) %>% 
    na.locf(fromLast=T) %>% 
    mutate(p1_crude_s = p1_crude*surv_crude,
           p2_crude_s = p2_crude*surv_crude,
           p3_crude_s = p3_crude*surv_crude,
           
           p1_wt1_s = p1_wt1*surv_wt1,
           p2_wt1_s = p2_wt1*surv_wt1,
           p3_wt1_s = p3_wt1*surv_wt1,
           subset = group)
  
  return(res2)
  
  
}

# Bootstrap resample ------------------------------------------------------

boot_rep <- function(r) {
  
  set.seed(r)
  
  samp <- table(base[sample(1:nrow(base), nrow(base), replace=T), (names(base)=="id")])
  
  boot <- NULL
  if (r==0) {
    boot <- dat %>%
      rename(bid = id)
  } else {
    for(zzz in 1:max(samp)){
      cc <- dat[dat$id %in% names(samp[samp %in% c(zzz:max(samp))]),]
      cc$bid <- paste0(cc$id, zzz)
      boot <- rbind(boot, cc)
    }
    boot <- select(boot, -id)
  }
  
 est1 <- run_models(boot, group = "Overall")
 est2 <- run_models(boot, group = "HIV")
 est3 <- run_models(boot, group = "No HIV")
 est4 <- run_models(boot, group = "Female") 
 est5 <- run_models(boot, group = "Male")
 est6 <- run_models(boot, group = "Black")
 est7 <- run_models(boot, group = "Other Race")
 
 est <- bind_rows(est1, est2, est3, est4, est5, est6, est7) %>% 
   mutate(boot = r)
 
 return(est)
 
}

# Overall analysis
c <- detectCores() - 2
all.res <- mclapply(0:nboot, function(x) {boot_rep(x)}, mc.set.seed=F, mc.cores=c)
#all.res <- lapply(0:nboot, function(x) {boot_rep(x)})
all.res <- bind_rows(all.res)

res <- filter(all.res, boot==0)

res.summ <- all.res %>% 
  group_by(subset, age) %>% 
  summarize(across(everything(), ~ sd(.x), .names="{col}_se"))

res <- left_join(res, res.summ, by=c("subset", "age")) %>% 
  mutate(rep = m)

write_csv(res, paste0("../results/res_", sens, "_", m, ".csv"))

}