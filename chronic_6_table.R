
###########################################################################
#
# Project: Prevalence of multimorbidity
#
# Purpose: Tabulate results
#
# Author: Jacqueline Rudolph
#
# Last Update: 01 Apr 2026
#
###########################################################################

packages <- c("tidyverse", "tableone")

for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package, repos='http://lib.stat.cmu.edu/R/CRAN')
  }
  library(package, character.only=T)
}

M <- 30


# Read in results ---------------------------------------------------------

dat <- read_csv("../data/alive_chronic_imputed_setup.csv") 
res <- read_csv("../results/res_main_multi.csv") 
sens1 <- read_csv("../results/res_all-control.csv") 
sens2 <- read_csv("../results/res_alt-htn.csv") 
sens3 <- read_csv("../results/res_alt-renal.csv") 
sens4 <- read_csv("../results/res_alt-renal2.csv")
sens5 <- read_csv("../results/res_alt-liver.csv") 
sens6 <- read_csv("../results/res_alt-liver2.csv") 
sens7 <- read_csv("../results/res_alt-dep.csv") 


# Mortality results -------------------------------------------------------

death <- select(res, subset, age, surv_crude_avg)
  # Overall:
  1 - min(death$surv_crude_avg[death$subset=="Overall"])
  death$age[death$surv_crude_avg<0.5 & death$subset=="Overall"][1]
  
  # PWH:
  1 - min(death$surv_crude_avg[death$subset=="HIV"])
  death$age[death$surv_crude_avg<0.5 & death$subset=="HIV"][1]

  # PWoH:
  1 - min(death$surv_crude_avg[death$subset=="No HIV"])
  death$age[death$surv_crude_avg<0.5 & death$subset=="No HIV"][1]
  
  
# Tabulate data -----------------------------------------------------------

dat <- dat %>% 
  mutate(age_cat = cut(age, breaks=c(30, 40, 45, 50, 55, 60, 70), right=F),
         multi = as.numeric(n_cond>=2),
         multi_control = ifelse(multi==0, NA, as.numeric(most_control==1)),
         multi_control2 = ifelse(multi==0, NA, as.numeric(all_control==1))) %>% 
  filter(state!=4)

# Baseline characteristics (within first imputation)
first <- filter(dat, 
                rep==1,
                !duplicated(id))

t <- print(
  CreateTableOne(vars=c("age", "black", "m0f1", "beduchs", "anyhomels", "work", "samedoc", "hiv",
                        "cigyn", "alc", "ivstat", "anntivwomj"),
                 strata="age_cat",
                 data=first,
                 factorVars=c("black", "m0f1", "beduchs", "anyhomels", "work", "samedoc", "hiv",
                              "cigyn", "alc", "ivstat", "anntivwomj"),
                 test=F, addOverall=T),
                 nonnormal="age")

write.csv(as.data.frame(t), "../results/table2.csv")

# Visit-specific characteristics (within first imputation)
visit <- filter(dat, 
                rep==1)

t <- print(
  CreateTableOne(vars=c("black", "m0f1", "beduchs", "anyhomels", "work", "samedoc", "hiv",
                        "cigyn", "alc", "ivstat", "anntivwomj"),
                 strata="age_cat",
                 data=visit,
                 factorVars=c("black", "m0f1", "beduchs", "anyhomels", "work", "samedoc", "hiv",
                              "cigyn", "alc", "ivstat", "anntivwomj"),
                 test=F, addOverall=T))

write.csv(as.data.frame(t), "../results/table_s1.csv")

# Baseline characteristics by HIV status (within first imputation)
t <- print(
  CreateTableOne(vars=c("age", "age_cat", "black", "m0f1", "beduchs", "anyhomels", "work", "samedoc", "hiv",
                        "cigyn", "alc", "ivstat", "anntivwomj"),
                 strata="hiv",
                 data=first,
                 factorVars=c("age_cat", "black", "m0f1", "beduchs", "anyhomels", "work", "samedoc", "hiv",
                              "cigyn", "alc", "ivstat", "anntivwomj"),
                 test=F, addOverall=T),
  nonnormal="age", contDigits=1)

write.csv(as.data.frame(t), "../results/table_s2_hiv.csv")

# Baseline characteristics by race
# t <- print(
#   CreateTableOne(vars=c("age", "age_cat", "black", "m0f1", "beduchs", "anyhomels", "work", "samedoc", "hiv",
#                         "cigyn", "alc", "ivstat", "anntivwomj"),
#                  strata="black",
#                  data=first,
#                  factorVars=c("age_cat", "black", "m0f1", "beduchs", "anyhomels", "work", "samedoc", "hiv",
#                               "cigyn", "alc", "ivstat", "anntivwomj"),
#                  test=F, addOverall=T),
#   nonnormal="age", contDigits=1)
# 
# write.csv(as.data.frame(t), "../results/table_s2_race.csv")

# Baseline characteristics by sex
# t <- print(
#   CreateTableOne(vars=c("age", "age_cat", "black", "m0f1", "beduchs", "anyhomels", "work", "samedoc", "hiv",
#                         "cigyn", "alc", "ivstat", "anntivwomj"),
#                  strata="m0f1",
#                  data=first,
#                  factorVars=c("age_cat", "black", "m0f1", "beduchs", "anyhomels", "work", "samedoc", "hiv",
#                               "cigyn", "alc", "ivstat", "anntivwomj"),
#                  test=F, addOverall=T),
#   nonnormal="age", contDigits=1)
# 
# write.csv(as.data.frame(t), "../results/table_s2_sex.csv")

# Conditions by age category (averaged across imputations)
cond <- dat %>% 
  group_by(id, visdate) %>% 
  summarize(across(c("hiv", "n_cond", "multi", "multi_control", "multi_control2",
                     "htn", "htn_control", "diab", "diab_control",
                     "lung", "lung_control", "renal", "renal_control", 
                     "liver", "liver_control", "dep", "dep_control"), ~ mean(.x, na.rm=T)),
            age_cat = first(age_cat)) %>% 
  mutate(across(everything(), ~ifelse(is.nan(.x), NA, .x)))

t <- print(
  CreateTableOne(vars=c("n_cond", "multi", "multi_control", "multi_control2",
                        "htn", "htn_control", "diab", "diab_control",
                        "lung", "lung_control", "renal", "renal_control", 
                        "liver", "liver_control", "dep", "dep_control"),
               strata="age_cat",
               data=cond,
               test=F, addOverall=T),
  contDigits=3,
  nonnormal="n_cond")

write.csv(as.data.frame(t), "../results/table3.csv")

# PWH
hiv <- filter(cond, hiv==1)
t2 <- print(
  CreateTableOne(vars=c("n_cond", "multi", "multi_control", "multi_control2",
                        "htn", "htn_control", "diab", "diab_control",
                        "lung", "lung_control", "renal", "renal_control", 
                        "liver", "liver_control", "dep", "dep_control"),
                 strata="age_cat",
                 data=hiv,
                 test=F, addOverall=T),
  contDigits=3,
  nonnormal="n_cond")

write.csv(as.data.frame(t2), "../results/table3_hiv.csv")

# PWoH
nohiv <- filter(cond, hiv==0)
t3 <- print(
  CreateTableOne(vars=c("n_cond", "multi", "multi_control", "multi_control2",
                        "htn", "htn_control", "diab", "diab_control",
                        "lung", "lung_control", "renal", "renal_control", 
                        "liver", "liver_control", "dep", "dep_control"),
                 strata="age_cat",
                 data=nohiv,
                 test=F, addOverall=T),
  contDigits=3,
  nonnormal="n_cond")

write.csv(as.data.frame(t3), "../results/table3_nohiv.csv")


# Tabulate findings -------------------------------------------------------

sens_table <- NULL

for (analysis in c("main", "sens1", "sens2", "sens3",
                   "sens4", "sens5", "sens6", "sens7")) {

  if (analysis=="main") {
    temp <- res
  } else if (analysis=="sens1") {
    temp <- sens1
  } else if (analysis=="sens2") {
    temp <- sens2
  } else if (analysis=="sens3") {
    temp <- sens3
  } else if (analysis=="sens4") {
    temp <- sens4
  } else if (analysis=="sens5") {
    temp <- sens5
  } else if (analysis=="sens6") {
    temp <- sens6
  } else if (analysis=="sens7") {
    temp <- sens7
  } 
  
# No multimorbidity
table1 <- filter(temp, subset=="Overall") %>% 
  mutate(age = floor(age),
         p1_lower = (p1_crude_avg - 1.96*p1_crude_stderr)*100,
         p1_upper = (p1_crude_avg + 1.96*p1_crude_stderr)*100,
         p1_crude = p1_crude_avg*100,
         no_multi = paste0(round(p1_crude, digits=1), " (",
                           round(p1_lower, digits=1), ", ",
                           round(p1_upper, digits=1), ")")) %>% 
  filter(!duplicated(age, fromLast=T)) %>% 
  filter(age %in% seq(30, 65, 5)) %>% 
  select(age, no_multi)

# Controlled multimorbidity
table2 <- filter(temp, subset=="Overall") %>% 
  mutate(age = floor(age),
         p2_lower = (p2_crude_avg - 1.96*p2_crude_stderr)*100,
         p2_upper = (p2_crude_avg + 1.96*p2_crude_stderr)*100,
         p2_crude = p2_crude_avg*100,
         control = paste0(round(p2_crude, digits=1), " (",
                           round(p2_lower, digits=1), ", ",
                           round(p2_upper, digits=1), ")")) %>% 
  filter(!duplicated(age, fromLast=T)) %>% 
  filter(age %in% seq(30, 65, 5)) %>% 
  select(age, control)

# Uncontrolled multimorbidity
table3 <- filter(temp, subset=="Overall") %>% 
  mutate(age = floor(age),
         p3_lower = (p3_crude_avg - 1.96*p3_crude_stderr)*100,
         p3_upper = (p3_crude_avg + 1.96*p3_crude_stderr)*100,
         p3_crude = p3_crude_avg*100,
         uncontrol = paste0(round(p3_crude, digits=1), " (",
                           round(p3_lower, digits=1), ", ",
                           round(p3_upper, digits=1), ")")) %>% 
  filter(!duplicated(age, fromLast=T)) %>% 
  filter(age %in% seq(30, 65, 5)) %>% 
  select(age, uncontrol)

sens <- left_join(table1, table2, by="age") %>% 
  left_join(table3, by="age") %>% 
  mutate(analysis = analysis)

sens_table <- bind_rows(sens_table, sens)

}

write_csv(sens_table, "../results/sens_table.csv")
