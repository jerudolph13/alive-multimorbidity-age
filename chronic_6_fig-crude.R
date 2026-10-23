
###########################################################################
#
# Project: Prevalence of multimorbidity
#
# Purpose: Visualize crude results
#
# Author: Jacqueline Rudolph
#
# Last Update: 01 Apr 2026
#
###########################################################################


packages <- c("tidyverse", "patchwork", "scales")

for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package, repos='http://lib.stat.cmu.edu/R/CRAN')
  }
  library(package, character.only=T)
}

thm <- theme_bw() +
  theme(axis.text = element_text(color="black", size=11),
        axis.title = element_text(color="black", size=12),
        plot.title = element_text(color="black", size=12),
        legend.text = element_text(color="black", size=11),
        legend.position = "bottom")


# Multimorbidity ----------------------------------------------------------

res <- read_csv(paste0("../results/res_main_multi.csv"))

# Overall
p <- ggplot(filter(res, subset=="Overall"), aes(x=age)) +
  labs(x="Age", y="Prevalence", color="", fill="") +
  geom_ribbon(aes(ymin=p1_crude_avg- 1.96*p1_crude_stderr, 
                  ymax=p1_crude_avg+ 1.96*p1_crude_stderr, 
                  fill="No Multimorbidity"), alpha=0.5) +
  geom_ribbon(aes(ymin=p2_crude_avg- 1.96*p2_crude_stderr, 
                  ymax=p2_crude_avg+ 1.96*p2_crude_stderr, 
                  fill="Multimorbidity, Majority Controlled"), alpha=0.5) +
  geom_ribbon(aes(ymin=p3_crude_avg- 1.96*p3_crude_stderr, 
                  ymax=p3_crude_avg+ 1.96*p3_crude_stderr, 
                  fill="Multimorbidity, Majority Not Controlled"), alpha=0.5) +
  geom_line(aes(y=p1_crude_avg, color="No Multimorbidity"), linewidth=0.75) +
  geom_line(aes(y=p2_crude_avg, color="Multimorbidity, Majority Controlled"), linewidth=0.75) +
  geom_line(aes(y=p3_crude_avg, color="Multimorbidity, Majority Not Controlled"), linewidth=0.75) +
  scale_color_manual(values=c("Multimorbidity, Majority Controlled" = "#1b9e77", 
                              "Multimorbidity, Majority Not Controlled" = "#d95f02", 
                              "No Multimorbidity" = "#7570b3"),
                     breaks=c("No Multimorbidity", "Multimorbidity, Majority Controlled",
                              "Multimorbidity, Majority Not Controlled", "Death")) +
  scale_fill_manual(values=c("Multimorbidity, Majority Controlled" = "#1b9e77", 
                             "Multimorbidity, Majority Not Controlled" = "#d95f02", 
                             "No Multimorbidity" = "#7570b3"),
                    breaks=c("No Multimorbidity", "Multimorbidity, Majority Controlled",
                             "Multimorbidity, Majority Not Controlled", "Death")) +
  scale_x_continuous(expand=c(0,0), breaks=seq(30, 70, 5), limits=c(30, 70)) +
  scale_y_continuous(labels=percent) +
  coord_cartesian(ylim=c(0, 1), expand=F) + 
  guides(color=guide_legend(ncol=2), fill=guide_legend(ncol=2)) +
  thm

jpeg(paste0("../figures/crude_overall_multi.jpeg"), height=5, width=7, units="in", res=300)
print(p)
dev.off()

# Mortality
p <- ggplot(filter(res, subset=="Overall"), aes(x=age)) +
  labs(x="Age", y="Incidence", color="", fill="") +
  geom_ribbon(aes(ymax=1 - (surv_crude_avg- 1.96*surv_crude_stderr), 
                  ymin=1 - (surv_crude_avg+ 1.96*surv_crude_stderr)), alpha=0.5) +
  geom_line(aes(y=1 - surv_crude_avg), linewidth=0.75) +
  scale_x_continuous(expand=c(0,0), breaks=seq(30, 70, 5), limits=c(30, 70)) +
  scale_y_continuous(labels=percent) +
  coord_cartesian(ylim=c(0, 1), expand=F) + 
  guides(color=guide_legend(ncol=2), fill=guide_legend(ncol=2)) +
  thm

jpeg(paste0("../figures/crude_overall_death.jpeg"), height=5, width=5, units="in", res=300)
print(p)
dev.off()

# By HIV status
p1 <- ggplot(filter(res, subset %in% c("HIV", "No HIV")), aes(x=age, linetype=subset)) +
  labs(title="Multimorbidity, Majority Controlled", 
       x="Age", y="Prevalence", color="", fill="", linetype="") +
  geom_ribbon(aes(ymin=p2_crude_avg- 1.96*p2_crude_stderr, 
                  ymax=p2_crude_avg+ 1.96*p2_crude_stderr, 
                  fill=subset), alpha=0.5) +
  geom_line(aes(y=p2_crude_avg, color=subset), linewidth=0.75) +
  scale_color_brewer(palette="Set1") +
  scale_fill_brewer(palette="Set1") +
  scale_x_continuous(expand=c(0,0), breaks=seq(30, 70, 5), limits=c(30, 70)) +
  scale_y_continuous(labels=percent) +
  coord_cartesian(ylim=c(0, 1), expand=F) + 
  thm

p2 <- ggplot(filter(res, subset %in% c("HIV", "No HIV")), aes(x=age, linetype=subset)) +
  labs(title="Multimorbidity, Majority Not Controlled", 
       x="Age", y="Prevalence", color="", fill="", linetype="") +
  geom_ribbon(aes(ymin=p3_crude_avg- 1.96*p2_crude_stderr, 
                  ymax=p3_crude_avg+ 1.96*p2_crude_stderr, 
                  fill=subset), alpha=0.5) +
  geom_line(aes(y=p3_crude_avg, color=subset), linewidth=0.75) +
  scale_color_brewer(palette="Set1") +
  scale_fill_brewer(palette="Set1") +
  scale_x_continuous(expand=c(0,0), breaks=seq(30, 70, 5), limits=c(30, 70)) +
  scale_y_continuous(labels=percent) +
  coord_cartesian(ylim=c(0, 1), expand=F) + 
  thm

p3 <- ggplot(filter(res, subset %in% c("HIV", "No HIV")), aes(x=age, linetype=subset)) +
  labs(title="No Multimorbidity", 
       x="Age", y="Prevalence", color="", fill="", linetype="") +
  geom_ribbon(aes(ymin=p1_crude_avg- 1.96*p2_crude_stderr, 
                  ymax=p1_crude_avg+ 1.96*p2_crude_stderr, 
                  fill=subset), alpha=0.5) +
  geom_line(aes(y=p1_crude_avg, color=subset), linewidth=0.75) +
  scale_color_brewer(palette="Set1") +
  scale_fill_brewer(palette="Set1") +
  scale_x_continuous(expand=c(0,0), breaks=seq(30, 70, 5), limits=c(30, 70)) +
  scale_y_continuous(labels=percent) +
  coord_cartesian(ylim=c(0, 1), expand=F) + 
  thm

p4 <- ggplot(filter(res, subset %in% c("HIV", "No HIV")), aes(x=age, linetype=subset)) +
  labs(title="Death",
       x="Age", y="Incidence", color="", fill="", linetype="") +
  geom_ribbon(aes(ymax=1 - (surv_crude_avg- 1.96*surv_crude_stderr), 
                  ymin=1 - (surv_crude_avg+ 1.96*surv_crude_stderr),
                  fill=subset), alpha=0.5) +
  geom_line(aes(y=1 - surv_crude_avg, color=subset), linewidth=0.75) +
  scale_x_continuous(expand=c(0,0), breaks=seq(30, 70, 5), limits=c(30, 70)) +
  scale_color_brewer(palette="Set1") +
  scale_fill_brewer(palette="Set1") +
  scale_y_continuous(labels=percent) +
  coord_cartesian(ylim=c(0, 1), expand=F) + 
  guides(color=guide_legend(ncol=2), fill=guide_legend(ncol=2)) +
  thm

p <- p1 + p2 + p3 + p4 + plot_layout(nrow=2, ncol=2, guides="collect") & thm

jpeg(paste0("../figures/crude_hiv_multi.jpeg"), height=10, width=10, units="in", res=300)
print(p)
dev.off()


# Separate conditions -----------------------------------------------------

for (outcome in c("htn", "diab", "lung", "renal", "liver", "dep")) {
  
res <- read_csv(paste0("../results/res_main_", outcome, ".csv"))

name <- case_when(outcome=="htn" ~ "Hypertension",
                   outcome=="diab" ~ "Diabetes",
                   outcome=="lung" ~ "Lung Disease",
                   outcome=="renal" ~ "Kidney Disease",
                   outcome=="liver" ~ "Liver Disease",
                   outcome=="dep" ~ "Depression")
  
# Overall
p <- ggplot(filter(res, subset=="Overall"), aes(x=age)) +
  labs(title=name, x="Age", y="Prevalence", color="", fill="") +
  geom_ribbon(aes(ymin=p1_crude_avg- 1.96*p1_crude_stderr, 
                  ymax=p1_crude_avg+ 1.96*p1_crude_stderr, 
                  fill="Condition-Free"), alpha=0.5) +
  geom_ribbon(aes(ymin=p2_crude_avg- 1.96*p2_crude_stderr, 
                  ymax=p2_crude_avg+ 1.96*p2_crude_stderr, 
                  fill="Condition Controlled"), alpha=0.5) +
  geom_ribbon(aes(ymin=p3_crude_avg- 1.96*p3_crude_stderr, 
                  ymax=p3_crude_avg+ 1.96*p3_crude_stderr, 
                  fill="Condition Not Controlled"), alpha=0.5) +
  geom_line(aes(y=p1_crude_avg, color="Condition-Free"), linewidth=0.75) +
  geom_line(aes(y=p2_crude_avg, color="Condition Controlled"), linewidth=0.75) +
  geom_line(aes(y=p3_crude_avg, color="Condition Not Controlled"), linewidth=0.75) +
  scale_color_manual(values=c("Condition Controlled" = "#1b9e77", 
                              "Condition Not Controlled" = "#d95f02", 
                              "Condition-Free" = "#7570b3"),
                     breaks=c("Condition-Free", "Condition Controlled",
                              "Condition Not Controlled", "Death")) +
  scale_fill_manual(values=c("Condition Controlled" = "#1b9e77", 
                             "Condition Not Controlled" = "#d95f02", 
                             "Condition-Free" = "#7570b3"),
                    breaks=c("Condition-Free", "Condition Controlled",
                             "Condition Not Controlled", "Death")) +
  scale_x_continuous(expand=c(0,0), breaks=seq(30, 70, 5), limits=c(30, 70)) +
  scale_y_continuous(labels=percent) +
  coord_cartesian(ylim=c(0, 1), expand=F) + 
  guides(color=guide_legend(ncol=2), fill=guide_legend(ncol=2)) +
  thm

jpeg(paste0("../figures/crude_overall_", outcome, ".jpeg"), height=5, width=5, units="in", res=300)
print(p)
dev.off()

# By HIV status
p1 <- ggplot(filter(res, subset %in% c("HIV", "No HIV")), aes(x=age, linetype=subset)) +
  labs(title="Condition Controlled",
       x="Age", y="Prevalence", color="", fill="", linetype="") +
  geom_ribbon(aes(ymin=p2_crude_avg- 1.96*p2_crude_stderr,
                  ymax=p2_crude_avg+ 1.96*p2_crude_stderr,
                  fill=subset), alpha=0.5) +
  geom_line(aes(y=p2_crude_avg, color=subset), linewidth=0.75) +
  scale_color_brewer(palette="Set1") +
  scale_fill_brewer(palette="Set1") +
  scale_x_continuous(expand=c(0,0), breaks=seq(30, 70, 5), limits=c(30, 70)) +
  scale_y_continuous(labels=percent) +
  coord_cartesian(ylim=c(0, 1), expand=F) +
  thm

p2 <- ggplot(filter(res, subset %in% c("HIV", "No HIV")), aes(x=age, linetype=subset)) +
  labs(title="Condition Not Controlled",
       x="Age", y="Prevalence", color="", fill="", linetype="") +
  geom_ribbon(aes(ymin=p3_crude_avg- 1.96*p2_crude_stderr,
                  ymax=p3_crude_avg+ 1.96*p2_crude_stderr,
                  fill=subset), alpha=0.5) +
  geom_line(aes(y=p3_crude_avg, color=subset), linewidth=0.75) +
  scale_color_brewer(palette="Set1") +
  scale_fill_brewer(palette="Set1") +
  scale_x_continuous(expand=c(0,0), breaks=seq(30, 70, 5), limits=c(30, 70)) +
  scale_y_continuous(labels=percent) +
  coord_cartesian(ylim=c(0, 1), expand=F) +
  thm

p3 <- ggplot(filter(res, subset %in% c("HIV", "No HIV")), aes(x=age, linetype=subset)) +
  labs(title="Condition-Free",
       x="Age", y="Prevalence", color="", fill="", linetype="") +
  geom_ribbon(aes(ymin=p1_crude_avg- 1.96*p2_crude_stderr,
                  ymax=p1_crude_avg+ 1.96*p2_crude_stderr,
                  fill=subset), alpha=0.5) +
  geom_line(aes(y=p1_crude_avg, color=subset), linewidth=0.75) +
  scale_color_brewer(palette="Set1") +
  scale_fill_brewer(palette="Set1") +
  scale_x_continuous(expand=c(0,0), breaks=seq(30, 70, 5), limits=c(30, 70)) +
  scale_y_continuous(labels=percent) +
  coord_cartesian(ylim=c(0, 1), expand=F) +
  thm

p <- p1 + p2 + p3 + plot_layout(nrow=1, ncol=3, guides="collect") & thm

jpeg(paste0("../figures/crude_hiv_", outcome, ".jpeg"), height=5, width=10, units="in", res=300)
print(p)
dev.off()

}

# For legend
res <- read_csv(paste0("../results/res_main_htn.csv"))

# Overall
p <- ggplot(filter(res, subset=="Overall"), aes(x=age)) +
  labs(title=name, x="Age", y="Prevalence", color="", fill="") +
  geom_ribbon(aes(ymin=p1_crude_avg- 1.96*p1_crude_stderr, 
                  ymax=p1_crude_avg+ 1.96*p1_crude_stderr, 
                  fill="Condition-Free"), alpha=0.5) +
  geom_ribbon(aes(ymin=p2_crude_avg- 1.96*p2_crude_stderr, 
                  ymax=p2_crude_avg+ 1.96*p2_crude_stderr, 
                  fill="Condition Controlled"), alpha=0.5) +
  geom_ribbon(aes(ymin=p3_crude_avg- 1.96*p3_crude_stderr, 
                  ymax=p3_crude_avg+ 1.96*p3_crude_stderr, 
                  fill="Condition Not Controlled"), alpha=0.5) +
  geom_line(aes(y=p1_crude_avg, color="Condition-Free"), linewidth=0.75) +
  geom_line(aes(y=p2_crude_avg, color="Condition Controlled"), linewidth=0.75) +
  geom_line(aes(y=p3_crude_avg, color="Condition Not Controlled"), linewidth=0.75) +
  scale_color_manual(values=c("Condition Controlled" = "#1b9e77", 
                              "Condition Not Controlled" = "#d95f02", 
                              "Condition-Free" = "#7570b3"),
                     breaks=c("Condition-Free", "Condition Controlled",
                              "Condition Not Controlled", "Death")) +
  scale_fill_manual(values=c("Condition Controlled" = "#1b9e77", 
                             "Condition Not Controlled" = "#d95f02", 
                             "Condition-Free" = "#7570b3"),
                    breaks=c("Condition-Free", "Condition Controlled",
                             "Condition Not Controlled", "Death")) +
  scale_x_continuous(expand=c(0,0), breaks=seq(30, 70, 5), limits=c(30, 70)) +
  scale_y_continuous(labels=percent) +
  coord_cartesian(ylim=c(0, 1), expand=F) + 
  guides(color=guide_legend(ncol=3), fill=guide_legend(ncol=3)) +
  thm

jpeg(paste0("../figures/wide.jpeg"), height=5, width=10, units="in", res=300)
print(p)
dev.off()