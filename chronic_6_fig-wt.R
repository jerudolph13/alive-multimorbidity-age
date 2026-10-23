
###########################################################################
#
# Project: Prevalence of multimorbidity
#
# Purpose: Visualize weighted results
#
# Author: Jacqueline Rudolph
#
# Last Update: 29 Sep 2025
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
  geom_ribbon(aes(ymin=p1_wt1_avg - 1.96*p1_wt1_stderr, 
                  ymax=p1_wt1_avg + 1.96*p1_wt1_stderr, 
                  fill="No Multimorbidity"), alpha=0.5) +
  geom_ribbon(aes(ymin=p2_wt1_avg - 1.96*p2_wt1_stderr, 
                  ymax=p2_wt1_avg + 1.96*p2_wt1_stderr, 
                  fill="Multimorbidity, Majority Controlled"), alpha=0.5) +
  geom_ribbon(aes(ymin=p3_wt1_avg - 1.96*p3_wt1_stderr, 
                  ymax=p3_wt1_avg + 1.96*p3_wt1_stderr, 
                  fill="Multimorbidity, Majority Not Controlled"), alpha=0.5) +
  geom_line(aes(y=p1_wt1_avg, color="No Multimorbidity"), linewidth=0.75) +
  geom_line(aes(y=p2_wt1_avg, color="Multimorbidity, Majority Controlled"), linewidth=0.75) +
  geom_line(aes(y=p3_wt1_avg, color="Multimorbidity, Majority Not Controlled"), linewidth=0.75) +
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

jpeg(paste0("../figures/wt_overall_multi.jpeg"), height=5, width=7, units="in", res=300)
print(p)
dev.off()

# Mortality
p <- ggplot(filter(res, subset=="Overall"), aes(x=age)) +
  labs(x="Age", y="Incidence", color="", fill="") +
  geom_ribbon(aes(ymax=1 - (surv_wt1_avg - 1.96*surv_wt1_stderr), 
                  ymin=1 - (surv_wt1_avg + 1.96*surv_wt1_stderr)), alpha=0.5) +
  geom_line(aes(y=1 - surv_wt1_avg), linewidth=0.75) +
  scale_x_continuous(expand=c(0,0), breaks=seq(30, 70, 5), limits=c(30, 70)) +
  scale_y_continuous(labels=percent) +
  coord_cartesian(ylim=c(0, 1), expand=F) + 
  guides(color=guide_legend(ncol=2), fill=guide_legend(ncol=2)) +
  thm

jpeg(paste0("../figures/wt_overall_death.jpeg"), height=5, width=5, units="in", res=300)
print(p)
dev.off()

# By HIV status
p1 <- ggplot(filter(res, subset %in% c("HIV", "No HIV")), aes(x=age, linetype=subset)) +
  labs(title="Multimorbidity, Majority Controlled", 
       x="Age", y="Prevalence", color="", fill="", linetype="") +
  geom_ribbon(aes(ymin=p2_wt1_avg - 1.96*p2_wt1_stderr, 
                  ymax=p2_wt1_avg + 1.96*p2_wt1_stderr, 
                  fill=subset), alpha=0.5) +
  geom_line(aes(y=p2_wt1_avg, color=subset), linewidth=0.75) +
  scale_color_brewer(palette="Set1") +
  scale_fill_brewer(palette="Set1") +
  scale_x_continuous(expand=c(0,0), breaks=seq(30, 70, 5), limits=c(30, 70)) +
  scale_y_continuous(labels=percent) +
  coord_cartesian(ylim=c(0, 1), expand=F) + 
  thm

p2 <- ggplot(filter(res, subset %in% c("HIV", "No HIV")), aes(x=age, linetype=subset)) +
  labs(title="Multimorbidity, Majority Not Controlled", 
       x="Age", y="Prevalence", color="", fill="", linetype="") +
  geom_ribbon(aes(ymin=p3_wt1_avg - 1.96*p2_wt1_stderr, 
                  ymax=p3_wt1_avg + 1.96*p2_wt1_stderr, 
                  fill=subset), alpha=0.5) +
  geom_line(aes(y=p3_wt1_avg, color=subset), linewidth=0.75) +
  scale_color_brewer(palette="Set1") +
  scale_fill_brewer(palette="Set1") +
  scale_x_continuous(expand=c(0,0), breaks=seq(30, 70, 5), limits=c(30, 70)) +
  scale_y_continuous(labels=percent) +
  coord_cartesian(ylim=c(0, 1), expand=F) + 
  thm

p3 <- ggplot(filter(res, subset %in% c("HIV", "No HIV")), aes(x=age, linetype=subset)) +
  labs(title="No Multimorbidity", 
       x="Age", y="Prevalence", color="", fill="", linetype="") +
  geom_ribbon(aes(ymin=p1_wt1_avg - 1.96*p2_wt1_stderr, 
                  ymax=p1_wt1_avg + 1.96*p2_wt1_stderr, 
                  fill=subset), alpha=0.5) +
  geom_line(aes(y=p1_wt1_avg, color=subset), linewidth=0.75) +
  scale_color_brewer(palette="Set1") +
  scale_fill_brewer(palette="Set1") +
  scale_x_continuous(expand=c(0,0), breaks=seq(30, 70, 5), limits=c(30, 70)) +
  scale_y_continuous(labels=percent) +
  coord_cartesian(ylim=c(0, 1), expand=F) + 
  thm

p4 <- ggplot(filter(res, subset %in% c("HIV", "No HIV")), aes(x=age, linetype=subset)) +
  labs(title="Death",
       x="Age", y="Incidence", color="", fill="", linetype="") +
  geom_ribbon(aes(ymax=1 - (surv_wt1_avg - 1.96*surv_wt1_stderr), 
                  ymin=1 - (surv_wt1_avg + 1.96*surv_wt1_stderr),
                  fill=subset), alpha=0.5) +
  geom_line(aes(y=1 - surv_wt1_avg, color=subset), linewidth=0.75) +
  scale_x_continuous(expand=c(0,0), breaks=seq(30, 70, 5), limits=c(30, 70)) +
  scale_color_brewer(palette="Set1") +
  scale_fill_brewer(palette="Set1") +
  scale_y_continuous(labels=percent) +
  coord_cartesian(ylim=c(0, 1), expand=F) + 
  guides(color=guide_legend(ncol=2), fill=guide_legend(ncol=2)) +
  thm

p <- p1 + p2 + p3 + p4 + plot_layout(nrow=2, ncol=2, guides="collect") & thm

jpeg(paste0("../figures/wt_hiv_multi.jpeg"), height=10, width=10, units="in", res=300)
print(p)
dev.off()

# Race
# p1 <- ggplot(filter(res, subset %in% c("Black", "Other Race")), aes(x=age, linetype=subset)) +
#   labs(title="Multimorbidity, Majority Controlled", 
#        x="Age", y="Prevalence", color="", fill="", linetype="") +
#   geom_ribbon(aes(ymin=p2_wt1_avg - 1.96*p2_wt1_stderr, 
#                   ymax=p2_wt1_avg + 1.96*p2_wt1_stderr, 
#                   fill=subset), alpha=0.5) +
#   geom_line(aes(y=p2_wt1_avg, color=subset), linewidth=0.75) +
#   scale_color_brewer(palette="Set1") +
#   scale_fill_brewer(palette="Set1") +
#   scale_x_continuous(expand=c(0,0), breaks=seq(30, 70, 5), limits=c(30, 70)) +
#   scale_y_continuous(labels=percent) +
#   coord_cartesian(ylim=c(0, 1), expand=F) + 
#   thm
# 
# p2 <- ggplot(filter(res, subset %in% c("Black", "Other Race")), aes(x=age, linetype=subset)) +
#   labs(title="Multimorbidity, Majority Not Controlled", 
#        x="Age", y="Prevalence", color="", fill="", linetype="") +
#   geom_ribbon(aes(ymin=p3_wt1_avg - 1.96*p2_wt1_stderr, 
#                   ymax=p3_wt1_avg + 1.96*p2_wt1_stderr, 
#                   fill=subset), alpha=0.5) +
#   geom_line(aes(y=p3_wt1_avg, color=subset), linewidth=0.75) +
#   scale_color_brewer(palette="Set1") +
#   scale_fill_brewer(palette="Set1") +
#   scale_x_continuous(expand=c(0,0), breaks=seq(30, 70, 5), limits=c(30, 70)) +
#   scale_y_continuous(labels=percent) +
#   coord_cartesian(ylim=c(0, 1), expand=F) + 
#   thm
# 
# p3 <- ggplot(filter(res, subset %in% c("Black", "Other Race")), aes(x=age, linetype=subset)) +
#   labs(title="No Multimorbidity", 
#        x="Age", y="Prevalence", color="", fill="", linetype="") +
#   geom_ribbon(aes(ymin=p1_wt1_avg - 1.96*p2_wt1_stderr, 
#                   ymax=p1_wt1_avg + 1.96*p2_wt1_stderr, 
#                   fill=subset), alpha=0.5) +
#   geom_line(aes(y=p1_wt1_avg, color=subset), linewidth=0.75) +
#   scale_color_brewer(palette="Set1") +
#   scale_fill_brewer(palette="Set1") +
#   scale_x_continuous(expand=c(0,0), breaks=seq(30, 70, 5), limits=c(30, 70)) +
#   scale_y_continuous(labels=percent) +
#   coord_cartesian(ylim=c(0, 1), expand=F) + 
#   thm
# 
# p4 <- ggplot(filter(res, subset %in% c("Black", "Other Race")), aes(x=age, linetype=subset)) +
#   labs(title="Death",
#        x="Age", y="Prevalence", color="", fill="", linetype="") +
#   geom_ribbon(aes(ymax=1 - (surv_wt1_avg - 1.96*surv_wt1_stderr), 
#                   ymin=1 - (surv_wt1_avg + 1.96*surv_wt1_stderr),
#                   fill=subset), alpha=0.5) +
#   geom_line(aes(y=1 - surv_wt1_avg, color=subset), linewidth=0.75) +
#   scale_x_continuous(expand=c(0,0), breaks=seq(30, 70, 5), limits=c(30, 70)) +
#   scale_color_brewer(palette="Set1") +
#   scale_fill_brewer(palette="Set1") +
#   scale_y_continuous(labels=percent) +
#   coord_cartesian(ylim=c(0, 1), expand=F) + 
#   guides(color=guide_legend(ncol=2), fill=guide_legend(ncol=2)) +
#   thm
# 
# p <- p1 + p2 + p3 + p4 + plot_layout(nrow=2, ncol=2, guides="collect") & thm
# 
# jpeg(paste0("../figures/wt_race_multi.jpeg"), height=10, width=10, units="in", res=300)
# print(p)
# dev.off()

# Sex
# p1 <- ggplot(filter(res, subset %in% c("Female", "Male")), aes(x=age, linetype=subset)) +
#   labs(title="Multimorbidity, Majority Controlled", 
#        x="Age", y="Prevalence", color="", fill="", linetype="") +
#   geom_ribbon(aes(ymin=p2_wt1_avg - 1.96*p2_wt1_stderr, 
#                   ymax=p2_wt1_avg + 1.96*p2_wt1_stderr, 
#                   fill=subset), alpha=0.5) +
#   geom_line(aes(y=p2_wt1_avg, color=subset), linewidth=0.75) +
#   scale_color_brewer(palette="Set1") +
#   scale_fill_brewer(palette="Set1") +
#   scale_x_continuous(expand=c(0,0), breaks=seq(30, 70, 5), limits=c(30, 70)) +
#   scale_y_continuous(labels=percent) +
#   coord_cartesian(ylim=c(0, 1), expand=F) + 
#   thm
# 
# p2 <- ggplot(filter(res, subset %in% c("Female", "Male")), aes(x=age, linetype=subset)) +
#   labs(title="Multimorbidity, Majority Not Controlled", 
#        x="Age", y="Prevalence", color="", fill="", linetype="") +
#   geom_ribbon(aes(ymin=p3_wt1_avg - 1.96*p2_wt1_stderr, 
#                   ymax=p3_wt1_avg + 1.96*p2_wt1_stderr, 
#                   fill=subset), alpha=0.5) +
#   geom_line(aes(y=p3_wt1_avg, color=subset), linewidth=0.75) +
#   scale_color_brewer(palette="Set1") +
#   scale_fill_brewer(palette="Set1") +
#   scale_x_continuous(expand=c(0,0), breaks=seq(30, 70, 5), limits=c(30, 70)) +
#   scale_y_continuous(labels=percent) +
#   coord_cartesian(ylim=c(0, 1), expand=F) + 
#   thm
# 
# p3 <- ggplot(filter(res, subset %in% c("Female", "Male")), aes(x=age, linetype=subset)) +
#   labs(title="No Multimorbidity", 
#        x="Age", y="Prevalence", color="", fill="", linetype="") +
#   geom_ribbon(aes(ymin=p1_wt1_avg - 1.96*p2_wt1_stderr, 
#                   ymax=p1_wt1_avg + 1.96*p2_wt1_stderr, 
#                   fill=subset), alpha=0.5) +
#   geom_line(aes(y=p1_wt1_avg, color=subset), linewidth=0.75) +
#   scale_color_brewer(palette="Set1") +
#   scale_fill_brewer(palette="Set1") +
#   scale_x_continuous(expand=c(0,0), breaks=seq(30, 70, 5), limits=c(30, 70)) +
#   scale_y_continuous(labels=percent) +
#   coord_cartesian(ylim=c(0, 1), expand=F) + 
#   thm
# 
# p4 <- ggplot(filter(res, subset %in% c("Female", "Male")), aes(x=age, linetype=subset)) +
#   labs(title="Death",
#        x="Age", y="Prevalence", color="", fill="", linetype="") +
#   geom_ribbon(aes(ymax=1 - (surv_wt1_avg - 1.96*surv_wt1_stderr), 
#                   ymin=1 - (surv_wt1_avg + 1.96*surv_wt1_stderr),
#                   fill=subset), alpha=0.5) +
#   geom_line(aes(y=1 - surv_wt1_avg, color=subset), linewidth=0.75) +
#   scale_x_continuous(expand=c(0,0), breaks=seq(30, 70, 5), limits=c(30, 70)) +
#   scale_color_brewer(palette="Set1") +
#   scale_fill_brewer(palette="Set1") +
#   scale_y_continuous(labels=percent) +
#   coord_cartesian(ylim=c(0, 1), expand=F) + 
#   guides(color=guide_legend(ncol=2), fill=guide_legend(ncol=2)) +
#   thm
# 
# p <- p1 + p2 + p3 + p4 + plot_layout(nrow=2, ncol=2, guides="collect") & thm
# 
# jpeg(paste0("../figures/wt_sex_multi.jpeg"), height=10, width=10, units="in", res=300)
# print(p)
# dev.off()


# Separate conditions -----------------------------------------------------

# for (outcome in c("htn", "diab", "lung", "renal", "liver", "dep")) {
#   
#   res <- read_csv(paste0("../results/res_main_", outcome, ".csv"))
#   
#   # Overall
#   p <- ggplot(filter(res, subset=="Overall"), aes(x=age)) +
#     labs(x="Age", y="Prevalence", color="", fill="") +
#     geom_ribbon(aes(ymin=p1_wt1_avg - 1.96*p1_wt1_stderr, 
#                     ymax=p1_wt1_avg + 1.96*p1_wt1_stderr, 
#                     fill="Condition-Free"), alpha=0.5) +
#     geom_ribbon(aes(ymin=p2_wt1_avg - 1.96*p2_wt1_stderr, 
#                     ymax=p2_wt1_avg + 1.96*p2_wt1_stderr, 
#                     fill="Condition Managed"), alpha=0.5) +
#     geom_ribbon(aes(ymin=p3_wt1_avg - 1.96*p3_wt1_stderr, 
#                     ymax=p3_wt1_avg + 1.96*p3_wt1_stderr, 
#                     fill="Condition Not Managed"), alpha=0.5) +
#     geom_line(aes(y=p1_wt1_avg, color="Condition-Free"), linewidth=0.75) +
#     geom_line(aes(y=p2_wt1_avg, color="Condition Managed"), linewidth=0.75) +
#     geom_line(aes(y=p3_wt1_avg, color="Condition Not Managed"), linewidth=0.75) +
#     scale_color_manual(values=c("Condition Managed" = "#1b9e77", 
#                                 "Condition Not Managed" = "#d95f02", 
#                                 "Condition-Free" = "#7570b3"),
#                        breaks=c("Condition-Free", "Condition Managed",
#                                 "Condition Not Managed", "Death")) +
#     scale_fill_manual(values=c("Condition Managed" = "#1b9e77", 
#                                "Condition Not Managed" = "#d95f02", 
#                                "Condition-Free" = "#7570b3"),
#                       breaks=c("Condition-Free", "Condition Managed",
#                                "Condition Not Managed", "Death")) +
#     scale_x_continuous(expand=c(0,0), breaks=seq(30, 70, 5), limits=c(30, 70)) +
#     coord_cartesian(ylim=c(0, 1), expand=F) + 
#     guides(color=guide_legend(ncol=2), fill=guide_legend(ncol=2)) +
#     thm
#   
#   jpeg(paste0("../figures/wt_overall_", outcome, ".jpeg"), height=5, width=5, units="in", res=300)
#   print(p)
#   dev.off()
#   
#   # By HIV status
#   p1 <- ggplot(filter(res, subset %in% c("HIV", "No HIV")), aes(x=age, linetype=subset)) +
#     labs(title="Condition Managed", 
#          x="Age", y="Prevalence", color="", fill="", linetype="") +
#     geom_ribbon(aes(ymin=p2_wt1_avg - 1.96*p2_wt1_stderr, 
#                     ymax=p2_wt1_avg + 1.96*p2_wt1_stderr, 
#                     fill=subset), alpha=0.5) +
#     geom_line(aes(y=p2_wt1_avg, color=subset), linewidth=0.75) +
#     scale_color_brewer(palette="Set1") +
#     scale_fill_brewer(palette="Set1") +
#     scale_x_continuous(expand=c(0,0), breaks=seq(30, 70, 5), limits=c(30, 70)) +
#     coord_cartesian(ylim=c(0, 1), expand=F) + 
#     thm
#   
#   p2 <- ggplot(filter(res, subset %in% c("HIV", "No HIV")), aes(x=age, linetype=subset)) +
#     labs(title="Condition Not Managed", 
#          x="Age", y="Prevalence", color="", fill="", linetype="") +
#     geom_ribbon(aes(ymin=p3_wt1_avg - 1.96*p2_wt1_stderr, 
#                     ymax=p3_wt1_avg + 1.96*p2_wt1_stderr, 
#                     fill=subset), alpha=0.5) +
#     geom_line(aes(y=p3_wt1_avg, color=subset), linewidth=0.75) +
#     scale_color_brewer(palette="Set1") +
#     scale_fill_brewer(palette="Set1") +
#     scale_x_continuous(expand=c(0,0), breaks=seq(30, 70, 5), limits=c(30, 70)) +
#     coord_cartesian(ylim=c(0, 1), expand=F) + 
#     thm
#   
#   p3 <- ggplot(filter(res, subset %in% c("HIV", "No HIV")), aes(x=age, linetype=subset)) +
#     labs(title="Condition-Free", 
#          x="Age", y="Prevalence", color="", fill="", linetype="") +
#     geom_ribbon(aes(ymin=p1_wt1_avg - 1.96*p2_wt1_stderr, 
#                     ymax=p1_wt1_avg + 1.96*p2_wt1_stderr, 
#                     fill=subset), alpha=0.5) +
#     geom_line(aes(y=p1_wt1_avg, color=subset), linewidth=0.75) +
#     scale_color_brewer(palette="Set1") +
#     scale_fill_brewer(palette="Set1") +
#     scale_x_continuous(expand=c(0,0), breaks=seq(30, 70, 5), limits=c(30, 70)) +
#     coord_cartesian(ylim=c(0, 1), expand=F) + 
#     thm
#   
#   p <- p1 + p2 + p3 + plot_layout(nrow=1, ncol=3, guides="collect") & thm
#   
#   jpeg(paste0("../figures/wt_hiv_", outcome, ".jpeg"), height=5, width=10, units="in", res=300)
#   print(p)
#   dev.off()
#   
#   # Race
#   p1 <- ggplot(filter(res, subset %in% c("Black", "Other Race")), aes(x=age, linetype=subset)) +
#     labs(title="Condition Managed", 
#          x="Age", y="Prevalence", color="", fill="", linetype="") +
#     geom_ribbon(aes(ymin=p2_wt1_avg - 1.96*p2_wt1_stderr, 
#                     ymax=p2_wt1_avg + 1.96*p2_wt1_stderr, 
#                     fill=subset), alpha=0.5) +
#     geom_line(aes(y=p2_wt1_avg, color=subset), linewidth=0.75) +
#     scale_color_brewer(palette="Set1") +
#     scale_fill_brewer(palette="Set1") +
#     scale_x_continuous(expand=c(0,0), breaks=seq(30, 70, 5), limits=c(30, 70)) +
#     coord_cartesian(ylim=c(0, 1), expand=F) + 
#     thm
#   
#   p2 <- ggplot(filter(res, subset %in% c("Black", "Other Race")), aes(x=age, linetype=subset)) +
#     labs(title="Condition Not Managed", 
#          x="Age", y="Prevalence", color="", fill="", linetype="") +
#     geom_ribbon(aes(ymin=p3_wt1_avg - 1.96*p2_wt1_stderr, 
#                     ymax=p3_wt1_avg + 1.96*p2_wt1_stderr, 
#                     fill=subset), alpha=0.5) +
#     geom_line(aes(y=p3_wt1_avg, color=subset), linewidth=0.75) +
#     scale_color_brewer(palette="Set1") +
#     scale_fill_brewer(palette="Set1") +
#     scale_x_continuous(expand=c(0,0), breaks=seq(30, 70, 5), limits=c(30, 70)) +
#     coord_cartesian(ylim=c(0, 1), expand=F) + 
#     thm
#   
#   p3 <- ggplot(filter(res, subset %in% c("Black", "Other Race")), aes(x=age, linetype=subset)) +
#     labs(title="Condition-Free", 
#          x="Age", y="Prevalence", color="", fill="", linetype="") +
#     geom_ribbon(aes(ymin=p1_wt1_avg - 1.96*p2_wt1_stderr, 
#                     ymax=p1_wt1_avg + 1.96*p2_wt1_stderr, 
#                     fill=subset), alpha=0.5) +
#     geom_line(aes(y=p1_wt1_avg, color=subset), linewidth=0.75) +
#     scale_color_brewer(palette="Set1") +
#     scale_fill_brewer(palette="Set1") +
#     scale_x_continuous(expand=c(0,0), breaks=seq(30, 70, 5), limits=c(30, 70)) +
#     coord_cartesian(ylim=c(0, 1), expand=F) + 
#     thm
#   
#   p <- p1 + p2 + p3 + plot_layout(nrow=1, ncol=3, guides="collect") & thm
#   
#   jpeg(paste0("../figures/wt_race_", outcome, ".jpeg"), height=5, width=10, units="in", res=300)
#   print(p)
#   dev.off()
#   
#   
#   # Sex
#   p1 <- ggplot(filter(res, subset %in% c("Female", "Male")), aes(x=age, linetype=subset)) +
#     labs(title="Condition Managed", 
#          x="Age", y="Prevalence", color="", fill="", linetype="") +
#     geom_ribbon(aes(ymin=p2_wt1_avg - 1.96*p2_wt1_stderr, 
#                     ymax=p2_wt1_avg + 1.96*p2_wt1_stderr, 
#                     fill=subset), alpha=0.5) +
#     geom_line(aes(y=p2_wt1_avg, color=subset), linewidth=0.75) +
#     scale_color_brewer(palette="Set1") +
#     scale_fill_brewer(palette="Set1") +
#     scale_x_continuous(expand=c(0,0), breaks=seq(30, 70, 5), limits=c(30, 70)) +
#     coord_cartesian(ylim=c(0, 1), expand=F) + 
#     thm
#   
#   p2 <- ggplot(filter(res, subset %in% c("Female", "Male")), aes(x=age, linetype=subset)) +
#     labs(title="Condition Not Managed", 
#          x="Age", y="Prevalence", color="", fill="", linetype="") +
#     geom_ribbon(aes(ymin=p3_wt1_avg - 1.96*p2_wt1_stderr, 
#                     ymax=p3_wt1_avg + 1.96*p2_wt1_stderr, 
#                     fill=subset), alpha=0.5) +
#     geom_line(aes(y=p3_wt1_avg, color=subset), linewidth=0.75) +
#     scale_color_brewer(palette="Set1") +
#     scale_fill_brewer(palette="Set1") +
#     scale_x_continuous(expand=c(0,0), breaks=seq(30, 70, 5), limits=c(30, 70)) +
#     coord_cartesian(ylim=c(0, 1), expand=F) + 
#     thm
#   
#   p3 <- ggplot(filter(res, subset %in% c("Female", "Male")), aes(x=age, linetype=subset)) +
#     labs(title="Condition-Free", 
#          x="Age", y="Prevalence", color="", fill="", linetype="") +
#     geom_ribbon(aes(ymin=p1_wt1_avg - 1.96*p2_wt1_stderr, 
#                     ymax=p1_wt1_avg + 1.96*p2_wt1_stderr, 
#                     fill=subset), alpha=0.5) +
#     geom_line(aes(y=p1_wt1_avg, color=subset), linewidth=0.75) +
#     scale_color_brewer(palette="Set1") +
#     scale_fill_brewer(palette="Set1") +
#     scale_x_continuous(expand=c(0,0), breaks=seq(30, 70, 5), limits=c(30, 70)) +
#     coord_cartesian(ylim=c(0, 1), expand=F) + 
#     thm
#   
#   p <- p1 + p2 + p3 + plot_layout(nrow=1, ncol=3, guides="collect") & thm
#   jpeg(paste0("../figures/wt_sex_", outcome, ".jpeg"), height=5, width=10, units="in", res=300)
#   print(p)
#   dev.off()
#   
# }
# 
# 
