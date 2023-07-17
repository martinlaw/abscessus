#### Load packages and data ####
librarian::shelf(survival, survminer,here)
dat <- read.csv("data/abscessus_data.csv")


#### Data manipulation/cleaning ####
dat$infected <- ifelse(test=dat$abscessuss_yes_1, yes="Infected", no="Not infected")
dat$alive <- ifelse(test=dat$alive_yes_1, yes="Alive", no="Not alive")
dat$lung.dysfun <- ifelse(test=dat$CLAD_yes_1, yes="Yes", no="No")
dat$airway.injury <- ifelse(test=dat$airway_injury_yes_1, yes="Injury", no="No Injury")


names(dat)[names(dat)=="survival_post_tx"] <- "time"
dat$status <- -1*(dat$alive_yes_1-1)


#### Begin with 2x2 table, treating survival as binary ####
# ORs or RRs? See
# https://stats.stackexchange.com/questions/326259/odds-ratios-are-inappropriate-for-a-cross-sectional-or-cohort-study
# https://stats.stackexchange.com/questions/466450/can-someone-explain-as-simply-as-possible-why-you-use-or-in-retrospective-studie

con <- table(dat$infected, dat$alive)
addmargins(con)
fisher.test(con) # No difference between the groups

table(dat$alive, dat$infected, dat$lung.dysfun)

#### Survival analysis: ####
# Simple explanation: https://www.graphpad.com/guides/survival-analysis
# https://www.emilyzabor.com/tutorials/survival_analysis_in_r_tutorial.html
# https://rpkgs.datanovia.com/survminer/


# Log-rank test:
fit1 <- survdiff(Surv(time, status) ~ infected, data = dat)
fit1$pvalue

# Plot:
fit <- survfit(Surv(time, status) ~ infected, data = dat)
fit
ggsurvplot(fit, data=dat)

# Cox PH:
cox.no.covar <- coxph(Surv(time, status) ~ infected, data = dat)



#### Lung dysfunction ####
con.lung <- table(dat$infected, dat$lung.dysfun)
addmargins(con.lung)
fisher.test(con.lung) # No difference between the groups

# Cox PH:
cox.covar <- coxph(Surv(time, status) ~ infected + lung.dysfun, data = dat)




#### Airway injury ####
con.airway <- table(dat$infected, dat$airway.injury)
addmargins(con.airway)
fisher.test(con.airway) # No difference between the groups



