librarian::shelf(readxl, survival, survminer)
getwd()
dat <- readxl::read_xlsx("data/NTM final dataset NR.xlsx")
dat

#### Begin with 2x2 table, treating survival as binary ####
# ORs or RRs? See
# https://stats.stackexchange.com/questions/326259/odds-ratios-are-inappropriate-for-a-cross-sectional-or-cohort-study
# https://stats.stackexchange.com/questions/466450/can-someone-explain-as-simply-as-possible-why-you-use-or-in-retrospective-studie

dat$infected <- ifelse(test=dat$abscessuss_yes_1, yes="Infected", no="Not infected")
dat$alive <- ifelse(test=dat$alive_yes_1, yes="Alive", no="Not alive")

con <- table(dat$infected, dat$alive)
addmargins(con)

# Risk of death:
# infected:
risk.if.infected <- 2/9  # 0.22
risk.if.not.infected <- 16/59 # 0.27
# Risk of death is greater in uninfected patients.
# Relative risk:
risk.if.infected/risk.if.not.infected # 0.82.

fisher.test(con) # No difference between the groups


#### Survival analysis: ####
# https://www.emilyzabor.com/tutorials/survival_analysis_in_r_tutorial.html
# https://rpkgs.datanovia.com/survminer/

head(dat)
names(dat)[names(dat)=="survival_post_tx"] <- "time"
dat$status <- -1*(dat$alive_yes_1-1)

fit1 <- survdiff(Surv(time, status) ~ infected, data = dat)
str(fit1)
fit1$p

fit <- survfit(Surv(time, status) ~ infected, data = dat)
fit
ggsurvplot(fit, data=dat)

