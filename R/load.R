#### First time only: Load data, fully anonymise, then save in data folder ####
# librarian::shelf(readxl)
# dat <- readxl::read_xlsx("../NTM final dataset NR.xlsx")
# dat.time.to.infect <- readxl::read_xlsx("../NTM group July 23.xlsx", range="B1:C10")
# colnames(dat.time.to.infect) <- c("hospno", "infect.day")
# dat <- merge(x=dat, y=dat.time.to.infect, all=T)
# dat[, c(1, ncol(dat))]
# dat <- dat[-1]
# write.csv(dat, file="data/abscessus_data.csv", )

