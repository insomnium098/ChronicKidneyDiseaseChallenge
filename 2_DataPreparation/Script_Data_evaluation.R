library(data.table)
source("Script_Utils.R")
####We will get by dataset, the mean time difference of the
#### evaluations

dbp <- read.csv("../1_OriginalDatasets/T_DBP.csv")
sbp <- read.csv("../1_OriginalDatasets/T_SBP.csv")
ldl <- read.csv("../1_OriginalDatasets/T_ldl.csv")
hgb <- read.csv("../1_OriginalDatasets/T_HGB.csv")
creatinine <- read.csv("../1_OriginalDatasets/T_creatinine.csv")
meds <- read.csv("../1_OriginalDatasets/T_meds.csv")
glucose <- read.csv("../1_OriginalDatasets/T_glucose.csv")

#######

glucose <- eval_times(glucose)

##Mean of glucose : 110 days
mean(glucose$diff)


dbp <- eval_times(dbp)

##Mean of dbp : 99 days
mean(dbp$diff)


sbp <- eval_times(sbp)

##Mean of sbp : 99 days
mean(sbp$diff)


ldl <- eval_times(ldl)

##Mean of ldl : 135 days
mean(ldl$diff)

hgb <- eval_times(hgb)

##Mean of hgb : 167 days
mean(hgb$diff)

creatinine <- eval_times(creatinine)

##Mean of creatinine : 120 days
mean(creatinine$diff)



meds$duration <- meds$end_day - meds$start_day

##Mean of meds duration: 79 days
mean(meds$duration)






