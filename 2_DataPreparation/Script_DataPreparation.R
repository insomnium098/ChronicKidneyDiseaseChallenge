####Antonio Daniel Martinez Gutierrez
###Chronic Kidney Disease Challenge

library(dplyr)

response <- read.csv("../1_OriginalDatasets/T_stage.csv")
dbp <- read.csv("../1_OriginalDatasets/T_DBP.csv")
sbp <- read.csv("../1_OriginalDatasets/T_SBP.csv")
demo <- read.csv("../1_OriginalDatasets/T_demo.csv")
ldl <- read.csv("../1_OriginalDatasets/T_ldl.csv")
hgb <- read.csv("../1_OriginalDatasets/T_HGB.csv")
creatinine <- read.csv("../1_OriginalDatasets/T_creatinine.csv")
meds <- read.csv("../1_OriginalDatasets/T_meds.csv")
glucose <- read.csv("../1_OriginalDatasets/T_glucose.csv")
###We first verify that all the patients were measured all the variables

length(unique(response$id)) == length(unique(dbp$id))
length(unique(response$id)) == length(unique(sbp$id))
length(unique(response$id)) == length(unique(demo$id))
length(unique(response$id)) == length(unique(ldl$id))
length(unique(response$id)) == length(unique(hgb$id))
length(unique(response$id)) == length(unique(creatinine$id))
length(unique(response$id)) == length(unique(glucose$id))
###It seems that meds has only been measured to 272 out of 300 
###patients, we will take care of that later.
length(unique(response$id)) == length(unique(glucose$id))

###We will process each dataset, making a time window of 30 days
###for each patient


meds$durationDose <- abs(meds$end_day - meds$start_day)

window <- 30
for (i in unique(meds$id)){ 
  message(i)
  ##First we obtain the patient drugs data
  df_filt <- filter(meds, id == i)
  maxDay <- max(df_filt$end_day)
  
  ##We calculate how many windows could be fitted in the period
  numbWindows <- ceiling(maxDay/window)
  
  counter <- 0
  for (partitionWindow in 1:numbWindows){
    ##We filter the data that falls between the window
    df_filt2 <- df_filt[df_filt$start_day < (counter + window) &
                          df_filt$end_day > counter
                      ,]
    
    ##Case when the patient has no data in the window
    if(nrow(df_filt2) == 0){
      counter <- counter + window
      next
    }
    
    ##We add the number of drugs taken in the same window
    df_filt2$WindowNumberDrugs <- length(unique(df_filt2$drug))
    df_filt2$WindowCombinationDrugs <- paste(unique(df_filt2$drug), collapse = "-")
    ###We add the combinationof drugs
    
    
    df_filt2$Window <- paste0(counter ,"-" ,counter + window)
    
    if(!exists("df_final")){
      df_final <- df_filt2
    } else{
      df_final <- rbind(df_final, df_filt2)
    }
    
    counter = counter + window
  }
  
  
  
  if(!exists("final")){
    final <- df_final
  } else{
    final <- rbind(final, df_final)
  }
  
  rm(df_final)
  

  
  }









