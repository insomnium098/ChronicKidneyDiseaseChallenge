####Antonio Daniel Martinez Gutierrez
###Chronic Kidney Disease Challenge

library(dplyr)
library(plyr)
library(fastDummies)
library(data.table)


source("Script_Utils.R")

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
length(unique(response$id)) == length(unique(meds$id))


##Per patient, we will calculate the max days evaluated in any of the datasets,
### to ease the processing
df_patients <- rbind(dbp, sbp, ldl, hgb, creatinine, glucose)
df_patients <- df_patients[,c(1,3)]
meds_patient <- subset(meds, select = c("id", "end_day"))
colnames(meds_patient)[2] <- "time"

###We will iterate by each patient and get the max day of evaluation

for (i in unique(df_patients$id)){
  max_pat <- max(df_patients[df_patients$id == i,"time"])
  max_pat_df <- as.data.frame(max_pat)
  max_pat_df$id <- i
  max_pat_df <- max_pat_df[,c(2,1)]
  colnames(max_pat_df)[2] <- "max_time"
  if(!exists("df_patient_maxTime")){
    df_patient_maxTime <- max_pat_df
  } else{
    df_patient_maxTime <- rbind(df_patient_maxTime, max_pat_df)
  }
  
  
}

rm(df_patients, max_pat_df)


###We will process each dataset, making a time window of 30 days
###for each patient

############WINDOW PROCESSING FOR MEDS

##Some patients have negative end and start day. We will assume
##that they were receiving the drugs before the diagnosis of CDK
meds$durationDose <- abs(meds$end_day - meds$start_day)

window <- 30

###By iterating all ids to 300 we will take care of patients
###With no drugs
for (i in 0:max(df_patient_maxTime$id)){ 
  message(i)
  ##First we obtain the patient drugs data
  df_filt <- filter(meds, id == i)
  #maxDay <- max(df_filt$end_day)
  maxDay <- df_patient_maxTime[df_patient_maxTime$id == i,"max_time"]
  
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
      df_filt2 <- as.data.frame(i)
      colnames(df_filt2) <- "id"
      df_filt2$drug <- "None"
      df_filt2$daily_dosage <- 0
      df_filt2$start_day <- 0
      df_filt2$end_day <- 0
      df_filt2$durationDose <- 0
      
      
      #counter <- counter + window
      #next
    }
    
    ###In this part, we need to summarize the meds by patient
    
    ##We add the number of drugs taken in the same window
    df_filt2$WindowNumberDrugs <- length(unique(df_filt2$drug))
    ###We add the combination of drugs,  the dosage and  the durationDose per med 
    df_filt2$WindowCombinationDrugs <-paste(c((df_filt2$drug), 
                                              (df_filt2$daily_dosage)), collapse = "-")
    
    
    
    
    
    
    df_filt2$Window <- counter + window#paste0(counter ,"-" ,counter + window)
    df_filt2$TotalDailyDosage <- sum(df_filt2$daily_dosage)
    
    
    
    ##We will merge the drug + daily dosage + duration dose
    df_filt2$Drug_DailyDosage_DurationDose <- 0
    for (ji in 1:nrow(df_filt2)){
      df_filt2[ji,"Drug_DailyDosage_DurationDose"] <- paste(c(df_filt2[ji,"drug"], 
                                                            df_filt2[ji,"daily_dosage"], 
                                                            df_filt2[ji,"durationDose"]), 
                                                            collapse = "-")
      
    }
    
  
    ###Making dummies and summarizing
    df_filt2 <- dummy_cols(df_filt2, 
                              select_columns = c("drug",
                                                 "WindowCombinationDrugs",
                                                 "Drug_DailyDosage_DurationDose"),
                           remove_selected_columns = TRUE)
    
    
    ###As we are summarizing we need to apply different grouping functions
    ###Depending on the columns. However as the drugs columns are variable
    ###this will be done dinamically
    ##the best way is to split in two dataframes and apply each dataframe
    ##their corresponding grouping function
    
    ##First we obtain the columns that are constant
    df_filt2_a <- df_filt2[,1:8]
    ##Next we obtain the columns that will vary between patients and windows
    df_filt2_b <- df_filt2[,c(1,9:ncol(df_filt2))]
    
    ##We group the constant variables
    df_filt2_a <- data.table(df_filt2_a)
    #######
    df_filt2_a <- df_filt2_a[,.(
      start_day=mean(start_day),
      end_day=mean(end_day),
      durationDose = mean(durationDose),
      WindowNumberDrugs = mean(WindowNumberDrugs),
      Window = mean(Window),
      TotalDailyDosage = mean(TotalDailyDosage)
      ),by=.(id)]
    ######
    
    ###Next we group the second df which will be variable
    df_filt2_b <- data.table(df_filt2_b)
    df_filt2_b <- df_filt2_b[,lapply(.SD, sum), by = id]
    
    df_filt2 <- merge(df_filt2_a, df_filt2_b,
                      by = "id")
    rm(df_filt2_a, df_filt2_b)
    
    
    
    
    if(!exists("df_final")){
      df_final <- df_filt2
    } else{
      df_final <- rbind.fill(df_final, df_filt2)
    }
    
    counter = counter + window
  }
  
  
  
  if(!exists("meds_window")){
    meds_window <- df_final
  } else{
    meds_window <- rbind.fill(meds_window, df_final)
  }
  
  rm(df_final)
  

  
}

rm(df_filt, df_filt2)

meds_window[is.na(meds_window)] <- 0



###TODO : RESEARCH EACH DRUG, ADDING clinical information for every
###drug



#################Creatinine
##As with the meds, we will make the df of creatinine by windows
##Since the rest of the dfs share the same structure, we will make
##a generic function to make a dataframe by window
creatinine_window <- makeWindows(30, creatinine, df_patient_maxTime)
colnames(creatinine_window)[2] <- "creatinine_value"

################DBP
dbp_window <- makeWindows(30, dbp, df_patient_maxTime)
colnames(dbp_window)[2] <- "dbp_value"
################Glucose
glucose_window <- makeWindows(30, glucose, df_patient_maxTime)
colnames(glucose_window)[2] <- "glucose_value"
################HGB
hgb_window <- makeWindows(30, hgb, df_patient_maxTime)
colnames(hgb_window)[2] <- "hgb_value"
################LDL
ldl_window <- makeWindows(30, ldl, df_patient_maxTime)
colnames(ldl_window)[2] <- "ldl_value"

################SBP
sbp_window <- makeWindows(30, sbp, df_patient_maxTime)
colnames(sbp_window)[2] <- "sbp_value"


rm(meds, creatinine, dbp, glucose, hgb, ldl, sbp)
########################################


#########Feature Engineering
###For this challenge, the clinical information and management
###of the disease was obtained from the 
##Kidney Disease: Improving Global Outcomes (KDIGO) Guidelines
##obtained at kdigo.org and from the WoltersKluwer UpToDate platform
##at uptodate.com


###Glucose
##It seems that the values provided are in mmol/L.
##According to the guidelines, a blood sugar less than 7.8 mmol/L
##is considered normal, between 7.8 and 11 prediabetes and
##more than 11.1 diabetes.
glucose_window$is_diabetes <- ifelse(glucose_window$glucose_value >= 11.1,
                                     "diabetes",ifelse(glucose_window$glucose_value < 7.8,
                                                       "normal", "prediabetes"))

##We will also measure the difference between the glucose per patient
## and the recommended max level
glucose_window$difference_glucose_normal <- 7.7 - glucose_window$glucose_value
glucose_window <- subset(glucose_window, select = -c(time))
glucose_window <- dummy_columns(glucose_window,
                                select_columns = "is_diabetes",
                                remove_selected_columns  = TRUE)




#####SBP and DBP
##Systolic and Diastolic blood pressure
##According to the guidelines, a high BloodPressure is 
##associated with a higher risk of CDK Progression.
##We will classify patients in th 5 bp categories:
##Normal (sbp less than 120 and dbp less than 80)
##Elevated (sbp 120-129 and dbp less than 80)
##Hypertension1 (sbp 130-139 or dbp 80-80)
##Hypertension2 (sbp >= 140 or dbp >= 90)
##HypertensiveCrisis (sbp > 180 and or dbp > 120)


dbp_window <- subset(dbp_window, select = -c(time))
sbp_window <- subset(sbp_window, select = -c(time))

bp_window <- merge(sbp_window, dbp_window,
                   by = c("id", "Window"))

rm(dbp_window, sbp_window)

bp <- bp_classifier(bp_window)
rm(bp_window)
##We will also calculate the difference between normal levels
##of dbp, sbp and the leveles of the patient
bp$sbp_diff_normal <- 119 - bp$sbp_value
bp$dbp_diff_normal <- 79 - bp$dbp_value


bp <- dummy_columns(bp,select_columns = c("bp_class",
                                          "bp_is_controlled"),
                                remove_selected_columns  = TRUE)
#Optimal BP control to 140/90 mm Hg or 130/80

#####LDL
##According to the CKD guidelines, higher levels of
##cholesterol is associated with a higher mortality,
## duration and severity of hypertension, among others.
##We will classify the patients in groups depending
##their LDL levels:
##Less than 100mg/dl : Normal
##100-129mg/dl : NearNormal
##130-159mg/dl : BorderlineHigh
##160-189mg/dl : High (Hypercholesterolemia)
##190mg/dL : VeryHigh

ldl <- ldl_class(ldl_window)
ldl <- subset(ldl, select = -c(time))
#We also add how much patients LDL levels differ from normal
ldl$diff_Ldl_normal <- 99 - ldl$ldl_value
ldl <- dummy_columns(ldl,select_columns = c("LDL_class"),
                    remove_selected_columns  = TRUE)
rm(ldl_window)

######HGB
###The value of hemoglobin is used to measure if the patient
###in anemic. Due to the range of the values, we will assume
###that the units are in g/dL
###Normal values:
###Womans: 	12.3 - 15.3  g/dL
###Mans: 14.0 - 17.5 g/dL

hgb_vals <- merge(hgb_window, demo,
                  by = "id")
hgb <- hgb_class(hgb_vals)


hgb<- subset(hgb, select = -c(race, gender, age))
hgb <- dummy_columns(hgb,select_columns = c("hgb_class"),
                     remove_selected_columns  = TRUE)

rm(hgb_vals, hgb_window)



#######Creatinine
#######According to the guidelines, creatinine is probably
##the most important variable for the disease, as it 
##classifies patients in multiple groups and is used to calculate
###the GFR. Also, albuminuria is an important factor. Further
##in the analysis we will infer patients with albuminuria
##by the drugs they are taking

creatinine_demo <- merge(creatinine_window,
                       demo, by = "id")
creatinine <- calculate_gfr(creatinine_demo)

##We will also clasiffy the patients in 5 categories
##depending on their GFR
creatinine <- gfr_classify(creatinine)

#Rapid progression is defined as a sustained decline in 
#eGFR of more than 5 ml/min/1.73 m2 per yr.
#Thus, we will calculate the change of GFR for each 3 windows per
#patient
creatinine <- gfr_evolution(creatinine)
creatinine <- subset(creatinine, select = -c(race, gender, age))
creatinine <- dummy_columns(creatinine,select_columns = c("gfr_class"),
                     remove_selected_columns  = TRUE)

rm(creatinine_window)

###Demo
demo <- dummy_columns(demo,select_columns = c("race", "gender"),
                      remove_selected_columns  = TRUE)

##response
##0 not progressed, 1 progressed
response$Stage_Progress <- gsub("False",0,response$Stage_Progress,
                                fixed = TRUE)
response$Stage_Progress <- gsub("True",1,response$Stage_Progress,
                                fixed = TRUE)




#####Finally we will merge the glucose, hgb,bp, ldl, creatinine,
##demo, meds and response
hgb <- subset(hgb, select = -c(time, window))
creatinine <- subset(creatinine, select = -c(time, window))



finalDF <- merge(glucose_window, hgb,
                 by = c("id", "Window"))

finalDF <- merge(finalDF, bp,
                 by = c("id", "Window"))

finalDF <- merge(finalDF, ldl,
                 by = c("id", "Window"))

finalDF <- merge(finalDF, creatinine,
                 by = c("id", "Window"))

finalDF <- merge(finalDF, demo,
                 by = c("id"))

finalDF <- merge(finalDF, meds_window,
                 by = c("id", "Window"))

finalDF <- merge(finalDF, response,
                 by = c("id"))

write.csv(finalDF, "DataPrepared.csv", row.names = FALSE)


















