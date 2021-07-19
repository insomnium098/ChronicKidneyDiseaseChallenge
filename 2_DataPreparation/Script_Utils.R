library(dplyr)
library(data.table)
###Utils functions for the data preparation

####Function to make windows from a given dataframe
###It receives the windows in days and a dataframe
####Returns a dataframe with the data by windows

makeWindows <- function(window, df, df_patient_maxTime){
  for (i in unique(df$id)){ 
    message(i)
    ##First we obtain the patient  data
    df_filt <- filter(df, id == i)
    maxDay <- max(df_patient_maxTime[df_patient_maxTime == i,"max_time"])
    
    ##We calculate how many windows could be fitted in the period
    numbWindows <- ceiling(maxDay/window)
    
    
    ##We need to expand te data by the windows. We will assumme
    ##that the values remain constant until the time they were
    ##evaluated (Im aware this is probably wrong and that it should
    ##be estimated using a regressor)
    
    for (xi in 1:(nrow(df_filt)-1)){
      base <- df_filt[xi,]
      ##UpperTime
      upperTime <- df_filt[xi + 1,"time"]
      ###We will repeat the base upperTime times
      new <- base[rep(seq_len(nrow(base)), (upperTime - base$time)),]
      new$time <- seq(base$time,(upperTime - 1), 1)
      
      if(!exists("newFinal")){
        newFinal <- new
      } else {
        newFinal <- rbind(newFinal, new)
      }
      
      rm(new)
    
      
      
    }
    
    ##We add the upper value
    newFinal <- rbind(newFinal, df_filt[df_filt$time == max(df_filt$time),])
    
    
    ##Case when the last time evaluated is lower than the maxTime
    maxTime <- df_patient_maxTime[df_patient_maxTime == i,
                                  "max_time"]
    if(max(newFinal$time) < maxTime){
      max <- newFinal[nrow(newFinal),]
      
      
      dfaux <- max[rep(seq_len(nrow(max)), (maxTime - max$time)+1),]
      dfaux$time <- seq(max$time,(maxTime), 1)
      dfaux <- dfaux[-c(1),]
      
      newFinal <- rbind(newFinal, dfaux)
      
      
    }
    
    ###NewFinal has the data expanded
    
    
    counter <- 0
    for (partitionWindow in 1:numbWindows){
      
      ###We get which values were evaluated in the windows
      df_filt2 <- newFinal[between(newFinal$time, counter,
                                   counter + window),]
      meanVal <- mean(df_filt2$value)
      df_filt2 <- df_filt2[1,]
      df_filt2$value <- meanVal
      
      
      df_filt2$Window <- counter + window#paste0(counter ,"-" ,counter + window)
      
      if(!exists("df_finalV2")){
        df_finalV2 <- df_filt2
      } else{
        df_finalV2 <- rbind(df_finalV2, df_filt2)
      }
      
      counter = counter + window
    }
    
    
    
    if(!exists("df_window")){
      df_window <- df_finalV2
    } else{
      df_window <- rbind(df_window, df_finalV2)
    }
    
    
    
    rm(df_finalV2, newFinal)
    
    
    
  }
  
  return(df_window)
  
  
  
  
}

#test <- makeWindows(30, creatinine, df_patient_maxTime)

##Function that receives the BP dataframe and classifies the
#patients in ther BP category as:
##Normal (sbp less than 120 and dbp less than 80)
##Elevated (sbp 120-129 and dbp less than 80)
##Hypertension1 (sbp 130-139 or dbp 80-80)
##Hypertension2 (sbp >= 140 or dbp >= 90)
##HypertensiveCrisis (sbp > 180 and or dbp > 120)
#Patient is controlled if < 140/90
#Returns: A dataframe with the colum bp_class and bp_controlled
bp_classifier <- function(df){
  df$bp_class <- 0
  df$bp_is_controlled <- 0
  df$sbp_value <- round(df$sbp_value)
  df$dbp_value <- round(df$dbp_value)
  for (i in 1:nrow(df)){
    sbp <- df[i,"sbp_value"]
    dbp <- df[i,"dbp_value"]
    
    if (sbp <120 & dbp <80){
      df[i,"bp_class"] <- "Normal"

    } else if (between(sbp, 120, 129) & (dbp < 80)){
      df[i,"bp_class"] <- "Elevated"

    } else if (between(sbp, 130, 139) | between(dbp, 80, 89)){
      df[i,"bp_class"] <- "Hypertension1"

    } else if (between(sbp, 140, 180) | between(dbp, 90, 120)){
      df[i,"bp_class"] <- "Hypertension2"
      
    } else{
      df[i,"bp_class"] <- "HypertensiveCrisis"
    }
    
    
    if (sbp <= 140 & dbp <=90){
      df[i,"bp_is_controlled"] <- 1
    }
    
    
  }
  
  return(df)
  
}

###Function to classify patients in categories depending
###their LDL Levels:
##Less than 100mg/dl : Normal
##100-129mg/dl : NearNormal
##130-159mg/dl : BorderlineHigh
##160-189mg/dl : High
##190mg/dL : VeryHigh
##It Receives the LDL windows dataframe
##Returns a dataframe with the LDL_class column

ldl_class <- function(df){
  df$LDL_class <- 0
  df$ldl_value <- round(df$ldl_value)
  for (i in 1:nrow(df)){
    ldl <- df[i,"ldl_value"]
    if(ldl < 100){
      df[i,"LDL_class"] <- "Normal"
    } else if(between(ldl, 100,129)){
      df[i,"LDL_class"] <- "NearNormal"
    } else if(between(ldl, 130,159)){
      df[i,"LDL_class"] <- "BorderlineHigh"
    } else if(between(ldl, 160,189)){
      df[i,"LDL_class"] <- "High"
    } else{
      df[i,"LDL_class"] <- "VeryHigh"
    }
    
    
    
  }
  
  return(df)
    

  
  
}


###Function to classify patients depending their HGB values
###Normal values:
###Womans: 	12.3 - 15.3  g/dL
###Mans: 14.0 - 17.5 g/dL
##Returns a dataframe with the hgb_class value

hgb_class <- function(df){
  df$hgb_class <- 0
  
  for (i in 1:nrow(df)){
    hgb <- df[i,"hgb_value"]
    sex <- df[i,"gender"]
    
    if(sex == "Male"){
      if (hgb < 14.0){
        df[i,"hgb_class"] <- "Low"
      } else if (hgb > 17.5){
        df[i,"hgb_class"] <- "High"
      } else {
        df[i,"hgb_class"] <- "Normal"
      }
      
    } else {
      #Female case
      if (hgb < 12.3){
        df[i,"hgb_class"] <- "Low"
      } else if (hgb > 15.3){
        df[i,"hgb_class"] <- "High"
      } else {
        df[i,"hgb_class"] <- "Normal"
      }
    }
    
  }
  
  return(df)
  
  
  
  
}



###Function to calculate the GFR of the patients
###It receives the creatinene window dataframe and
##returns a dataframe with the GFR column 
##GFR FORMULA:
##Female, cr <= 0.7 = 144 * (cr / 0.7)^(-0.329) * 0.993^(age)
##Female, cr > 0.7 = 144 * (cr / 0.7)^(-1.209) * 0.993^(age)
##Male, cr <= 0.9 = 141 * (cr / 0.9)^(-0.411) * 0.993^(age)
##Male, cr > 0.9 = 141 * (cr / 0.9)^(-1.209) * 0.993^(age)

##If the patient race is black, multiply the formula by 1.159

calculate_gfr <- function(df){
  df$GFR <- 0
  for (i in 1:nrow(df)){
    cr <- df[i,"creatinine_value"]
    sex <- df[i,"gender"]
    race <- df[i,"race"]
    age <- df[i,"age"]
    
    
    if(sex == "Female"){
      if(cr <= 0.7){
        gfr <- 144 * ((cr / 0.7)^(-0.329)) * (0.993^(age))
      } else {
        gfr <- 144 * ((cr / 0.7)^(-1.209)) * (0.993^(age))
      }
      
      
    } else {
      if(cr <= 0.9){
        gfr <- 141 * ((cr / 0.9)^(-0.411)) * (0.993^(age))
      } else {
        gfr <- 141 * ((cr / 0.9)^(-1.209)) * (0.993^(age))
      }
      
      
    }
    
    if (sex == "Black"){
      gfr <- gfr * 1.159
    }
    
    df[i,"GFR"] <- round(gfr)
    
    
  }
  
  return(df)
  
  

  
  
}



###Function to classify patients depending on their GFR 
##It receives a creatinine GFR dataframe and returns it with
##the gfr_class column
##GFR >= 90 = G1
##GFR 60-89 = G2
##GFR 45-59 = G3a
##GFR 30-44 = G3b
##GFR 16-29 = G4
##GFR <= 15 = G5


gfr_classify <- function(df){
  df$gfr_class <- 0
  for (i in 1:nrow(df)){
    gfr <- df[i,"GFR"]
    if (gfr >= 90 ){
      df[i,"gfr_class"] <- "G1"
    } else if (between(gfr, 60, 89)){
      df[i,"gfr_class"] <- "G2"
    } else if (between(gfr, 45, 59)){
      df[i,"gfr_class"] <- "G3a"
    } else if (between(gfr, 30, 44)){
      df[i,"gfr_class"] <- "G3b"
    } else if (between(gfr, 16, 29)){
      df[i,"gfr_class"] <- "G4"
    } else {
      df[i,"gfr_class"] <- "G5"
    }
    
  }
  
  return(df)
    

}


##Function to calculate the GFR by 3 window units

gfr_evolution <- function(df){
  
  ##First we will filter by patient
  for (patient in unique(df$id)){
    df_filt1 <- filter(df, id == patient)
    df_filt1$gfr_evolution <- -1
    
    #For each patient we will iterate over all of its windows
    counter <- 0
    for (window in 1:nrow(df_filt1)){
      
      currentGFR <- df_filt1[window,"GFR"]
      ##Base case
      if(window == 1){
        df_filt1[window,"gfr_evolution"] <- 0
        counter <- counter + 1
        
      } else if (window < 3 & window != 1) {
        df_filt1[window,"gfr_evolution"] <- currentGFR -
          df_filt1[window-1,"GFR"]
        counter <- counter + 1
        
      } else {
        df_filt1[window,"gfr_evolution"] <- currentGFR - 
          mean(df_filt1[tail(1:window,4)[1:3],"GFR"])
        counter = counter + 1
        
        
      }
      
    }
    
    if(!exists("df_patient")){
      df_patient <- df_filt1
    } else {
      df_patient <- rbind(df_patient, df_filt1)
    }
    
  }
  
  return(df_patient)
  
  
}


test <- gfr_evolution(creatinine)









