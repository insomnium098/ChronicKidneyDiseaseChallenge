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



