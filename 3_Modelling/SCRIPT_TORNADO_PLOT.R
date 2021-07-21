library(dplyr)
library(ggplot2)

datos <- read.csv("SHAP_ABS_180.csv", row.names = 1)
datos <- filter(datos, Corr != 0)


######Rename Variable Names
datos$Variable <- gsub("glucose_value", "Glucose", datos$Variable, fixed = TRUE)
datos$Variable <- gsub("age", "Age", datos$Variable, fixed = TRUE)
datos$Variable <- gsub("GFR", "Glomerular Filtration Rate", datos$Variable, fixed = TRUE)
datos$Variable <- gsub("hgb_value", "Hemoglobin", datos$Variable, fixed = TRUE)
datos$Variable <- gsub("creatinine_value", "Creatinine", datos$Variable, fixed = TRUE)
datos$Variable <- gsub("gfr_class_G3b", "GFR Class: G3b", datos$Variable, fixed = TRUE)
datos$Variable <- gsub("ldl", "LDL Cholesterol", datos$Variable, fixed = TRUE)
datos$Variable <- gsub("dbp_value", "Diastolic Blood Pressure", datos$Variable, fixed = TRUE)
datos$Variable <- gsub("sbp_value", "Systolic Blood Pressure", datos$Variable, fixed = TRUE)
datos$Variable <- gsub("gender_Female", "Gender: Female", datos$Variable, fixed = TRUE)
datos$Variable <- gsub("gfr_evolution", "GFR Evolution (3 previous time windows)", datos$Variable, fixed = TRUE)
datos$Variable <- gsub("drug_metoprolol", "Drug: Metoprolol", datos$Variable, fixed = TRUE)
datos$Variable <- gsub("race_Black", "Race: Black", datos$Variable, fixed = TRUE)
datos$Variable <- gsub("hgb_class_Low", "Hemoglobin Class: Low", datos$Variable, fixed = TRUE)
datos$Variable <- gsub("drug_atorvastatin", "Drug: Atorvastatin", datos$Variable, fixed = TRUE)
datos$Variable <- gsub("bp_class_Hypertension1", "Blood Pressure Class: Hypertension1", datos$Variable, fixed = TRUE)
datos$Variable <- gsub("drug_pravastatin", "Drug: Pravastatin", datos$Variable, fixed = TRUE)
datos$Variable <- gsub("gfr_class_G3a", "GFR Class: G3a", datos$Variable, fixed = TRUE)
datos$Variable <- gsub("race_Hispanic", "Race: Hispanic", datos$Variable, fixed = TRUE)
datos$Variable <- gsub("Drug_DailyDosAge_DurationDose_losartan-100-90", "Losartan Daily Dosage : 100 Duration: 90 days", datos$Variable, fixed = TRUE)
datos$Variable <- gsub("race_Unknown", "Race: Unknown", datos$Variable, fixed = TRUE)
datos$Variable <- gsub("bp_class_Elevated", "Blood Pressure Class: Elevated", datos$Variable, fixed = TRUE)
datos$Variable <- gsub("drug_simvastatin", "Drug: Simvastatin", datos$Variable, fixed = TRUE)
datos$Variable <- gsub("race_White", "Race: White", datos$Variable, fixed = TRUE)
datos$Variable <- gsub("hgb_class_High", "Hemoglobin Class: High", datos$Variable, fixed = TRUE)
datos$Variable <- gsub("Drug_DailyDosAge_DurationDose_metoprolol-50-90", "Metoprolol Daily Dosage : 50 Duration: 90 days", datos$Variable, fixed = TRUE)
datos$Variable <- gsub("Drug_DailyDosAge_DurationDose_metformin-1000-90", "Metformin Daily Dosage : 1000 Duration: 90 days", datos$Variable, fixed = TRUE)
datos$Variable <- gsub("bp_is_controlled_0", "Blood Pressure is NOT Controlled", datos$Variable, fixed = TRUE)
datos$Variable <- gsub("gfr_class_G2", "GFR Class: G2", datos$Variable, fixed = TRUE)
datos$Variable <- gsub("hgb_class_Normal", "Hemoglobin Class: Normal", datos$Variable, fixed = TRUE)
datos$Variable <- gsub("Drug_DailyDosAge_DurationDose_atorvastatin-40-90", "Atorvastin Daily Dosage : 40 Duration: 90 days", datos$Variable, fixed = TRUE)
datos$Variable <- gsub("LDL_class_BorderlineHigh", "LDL Class: BorderlineHigh", datos$Variable, fixed = TRUE)
datos$Variable <- gsub("drug_atenolol", "Drug: Atenolol", datos$Variable, fixed = TRUE)
datos$Variable <- gsub("drug_rosuvastatin", "Drug: Rosuvastatin", datos$Variable, fixed = TRUE)
datos$Variable <- gsub("Drug_DailyDosAge_DurationDose_simvastatin-40-90", "Simvastatin Daily Dosage : 40 Duration: 90 days", datos$Variable, fixed = TRUE)
datos$Variable <- gsub("LDL_class_NearNormal", "LDL Class: Near Normal", datos$Variable, fixed = TRUE)
datos$Variable <- gsub("bp_class_Hypertension2", "Blood Pressure Class: Hypertension2", datos$Variable, fixed = TRUE)







###Primero separar en positivos y negativos

datPos <- filter(datos, Corr > 0)
datosNeg <- filter(datos, Corr < 0)

###Ordenamos ambos
datPos <- datPos[order(datPos$SHAP_abs, decreasing = TRUE),]
datosNeg <- datosNeg[order(datosNeg$SHAP_abs, decreasing = TRUE),]


###Haremos negativo el SHAP_abs de los negativos corr para el plot
datosNeg$SHAP_abs <- datosNeg$SHAP_abs * -1

####Haremos un loop para ir extrayendo
df_final <- datPos[1,]
datPos <- datPos[-(1),]
contador <- 0
while(nrow(datosNeg) != 0 && nrow(datPos) !=0){
  
  if(contador%%2 == 1){
    ##Pos
    df_final <- rbind(df_final, datPos[1,])
    datPos <- datPos[-(1),]
  } else{
    
    #Neg
    df_final <- rbind(df_final, datosNeg[1,])
    datosNeg <- datosNeg[-(1),]
    
  }
  
  contador <- contador + 1
  
  
}

###Agregamos los que faltan

if(nrow(datPos) == 0){
  df_final <- rbind(df_final, datosNeg)
} else {
  df_final <- rbind(df_final, datPos)
}


df_final <- df_final[1:35,]

###Hacemos Plot
df_final$Variable<- factor(df_final$Variable,
                            levels = rev(df_final$Variable))
###Plot


plot <- ggplot(data=df_final, aes(x =Variable, y = SHAP_abs,
                                   fill = Sign)) +
  geom_bar(stat="identity", position = position_dodge()) +
  theme_minimal() + coord_flip() + labs(y = "Effect on Stage Progression (Shap Value Impact)", x = "") +
  #scale_fill_gradient(low = "#2ECC71",high = "#c00000")+
  scale_fill_manual(values =c("#2ECC71","#c00000"))+
  
  
  theme(legend.position = "none",
        axis.text.y = element_text(face="bold", colour = "black"))

print(plot)




####


