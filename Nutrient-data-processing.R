#
# Bryn Chaffee
#
# Code to process initial soil extract nutrient data (AA500) for the Wapiti Fire
#
#
library(dplyr)
rm(list=ls()) #clear previous variables

#set working directory
setwd("C:/Users/bryn_/OneDrive/Documents/5108 LAB/Wapiti/Wapiti")
list.files()

#import necessary data 
field_metadata <- read.csv("C:/Users/bryn_/OneDrive/Documents/5108 LAB/Wapiti/Wapiti/Wapiti Soil Data_Fall24.csv")  #full dataframe w/ all field and lab metadata 
aa500_outputdata <- read.csv("C:/Users/bryn_/OneDrive/Documents/5108 LAB/Wapiti/Wapiti/Wapiti_lab_data_Chaffee.csv")

WF_sed_calcs <- data.frame(matrix(NA, nrow = 73))  #creates data frame to be used for calculations

field_metadata <- field_metadata %>%
  slice(-c(74:80))

WF_sed_calcs <- WF_sed_calcs %>%
  mutate(Lab_ID = aa500_outputdata$Lab_ID,
         NOx_Conc = aa500_outputdata$NOx.Conc..y,
         NH3_Conc = aa500_outputdata$NH3.Conc..x,
         PO4_Conc = aa500_outputdata$PO4.Conc.,
         dilution_factor = 10,
         KCl_weight = field_metadata$KCl.soil.wt..g.,
         Bicarb_weight = field_metadata$bicarb.soil.wt..g.,
         KCl_vol_ml = 50,
         Bicarb_vol_ml = 40,
         GWC = field_metadata$GWC..g.g.)

WF_sed_calcs <- WF_sed_calcs %>%
  mutate(conc.NOx.perg = ((NOx_Conc * dilution_factor * KCl_vol_ml)/1+GWC)/KCl_weight,
         conc.NH3.perg = ((NH3_Conc * dilution_factor * KCl_vol_ml)/1+GWC)/KCl_weight,
         conc.PO4.perg = ((PO4_Conc * dilution_factor * Bicarb_vol_ml)/1+GWC)/Bicarb_weight)

#create export for plotting
colnames(field_metadata)[colnames(field_metadata) == "Vial.No"] <- "Lab_ID"

WF_sed_plotting <- left_join(WF_sed_calcs, field_metadata, by = "Lab_ID")
WF_sed_plotting <- WF_sed_plotting %>%
  select(Lab_ID,
         conc.NOx.perg,
         conc.NH3.perg,
         conc.PO4.perg, 
         Site,
         Classification,
         Position,
         Transect,
         Sample.Depth..cm.)

write.csv(WF_sed_plotting, "C:/Users/bryn_/OneDrive/Documents/5108 LAB/Wapiti/Wapiti/WF_sed_plotting.csv")
  
