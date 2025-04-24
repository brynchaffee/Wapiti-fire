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

#create data frame to be used for calculations
WF_sed_calcs <- data.frame(matrix(NA, nrow = 73))  

#remove unnecessary rows
field_metadata <- field_metadata %>%
  slice(-c(74:80))

#select/create columns needed for calculation and/or plotting
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
         GWC = field_metadata$GWC..g.g.,
         STD.NOx = aa500_outputdata$NOx.STD.y,
         STD.NH3 = aa500_outputdata$NH3.STD.x,
         STD.PO4 = aa500_outputdata$PO4.STD)

#correct concentration for dilutions and divide by grams of dry soil
WF_sed_calcs <- WF_sed_calcs %>%
  mutate(NOx.mg.per.g = ((NOx_Conc * dilution_factor * (KCl_vol_ml/1000)))/(KCl_weight - (KCl_weight * GWC)),
         NH3.mg.per.g = ((NH3_Conc * dilution_factor * (KCl_vol_ml/1000)))/(KCl_weight - (KCl_weight * GWC)),
         PO4.mg.per.g = ((PO4_Conc * dilution_factor * (Bicarb_vol_ml/1000)))/(Bicarb_weight - (Bicarb_weight * GWC)))

#create export for plotting
colnames(field_metadata)[colnames(field_metadata) == "Vial.No"] <- "Lab_ID"

WF_sed_plotting <- left_join(WF_sed_calcs, field_metadata, by = "Lab_ID")
WF_sed_plotting <- WF_sed_plotting %>%
  select(Lab_ID,
         NOx.mg.per.g,
         NH3.mg.per.g,
         PO4.mg.per.g, 
         Site,
         Classification,
         Position,
         Transect,
         Sample.Depth..cm.,
         STD.NOx,
         STD.NH3,
         STD.PO4)

#add apostrophe to prevent excel from seeing dates in depth data
WF_sed_plotting <- WF_sed_plotting %>%
  mutate(Sample.Depth..cm. = paste0("'", Sample.Depth..cm.))


write.csv(WF_sed_plotting, "C:/Users/bryn_/OneDrive/Documents/5108 LAB/Wapiti/Wapiti/WF_sed_plotting.csv")

