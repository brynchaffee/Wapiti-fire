#
#Bryn Chaffee
#Filling AA500 output data into master data sheet
#
#
library(dplyr)
rm(list=ls())

#input compiled AA500 outputs
aa500_outputdata <- read.csv("C:/Users/bryn_/OneDrive/Documents/5108 LAB/Wapiti/Wapiti/Wapiti_lab_data_Chaffee.csv")

#input individual processed AA500 run file, remove extra (unnecessary) column
input_data <- read.csv("C:/Users/bryn_/OneDrive/Documents/5108 LAB/Wapiti/AA500 data/WF_KCl_NOx_NH3_12-73_14Feb25_Corr_OUTPUT.csv")
input_data <- input_data %>%
  dplyr::rename(Lab_ID = Sample) %>%
  select(-X)

#IF NEEDED, rename Lab ID column to allow for a left_join
colnames(input_data)[colnames(input_data) == "Lab ID"] <- "Lab_ID"
colnames(aa500_outputdata)[colnames(aa500_outputdata) == "Lab.ID"] <- "Lab_ID"
#input_data <- input_data[,-1]

#join the input data to the compiled sheet, select necessary columns
merged_data <- aa500_outputdata %>%
  left_join(input_data, by = "Lab_ID") %>%
  select(Lab_ID, NOx.Conc..y, NOx.STD.y, NOx.DL.y, NH3.Conc..x, NH3.STD.x, NH3.DL.x, PO4.Conc., PO4.STD, PO4.DL, KCl_run_name, Ol.P_run_name)

#repeat above steps for a new input dataset
input_data2 <- read.csv("C:/Users/bryn_/OneDrive/Documents/5108 LAB/Wapiti/AA500 data/WF_KCl_NOx_NH3_28Feb25_Corr_OUTPUT.csv")
input_data2 <- input_data2 %>%
  dplyr::rename(Lab_ID = Sample) %>%
  select(-X)

merged_data <- merged_data %>%
  left_join(input_data2, by = "Lab_ID")
  
merged_data <- merged_data %>%
  mutate(NOx.Conc..y = ifelse(is.na(NOx.Conc..y), NOx.Conc., NOx.Conc..y),
         NOx.DL.y = ifelse(is.na(NOx.DL.y), NOx.DL, NOx.DL.y),
         NOx.STD.y = ifelse(is.na(NOx.STD.y), NOx.STD, NOx.STD.y))

merged_data <- merged_data %>%
  select(Lab_ID, NOx.Conc..y, NOx.STD.y, NOx.DL.y, NH3.Conc..x, NH3.STD.x, NH3.DL.x, PO4.Conc., PO4.STD, PO4.DL, KCl_run_name, Ol.P_run_name)

#export to CSV
write.csv(merged_data, "C:/Users/bryn_/OneDrive/Documents/5108 LAB/Wapiti/Wapiti/Wapiti_lab_data_Chaffee.csv")


