#
# Bryn Chaffee
#
# Wapiti fire rapid project
#
# Plotting nutrient concentrations by depth and site for burned and unburned


library(dplyr)
library(ggplot2)

rm(list=ls())

setwd("C:/Users/bryn_/OneDrive/Documents/5108 LAB/Wapiti/")
WF_sed_data <- read.csv("Wapiti/WF_sed_plotting.csv")


## filter data for aspects and channel, remove bluejay
aspect_subset <- WF_sed_data %>%
  filter(Classification %in% c("North Facing", "East Facing", "South Facing", "West Facing", "Channel") &
           Site %in% c("Wapiti SW", "Wapiti SE", "SH Debri Flow @SFP", "Summer Home-1",
                       "Summer Home-4", "SH Debri Flow @SFP", "SFP@Summer Home-4",
                       "SFP@Summer Home-2", "Rill-1", "Camp Creek", "Wapiti Creek",
                       "GO Creek"))
#### BURNED PLOTS:
## plot w/o limits on NOx concentration data
ggplot(aspect_subset, aes(fill = as.factor(Sample.Depth..cm.), y = conc.NOx.perg, x = Classification)) +
  geom_boxplot(aes(color = as.factor(Transect))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = c("Lower" = "#32387c", "Upper" = "#adbfd4")) +
  scale_fill_manual(values = c("0-1" = "#77f07f", "0-2" = "#3ec995", "0-3" = "#41a0ae", "1-6" = "#36669c", "2-6" = "#3a2f6b", "3-6" = "#022954")) +
  labs(fill = "Soil Depth (cm)", color = "Transect Position")

## plot w/ limit of 14 mg/L on NOx concentration
ggplot(subset(aspect_subset, conc.NOx.perg <= 14), aes(fill = as.factor(Sample.Depth..cm.), y = conc.NOx.perg, x = Classification)) +
  geom_boxplot(aes(color = as.factor(Transect))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = c("Lower" = "#32387c", "Upper" = "#adbfd4")) +
  scale_fill_manual(values = c("0-1" = "#77f07f", "0-2" = "#3ec995", "0-3" = "#41a0ae", "1-6" = "#36669c", "2-6" = "#3a2f6b", "3-6" = "#022954")) +
  labs(fill = "Soil Depth (cm)", color = "Transect Position")

## NH3 plot
ggplot(aspect_subset, aes(fill = as.factor(Sample.Depth..cm.), y = conc.NH3.perg, x = Classification)) +
  geom_boxplot(aes(color = as.factor(Transect))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = c("Lower" = "#32387c", "Upper" = "#adbfd4")) +
  scale_fill_manual(values = c("0-1" = "#77f07f", "0-2" = "#3ec995", "0-3" = "#41a0ae", "1-6" = "#36669c", "2-6" = "#3a2f6b", "3-6" = "#022954")) +
  labs(fill = "Soil Depth (cm)", color = "Transect Position")

##PO4 plot
ggplot(aspect_subset, aes(fill = as.factor(Sample.Depth..cm.), y = conc.PO4.perg, x = Classification)) +
  geom_boxplot(aes(color = as.factor(Transect))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = c("Lower" = "#32387c", "Upper" = "#adbfd4")) +
  scale_fill_manual(values = c("0-1" = "#77f07f", "0-2" = "#3ec995", "0-3" = "#41a0ae", "1-6" = "#36669c", "2-6" = "#3a2f6b", "3-6" = "#022954")) +
  labs(fill = "Soil Depth (cm)", color = "Transect Position")

#### UNBURNED PLOTS: 
unburned_subset <- WF_sed_data %>%
  filter(Classification %in% c("Northeast Facing", "South Facing", "Channel") &
           Site %in% c("Blue Jay Creek"))

## NOx
ggplot(unburned_subset, aes(fill = as.factor(Sample.Depth..cm.), y = conc.NOx.perg, x = Classification)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## NH3
ggplot(unburned_subset, aes(fill = as.factor(Sample.Depth..cm.), y = conc.NH3.perg, x = Classification)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## PO4
ggplot(unburned_subset, aes(fill = as.factor(Sample.Depth..cm.), y = conc.PO4.perg, x = Classification)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##factor sets the drawing order of listed variables (maybe not needed)
WF_sed_data$Site <- factor(WF_sed_data$Site, levels = c("Wapiti SW", "Wapiti SE", "SH Debri Flow @SFP", "Summer Home-1",
                                                        "Summer Home-4", "SH Debri Flow @SFP", "SFP@Summer Home-4",
                                                        "SFP@Summer Home-2", "Rill-1", "Camp Creek", "Wapiti Creek",
                                                        "GO Creek"))

#example from previous plotting:

#sm.data$Site <- factor(sm.data$Site, levels = c("Middle Rose", "Con 2 East", "Con 1 East"))

#ggplot(sm.data, aes(fill = as.factor(Direction), y = Average.VMC, x = Site)) +
 # geom_boxplot() +
  #scale_fill_manual(
   # name = "Aspect",
    #values = c("N" = "#f95b51", "S" = "#4460fd")
  #) +
  #labs(
   # x = "",
   # y = "Volumetric Moisture Content (%)"
#  ) +
 # theme_light()