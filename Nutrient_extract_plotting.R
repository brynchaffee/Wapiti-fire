#
# Bryn Chaffee
#
# Wapiti fire rapid project
#
# Plotting nutrient concentrations by depth and site for burned and unburned


library(dplyr)
library(ggplot2)
library(ggpmisc)
library(ggpubr)
library(grDevices)

rm(list=ls())

setwd("C:/Users/bryn_/OneDrive/Documents/5108 LAB/Wapiti/")
WF_sed_data <- read.csv("Wapiti/WF_sed_plotting_keep.csv")

#remove apostroEC..uS.cm.e from depth column (was used to prevent excel from recognizing dates)
WF_sed_data <- WF_sed_data %>%
  mutate(Sample.Depth..cm. = sub("^'", "", Sample.Depth..cm.))

#find top depth and classify as surface sample or subsurface sample
WF_sed_data <- WF_sed_data %>%
  mutate(
    top_depth = as.numeric(sub("-.*", "", Sample.Depth..cm.)),
    depth_class = ifelse(top_depth == 0, "Surface", "Subsurface")
  )

#filter data for site and position, removing bluejay (unburned)
burned_subset <- WF_sed_data %>%
  filter(Site %in% c("Wapiti SW", "Wapiti SE", "SH Debri Flow @SFP", "Summer Home-1",
                       "Summer Home-4", "SH Debri Flow @SFP", "SFP@Summer Home-4",
                       "SFP@Summer Home-2", "Rill-1", "Camp Creek", "Wapiti Creek",
                       "GO Creek") &
           depth_class %in% c("Surface", "Subsurface"))

#create unburned subset, only Blue Jay points
unburned_subset <- WF_sed_data %>%
  filter(Position %in% c("Channel", "Hill") & 
           depth_class %in% c("Surface", "Subsurface") &
           Site %in% c("Blue Jay Creek"))

#create a subset of hillslope points for BSI plotting and calculations
hillslope_subset <- WF_sed_data %>%
  filter(Classification %in% c("Hillslope"))

channel_subset <- WF_sed_data %>%
  filter(Classification %in% c("Channel"))

#### BURNED PLOTS:
## NOx plot
ggplot(burned_subset, aes(fill = as.factor(depth_class), y = NOx.mg.per.g, x = Classification)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Surface" = "#9ead85", "Subsurface" = "#406446")) +
  ylim(0,0.02) + 
  labs(y = "Nitrate Concentration (mg/g)", x = "") +
  guides(fill = guide_legend(title = NULL)) +
  stat_compare_means(aes(group = depth_class), method = "t.test", label = "p.format",
                     label.y = 0.019, size = 5) +
  theme_light() +
  theme(
    legend.position = "none",
    axis.title.y = element_text(size = 16, margin = margin(r = 14)),
    axis.text.x = element_text(size = 15, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    plot.margin = margin(20, 15, 5, 20)
  )
ggsave("C:/Users/bryn_/OneDrive/Documents/5108 LAB/Wapiti/Poster-construction/nitrate_boxplot.tiff", 
       width = 4.9, height = 6.3, units = "in", dpi = 300)


## NH3 plot
ggplot(burned_subset, aes(fill = as.factor(depth_class), y = NH3.mg.per.g, x = Classification)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Surface" = "#9ead85", "Subsurface" = "#406446")) +
  labs(y = "Ammonia Concentration (mg/g)", x = "") +
  guides(fill = guide_legend(title = NULL)) +
  stat_compare_means(aes(group = depth_class), method = "t.test", label = "p.format",
                     label.y = 0.25, size = 5) +
  theme_light() +
  theme(
    legend.position = "none",
    axis.title.y = element_text(size = 16, margin = margin(r = 14)),
    axis.text.x = element_text(size = 15, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    plot.margin = margin(20, 15, 5, 20)
  )
ggsave("C:/Users/bryn_/OneDrive/Documents/5108 LAB/Wapiti/Poster-construction/ammonium_boxplot.tiff", 
       width = 4.9, height = 6.3, units = "in", dpi = 300)

## PO4 plot
ggplot(burned_subset, aes(fill = as.factor(depth_class), y = PO4.mg.per.g, x = Classification)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Surface" = "#9ead85", "Subsurface" = "#406446")) +
  labs(y = "Phosphate Concentration (mg/g)", x = "") +
  guides(fill = guide_legend(title = NULL)) +
  stat_compare_means(aes(group = depth_class), method = "t.test", label = "p.format",
                     label.y = 0.45, size = 5) +
  theme_light() +
  theme(
    legend.position = "none",
    axis.title.y = element_text(size = 16, margin = margin(r = 14)),
    axis.text.x = element_text(size = 15, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    plot.margin = margin(20, 15, 5, 20)
  )
ggsave("C:/Users/bryn_/OneDrive/Documents/5108 LAB/Wapiti/Poster-construction/phosphate_boxplot.tiff", 
       width = 4.9, height = 6.3, units = "in", dpi = 300)

#### UNBURNED PLOTS: 

## NOx
ggplot(unburned_subset, aes(color = as.factor(depth_class), 
                            y = NOx.mg.per.g, x = Classification)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = NOx.mg.per.g - STD.NOx, 
                    ymax = NOx.mg.per.g + STD.NOx), width = 0.1) +
  scale_color_manual(values = c("Surface" = "#9ead85", "Subsurface" = "#406446")) +
  ylim(0, 0.02) +
  labs(y = NULL, x = "") +
  guides(color = guide_legend(title = NULL)) +
  theme_light() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 16, color = "black"),
    axis.text.y = element_blank(),
    plot.margin = margin(20, 5, 5, 5))

ggsave("C:/Users/bryn_/OneDrive/Documents/5108 LAB/Wapiti/Poster-construction/noxctrl_boxplot.tiff", 
       width = 2.5, height = 6.3, units = "in", dpi = 300)

## NH3
ggplot(unburned_subset, aes(color = as.factor(depth_class), 
                            y = NH3.mg.per.g, x = Classification)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = NH3.mg.per.g - STD.NH3, 
                    ymax = NH3.mg.per.g + STD.NH3), width = 0.1) +
  scale_color_manual(values = c("Surface" = "#9ead85", "Subsurface" = "#406446")) +
  ylim (0, 0.3) +
  labs(y = NULL, x = "") +
  guides(color = guide_legend(title = NULL)) + 
  theme_light() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 16, color = "black"),
    axis.text.y = element_blank(),
    plot.margin = margin(20, 5, 5, 5))

ggsave("C:/Users/bryn_/OneDrive/Documents/5108 LAB/Wapiti/Poster-construction/nh3ctrl_boxplot.tiff", 
       width = 2.5, height = 6.3, units = "in", dpi = 300)

## PO4
ggplot(unburned_subset, aes(color = as.factor(depth_class), 
                            y = PO4.mg.per.g, x = Classification)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = PO4.mg.per.g - STD.PO4, 
                    ymax = PO4.mg.per.g + STD.PO4), width = 0.1) +
  scale_color_manual(values = c("Surface" = "#9ead85", "Subsurface" = "#406446")) +
  ylim(0, 0.5) +
  labs(y = NULL, x = "") +
  guides(color = guide_legend(title = NULL)) + 
  theme_light() + 
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 16, color = "black"),
    axis.text.y = element_blank(),
    plot.margin = margin(20, 5, 5, 5))

ggsave("C:/Users/bryn_/OneDrive/Documents/5108 LAB/Wapiti/Poster-construction/po4ctrl_boxplot.tiff", 
       width = 2.5, height = 6.3, units = "in", dpi = 300)

#### pH and EC plots: 

## pH

#burned
ggplot(burned_subset, aes(fill = as.factor(depth_class), y = pH, x = Classification)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Surface" = "#9ead85", "Subsurface" = "#406446")) +
  labs(y = "pH", x = "") +
  guides(fill = guide_legend(title = NULL)) +
  stat_compare_means(aes(group = depth_class), method = "t.test", label = "p.format",
                     label.y = 8.5, size = 5) +
  theme_light() +
  theme(
    legend.position = "none",
    axis.title.y = element_text(size = 16, margin = margin(r = 14)),
    axis.text.x = element_text(size = 16, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    plot.margin = margin(20, 15, 5, 20)
  )
ggsave("C:/Users/bryn_/OneDrive/Documents/5108 LAB/Wapiti/Poster-construction/pH_boxplot.tiff", 
       width = 4.9, height = 6.3, units = "in", dpi = 300)

#unburned points
ggplot(unburned_subset, aes(color = as.factor(depth_class), 
                            y = pH, x = Classification)) +
  geom_point(size = 4) +
  scale_color_manual(values = c("Surface" = "#9ead85", "Subsurface" = "#406446")) +
  labs(y = "", x = "") +
  ylim(6.25,8.75) +
  guides(color = guide_legend(title = NULL)) + 
  theme_light() + 
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 16, color = "black"),
    axis.text.y = element_blank(),
    plot.margin = margin(20, 5, 5, 5))

ggsave("C:/Users/bryn_/OneDrive/Documents/5108 LAB/Wapiti/Poster-construction/pHctrl.tiff", 
       width = 2.5, height = 6.3, units = "in", dpi = 300)

## EC

#burned
ggplot(burned_subset, aes(fill = as.factor(depth_class), y = EC..uS.cm., x = Classification)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Surface" = "#9ead85", "Subsurface" = "#406446")) +
  labs(y = "Electrical Conductivity (uS/cm)", x = "") +
  guides(fill = guide_legend(title = NULL)) +
  stat_compare_means(aes(group = depth_class), method = "t.test", label = "p.format",
                     label.y = 650, size = 5) +
  theme_light() +
  theme(
    legend.position = "none",
    axis.title.y = element_text(size = 16, margin = margin(r = 14)),
    axis.text.x = element_text(size = 16, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    plot.margin = margin(20, 15, 5, 20)
  )
ggsave("C:/Users/bryn_/OneDrive/Documents/5108 LAB/Wapiti/Poster-construction/EC_boxplot.tiff", 
       width = 4.9, height = 6.3, units = "in", dpi = 300)

#unburned points
ggplot(unburned_subset, aes(color = as.factor(depth_class), 
                            y = EC..uS.cm., x = Classification)) +
  geom_point(size = 4) +
  scale_color_manual(values = c("Surface" = "#9ead85", "Subsurface" = "#406446")) +
  labs(y = "", x = "") +
  ylim(0,750) +
  guides(color = guide_legend(title = NULL)) + 
  theme_light() + 
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 16, color = "black"),
    axis.text.y = element_blank(),
    plot.margin = margin(20, 5, 5, 5))

ggsave("C:/Users/bryn_/OneDrive/Documents/5108 LAB/Wapiti/Poster-construction/ECctrl.tiff", 
       width = 4.9, height = 6.3, units = "in", dpi = 300)

#### NBR plots

WF_sed_data$depth_class <- factor(WF_sed_data$depth_class, levels = c("Surface", "Subsurface"))

#NOx
ggplot(channel_subset, aes(color = as.factor(depth_class), 
                            y = NOx.mg.per.g, x = RdNBR)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, aes(color = as.factor(depth_class))) +
  stat_poly_eq(
    aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~"),
        color = as.factor(depth_class),
        group = as.factor(depth_class)),
    formula = y ~ x,
    parse = TRUE,
    label.x.npc = "left",
    label.y.npc = c(0.95, 0.85),  # One label per group, adjust if needed
    size = 5
  ) +
  scale_color_manual(values = c("Surface" = "#9ead85", "Subsurface" = "#406446")) +
  labs(y = "Nitrate Concentration (mg/g)", x = "RdNBR") +
  ylim(-0.001,0.05) +
  guides(color = guide_legend(title = NULL)) + 
  theme_light() + 
  theme(
    legend.position = "none",
    axis.title.y = element_text(size = 16, margin = margin(r = 14)),
    axis.title.x = element_text(size = 16, margin = margin(t = 10)),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    plot.margin = margin(20, 20, 10, 20)
  )

ggsave("C:/Users/bryn_/OneDrive/Documents/5108 LAB/Wapiti/Poster-construction/NOxburn.tiff", 
       width = 5.0, height = 6.2, units = "in", dpi = 300)

#NH3
ggplot(channel_subset, aes(color = as.factor(depth_class), 
                            y = NH3.mg.per.g, x = RdNBR)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, aes(color = as.factor(depth_class))) +
  stat_poly_eq(
    aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~"),
        color = as.factor(depth_class),
        group = as.factor(depth_class)),
    formula = y ~ x,
    parse = TRUE,
    label.x.npc = "left",
    label.y.npc = c(0.95, 0.85),  # One label per group, adjust if needed
    size = 5
  ) +
  scale_color_manual(values = c("Surface" = "#9ead85", "Subsurface" = "#406446")) +
  labs(y = "Ammonia Concentration (mg/g)", x = "RdNBR") +
  guides(color = guide_legend(title = NULL)) + 
  theme_light() + 
  theme(
    legend.position = "none",
    axis.title.y = element_text(size = 16, margin = margin(r = 14)),
    axis.title.x = element_text(size = 16, margin = margin(t = 10)),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    plot.margin = margin(20, 20, 10, 20)
  )
ggsave("C:/Users/bryn_/OneDrive/Documents/5108 LAB/Wapiti/Poster-construction/NH3burn.tiff", 
       width = 5.0, height = 6.2, units = "in", dpi = 300)

#PO4
ggplot(channel_subset, aes(color = as.factor(depth_class), 
                        y = PO4.mg.per.g, x = RdNBR)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, aes(color = as.factor(depth_class))) +
  stat_poly_eq(
    aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~"),
        color = as.factor(depth_class),
        group = as.factor(depth_class)),
    formula = y ~ x,
    parse = TRUE,
    label.x.npc = "left",
    label.y.npc = c(0.95, 0.85),  # One label per group, adjust if needed
    size = 5
  ) +
  scale_color_manual(values = c("Surface" = "#9ead85", "Subsurface" = "#406446")) +
  labs(y = "Phosphate Concentration (mg/g)", x = "RdNBR") +
guides(color = guide_legend(title = NULL)) + 
  theme_light() + 
  theme(
    legend.position = "none",
    axis.title.y = element_text(size = 16, margin = margin(r = 14)),
    axis.title.x = element_text(size = 16, margin = margin(t = 10)),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    plot.margin = margin(20, 20, 10, 20)
  )
ggsave("C:/Users/bryn_/OneDrive/Documents/5108 LAB/Wapiti/Poster-construction/PO4burn.tiff", 
       width = 5.0, height = 6.2, units = "in", dpi = 300)

