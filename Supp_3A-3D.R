###FIGURE Suppmental 3A-3D Script 
## Description: This code was used for generating ridgeline relative density (S3a-S3d; Acevedo et. al., in prep) to determine overall
## distribution of measured angles for each sampled segment at the three selected developmental stages. 

#Load packages
library(ggridges)
library(ggplot2)
library(rcartocolor)
library(RColorBrewer)
library(dplyr)

data <- read.csv("~/Downloads/IN6_Angle_Stages.csv", stringsAsFactors=TRUE) #insert dataset here

#1. Omit NAs and transform data to be continuous (0 to 180 degrees)
data <-na.omit(data)
data$Angle[data$Angle < 0] <- (data$Angle[data$Angle < 0]) + 180 #Transform data 

#2. Arrange the order of appearance of '$stage' on the graph
data$Stage <- factor(data$Stage,levels = c("S5", "S3", "S2"))

#OPTIONAL
data$Cell_Type = "Internode 6" #This is just for aesthetics to get a facet_wrapped-esque title

#3. Use brewer to obtain hex code that correlates with a selected color palettes (4 grid scale)
display.brewer.pal(4, "BuPu")
brewer.pal(4, "BuPu")

#4. Visualization of a ridgeline graph using ggplot using a predetermined palette.
ggplot(data, aes(x = Angle, y = Stage, fill = factor(stat(quantile)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, quantiles = 4, quantile_lines = TRUE)+ #quartile lines will be highlighted
  scale_x_continuous(limits=c(0, 180), breaks = seq (0, 180, by = 10))+
  scale_fill_manual(values = c("#EDF8FBAD", "#B3CDE3AD", "#8C96C6AD", "#88419DAD"), labels = c("Q1 (0-25%)", "Q2 (25-50%)", "Q3 (50-75%)", "Q4 (75-100%)"), name = "Quartiles") + #adding AD after the hexcode translated to XX% opacity of the color # Custom labels for quartiles created using the display brewer
  
  ##OPTIONAL: red lines to demarcate the angle boundaries between microtubule binning groups
  #geom_vline(xintercept = 22.5, linetype="dashed", color = "red", size=1, alpha=.7)+ 
  #geom_vline(xintercept = 67.5, linetype="dashed", color = "red", size=1, alpha=.7)+
  #geom_vline(xintercept = 112.5, linetype="dashed", color = "red", size=1, alpha=.7)+
  #geom_vline(xintercept = 157.5, linetype="dashed", color = "red", size=1, alpha=.7)+
 
  geom_point(data = data, aes(x = Angle, y = Stage), position=position_jitter(height = .05), color = "black", alpha = 0.2, size =2.5, inherit.aes = FALSE)+ #visualize individual data points
  facet_wrap(~Cell_Type)+ #used to achieve the facet-wrap title aesthetic
  theme_classic()+
  theme(text = element_text(size=20), plot.title=element_text( hjust=.5, vjust=0.5, face='bold')) #used to adjust font size of the graph

