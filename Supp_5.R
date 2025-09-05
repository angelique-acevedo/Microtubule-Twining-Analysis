###SUPPLEMENTAL Figure 5: Correlation Analysis of twined angle vs CMT angles
##DESCRIPTION: This script was used to generate supplemental figure 5 in Acevedo et al., (in review). The plot documents the relationship between the angle of ascent (pitch angle of twined stem) vs. corresponding cortical microtubule angles found at internode 6 stage 5. 

#Load packages and dataset
library(ggplot2)
datum <- read.csv("~/Downloads/Supp_5_data.csv")

#Transform data set to be 0-180
datum$Angle[datum$Angle < 0] <- datum$Angle[datum$Angle < 0] + 180

#Plot correlation between microtubule angle and pitch angle of the ascending twined stem. 
ggplot(datum, aes(x=Incline, y=Angle))+
  geom_hline(yintercept = 22.5, linetype="dashed", color = "red", size=1, alpha=.5)+ 
  geom_hline(yintercept = 67.5, linetype="dashed", color = "red", size=1, alpha=.5)+
  geom_hline(yintercept = 112.5, linetype="dashed", color = "red", size=1, alpha=.5)+
  geom_hline(yintercept = 157.5, linetype="dashed", color = "red", size=1, alpha=.5)+
  geom_point(alpha=.35)+
  geom_smooth(method = "lm", se = TRUE, color = "blue") +  # regression line + SEM shading
  scale_y_continuous(limits = c(0, 190), breaks = seq(0, 190, by = 20))+
  scale_x_continuous(limits = c(44, 70), breaks = seq(44, 70, by = 2))+
  theme_classic(base_size = 18)+
  labs(title = "Distribution of MT Angles Internode 6 by inclination angle of twining stem",
       x = "Pitch Angle (Torsion)",
       y = "MT Angle Distribution")

#Perform correlation analysis (nonparametric)
cor.test(formula = ~datum$Incline + datum$Angle, method = "spearman")
