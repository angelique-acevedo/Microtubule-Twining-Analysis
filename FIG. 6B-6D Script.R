###FIGURE 6B-D Script 
## Description: This code was used for generating stacked barplots (Fig.6b-6d; Acevedo et. al., in prep) to indicate the 
## percent (%) composition of longitudinal, skewed, and transverse microtubule angles found along the lobes and furrows of 
## common bean at developmental stage 5, internode 1, and internode 3. 

#1. Load packages
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(ggpattern)

#2. Load in dataset and clean
datum <- read.csv("~/Downloads/FIG_6_IN1_FurrowvLobe.csv", stringsAsFactors=TRUE)
view(datum)
#3. Correct the angle column
datum$Angle[datum$Angle < 0] <- datum$Angle[datum$Angle < 0] + 180


#4. Bin angles
datum$Angle_Category <- cut(
  datum$Angle,
  breaks = c(-Inf, 22.5, 67.5, 112.5, 157.5, Inf),
  labels = c("Transverse", "Right-skewed", "Longitudinal", "Left-skewed", "Transverse"),
  include.lowest = TRUE
)

#5. Filter for Stage 3
#stage3_data <- datum %>% filter(Stage == "S3")


#7. Compute percentages *within each Lobe_Furrow*
datum <- datum %>%
  group_by(Lobe_Furrow, Angle_Category) %>%
  summarise(n = n()) %>%
  group_by(Lobe_Furrow) %>%
  mutate(Percentage = n / sum(n) * 100)

#8. Plot grouped bars
colr <- c("#8AB0D0", "#F2B770", "#EB8777", "#B7D876")

p <- ggplot(datum, aes(x = Angle_Category, 
                               y = Percentage, 
                               fill = Angle_Category,
                               pattern_angle = Angle_Category)) +
  geom_col_pattern(aes(color = Angle_Category),
                   pattern_fill="white", 
                   pattern_color="white", 
                   pattern_spacing=.05,
                   position = position_dodge(width = 0.9)) +
  
  geom_text(aes(label = paste0(round(Percentage, 1), "%"),
                group = Lobe_Furrow),
            vjust = -0.5, size = 5,
            position = position_dodge(width = 0.9)) +
  
  facet_wrap(~Lobe_Furrow) +   # <-- separate plots per group
  scale_color_manual(values = colr) +
  scale_fill_manual(values = colr) +
  scale_pattern_angle_manual(values = c(0, 45, 90, -45)) +
  
  labs(title = "Internode 3 Angle Distribution Lobe vs. Furrow",
       x = "Angle Category", y = "% of Cells") +
  theme_classic(base_size = 18) +
  theme(
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x  = element_text(angle = -20, hjust = .5, vjust=0, size = 14),
    axis.text.y  = element_text(size = 16),
    plot.title   = element_text(size = 20, hjust = 0.5)
  ) +
  ylim(0,100)

print(p)

#Violin Plot 
#Load in packages
library (ggpubr)
library (RColorBrewer)
library (tidyverse)
library(dplyr)

#1. Load the dataset and transform microtubule (MT) angles to consider only absolute values (0-90 degrees). 
data <-read.csv("~/Downloads/FIG_6_IN1_FurrowvLobe.csv", stringsAsFactors=TRUE)
data$Angle[data$Angle < 0] <- (data$Angle[data$Angle < 0]) + 180

#2. Calculate mean and CI intervals
datum <- data %>%
  dplyr::group_by(Lobe_Furrow)%>%
  dplyr::summarise(
    Angle = Angle,
    mean = mean(Angle), 
    lci = t.test(Angle, conf.level =.95)$conf.int[1], 
    uci = t.test(Angle, conf.level =.95)$conf.int[2])


#3. Plot distribution 
ggplot(datum, aes(y = Lobe_Furrow, fill = Lobe_Furrow))+
  #geom_density(aes(y = Lobe_Furrow, x = Angle))+
  #geom_density_ridges() +
  geom_point(aes(x = Angle), color = "black", size = 1.5, alpha = .75) +
  geom_violin(aes(y = Lobe_Furrow, x =Angle), alpha = 0.7, color = "black") +
  scale_fill_manual(values = c("#31A35499", "#E5F5E099")) +
  geom_vline(xintercept = 22.5, linetype="dashed", color = "red", size=1, alpha=.5)+ 
  geom_vline(xintercept = 67.5, linetype="dashed", color = "red", size=1, alpha=.5)+
  geom_vline(xintercept = 112.5, linetype="dashed", color = "red", size=1, alpha=.5)+
  geom_vline(xintercept = 157.5, linetype="dashed", color = "red", size=1, alpha=.5)+
  geom_errorbar(aes(y = Lobe_Furrow, xmin=lci, xmax=uci), size=.9, width = .25)+
  geom_point(aes(y = Lobe_Furrow, x = mean), color = "red", size =3, alpha=.2)+
  scale_x_continuous(limits=c(0, 180), breaks = seq (0, 180, by = 10))+  
  theme_classic(base_size = 20) +
  coord_cartesian(ylim = c(0.5, 7.5))
#View (datum)
