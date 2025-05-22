###FIGURE 6B-6C Script 
## Description: This code was used for generating stacked barplots (Fig.6b-6c; Acevedo et. al., in prep) to indicate the 
## percent (%) composition of longitudinal, skewed, and transverse microtubule angles found along the lobes and furrows of 
## common bean at developmental stage 5, internode 1 and internode 3. 

#Load packages
library(ggplot2)
library (ggpubr)
library (RColorBrewer)
library (tidyverse)
library(dplyr)

#1. Load the dataset and transform microtubule (MT) angles to consider only absolute values (0-90 degrees). 
data <- read.csv("~/Library/CloudStorage/Box-Box/Onyenedum_Lab/Angelique_Acevedo/Projects/2. Bean_Team (NYU)/Immunohistochemistry/Microtubules/MT_Dataset/Lobe or Furrow/S5_IN3_FurrowvLobe.csv", stringsAsFactors=TRUE)
data$Angle[data$Angle < 0] <- (data$Angle[data$Angle < 0]) * (-1)

#2. Assign cut-off values for the MT angle groups (longitudinal, transverse, and skewed. 
data<- data%>%
mutate(Group = ifelse(Angle>=60, "Longitudinal", 
                      ifelse(Angle<=30, "Transverse", "Skewed")))

#3. Calculate the percent frequency of angle occurrence
data_summary <- data %>% 
  group_by(Lobe_Furrow,Group) %>%
  summarize(n = n()) %>% 
  mutate(freq = n / sum(n) * 100, sd = sd(freq))

#4. OPTIONAL: Assign colors to barplot groups (colr) and colors for printing text (col)
colr<- c("Longitudinal" = "#D0352B", "Transverse" = "#3C76AF", "Skewed" = "#FFFFA6")
col<- c("white", "gray", "white", "white", "gray", "white")

#5. Visualize stacked barplot 
p <- ggplot(data_summary, aes(x = Lobe_Furrow, y = freq, fill = Group)) +
  geom_bar(stat = "identity", position = "stack", width=.8, alpha=1, color = "white") +
  ylab("Cell %")+
  scale_y_continuous(limits = c(0, 100), breaks = seq (0, 100, by = 10))+
  geom_text(aes(label = paste0(round(freq, 1), "%")), position= position_stack(vjust = .5), color= col, size = 6)+
  scale_fill_manual(values = c("Longitudinal" = "#D0352B", "Transverse" = "#3C76AF", "Skewed" = "#FFFFA6"))+
  facet_wrap(~Lobe_Furrow, scales = "free_x")+
  theme_classic()+
  theme(text = element_text(size=22))
p
