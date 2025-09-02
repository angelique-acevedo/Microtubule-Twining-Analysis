###FIGURE 3E-3G Script
## DESCRIPTION: This code was created to generate bar plots in Figures 3E-3G of Acevedo et al. (in preparation), demonstrating the percentage of angle groupings of microtubules across all selected samples. 

#1. Load library packages
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library (ggpattern)

#2. Load in dataset and remove NAs from data. Datasets for this graph include read.csv("~/Downloads/[Hypocotyl,IN1,IN3,IN6]_Angle_Stages.csv", stringsAsFactors=TRUE)
datum<- read.csv("~/Downloads/Hypocotyl_Angle_Stages.csv", stringsAsFactors=TRUE)
datum <- na.omit(datum)

#3. Correct the angle column: Add 180 to negative angles to transform data to be continuous from 0 to 180 degrees
datum$Angle[datum$Angle < 0] <- datum$Angle[datum$Angle < 0] + 180

#4. Bin the angles into four categories based on angle cutoffs
datum$Angle_Category <- cut(
  datum$Angle,
  breaks = c(-Inf, 22.5, 67.5, 112.5, 157.5, Inf),
  labels = c("Transverse", "Right-skewed", "Longitudinal", "Left-skewed", "Transverse"),
  include.lowest = TRUE
)

#5. Filter for Stage 2
stage2_data <- datum %>% filter(Stage == "S2")

#6. Get the count of each angle category for Stage 2
stage2_counts <- table(stage2_data$Angle_Category)


#7. Calculate cell percentages based on the binning system
stage2_df <- as.data.frame(stage2_counts)
stage2_df$Percentage <- (stage2_df$Freq / sum(stage2_df$Freq)) * 100

#8. Visualize counts with percentages as labels
colr <- c("#8AB0D0", "#F2B770", "#EB8777", "#B7D876") #Main background color of barplor

p <- ggplot(stage2_df, aes(x = factor(Var1,
                                      levels = c("Transverse",
                                                 "Right-skewed",
                                                 "Left-skewed",
                                                 "Longitudinal")),
                           y = Percentage, fill=Var1)) +
  geom_bar(stat = "identity", color = "black") +  
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),
            vjust = -0.5, size = 6) +  # Percentage labels slightly smaller
  labs(
    title = "Internode 6 Angle Distribution for Stage 3",
    x = "Angle Category",
    y = "% of Cells"
  ) +
#ADD STRIPED TO BARPLOTS
 geom_col_pattern(aes(pattern_angle=Var1, color=Var1, fill=Var1),
                   pattern_fill="white", pattern_color="white", pattern_spacing=.05) +
  scale_color_manual(values = colr) +
  scale_fill_manual(values = colr) +
  scale_pattern_angle_manual(values = c(0, 45, 90, -45)) +
  
  theme_classic(base_size = 20) +  # Set base font size to 6
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(angle = -20,hjust = .5, vjust=0, size = 16),
    axis.text.y = element_text(size = 20),
    plot.title = element_text(size = 20, hjust = 0.5)
  )+
  ylim(0,100)

print(p)
