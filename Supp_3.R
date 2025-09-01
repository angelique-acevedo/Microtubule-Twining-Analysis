###FIGURE Supplemental 3 Script
## DESCRIPTION: This code was created to generate bar plots in Figure 3 of Acevedo et al. (in review), demonstrating the percentage of angle groupings of microtubules across all selected samples and evaluating 
## the overall CMT distribution of inner and outer CMT Angles of the Hypocotyl, using a violin and boxplot + SE bars. 

#1 Load Packages
library(ggplot2)
library(ggsignif)
library(dplyr)
library(ggpubr)

#2 Load Dataset
datum<- read.csv("~/Downloads/Supp_3_Data.csv", stringsAsFactors=TRUE)
datum <- na.omit(datum)

#3 Transform dataset (Angles from range 0 - 180)
datum$Angle[datum$Angle < 0] <- datum$Angle[datum$Angle < 0] + 180

#4. Bin angles based cutoff values
datum$Angle_Category <- cut(
  datum$Angle,
  breaks = c(-Inf, 22.5, 67.5, 112.5, 157.5, Inf),
  labels = c("Transverse", "Right-skewed", "Longitudinal", "Left-skewed", "Transverse"),
  include.lowest = TRUE
)

#5. Filter for Stages
datum<- datum %>% filter(Stage == "S5") #change based on which stage to evaluate

#6 Plot general distribution by inner and outer faces
summary_df <- datum %>%
  group_by(Cell_Type) %>%
  summarise(
    Angle = Angle,
    n=n(),
    mean = mean(Angle, na.rm = TRUE),
    se   = sd(Angle, na.rm = TRUE) / sqrt(n()),  # Standard Error
    .groups = "drop"
  )

ggplot(datum, aes(x = Cell_Type, y = Angle)) +
  geom_hline(yintercept = 22.5, linetype="dashed", color = "red", size=1, alpha=.5)+ 
  geom_hline(yintercept = 67.5, linetype="dashed", color = "red", size=1, alpha=.5)+
  geom_hline(yintercept = 112.5, linetype="dashed", color = "red", size=1, alpha=.7)+
  geom_hline(yintercept = 157.5, linetype="dashed", color = "red", size=1, alpha=.7)+
  geom_violin(trim = FALSE, alpha = 0.6) +   
  geom_boxplot(width = 0.1, outlier.shape = NA) +  # boxplot inside
  geom_jitter(width = 0.15, alpha = 0.35, size = 2)+
  geom_errorbar(data = summary_df,aes(x = Cell_Type, ymin = mean - se, ymax = mean + se), width = 0.2, color = "dark red", linewidth = 0.8) +
  geom_point(data = summary_df, aes(x = Cell_Type, y = mean), 
             color = "red", size = 3, shape = 21, fill = "dark red") + # Mean marker
  stat_compare_means(
    comparisons = list(c("Inner", "Outer")),
    method = "wilcox.test",
    label = "p.format"
  )+
  theme_classic(base_size=22)+
    scale_y_continuous(limits = c(0, 190), breaks = seq(0, 190, by = 20))

#7. Compute percentages for Angle Categories
datum <- datum %>%
  group_by(Cell_Type, Angle_Category) %>%
  summarise(n = n()) %>%
  group_by(Cell_Type) %>%
  mutate(Percentage = n / sum(n) * 100)

#8. Plot Angle Categories via a barplot (percentages)
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
                group = Cell_Type),
            vjust = -0.5, size = 4.5,
            position = position_dodge(width = 0.9)) +
  facet_wrap(~Cell_Type) +   # <-- separate plots per group
  scale_color_manual(values = colr) +
  scale_fill_manual(values = colr) +
  scale_pattern_angle_manual(values = c(0, 45, 90, -45)) +
  labs(title = "Inner vs. Outer Epidermal MT Orientations - Hypocotyl (Stage 5)",
       x = "Angle Category", y = "% of Cells") +
  theme_classic(base_size = 18) +
  theme(
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x  = element_text(angle = -20, hjust = 0.2, vjust=1, size = 14),
    axis.text.y  = element_text(size = 16),
    plot.title   = element_text(size = 20, hjust = 0.5)
  ) +
  ylim(0,100)

print(p)
