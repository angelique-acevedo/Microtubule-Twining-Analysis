###TABLE 1 AND TABLE 2
##DESCRIPTION: This script was written by Dr. Joyce Onyenedum and modified by Angelique Acevedo to define statistical significance of microtubule angle groups using chi-square residuals based on their point in development and across our four selected samples. 
##Script was adjusted for lobe vs. furrow analysis and overall angle distribution analysis in (Acevedo et al., in prep). Datasets include [Hypocotyl,IN1,IN3,IN6_Angle_Stages.csv] AND [FIG_6_IN1,IN3_FurrowvLobe.csv]. 

#Load Packages
library(ggplot2)
library(dplyr)

#1. Input the file for lobe v. furrow analysis (use 'Hypocotyl_Angle_Stages.csv' for overall MT distribution analysis)
datum <-read.csv("~/Downloads/FIG_6_IN3_FurrowvLobe.csv", stringsAsFactors=TRUE)

#2. Correct the angle column: Add 180 to negative angles for 'Hypocotyl_Angle_Stages.csv' OR take absolute values for [FIG_6_IN1,IN3_FurrowvLobe.csv]
datum$Angle[datum$Angle < 0] <- datum$Angle[datum$Angle < 0] + 180

#3. Bin the angles into four categories
datum$Angle_Category <- cut(
  datum$Angle,
  breaks = c(-Inf, 22.5, 67.5, 112.5, 157.5, Inf),
  labels = c("Transverse", "Right-Handed", "Longitudinal", "Left-Handed", 
            "Transverse"),
  include.lowest = TRUE
)

#4. Filter for Lobe or furrow, here we are looking at lobe
stage2_data <- datum %>% filter(Lobe_Furrow == "Lobe  ")

#5. Get count of each angle category for lobe
stage2_counts <- table(stage2_data$Angle_Category)
View(stage2_counts)

#6. Perform Chi-square test to see if counts differ significantly
chi_test <- chisq.test(stage2_counts)
print(chi_test)

#8. Get standardized residuals to identify dominant categories
residuals <- chi_test$stdres
print(residuals)

#9. Calculate percentages
stage2_df <- as.data.frame(stage2_counts)
stage2_df$Percentage <- (stage2_df$Freq / sum(stage2_df$Freq)) * 100

# 10. Post-hoc test if significant (if necessary)
if (chi_test$p.value < 0.05) {
  print("Significant difference found. Perform post-hoc analysis if necessary.")
} else {
  print("No significant difference found.")
}
#pairwise.t.test(datum$Angle_Category, datum$Lobe_Furrow, p.adj='bonferroni')
