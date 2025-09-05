###FIGURE 5
##DESCRIPTION: This script was used to generate boxplots in fig. 5a-i of (Acevedo et al., in prep) of cell area, circularity, and geodesic diameter between the lobes and furrows of selected samples. 

# Load Packages
library(ggplot2)
library(ggsignif)
library(dplyr)
#library(cowplot)
library(ggpubr)

# 1. Load dataset
data <- read.csv("~/Downloads/FIG_5_Data.csv", stringsAsFactors = TRUE)

#Filter out objects that got segmented as 'cells'. Area of <100 and GD <10 are too small to have been cells
data <- data %>%
  group_by(ID) %>%
  filter(Area > 100) %>%
  filter(GedesicDiameter > 10)

# 1.5 Test dataset for normality 
##AREA
normality_results <- data %>%
  group_by(ID, Lobe_Furrow) %>%
  summarise(
    shapiro_p = shapiro.test(Area)$p.value,
    .groups = "drop"
  )
print(normality_results) #S5_IN1 Furrow is normal (pvalue = 7.64x10e-01) but the rest are not!

##CIRCULARTIY
normality_results1 <- data %>%
  group_by(ID, Lobe_Furrow) %>%
  summarise(
    shapiro_p = shapiro.test(Circularity)$p.value,
    .groups = "drop"
  )
print(normality_results1)

##GEODESIC DIAMETER
normality_results2 <- data %>%
  group_by(ID, Lobe_Furrow) %>%
  summarise(
    shapiro_p = shapiro.test(GedesicDiameter)$p.value,
    .groups = "drop"
  )
print(normality_results2)
##Due to the variability of normality and non-normalcy, we've decided to select a nonparametric test such as wilcoxon testing

# 2. Function to remove outliers per facet (ID × Lobe_Furrow)
remove_outliers <- function(df, value_col) {
  df %>%
    group_by(ID, Lobe_Furrow) %>%
    mutate(
      Q1 = quantile(.data[[value_col]], 0.25, na.rm = TRUE),
      Q3 = quantile(.data[[value_col]], 0.75, na.rm = TRUE),
      IQR = Q3 - Q1,
      lower = Q1 - 1.5 * IQR,
      upper = Q3 + 1.5 * IQR
    ) %>%
    ungroup() %>%
    filter(.data[[value_col]] >= lower & .data[[value_col]] <= upper) %>%
    select(-Q1, -Q3, -IQR, -lower, -upper)
}


# 3. Apply per variable
data_area <- remove_outliers(data, "Area")
data_circ <- remove_outliers(data, "Circularity")
data_geo  <- remove_outliers(data, "GedesicDiameter")

# 4. Visualizations
## AREA BOXPLOT
ggplot(data_area, aes(x = Lobe_Furrow, y = Area, fill = Lobe_Furrow)) +
  geom_boxplot() +
  geom_jitter(alpha=.3) +
  labs(title = "Area Internode 1", x = "Furrow or Lobe", y = "Area") +
  scale_y_continuous(limits = c(0, 3600), breaks = seq(0, 3600, by = 300)) +
  scale_fill_manual(values=c("#43884E","#E8F2E1", "#AAAAAA")) +
  facet_wrap(~ID, scales="free") +
  stat_compare_means(
    comparisons = list(c("Furrow", "Lobe")),
    method = "wilcox.test",
    label = "p.format",
    p.adjust.method = "bonferroni"
  )+ 
  theme_classic(base_size=22)

## CIRCULARITY BOXPLOT
ggplot(data_circ, aes(x = Lobe_Furrow, y = Circularity, fill = Lobe_Furrow)) +
  geom_boxplot() +
  geom_jitter(alpha=.3) +
  labs(title = "Circularity", x = "Furrow or Lobe", y = "Circularity") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = .1)) +
  scale_fill_manual(values=c("#43884E","#E8F2E1", "#AAAAAA")) +
  facet_wrap(~ID, scales="free") +
  stat_compare_means(
    comparisons = list(c("Furrow", "Lobe")),
    method = "wilcox.test",
    label = "p.format",
    p.adjust.method = "bonferroni"
  )+ 
  theme_classic(base_size=22)

## GEODESIC DIAMETER BOXPLOT
ggplot(data_geo, aes(x = Lobe_Furrow, y = GedesicDiameter, fill = Lobe_Furrow)) +
  geom_boxplot() +
  geom_jitter(alpha=.3) +
  labs(title = "Geodesic Diameter", x = "Furrow or Lobe", y = "Geodesic Diameter") +
  scale_y_continuous(limits = c(0, 200), breaks = seq(0, 200, by = 10)) +
  scale_fill_manual(values=c("#43884E","#E8F2E1", "#AAAAAA")) +
  facet_wrap(~ID, scales="free") +
  stat_compare_means(
    comparisons = list(c("Furrow", "Lobe")),
    method = "wilcox.test",
    label = "p.format",
    p.adjust.method = "bonferroni"
  )+
  theme_classic(base_size=22)

                     hide.ns = TRUE)+
  theme_classic()+
  theme(text = element_text(size=22))
