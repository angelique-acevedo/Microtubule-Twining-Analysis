###FIGURE 5B, 5D, 5F 
##DESCRIPTION: This script was used to generate boxplots in fig. 5b, 5d, 5f of (Acevedo et al., in prep) of cell area, circularity, and geodesic diameter between the lobes and furrows of selected samples. 

#Load Packages
library(ggplot2)
library(ggsignif)
library(dplyr)
library(cowplot)

#1 Load dataset
data <- read.csv("~/Downloads/FIG_5_Cells.csv", stringsAsFactors=TRUE)

#2. Calculate IQR and threshold ranges for each category (area, circularity, and geodesic diameter)
#AREA
Q1 <- quantile(data$Area, 0.25)
Q3 <- quantile(data$Area, 0.75)
IQR <- Q3 - Q1

lower <- Q1 - 1.5 * IQR
upper <- Q3 + 1.5 * IQR

#CIRCULARITY
Q11 <- quantile(data$Circularity, 0.25)
Q33 <- quantile(data$Circularity, 0.75)
IQR1 <- Q33 - Q11

lower1 <- Q11 - 1.5 * IQR1
upper1 <- Q33 + 1.5 * IQR1

#GEODESIC DIAMETER
Q111 <- quantile(data$GedesicDiameter, 0.25)
Q333 <- quantile(data$GedesicDiameter, 0.75)
IQR2 <- Q333 - Q111

lower11 <- Q111 - 1.5 * IQR2
upper11 <- Q333 + 1.5 * IQR2

#3. Filter outliers from large datasets based on IQR thresholds
data_filtered <- data %>% filter(Area > lower & Area < upper)
data_filtered1 <- data %>% filter(Circularity > lower1 & Circularity < upper1)
data_filtered11 <- data %>% filter(GedesicDiameter > lower11 & GedesicDiameter < upper11)

#4. Complete visualization of boxplots
##AREA BOXPLOT
ggplot(data_filtered, aes(x = Lobe_Furrow, y = Area, fill = Lobe_Furrow)) +
  geom_boxplot() +
  geom_jitter(alpha=.3)+
  labs(title = "Area Internode 1", x = "Furrow or Lobe", y = "Area", at=1) +
  scale_y_continuous(limits = c(0, 3600), breaks = seq (0, 3600, by = 300))+
  scale_fill_manual(values=c("#43884E","#E8F2E1", "#AAAAAA"))+
  theme_classic()+
  facet_wrap(~ID, scales="free")+
  geom_signif(comparisons = list(c("Furrow", "Lobe")), test="wilcox.test", map_signif_level=TRUE)+
     stat_compare_means(aes(group=Lobe_Furrow), 
                     method = "wilcox.test", 
                     label = "p.format", 
                     hide.ns = TRUE)+
  theme_classic()+
  theme(text = element_text(size=22))

#CIRCULARITY BOXPLOT
ggplot(data_filtered1, aes(x = Lobe_Furrow, y = Circularity, fill = Lobe_Furrow)) +
  geom_boxplot() +
  geom_jitter(alpha=.3)+
  labs(title = "Circularity Internode 1", x = "Furrow or Lobe", y = "Circularity", at=1) +
  scale_y_continuous(limits = c(0, 1), breaks = seq (0, 1, by = .1))+
   scale_fill_manual(values=c("#43884E","#E8F2E1", "#AAAAAA"))+
  theme_classic()+
  facet_wrap(~ID, scales="free")+
  geom_signif(comparisons = list(c("Furrow", "Lobe")), test="wilcox.test", map_signif_level=TRUE)+
      stat_compare_means(aes(group=Lobe_Furrow), 
                     method = "wilcox.test", 
                     label = "p.format", 
                     hide.ns = TRUE)+
  theme_classic()+
  theme(text = element_text(size=22))

#GEODESIC DIAMETER BOXPLOT
ggplot(data_filtered11, aes(x = Lobe_Furrow, y = GedesicDiameter, fill = Lobe_Furrow)) +
  geom_boxplot() +
  geom_jitter(alpha=.3)+
  labs(title = "Geodesic Diameter Internode 1", x = "Furrow or Lobe", y = "Geodesic Diameter", at=1) +
  scale_y_continuous(limits = c(0, 160), breaks = seq (0, 160, by = 10))+
   scale_fill_manual(values=c("#43884E","#E8F2E1", "#AAAAAA"))+
  theme_classic()+ 
  facet_wrap(~ID, scales="free")+
  geom_signif(comparisons = list(c("Furrow", "Lobe")),test="wilcox.test", map_signif_level=TRUE)+
     stat_compare_means(aes(group=Lobe_Furrow), 
                     method = "wilcox.test", 
                     label = "p.format", 
                     hide.ns = TRUE)+
  theme_classic()+
  theme(text = element_text(size=22))
