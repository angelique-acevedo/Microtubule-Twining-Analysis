###Supplemental Figure 1
## Description: This script was used to generate line and boxplots in supplemental figure 1 of (Acevedo et al., in prep), demonstrating
## the progression of internode elongation across stages (code was made with assistance from Dr. Joyce Onyenedum). 

##LINEPLOT
#1. Load the dataset and packages
library(ggplot2)
library(rstatix)
library(ggpubr)

datum <-  read.csv("~/Downloads/Supp_1_Data_pt1.csv", stringsAsFactors=TRUE) 

#2. Arrange the order in which $stages and $section will appear on the graph and omit all NAs from dataset
datum$Stage <- factor(datum$Stage,levels = c("S2", "S3", "S5"))
datum$Section <- factor(datum$Section,levels = c("Hyp","IN1", "IN3", "IN6"))
datum<- na.omit (datum)

#3. Generate a summary of statistics to assist with SD/SE on lineplot
#calculate summary statistics
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE),
      se = sqrt(var(x[[col]])/length(x[[col]])))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}
head(datum$Height)
df3 <- data_summary(datum, varname="Height",  groupnames=c("Section", "Stage"))

#OPTIONAL: Prevent overlapping of datapoints 
pd <- position_dodge(0.3) # move them .05 to the left and right

#4. Complete visualization of this line plot showing height across stages for the four selected internodes 
p<- ggplot(df3, aes(x=Stage, y= Height, colour=Section, group=Section))+ 
  geom_errorbar(aes(ymin= Height-se, ymax= Height+se), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=2.5) + 
  xlab("Stage") +
  ylab("Internode length (cm)") +
  ggtitle("Internode Development through Stages")  + theme(plot.title = element_text(size = 7), 
                                                           axis.text.x = element_text(size= 7), 
                                                           axis.text.y = element_text(size = 7,), 
                                                           axis.title.x = element_text(size = 7),  
                                                           axis.title.y= element_text(size = 7), legend.position = "top") + theme_bw(base_size = 18)+
  theme_bw(base_size = 20) +
  scale_color_manual(values = c("#225EA8", "#FE9929", "#CE1256", "#88419D"))+
  ylim(0, 15) 
p

##BOXPLOTS
#1. Load in packages and import in dataset
library(ggplot2)
library(rstatix)
library(ggpubr)

#import data
datum <-  read.csv("~/Downloads/Supp_1_Data_pt2.csv", stringsAsFactors=TRUE) 
datum<- na.omit (datum)

datum <- datum %>% filter(Stage == "S2")

#2. Arrange the order in which $section will appear on the graph 
datum$Section <- factor(datum$Section,levels = c("Hyp", "Epi","IN1", "IN2", "IN3", "IN4", "IN5", "IN6", "IN7", "IN8", "IN9"))

#3. Test if data is normally distributed - if p value is < .05, then it is not normal
head(datum$Height)
model  <- lm(Height ~ Section, data = datum)
ggqqplot(residuals(model))
shapiro_test(residuals(model))

#KRUSKAL if data is non normal use this (correct one to use)
res.kruskal <- datum %>% kruskal_test(Height ~ Section)
pwc <- datum %>% dunn_test(Height ~ Section, p.adjust.method = "bonferroni") 
pwc <- pwc %>% add_xy_position(x = "Group")

#ANOVA if data is normal use this
#anova <- datum %>% t_test(Height ~ Section)
#pwc <- datum %>% tukey_hsd(Height ~ Section, p.adjust.method = "bonferroni") 
#pwc <- pwc %>% add_xy_position(x = "Treatment

#4. Generates boxplots with data points jittered for each stage and selected sample 
ggboxplot(datum, x = "Section", y = "Height",
          title= "Length_of_Internodes(cm)", add = "jitter", fill= "Section") +
  #stat_pvalue_manual(pwc, hide.ns = TRUE, y.position = 12) +
  #labs(subtitle = get_test_label(res.kruskal, detailed = TRUE),
  # caption = get_pwc_label(pwc)) +
  theme_bw(base_size = 20) +
  ylim(0,5)+
  ggtitle("Length of Internodes") + 
  xlab("Internodes") + ylab("Length (cm)") + 
  scale_fill_manual(values = c("#225EA8", "#A5A6A5", "#FE9929","#A5A6A5", "#CE1256","#A5A6A5", "#A5A6A5", "#88419D", "#A5A6A5",  "#A5A6A5", "#A5A6A5"))+
  theme (plot.title = element_text(size = 20), 
         axis.text.x = element_text(angle= 90, size = 12), 
         axis.text.y = element_text(size = 16), 
         axis.title.x = element_text(size = 16),  
         axis.title.y= element_text(size = 16))
