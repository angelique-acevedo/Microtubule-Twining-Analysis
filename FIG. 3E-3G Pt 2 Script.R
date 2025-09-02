###FIGURE 3E-3G Part 2Script
## DESCRIPTION: This code was created to generate density plots in Figures 3E-3G of Acevedo et al. (in preparation), demonstrating the distribution of angle groupings of microtubules across all selected samples and paired with SE bars around the mean.  

#1 Load in dataset and remove NAs
datum<- read.csv("~/Downloads/Hypocotyl_Angle_Stages.csv", stringsAsFactors=TRUE)
datum <- na.omit(datum)

#2 Correct the angle column: Add 180 to negative angles to transform data to be continuous from 0 to 180 degrees
datum$Angle[datum$Angle < 0] <- datum$Angle[datum$Angle < 0] + 180

#3. Filter by Stages and generate a summary statistics dataframe
datum <- datum %>% filter(Stage == "S2")

summary_df <- datum %>%
    group_by(Stage) %>%
  summarise(
    Angle = Angle,
    n=n(),
    mean = mean(Angle, na.rm = TRUE),
    se   = sd(Angle, na.rm = TRUE) / sqrt(n()),  # Standard Error
    .groups = "drop"
  )

#4. Generate density plot 
ggplot (datum, aes(x=Angle))+
  geom_vline(xintercept = 22.5, linetype="dashed", color = "red", size=1, alpha=.5)+ 
  geom_vline(xintercept = 67.5, linetype="dashed", color = "red", size=1, alpha=.5)+
  geom_vline(xintercept = 112.5, linetype="dashed", color = "red", size=1, alpha=.5)+
  geom_vline(xintercept = 157.5, linetype="dashed", color = "red", size=1, alpha=.5)+
  geom_density(color="black", fill="#8DD9A7", alpha=.6)+
  geom_jitter(aes(x=Angle, y = 0.001), width = 0, height = 0.001, 
              alpha = 0.15, size = 9) +
  theme_classic(base_size=50)+
  scale_x_continuous(limits=c(0, 180), breaks = seq (0, 180, by = 20))+
  geom_errorbar(data = summary_df, aes(x = mean,  y=0.01, xmin = mean - se, xmax = mean + se), color = "dark red", linewidth = 5) +
  geom_point(data = summary_df, aes(x = mean, y = 0.01), 
             color = "white", size = 3, shape = 21, fill = "dark red") + # Mean marker
  ylim (0, 0.030)
