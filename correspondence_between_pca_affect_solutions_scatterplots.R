"This script creates scatterplots to show correspondence between affect PCA solutions
applied to both datasets and each dataset separately."

#load libraries
library(ggplot2) #for plots
library(plyr) #for data manipulation
library(tidyverse) #for data manipulation
library(ggpubr) #for plots
library(patchwork) #for putting plots together

#set current working directory of ESQ data. 
setwd("C:\\Users\\bront\\OneDrive\\Documents\\PhD\\Projects\\covid_study_2.7\\data\\processed_data\\mendeley")

#read in csv file with 22-item ESQ data (both ESQ datasets)
dataframe <- read.csv("pnas_lockdown_data_mendeley.csv")

#set current directory to results folder to write output to results/R text file below
setwd("C:\\Users\\bront\\OneDrive\\Documents\\PhD\\Projects\\covid_study_2.7\\results\\R")

#remove empty rows for variables of interest
df1 <- dataframe %>% drop_na(fac1_affect_both_samples, fac2_affect_both_samples, sample,
                             age_group, physical_social_environment)


# plot both factors and pre factors

#factor 1 both with factor 1 pre
fac1.both.fac1.pre <- ggplot(df1, aes(x=fac1_affect_both_samples, y=fac1_affect_pre_lockdown)) + geom_point(size = 0.00001) +
  labs(y = "Factor 1 pre-lockdown",
       x = "Factor 1 both")+ theme_bw()+ ylim(-5,5)+xlim(-5,5)+ 
  theme(axis.text.y = element_text(size = 8,color = "black"),
        axis.text.x = element_text(size = 8,color = "black"),
        axis.title.y = element_text(size = 8, color = "black",
                                    margin = margin(t = 0, r = 0, b = 0, l = 0)),
        axis.title.x = element_text(size = 8,color = "black",
                                    margin = margin(t = 0, r = 0, b = 0, l = 0)))

#add label for pearson R and p-value to plot
fac1.both.fac1.pre <- fac1.both.fac1.pre +stat_cor(label.x = -4, label.y = 4, method = "pearson",size=2.5, p.accuracy = 0.001, r.accuracy = 0.001)

#factor 2 both with factor 2 pre
fac2.both.fac2.pre <- ggplot(df1, aes(x=fac2_affect_both_samples, y=fac2_affect_pre_lockdown)) + geom_point(size = 0.00001) +
  labs(y = "Factor 2 pre-lockdown",
       x = "Factor 2 both")+ theme_bw()+ ylim(-5,5)+xlim(-5,5)+ 
  theme(axis.text.y = element_text(size = 8,color = "black"),
        axis.text.x = element_text(size = 8,color = "black"),
        axis.title.y = element_text(size = 8, color = "black",
                                    margin = margin(t = 0, r = 0, b = 0, l = 0)),
        axis.title.x = element_text(size = 8,color = "black",
                                    margin = margin(t = 0, r = 0, b = 0, l = 0)))

#add label for pearson R and p-value to plot
fac2.both.fac2.pre <- fac2.both.fac2.pre +stat_cor(label.x = -4, label.y = 4, method = "pearson",size=2.5, p.accuracy = 0.001, r.accuracy = 0.001)


#both factors and post-lockdown factors

#factor 1 both with factor 1 post
fac1.both.fac1.post <- ggplot(df1, aes(x=fac1_affect_both_samples, y=fac1_affect_post_lockdown)) + geom_point(size = 0.00001) +
  labs(y = "Factor 1 during-lockdown",
       x = "Factor 1 both")+ theme_bw()+ ylim(-5,5)+xlim(-5,5)+ 
  theme(axis.text.y = element_text(size = 8,color = "black"),
        axis.text.x = element_text(size = 8,color = "black"),
        axis.title.y = element_text(size = 8, color = "black",
                                    margin = margin(t = 0, r = 0, b = 0, l = 0)),
        axis.title.x = element_text(size = 8,color = "black",
                                    margin = margin(t = 0, r = 0, b = 0, l = 0)))
#add label for pearson R and p-value to plot
fac1.both.fac1.post <- fac1.both.fac1.post +stat_cor(label.x = -4, label.y = 4, method = "pearson",size=2.5, p.accuracy = 0.001, r.accuracy = 0.001)

#factor 2 both with factor 2 post
fac2.both.fac2.post <- ggplot(df1, aes(x=fac2_affect_both_samples, y=fac2_affect_post_lockdown)) + geom_point(size = 0.00001) +
  labs(y = "Factor 2 during-lockdown",
       x = "Factor 2 both")+ theme_bw()+ ylim(-5,5)+xlim(-5,5)+ 
  theme(axis.text.y = element_text(size = 8,color = "black"),
        axis.text.x = element_text(size = 8,color = "black"),
        axis.title.y = element_text(size = 8, color = "black",
                                    margin = margin(t = 0, r = 0, b = 0, l = 0)),
        axis.title.x = element_text(size = 8,color = "black",
                                    margin = margin(t = 0, r = 0, b = 0, l = 0)))

#add label for pearson R and p-value to plot
fac2.both.fac2.post <- fac2.both.fac2.post+stat_cor(label.x = -4, label.y = 4, method = "pearson",size=2.5, p.accuracy = 0.001, r.accuracy = 0.001)


#plot all 4 plots in one figure using patchwork
all_plots <- (fac1.both.fac1.pre | fac2.both.fac2.pre)/
  (fac1.both.fac1.post | fac2.both.fac2.post ) & theme(plot.margin = unit(c(0.1, 0.25, 0.4, 0),"cm")) #top, right, bottom, left)

#save all plots as tiff image
ggsave(
  "pca_affect_scatterplots_both_and_separate_2_column.tiff",
  all_plots, units = "cm",
  width =  17.8,
  height = 9.5,
  dpi = 1000, 
)

