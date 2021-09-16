"This script creates scatterplots to show correspondence between PCA solutions
applied to both datasets and each dataset separately. "

# load libraries
library(ggplot2) #for plots
library(plyr) #for data manipulation
library(tidyverse) #for data manipulation
library(ggpubr) #for plots
library(patchwork) #for putting plots together

#set current working directory of ESQ data. 
# See Mendeley data: doi: 10.17632/n3wz7y8mhs.1
setwd("")

#read in csv file with 22-item ESQ data (both ESQ datasets)
dataframe <- read.csv("pnas_lockdown_data_mendeley.csv")

#set current directory to results folder to write output to results/R text file below
setwd("")

#remove empty rows for variables of interest
df1 <- dataframe %>% drop_na(fac1_both_samples, fac2_both_samples, fac3_both_samples, fac4_both_samples, 
                             fac5_both_samples, sample, age_group, physical_social_environment)


# plot both factors and pre factors

#factor 1 both with factor 1 pre
fac1.both.fac1.pre <- ggplot(df1, aes(x=fac1_both_samples, y=fac1_pre_lockdown)) + geom_point(size = 0.00001) +
  labs(y = "Factor 1 pre-lockdown",
       x = "Factor 1 both")+ theme_bw()+ ylim(-4,4)+xlim(-4,4)+ 
  theme(axis.text.y = element_text(size = 8,color = "black"),
        axis.text.x = element_text(size = 8,color = "black"),
        axis.title.y = element_text(size = 8, color = "black",
                                    margin = margin(t = 0, r = 0, b = 0, l = 0)),
        axis.title.x = element_text(size = 8,color = "black",
                                    margin = margin(t = 0, r = 0, b = 0, l = 0)))

#add label for pearson R and p-value to plot
fac1.both.fac1.pre <- fac1.both.fac1.pre +stat_cor(label.x = -4, label.y = 4, method = "pearson",size=2.5, p.accuracy = 0.001, r.accuracy = 0.01)

# factor 2 both with factor 4 pre
fac2.both.fac4.pre <- ggplot(df1, aes(x=fac2_both_samples, y=fac4_pre_lockdown)) + geom_point(size = 0.00001) +
  labs(y = "Factor 4 pre-lockdown",
       x = "Factor 2 both")+ theme_bw()+ ylim(-4,4)+xlim(-4,4)+ 
  theme(axis.text.y = element_text(size = 8,color = "black"),
        axis.text.x = element_text(size = 8,color = "black"),
        axis.title.y = element_text(size = 8, color = "black",
                                    margin = margin(t = 0, r = 0, b = 0, l = 0)),
        axis.title.x = element_text(size = 8,color = "black",
                                    margin = margin(t = 0, r = 0, b = 0, l = 0)))

#add label for pearson R and p-value to plot
fac2.both.fac4.pre <- fac2.both.fac4.pre +stat_cor(label.x = -4, label.y = 4, method = "pearson",size=2.5, p.accuracy = 0.001, r.accuracy = 0.01)

#factor 3 both with factor 2 pre
fac3.both.fac2.pre <- ggplot(df1, aes(x=fac3_both_samples, y=fac2_pre_lockdown)) + geom_point(size = 0.00001) +
  labs(y = "Factor 2 pre-lockdown",
       x = "Factor 3 both")+ theme_bw()+ ylim(-4,4)+xlim(-4,4)+ 
  theme(axis.text.y = element_text(size = 8,color = "black"),
        axis.text.x = element_text(size = 8,color = "black"),
        axis.title.y = element_text(size = 8, color = "black",
                                    margin = margin(t = 0, r = 0, b = 0, l = 0)),
        axis.title.x = element_text(size = 8,color = "black",
                                    margin = margin(t = 0, r = 0, b = 0, l = 0)))

#add label for pearson R and p-value to plot
fac3.both.fac2.pre <- fac3.both.fac2.pre +stat_cor(label.x = -4, label.y = 4, method = "pearson",size=2.5, p.accuracy = 0.001, r.accuracy = 0.01)

#factor 4 both with factor 3 pre
fac4.both.fac3.pre <- ggplot(df1, aes(x=fac4_both_samples, y=fac3_pre_lockdown)) + geom_point(size = 0.00001) +
  labs(y = "Factor 3 pre-lockdown",
       x = "Factor 4 both")+ theme_bw()+ ylim(-4,4)+xlim(-4,4)+ 
  theme(axis.text.y = element_text(size = 8,color = "black"),
        axis.text.x = element_text(size = 8,color = "black"),
        axis.title.y = element_text(size = 8, color = "black",
                                    margin = margin(t = 0, r = 0, b = 0, l = 0)),
        axis.title.x = element_text(size = 8,color = "black",
                                    margin = margin(t = 0, r = 0, b = 0, l = 0)))

#add label for pearson R and p-value to plot
fac4.both.fac3.pre <- fac4.both.fac3.pre +stat_cor(label.x = -4, label.y = 4, method = "pearson",size=2.5, p.accuracy = 0.001, r.accuracy = 0.01)

# factor 5 both with factor 5 pre
fac5.both.fac5.pre <- ggplot(df1, aes(x=fac5_both_samples, y=fac5_pre_lockdown)) + geom_point(size = 0.00001) +
  labs(y = "Factor 5 pre-lockdown",
       x = "Factor 5 both")+ theme_bw()+ ylim(-4,4)+xlim(-4,4)+ 
  theme(axis.text.y = element_text(size = 8,color = "black"),
        axis.text.x = element_text(size = 8,color = "black"),
        axis.title.y = element_text(size = 8, color = "black",
                                    margin = margin(t = 0, r = 0, b = 0, l = 0)),
        axis.title.x = element_text(size = 8,color = "black",
                                    margin = margin(t = 0, r = 0, b = 0, l = 0)))

#add label for pearson R and p-value to plot
fac5.both.fac5.pre <- fac5.both.fac5.pre +stat_cor(label.x = -4, label.y = 4, method = "pearson",size=2.5, p.accuracy = 0.001, r.accuracy = 0.01)

#both factors and post-lockdown factors

#factor 1 both with factor 1 post
fac1.both.fac1.post <- ggplot(df1, aes(x=fac1_both_samples, y=fac1_post_lockdown)) + geom_point(size = 0.00001) +
  labs(y = "Factor 1 during-lockdown",
       x = "Factor 1 both")+ theme_bw()+ ylim(-4,4)+xlim(-4,4)+ 
  theme(axis.text.y = element_text(size = 8,color = "black"),
        axis.text.x = element_text(size = 8,color = "black"),
        axis.title.y = element_text(size = 8, color = "black",
                                    margin = margin(t = 0, r = 0, b = 0, l = 0)),
        axis.title.x = element_text(size = 8,color = "black",
                                    margin = margin(t = 0, r = 0, b = 0, l = 0)))
#add label for pearson R and p-value to plot
fac1.both.fac1.post <- fac1.both.fac1.post +stat_cor(label.x = -4, label.y = 4, method = "pearson",size=2.5, p.accuracy = 0.001, r.accuracy = 0.01)

#factor 2 both with factor 2 post
fac2.both.fac2.post <- ggplot(df1, aes(x=fac2_both_samples, y=fac2_post_lockdown)) + geom_point(size = 0.00001) +
  labs(y = "Factor 2 during-lockdown",
       x = "Factor 2 both")+ theme_bw()+ ylim(-4,4)+xlim(-4,4)+ 
  theme(axis.text.y = element_text(size = 8,color = "black"),
        axis.text.x = element_text(size = 8,color = "black"),
        axis.title.y = element_text(size = 8, color = "black",
                                    margin = margin(t = 0, r = 0, b = 0, l = 0)),
        axis.title.x = element_text(size = 8,color = "black",
                                    margin = margin(t = 0, r = 0, b = 0, l = 0)))

#add label for pearson R and p-value to plot
fac2.both.fac2.post <- fac2.both.fac2.post+stat_cor(label.x = -4, label.y = 4, method = "pearson",size=2.5, p.accuracy = 0.001, r.accuracy = 0.01)

#factor 3 both with factor 3 post
fac3.both.fac3.post <- ggplot(df1, aes(x=fac3_both_samples, y=fac3_post_lockdown)) + geom_point(size = 0.00001) +
  labs(y = "Factor 3 during-lockdown",
       x = "Factor 3 both")+ theme_bw()+ ylim(-4,4)+xlim(-4,4)+ 
  theme(axis.text.y = element_text(size = 8,color = "black"),
        axis.text.x = element_text(size = 8,color = "black"),
        axis.title.y = element_text(size = 8, color = "black",
                                    margin = margin(t = 0, r = 0, b = 0, l = 0)),
        axis.title.x = element_text(size = 8,color = "black",
                                    margin = margin(t = 0, r = 0, b = 0, l = 0)))

#add label for pearson R and p-value to plot
fac3.both.fac3.post <- fac3.both.fac3.post+stat_cor(label.x = -4, label.y = 4, method = "pearson",size=2.5, p.accuracy = 0.001, r.accuracy = 0.01)

#factor 4 both with factor 4 post
fac4.both.fac4.post <- ggplot(df1, aes(x=fac4_both_samples, y=fac4_post_lockdown)) + geom_point(size = 0.00001) +
  labs(y = "Factor 4 during-lockdown",
       x = "Factor 4 both")+ theme_bw()+ ylim(-4,4)+xlim(-4,4)+ 
  theme(axis.text.y = element_text(size = 8,color = "black"),
        axis.text.x = element_text(size = 8,color = "black"),
        axis.title.y = element_text(size = 8, color = "black",
                                    margin = margin(t = 0, r = 0, b = 0, l = 0)),
        axis.title.x = element_text(size = 8,color = "black",
                                    margin = margin(t = 0, r = 0, b = 0, l = 0)))

#add label for pearson R and p-value to plot
fac4.both.fac4.post <- fac4.both.fac4.post +stat_cor(label.x = -4, label.y = 4, method = "pearson",size=2.5, p.accuracy = 0.001, r.accuracy = 0.01)

# factor 5 both with factor 5 post
fac5.both.fac5.post <- ggplot(df1, aes(x=fac5_both_samples, y=fac5_post_lockdown)) + geom_point(size = 0.00001) +
  labs(y = "Factor 5 during-lockdown",
       x = "Factor 5 both")+ theme_bw()+ ylim(-4,4)+xlim(-4,4)+ 
  theme(axis.text.y = element_text(size = 8,color = "black"),
        axis.text.x = element_text(size = 8,color = "black"),
        axis.title.y = element_text(size = 8, color = "black",
                                    margin = margin(t = 0, r = 0, b = 0, l = 0)),
        axis.title.x = element_text(size = 8,color = "black",
                                    margin = margin(t = 0, r = 0, b = 0, l = 0)))


fac5.both.fac5.post <- fac5.both.fac5.post+stat_cor(label.x = -4, label.y = 4, method = "pearson",size=2.5, p.accuracy = 0.001, r.accuracy = 0.01)

#plot all 10 plots in one figure using patchwork
all_plots <- (fac1.both.fac1.pre | fac2.both.fac4.pre | fac3.both.fac2.pre | fac4.both.fac3.pre | fac5.both.fac5.pre)/
  (fac1.both.fac1.post | fac2.both.fac2.post | fac3.both.fac3.post | fac4.both.fac4.post | fac5.both.fac5.post) & theme(plot.margin =
                                                                                                                          unit(c(0.1, 0.25, 0.4, 0),
                                                                                                                               "cm")) #top, right, bottom, left)

#save all plots as tiff image
ggsave(
  "pca_thoughts_scatterplots_both_and_separate_2_column.tiff",
  all_plots, units = "cm",
  width =  17.8,
  height = 9.5,
  dpi = 1000, 
)

