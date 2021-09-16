"This script creates location pie chart for lockdown sample and social environment
bar chart for both samples which are presented in figure 1 of manuscript.
It also computes two-way anova for 'alone' percentage between samples and age groups."

################################################################################

# load libraries
library(plyr)# data manipulation
library(tidyverse)#for removing any rows with missing values.
library(dplyr)# data manipulation
library(reshape2)# data manipulation
library(patchwork)# putting together plots
library(ggpubr) # for graphs
library(effectsize) # for calculating partial eta sqaured for anova
library(rcompanion) # for calculating means
library(car) # for computing two-way anova

########################## Read in ESQ data ####################################

#set current working directory of ESQ data. 
setwd("")

# read in csv file with 22-item ESQ data 
dataframe <- read.csv("pnas_lockdown_data_mendeley.csv",
                      na.strings=c(""," ","NA", "nan"))


#set current working directory of results folder for saving out graphs
setwd("")

#Remove rows with missing values for variables of interest.
df1 <- dataframe %>% drop_na(physical_social_environment,
                             location)

########################## Location pie chart ##################################
# Prepare data for location pie chart

# set location as factor and check levels
df1$location <- as.factor(df1$location) 
levels(df1$location)

# create new dataframe with frequency of each location
df2 <- data.frame(table(df1$location))

# add percentage column by dividing frequency by total number of rows in df1
df2 <- transform(df2, percent = Freq / nrow(df1)*100)


# set variable column as factor for modeling below.
df2$Var1 <- as.factor(df2$Var1)
#to check factor and factor levels
levels(df2$Var1)

# to change order of levels:
df2$Var1 <- factor(df2$Var1, levels = c("Inside at home",
                                        "Inside at workplace",
                                        "Inside shop",
                                        "Outside in nature",
                                        "Outside in a town/city",
                                        "Outside (other)",
                                        "Inside (other)"), 
                     ordered = FALSE) #SET ORDERED TO FALSE
#to check levels again.
levels(df2$Var1)

# create a basic bar chart
pie = ggplot(df2, aes(x="", y=percent, fill=Var1)) + geom_bar(stat="identity", width=1)

# Convert to pie (polar coordinates) with labels
pie = pie + coord_polar("y", start=0) 

# Add color scale (hex colors)
pie = pie + scale_fill_manual(values=c("#2C3531", "#79927A", "#AC99BA", "#D3E6D2",
                                       "#B6BDBD", "#808080","#EBF0BB")) 

# Remove labels and title
pie = pie + labs(x = NULL, y = NULL, fill = NULL, title = NULL)

# Tidy up the theme
pie = pie + theme_classic() + theme(axis.line = element_blank(),
                                    axis.text = element_blank(),
                                    axis.ticks = element_blank(),
                                    plot.title = element_text(hjust = 0.5, color = "#666666"),
                                    legend.text = element_text(color = "black", size = 8,face = "bold"),
                                    legend.position = "right", legend.key.size = unit(1,"line"))


# show pie chart
pie

# save location frequency values as csv file
write.csv(df2,'location_frequency.csv')

######################## Social environment bar chart ##########################
#Remove rows with missing values for variables of interest.
#only need to get rid of rows which have a missing value for Age group or Alone
df3 <- dataframe %>% drop_na(sample, age_group, physical_social_environment)

## Prepare age group variable
# Recode strings for Age Group for making plots below.
df3$age_group <- as.character(df3$age_group) 
df3$age_group[ df3$age_group== "young"] <- "Younger"
df3$age_group[ df3$age_group== "old"] <- "Older" 

#specify Age Group as a factor for graph below
df3$age_group <- as.factor(df3$age_group) 
#to check that Age Group is a factor, run this command and see print out.
class(df3$age_group)
#to check the levels of this factor, run this command and see print out.
levels(df3$age_group)

#Change order of levels to make Young the first level and old the second level.
df3$age_group <- factor(df3$age_group, levels = c("Younger", 
                                                "Older"), 
                       ordered = FALSE) #set order to False.
#to check it's worked, run this command and see print out.
levels(df3$age_group)

## Preparing 'sample' variable 

#Recode sample strings for graphs below.
df3$sample <- as.character(df3$sample)
df3$sample[ df3$sample== 'post'] <- "Lockdown"
df3$sample[ df3$sample== 'pre'] <- "Pre-lockdown"

#set sample as a factor for graph below
df3$sample <- as.factor(df3$sample)
#to check that sample is a factor, run this command and see print out.
class(df3$sample)
#to check the ordering of levels for this factor, run this command.
levels(df3$sample)

#Make sure Pre-lockdown is first and Lockdown is second.
df3$sample <- factor(df3$sample, levels = c("Pre-lockdown", 
                                          "Lockdown"), 
                    ordered = FALSE) #set order to False.
#if we want to check order of levels:
levels(df3$sample)

## Preparing 'physical_social_environment' variable 

#Recode numerical values for physical_social_environment to meaningful strings for graphs below.
df3$physical_social_environment <- as.character(df3$physical_social_environment) 
df3$physical_social_environment[ df3$physical_social_environment== "Alone"] <- "Alone" 
df3$physical_social_environment[ df3$physical_social_environment== "Around people but NOT interacting with them"] <- "Around people but NOT interacting" 
df3$physical_social_environment[ df3$physical_social_environment== "Around people and interacting with them"] <- "Around people and interacting" 

#specify physical_social_environment as a factor for graph below.
df3$physical_social_environment <- as.factor(df3$physical_social_environment) 
#to check factor and factor levels
class(df3$physical_social_environment)
levels(df3$physical_social_environment)

#To change order of all three levels:
df3$physical_social_environment <- factor(df3$physical_social_environment, levels = c("Alone", 
                                          "Around people but NOT interacting",
                                          "Around people and interacting"), 
                    ordered = FALSE) #set to False.
#check levels and ordering.
levels(df3$physical_social_environment)

#for each participant, calculate number of observations per social environment option
# (alone, not interacting and interacting)
df4 <- df3 %>% group_by(participant_id) %>%
  summarise(alone_count = sum(physical_social_environment=='Alone'), 
            not_interacting_count = sum(physical_social_environment=='Around people but NOT interacting'), 
            interacting_count = sum(physical_social_environment=='Around people and interacting'))

#for each participant, calculate total number of observations
df5 <- data.frame(table(df3$participant_id))

# rename columns to be 'participant_id' and 'Total_number_of_rows'
names(df5)[names(df5)=="Var1"] <- "participant_id"
names(df5)[names(df5)=="Freq"] <- "Total_number_of_rows"

#merge dataframes with number of observations per social environment option and total n observations
df6 <- merge(df4, df5, by="participant_id")

# calculate percentages per participant for each social environment option
df6 <- transform(df6, Alone_percent = (alone_count / Total_number_of_rows)*100, 
                 not_interacting_percent = (not_interacting_count/Total_number_of_rows)*100,
                 interacting_percent = (interacting_count/Total_number_of_rows)*100)

# add df6 columns to df3 so that you can access Sample and Age group and percentages at the same time
df7 <-merge(df3,df6, by = "participant_id")


#prepare data for calculating group means for each percentage
df7 <- df7 %>% as_tibble()

#calculate group-mean for alone percent by Sample and age group
alone_means <- groupwiseMean(Alone_percent ~ sample + age_group,
                             data = df7,
                             conf = 0.95,
                             digits = 3)
#add column to indicate that these are percentages for 'Alone' for merging all means below
alone_means['social_env'] = 'Alone'

#calculate group-mean for not interacting percent by Sample and age group
not_interacting_means <- groupwiseMean(not_interacting_percent ~ sample + age_group,
                                       data = df7,
                                       conf = 0.95,
                                       digits = 3)

#add column to indicate that these are percentages for 'not interacting' for merging all means below
not_interacting_means['social_env'] = 'Not interacting'

#calculate group-mean for interacting percent by Sample and age group
interacting_means <- groupwiseMean(interacting_percent ~ sample + age_group,
                                   data = df7,
                                   conf = 0.95,
                                   digits = 3)

#add column to indicate that these are the percentages for 'interacting' for merging all means below
interacting_means['social_env'] = 'Interacting'

# combine all three group means
all_means <- rbind.data.frame(alone_means, not_interacting_means, interacting_means)

#specify social_env as a factor
all_means$social_env <- as.factor(all_means$social_env) 
#to check factor and factor levels
class(all_means$social_env)
levels(all_means$social_env)
#To change order of all three levels:
all_means$social_env <- factor(all_means$social_env, levels = c("Alone", 
                                                      "Not interacting",
                                                      "Interacting"), 
                          ordered = FALSE) #set to False.
#check levels and ordering.
levels(all_means$social_env)

#set x-axis labels for graph
social_env.bar.labs <- c("Alone", "Not \n interacting", "Interacting")

#create bar chart using all_means dataframe (bar colours = sample, facet wrap = age group)
#It is very important to specify the fill parameter in the main aes() and not in the geom_barplot.
alone_plot <- ggplot(data = all_means, aes(x = social_env, y = Mean, fill = sample)) + facet_wrap(~age_group)+
  theme_bw()+geom_bar(stat = "identity", position = "dodge", width = 0.5, color = 'black', size =0.5)+
  labs(y = "% of responses", x = "Social environment")+
  ylim(0, 100)+
  theme(legend.title = element_text(color = "black", size = 8, face = "bold" ),
        legend.text = element_text(color = "black", size = 8, face = "bold"),
        strip.text.x = element_text(size = 8, color = "black", face = "bold"),
        axis.text.x = element_text(size = 8, color = "black", face = "bold"),
        axis.title = element_text(size = 8,  color = "black", face = "bold"),
        axis.text.y = element_text(size = 8,  color = "black", face = "bold"),
        axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 40, r = 0, b = 0, l = 0)))

#add error bars and extra formatting
alone_plotE <- alone_plot + 
  geom_errorbar(data=all_means, 
                mapping=aes(x=social_env, ymin=Trad.upper, ymax=Trad.lower), 
                position=position_dodge(0.5), width=0.25,size = 0.4,  color="black")+ 
  scale_fill_manual("sample", values = c("Pre-lockdown" = "#FFFFFF", "Lockdown" = "#808080"))+
  theme(legend.title=element_blank())+
  scale_x_discrete(labels= social_env.bar.labs)+ theme(legend.position="top")+
  theme(legend.key.size = unit(0.5,"line"))+
  theme(axis.title.x=element_blank())+
theme(axis.ticks.length.x = unit(2, "pt")) +
  theme(axis.ticks.x = element_line(size = 2))

#view plot
alone_plotE

# put location pie chart and social env bar chart together using patchwork package
all_plots <- alone_plotE +  pie + plot_layout(widths = c(2,1))

#view all plots
all_plots

#save as tiff
ggsave(
  "figure_1_alone_location_horizontal_2_column.tiff",
  all_plots,limitsize = FALSE, units = 'cm',
  width = 17.8,
  height = 6,
  dpi = 1000
)

############################# ANOVA ############################################
## Compute two-way ANOVA to compare 'alone' percent between samples and age groups
#see this tutorial: http://www.sthda.com/english/wiki/two-way-anova-test-in-r

# create dataframe with participant_id, alone percent, Sample (pre vs during) and age group (young vs old)
df8 <- df7 %>% group_by(participant_id) %>%
  summarise(alone_percent = mean(Alone_percent), 
            sample = first(sample), 
            age_group = first(age_group))

# set contrasts to contr.sum
options(contrasts = c("contr.sum","contr.poly"))
options("contrasts") 

# set up model
alone_anova <- aov(alone_percent ~ sample * age_group, data = df8)
# run anova using type 3 sum of squares
model <- Anova(alone_anova, type = "III")

# use effect size package to get partial eta squared for each predictor
eta_squared_model <- eta_squared(model, partial = TRUE)

#save results to txt file
fp <- "alone_percent_anova_results.txt"

cat("Did the 'alone' percentage significantly differ between samples and age groups? (2-way ANOVA)\n\n", file = fp, append = TRUE)
capture.output(all_means, file = fp, append = TRUE)
cat("\n\n\n", file = fp, append = TRUE)
capture.output(df8, file = fp, append = TRUE)
cat("\n\n\n", file = fp, append = TRUE)
capture.output(alone_anova, file = fp, append = TRUE)
cat("\n\n\n", file = fp, append = TRUE)
capture.output(model, file = fp, append = TRUE)
cat("\n\n\n", file = fp, append = TRUE)
capture.output(eta_squared_model, file = fp, append = TRUE)
cat("\n\n\n", file = fp, append = TRUE)
