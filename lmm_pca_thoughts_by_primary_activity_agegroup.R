"This script runs Linear Mixed Models with primary_activity and age group as predictors
(and their interaction).
Reads in experience-sampling data (see Mendeley Data, doi: 10.17632/xv6dv2drm8.1)
Sets up fixed factors: primary_activity (5 levels), Age-group (2 levels)
Sets up random factors: Participant id and day_number
Uses loop to run all 5 models and create bar charts.
Saves all results to results folder"

######################### load libraries #######################################

library(plyr) # data manipulation
library(tidyverse) # data manipulation
library(lme4) # for linear mixed models
library(lmerTest) # for p-values for lme4
library(emmeans) # predicted means and post hoc comparisons
library(data.table) # data manipulation
library(ggthemes) # formatting figures
library(patchwork) # putting together figures
library(sjPlot)# for supp. tables

########################### Read in data #######################################

# set current working directory of experience-sampling questionnaire (ESQ) data.
# (see Mendeley Data, doi: 10.17632/xv6dv2drm8.1 for a copy of data)
setwd("")

# read in csv file with ESQ data
dataframe <- read.csv("pnas_lockdown_data_mendeley.csv", 
                      na.strings=c(""," ","NA", "nan"))


# Remove rows with missing values for variables of interest.
data <- dataframe %>% drop_na(fac1_both_samples, fac2_both_samples,
                              fac3_both_samples, fac4_both_samples,
                              fac5_both_samples,
                              age_group, primary_activity)

# remove row with 'OTHER' for primary_activity
# Leaves 1777 rows; 81 participants for analysis
df1 <- data[data$primary_activity != "OTHER", ]

###################### Fixed Factors ###########################################
## age_group

# Re-code age_group strings for graphs below
df1$age_group <- as.character(df1$age_group) # means that you can change the values
df1$age_group[ df1$age_group== 'young'] <- "Younger"
df1$age_group[ df1$age_group== 'old'] <- "Older"

# specify age_group as a factor for modeling below
df1$age_group <- as.factor(df1$age_group) 

# can change order of levels for plotting/modeling purposes
# age_group1 = first level in output
df1$age_group <- factor(df1$age_group, levels = c("Younger", 
                                                  "Older"), 
                        ordered = FALSE) #set order to False.

# to check it's worked, run this command and see print out.
levels(df1$age_group)

## primary_activity
# group 23 options into 5 categories for analysis
df1$primary_activity <- as.character(df1$primary_activity) # so can modify

df1$primary_activity[df1$primary_activity== "Working"] <- "Work" 

df1$primary_activity[df1$primary_activity == "Talking/conversation (in person)"] <- "Social" 
df1$primary_activity[df1$primary_activity == "Talking/conversation (virtually)"] <- "Social" 

df1$primary_activity[df1$primary_activity == "Social media"] <- "Media"
df1$primary_activity[df1$primary_activity == "Leisure: watching TV/film"] <- "Media"
df1$primary_activity[df1$primary_activity == "Leisure: listening to music"] <- "Media" 
df1$primary_activity[df1$primary_activity == "Leisure: listening to radio/podcast"] <- "Media"
df1$primary_activity[df1$primary_activity == "Reading/listening to/watching the news"  ] <- "Media"
df1$primary_activity[df1$primary_activity == "Leisure: reading for pleasure"] <- "Media"

df1$primary_activity[df1$primary_activity == "Leisure: other"  ] <- "Leisure"
df1$primary_activity[df1$primary_activity == "Leisure: arts and crafts"] <- "Leisure"
df1$primary_activity[df1$primary_activity == "Leisure: gardening"] <- "Leisure"
df1$primary_activity[df1$primary_activity == "Leisure: playing a game"] <- "Leisure"
df1$primary_activity[df1$primary_activity == "Exercising"] <- "Leisure"

df1$primary_activity[df1$primary_activity == "Caring for an adult(s)"] <- "Essential"
df1$primary_activity[df1$primary_activity == "Childcare" ] <- "Essential"
df1$primary_activity[df1$primary_activity == "Cooking" ] <- "Essential"
df1$primary_activity[df1$primary_activity == "Eating and/or drinking" ] <- "Essential"
df1$primary_activity[df1$primary_activity == "Getting ready for bed" ] <- "Essential"
df1$primary_activity[df1$primary_activity == "Gettting ready for the day"  ] <- "Essential"
df1$primary_activity[df1$primary_activity == "Household chores"  ] <- "Essential"
df1$primary_activity[df1$primary_activity == "Shopping"  ] <- "Essential"
df1$primary_activity[df1$primary_activity == "Sleeping"  ] <- "Essential"

#set primary_activity as factor for models
df1$primary_activity <- as.factor(df1$primary_activity) 
levels(df1$primary_activity) # to check levels

# Change order of all five levels:
# first level will be primary_activity1 and so on in output
# last level will be ommited from output
df1$primary_activity <- factor(df1$primary_activity, levels = 
                                 c("Work","Leisure", "Social",
                                   "Media","Essential"), 
                    ordered = FALSE) #set to False.
# to check levels and ordering.
levels(df1$primary_activity)

############################ Random factors#####################################

# specify participant_id as factor
df1$participant_id <- as.factor(df1$participant_id)

#specify day_number as factor
df1$day_number <- as.factor(df1$day_number)

# Order day_number
df1$day_number <- factor(df1$day_number,levels = c("1","2","3","4","5",
                                                   "6","7"), 
                         ordered = TRUE)
# check levels
levels(df1$day_number)

##################### Setting up for linear mixed models #######################

#At default, summary() from lmer will produce type 1 sums of squares.
#don't want this as the ordering of factors will change results in unbalanced dataset
#At default, the anova() from lmerTest will produce type 3 sums of squares. 
#At default, the contrasts are set to treatment for un-ordered factors.
#At default, the contrasts are set to poly for ordered factors.
#However, type 3 tests will normally only be sensible when using contrasts that,
#for different terms, are Anova orthogonal in the row-basis of the model, 
#such as those produced by contr.sum, contr.poly, or contr.helmert
#but not by the default contr.treatment
#therefore, need to change default contrasts to contr.sum and contr.poly
#contr.sum means the coefficients for each categorical are constrained to add up to 0

options(contrasts = c("contr.sum","contr.poly"))
# check it has worked
options("contrasts") 

# Globally set lmertest.limit to 1777 to calculate df for emmeans plots
# also set to sattherthwaite
emm_options(lmerTest.limit = 1777, lmer.df = "satterthwaite")


############################ Set up for results ################################

#set current directory to results folder to write output to below
setwd("")

# set results directory variable for saving tables below
results_dir <- ""

#set file name for lmer text output
fp = "lmm_thoughts_by_primary_activity_agegroup.txt"

#################### primary_activity by age group counts ######################

#create table of primary_activity counts by age group and save to results folder
table1 <- with(df1, table(primary_activity, age_group))
write.table(table1, file = "primary_activity_by_agegroup_counts.csv",
            sep = ",", quote = FALSE, row.names = T)

############################ Models ##########################################
# set up list of dependent variables for model loop below
dv <- c("fac1_both_samples", "fac2_both_samples", "fac3_both_samples",
        "fac4_both_samples", "fac5_both_samples")

# run all 5 models using each DV from list using loop
for(i in 1:length(dv)){
  model <- paste("model",i, sep="") # create name of model (e.g. model1)
  summ <- paste("summary",i, sep = "") # create name of summary (e.g. summary1)
  an <- paste("anova",i, sep = "") # create name of anova (e.g anova1)
  emm <- paste("emmean",i, sep = "") # create name of emmean (e.g. emmean1)
  # set specs for saving predicted means
  emmean.specs <- c("primary_activity", "age_group")
  
  m <- lmer(as.formula(paste(dv[i],"~ primary_activity * age_group +
                             (1|participant_id/day_number)")), data=df1) #run model
  s <- summary(m) # create summary
  a <- anova(m) # create anova
  e <- emmeans(m, specs = emmean.specs, type = "response") # create emmeans
  
  assign(model,m) # assign model to model name
  assign(summ,s) # assign summary to summary name
  assign(an, a) # assign anova to anova name
  assign(emm, e) # assign emmean to emmean name
  
  #save output to txt file
  capture.output(s,file = fp, append = TRUE)
  cat("\n\n\n", file = fp, append = TRUE)
  capture.output(a,file = fp, append = TRUE)
  cat("\n\n\n", file = fp, append = TRUE)
  capture.output(e,file = fp, append = TRUE)
  cat("\n\n\n", file = fp, append = TRUE)
} 

# combine all model summaries into one table for supplementary
myfile2 <- file.path(results_dir,"primary_activity_agegroup_summary_tables")
tab_model(model1,model2,model3,model4,model5, file = myfile2,
          show.r2 = FALSE,
          show.stat = TRUE, show.icc = FALSE,
          show.re.var = TRUE)

# combine all anova summaries in one document for supplementary 
anova_list <- list(anova1, anova2, anova3, anova4, anova5)
myfile3 <- file.path(results_dir,"primary_activity_agegroup_anova_tables")
tab_dfs(anova_list, file = myfile3, digits = 3, show.rownames = TRUE)

############################# BAR CHARTS #######################################
# set up list with names of emmeans for loop below
list <- c("emmean1", "emmean2","emmean3", "emmean4", "emmean5")

# set up list with plot titles for loop below
titles <- c("Future-directed problem-solving","Pleasant engagement",
            "Episodic social", "Imagery", "Detailed task focus")

#create function for making plots
myplot <- function(data, title){
  ggplot(summary(data), aes(x = primary_activity, y = emmean, fill = age_group)) +
    theme_bw() +
    geom_bar(stat="identity",width = 0.6, position="dodge",color = "black" ,size = 0.5) +
    ylim(-1.5, 1.5)+
    labs(title = title)+
    theme(legend.text = element_text(color = "black", size = 8,face = "bold"),
          axis.title = element_text(size = 8,color = "black",face = "bold"),
          axis.text.y = element_text(size = 8,color = "black",face = "bold"),
          legend.title=element_blank(),
          legend.key.size = unit(2,"line"),
          legend.position="top",
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 8, face = "bold", color = "black"))+
    # add error bars
    geom_errorbar(position=position_dodge(.6),width=0.25,size = 0.5, 
                  aes(ymax=upper.CL, ymin=lower.CL),alpha=1)+
    scale_fill_manual("age_group", values = c("Younger" = "#FFFFFF", "Older" = "#808080"))
}

# call function for list of emmeans and store each bar (bar1, bar2 etc)
for(i in seq_along(list)){
  bar <- paste("bar",i, sep="")
  b <- myplot(get(list[i]), titles[i])
  assign(bar, b)
}

# add x-axis to bar5
bar5 <- bar5 + theme(axis.text.x=element_text(size = 8, color = "black", face = "bold"))+
  theme(axis.ticks.length.x = unit(3, "pt"),axis.ticks.x = element_line(size = 1.5 ))

# add all 5 plots together using patchwork package
all_plots <- (bar1/bar2/bar3/bar4/bar5) + plot_layout(guides = "collect")&
  geom_hline(yintercept = 0, size = 0.2)  & theme(legend.margin=margin(0,0,0,0),
                                                  legend.position = 'bottom', 
                                                  legend.key.size = unit(1,"line"), 
                                                  axis.title.x = element_blank(),
                                                  plot.title = element_text(margin = margin(t =1, b = 1)))

# save all plots as tiff
ggsave(
  "primary_activity_agegroup.tiff",
  all_plots, units = "cm",
  width = 8.7,
  height = 15,
  dpi = 1000, 
)

# OR save all plots as pdf
ggsave(
  "primary_activity_agegroup.pdf",
  all_plots, units = "cm",
  width = 8.7,
  height = 15,
  dpi = 1000, 
)

######################### Post hoc comparisons #################################
# set name of new txt file for post hoc comparisons
fp2 <- "lmm_thoughts_by_primary_activity_agegroup_posthoc.txt"

# Model 2
# 2-way interaction between primary_activity and age group
# Are primary_activity estimates significantly different between age groups?
emm2.age.primary_activity <- emmeans(model2, specs = pairwise ~ age_group|primary_activity, type = "response")
emm2.age.primary_activity.contrasts <- emm2.age.primary_activity$contrasts %>%
  rbind()%>% summary(infer = TRUE)

cat("Probing 2-way interaction between agegroup and primary_activity for model 2:\n", file = fp2, append = TRUE)
capture.output(emm2.age.primary_activity.contrasts,file = fp2, append = TRUE)
cat("\n\n\n", file = fp2, append = TRUE)

#Model 5
# 2-way interaction between primary_activity and age group
# Are primary_activity estimates significantly different between age groups?
emm5.age.primary_activity <- emmeans(model5, specs = pairwise ~ age_group|primary_activity, type = "response")
emm5.age.primary_activity.contrasts <- emm5.age.primary_activity$contrasts %>%
  rbind()%>% summary(infer = TRUE)

cat("Probing 2-way interaction between agegroup and primary_activity for model 5:\n", file = fp2, append = TRUE)
capture.output(emm5.age.primary_activity.contrasts,file = fp2, append = TRUE)
cat("\n\n\n", file = fp2, append = TRUE)

###################### Checking assumptions ####################################
# Save diagnostic plots to results folder
models <- c(model1, model2, model3, model4, model5)
#QQ plots
for (i in seq_along(models)) {
  jpeg(paste("qq_plot", i, ".png", sep = ""))
    qq <- qqnorm(resid(models[[i]]))
    dev.off()
  }

#histograms
for (i in seq_along(models)) {
  jpeg(paste("hist_plot", i, ".png", sep = ""))
  hist <- hist(resid(models[[i]]))
  dev.off()
}

#residual plots
for (i in seq_along(models)) {
  jpeg(paste("fitted_residual_plot", i, ".png", sep = ""))
  fitted.resid <- plot(fitted(models[[i]]),resid(models[[i]]),xlim=c(-1.5,1.5), ylim=c(-3,3))
  dev.off()
}