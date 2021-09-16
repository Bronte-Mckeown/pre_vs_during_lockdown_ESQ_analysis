"This script runs Linear Mixed Models to predict thought PCAs by 
Sample, Social environment & Age-Group (and their interactions).
Includes affect PCAs as covariates.
Reads in experience-sampling data (see Mendeley Data, doi: 10.17632/n3wz7y8mhs.1)
Sets up fixed factors: Sample (2 levels), Age-group (2 levels), Social Environment (3 levels)
Sets up random factors: Participant id and day_number
Uses loop to run all 5 models and create bar charts.
Saves all results to results folder"

####################### Load libraries #########################################

library(plyr) # for data manipulation
library(tidyverse) # for removing rows with missing values prior to analysis
library(lme4) # for running linear mixed models
library(lmerTest) # for returning p-values for linear mixed models
library(emmeans) # for saving predicted values for graphs and post-hoc contrasts
library(data.table) # for data manipulation
library(ggthemes) # for formatting graphs
library(patchwork) # for putting plots together in one figure
library(sjPlot) # for creating supplementary tables

########################## Read in ESQ data ####################################

# set current working directory of experience-sampling questionnaire (ESQ) data.
# (see Mendeley Data, doi: 10.17632/xv6dv2drm8.1 for a copy of data)
setwd("")

# read in csv file with ESQ data
dataframe <- read.csv("pnas_lockdown_data_mendeley.csv",
                      na.strings=c(" ","NA"))

# remove rows with missing values for variables of interest.
df1 <- dataframe %>% drop_na(fac1_both_samples, fac2_both_samples,
                             fac3_both_samples, fac4_both_samples,
                             fac5_both_samples,
                             fac1_affect_both_samples, fac2_affect_both_samples,
                             sample, age_group, physical_social_environment)


######################## Fixed Factors #########################################
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

#to check it's worked, run this command and see print out.
levels(df1$age_group)

## sample

# Re-code sample strings for graphs below
df1$sample <- as.character(df1$sample) # means that you can change the values
df1$sample[ df1$sample== 'post'] <- "Lockdown"
df1$sample[ df1$sample== 'pre'] <- "Pre-lockdown"

# set sample as a factor for modeling below.
df1$sample <- as.factor(df1$sample)

# can change order of levels for plotting/modeling purposes
# sample1 = first level in output
df1$sample <- factor(df1$sample, levels = c("Pre-lockdown", 
                                          "Lockdown"), 
                    ordered = FALSE) #set order to False.
#if we want to check order of levels:
levels(df1$sample)

## physical_social_environment

# Re-code values for physical_social_environment for graphs below
df1$physical_social_environment <- as.character(df1$physical_social_environment) 
df1$physical_social_environment[ df1$physical_social_environment== 'Alone'] <- "Alone" 
df1$physical_social_environment[ df1$physical_social_environment== 'Around people but NOT interacting with them'] <- "Not interacting" 
df1$physical_social_environment[ df1$physical_social_environment== 'Around people and interacting with them'] <- "Interacting" 

# specify physical_social_environment as a factor for modeling below
df1$physical_social_environment <- as.factor(df1$physical_social_environment) 

# To change order of all three levels:
# For plots, you want it logically ordered (alone -> interacting)
# For summary tables of model results, can move about to get 3rd level estimate
df1$physical_social_environment <- factor(df1$physical_social_environment, levels = c("Alone", #physical_social_environment1 in output
                                          "Not interacting", #physical_social_environment2 in output
                                          "Interacting" #doesn't show in output
),ordered = FALSE) #set to False.

# check levels and ordering
levels(df1$physical_social_environment)

#############################  Random factors ##################################

# specify participant_id as factor
df1$participant_id <- as.factor(df1$participant_id)

#specify day_number as factor
df1$day_number <- as.factor(df1$day_number)

# Order day_number
df1$day_number <- factor(df1$day_number,levels = c("1","2","3","4","5",
                                                   "6","7","8","9","10"), 
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

# Globally set lmertest.limit to 4850 to calculate df for emmeans plots
# also set to sattherthwaite
emm_options(lmerTest.limit = 4850, lmer.df = "satterthwaite")

########################## Saving results ######################################
#set file name for lmer text output
fp = "lmm_thoughts_sample_social_agegroup_centered_age_affect_covariates.txt"

#set current directory to results folder to write output to results/R text file below
setwd("")

# set results directory as variable for saving tables below
results_dir <- ""

############################# Models ###########################################
# set up list of dependent variables for model loop below
dv <- c("fac1_both_samples", "fac2_both_samples", "fac3_both_samples",
        "fac4_both_samples", "fac5_both_samples")

# run all 5 models using each DV from list using loop
for(i in 1:length(dv)){
  model <- paste("model",i, sep="") # create name of model (e.g. model1)
  summ <- paste("summary",i, sep = "") # create name of summary (e.g. summary1)
  an <- paste("anova",i, sep = "") # create name of anova (e.g anova1)
  emmean <- paste("emmean",i, sep = "") # create name of emmean (e.g. emmean1)
  # set specs for saving predicted means
  emmean.specs <- c("sample", "physical_social_environment", "age_group")
  
  # run model
  m <- lmer(as.formula(paste(dv[i],"~ sample * age_group * physical_social_environment +
                             agegroup_mean_centred_age + fac1_affect_both_samples +
                             fac2_affect_both_samples + (1|participant_id/day_number)")), data=df1) 
  s <- summary(m) # save summary
  a <- anova(m) # save anova
  e <- emmeans(m, specs = emmean.specs, type = "response") # save emmean
  
  assign(model,m) # assign model to model name
  assign(summ,s) # assign summary to summary name
  assign(an, a) # assign anova to anova name
  assign(emmean, e) # assign emmean to emmean name
  
  #save outputs to txt file (file name set above as fp)
  capture.output(s,file = fp, append = TRUE)
  cat("\n\n\n", file = fp, append = TRUE)
  capture.output(a,file = fp, append = TRUE)
  cat("\n\n\n", file = fp, append = TRUE)
  capture.output(e,file = fp, append = TRUE)
  cat("\n\n\n", file = fp, append = TRUE)
} 


# combine all model summaries into one table for supplementary materials
myfile2 <- file.path(results_dir,"affect_covariates_summary_tables")
tab_model(model1,model2,model3,model4,model5, file = myfile2,
          show.r2 = FALSE,
          show.stat = TRUE, show.icc = FALSE,
          show.re.var = TRUE)

# combine all anova summaries in one document for supplementary materials
anova_list <- list(anova1, anova2, anova3, anova4, anova5)
myfile3 <- file.path(results_dir,"affect_covariates_anova_tables")
tab_dfs(anova_list, file = myfile3, digits = 3, show.rownames = TRUE)

########################## Bar charts ##########################################
# set up list with names of emmeans (predicted means) (sig. models only)
list <- c("emmean1", "emmean2","emmean3", "emmean4")

# set up list with plot titles (sig. models only)
titles <- c("Future-directed problem-solving","Pleasant engagement",
            "Episodic social", "Imagery")

# function for making plots
myplot <- function(data, title){
  # x axis = social environment, y axis = emmean, bars = sample, wrap = age_group
  ggplot(summary(data), aes(x = physical_social_environment, y = emmean, fill = sample)) +
    facet_wrap(~ age_group)+ theme_light() +
    geom_bar(stat="identity",width = 0.6, position="dodge",color = "black" ,size = 0.5) +
    ylim(-.8, .8)+
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
          strip.background = element_blank(),
          strip.text.x = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 8, face = "bold", color = "black"))+
    # add error bars
    geom_errorbar(position=position_dodge(.6),width=0.25,size = 0.5, 
                  aes(ymax=upper.CL, ymin=lower.CL),alpha=1)+
    scale_fill_manual("sample", values = c("Pre-lockdown" = "#FFFFFF", "Lockdown" = "#808080"))
}

# call function for list of emmeans set above and store each one (bar1, bar2 etc)
for(i in seq_along(list)){
  bar <- paste("bar",i, sep="")
  b <- myplot(get(list[i]), titles[i])
  assign(bar, b)
}

# add facet wrap text to bar1
bar1 <- bar1 + theme(strip.text.x = element_text(size = 8, color = "black", face = "bold"),
                     strip.background = element_rect(fill = "gray89"))

# add x-axis labels to bar4
physical_social_environment.bar.labs <- c("Alone\n", "Not\nInteracting", "Interacting\n")
bar4 <- bar4 + theme(axis.text.x=element_text(size = 8, color = "black", face = "bold"))+
  scale_x_discrete(labels= physical_social_environment.bar.labs)+
  theme(axis.ticks.length.x = unit(3, "pt"),axis.ticks.x = element_line(size = 1.5 ))

# put together all plots using patchwork package
# collect legend and put at bottom of figure
# add horizontal line at y axis = 0
all_plots <- bar1/bar2/bar3/bar4 + plot_layout(guides = "collect")&
  geom_hline(yintercept = 0, size = 0.2)  & theme(legend.margin=margin(0,10,10,10),
                                                  legend.position = 'bottom', 
                                                  legend.key.size = unit(1,"line"))

# save all plots as tiff
ggsave(
  "thoughts_sample_social_agegroup_affect_covariates.tiff",
  all_plots, units = "cm",
  width = 14,
  height = 15,
  dpi = 1000, 
)

# OR save all plots as PDF
ggsave(
  "thoughts_sample_social_agegroup_affect_covariates.pdf",
  all_plots, units = "cm",
  width = 11.4,
  height = 15,
  dpi = 1000, 
)

########################## Post hoc comparisons ################################
# create new txt file for post hoc comparisons
fp2 <- "lmm_thoughts_sample_social_agegroup_centered_age_affect_covariates_posthoc.txt"

## Model 2

# two-way interaction between age group and social environment
# but no longer passess Bonferroni cut-off
# Does age group moderate the social environment effect?
# Need to obtain simple effects (differences of predicted values)
# Then compare the simple effects of social environment between samples
emm2.age.physical_social_environment = emmeans(model2, specs = pairwise ~ physical_social_environment|age_group, type = "response")
emm2.age.physical_social_environment.contrasts <- emm2.age.physical_social_environment$contrasts %>%
  rbind()%>% summary(infer = TRUE)

# save to txt file
cat("Probing 2-way interaction between age_group and social envir. for model 2:\n", file = fp2, append = TRUE)
capture.output(emm2.age.physical_social_environment.contrasts,file = fp2, append = TRUE)
cat("\n\n\n", file = fp2, append = TRUE)

## Model 3

# two-way interaction between sample and social environment
# Does sample moderate the social environment effect?
# First need to obtain simple effects (differences of predicted values)
# Then compare the simple effects of social environment between samples
emm3.physical_social_environment.by.sample = emmeans(model3, specs = pairwise ~ physical_social_environment|sample, type = "response")
emm3.physical_social_environment.by.sample.contrast <- emm3.physical_social_environment.by.sample$contrasts %>%
  rbind()%>% summary(infer = TRUE)

# shows that the effect of social environment is only present in lockdown sample
# use contrast of contrasts to test the difference of differences formally
# first save emmeans for sample and physical_social_environment (6 in total)
emmean3.sample.physical_social_environment.specs <- c("sample", "physical_social_environment")
emmean3.sample.physical_social_environment<- emmeans(model3, specs= emmean3.sample.physical_social_environment.specs, type = "response")

# building a custom contrast involves pulling out specific emmeans of interest 
# Pull out emmean by making a vector to represent 
# the specific emmean of interest. 
# assign a 1 to emmean interest and a 0 to the other emmeans
before.alone <- c(1,0, 0, 0, 0, 0)
during.alone <- c(0,1, 0, 0, 0, 0)

before.not.interacting<- c(0,0, 1, 0, 0, 0)
during.not.interacting<- c(0,0, 0, 1, 0, 0)

before.interacting <- c(0,0, 0, 0, 1, 0)
during.interacting <- c(0,0, 0, 0, 0, 1)

# alone - interacting contrast of contrasts

emm3.alone.interacting.contrast.of.contrasts <- 
  contrast(emmean3.sample.physical_social_environment, 
  method = list("(before.alone - before.interacting) - (during.alone - during.interacting) " 
  = (before.alone - before.interacting)-(during.alone - during.interacting) ), adjust ='bonferroni' )%>% summary(infer = TRUE)

# not interacting - interacting contrast of contrasts
emm3.not.interacting.interacting.contrast.of.contrasts <- 
  contrast(emmean3.sample.physical_social_environment, 
  method = list("(before.not.interacting - before.interacting) - (during.not.interacting - during.interacting) " 
  = (before.not.interacting - before.interacting)-(during.not.interacting - during.interacting) ), adjust ='bonferroni' )%>% summary(infer = TRUE)

# save output to txt file
cat("Probing 2-way interaction between Sample and social envir. for model 3:\n", file = fp2, append = TRUE)
capture.output(emm3.physical_social_environment.by.sample.contrast,file = fp2, append = TRUE)
cat("\n\n\n", file = fp2, append = TRUE)
capture.output(emm3.alone.interacting.contrast.of.contrasts,file = fp2, append = TRUE)
cat("\n\n\n", file = fp2, append = TRUE)
capture.output(emm3.not.interacting.interacting.contrast.of.contrasts,file = fp2, append = TRUE)
cat("\n\n\n", file = fp2, append = TRUE)

# Model 3
# 3-way interaction (doesn't pass Bonferroni cut-off)
# 12 pairwise comparisons
emm3.age.physical_social_environment.sample = emmeans(model3, specs = pairwise ~physical_social_environment|sample|age_group, type = "response")
emm3.age.physical_social_environment.sample.contrasts <-  emm3.age.physical_social_environment.sample$contrasts %>%
  rbind()%>% summary(infer = TRUE)

#use emmean3 to run contrast of contrasts (already set in model loop)
#contrast of contrasts to test difference of social environment effect between samples for younger PS
before.alone.young <- c(1,0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0 )
during.alone.young <- c(0,1, 0, 0, 0, 0, 0, 0, 0, 0, 0,0 )

before.interacting.young <- c(0,0, 0, 0, 1, 0, 0, 0, 0, 0, 0,0 )
during.interacting.young <- c(0,0, 0, 0, 0, 1, 0, 0, 0, 0, 0,0 )

fac3.contrast.of.contrasts <- contrast(emmean3, method = list("(before.alone.young-before.interacting.young) - (during.alone.young - during.interacting.young) " 
                                                              = (before.alone.young-before.interacting.young) - (during.alone.young - during.interacting.young) ))%>% summary(infer = TRUE)

cat("Probing 3-way interaction for model 3:\n", file = fp2, append = TRUE)
capture.output(emm3.age.physical_social_environment.sample.contrasts,file = fp2, append = TRUE)
cat("\n\n\n", file = fp2, append = TRUE)
capture.output(fac3.contrast.of.contrasts,file = fp2, append = TRUE)
cat("\n\n\n", file = fp2, append = TRUE)


## Model 4
# three-way interaction betwen sample, social environment and age group
# Run 12 pairwise comparisons
emm4.age.physical_social_environment.sample = emmeans(model4, specs = pairwise ~physical_social_environment|sample|age_group, type = "response")
emm4.age.physical_social_environment.sample.contrasts <-  emm4.age.physical_social_environment.sample$contrasts %>%
  rbind()%>% summary(infer = TRUE)

# save output to txt file
capture.output(emm4.age.physical_social_environment.sample.contrasts,file = fp2, append = TRUE)
cat("\n\n\n", file = fp2, append = TRUE)


########################### Check Assumptions ##################################
# Save diagnostic plots to results folder
models <- c(model1, model2, model3, model4, model5)
# QQ plots
for (i in seq_along(models)) {
  jpeg(paste("qq_plot", i, ".png", sep = ""))
  qq <- qqnorm(resid(models[[i]]))
  dev.off()
}

# histograms
for (i in seq_along(models)) {
  jpeg(paste("hist_plot", i, ".png", sep = ""))
  hist <- hist(resid(models[[i]]))
  dev.off()
}

# residual plots
for (i in seq_along(models)) {
  jpeg(paste("fitted_residual_plot", i, ".png", sep = ""))
  fitted.resid <- plot(fitted(models[[i]]),resid(models[[i]]),xlim=c(-1.5,1.5), ylim=c(-3,3))
  dev.off()
}

