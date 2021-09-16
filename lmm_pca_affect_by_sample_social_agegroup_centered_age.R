"This script runs Linear Mixed Models to predict affect PCAs by 
Sample, Social environment & Age-Group (and their interactions).
Reads in experience-sampling data (see Mendeley Data, doi: 10.17632/n3wz7y8mhs.1)
Sets up fixed factors: Sample (2 levels), Age-group (2 levels), Social Environment (3 levels)
Sets up random factors: Participant id and day number
Uses loop to run all 5 models and create bar charts.
Saves all results to results folder"

####################### Load libraries #########################################

library(plyr) # for data manipulation
library(tidyverse) # for removing rows with missing values
library(lme4) # for LMMs
library(lmerTest) # for p-values for lme4
library(emmeans) # for saving predicted values for graphs and posthoc contrasts
library(data.table) # for data manipulation
library(ggthemes) # for formatting graphs
library(patchwork) # for putting plots together in one figure
library(sjPlot) # for creating supplementary tables
library(dplyr) # for data manipulation

########################## Read in ESQ data ####################################

# set current working directory of experience-sampling questionnaire (ESQ) data.
# (see Mendeley Data, doi: 10.17632/xv6dv2drm8.1 for a copy of data)
setwd("")

# read in csv file with ESQ data
dataframe <- read.csv("pnas_lockdown_data_mendeley.csv",
                      na.strings=c(" ","NA"))

# remove rows with missing values for variables of interest.
df1 <- dataframe %>% drop_na(fac1_affect_both_samples, fac2_affect_both_samples,
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

####################### Prepare Random factors##################################

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

# Globally set lmertest.limit to 4926 to calculate df for emmeans plots
# also set to sattherthwaite
emm_options(lmerTest.limit = 4926, lmer.df = "satterthwaite")

########################## Saving results ######################################
#set file name for lmer text output
fp = "lmm_pca_affect_by_sample_social_agegroup_centered_age.txt"

#set current directory to results folder to write output to text file below
setwd("")

# set results directory for saving tables below
results_dir <- ""

############################# Models ###########################################
# set up list of dependent variables for model loop below
dv <- c("fac1_affect_both_samples", "fac2_affect_both_samples")

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
                             agegroup_mean_centred_age +(1|participant_id/day_number)")), data=df1) 
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

#combine all model summaries into one table 
myfile2 <- file.path(results_dir,"affect_sample_social_agegroup_summary_tables")
tab_model(model1,model2, file = myfile2,
          show.r2 = FALSE,
          show.stat = TRUE, show.icc = FALSE,
          show.re.var = TRUE)

#combine all anova summaries in one document 
anova_list <- list(anova1, anova2)
myfile3 <- file.path(results_dir,"affect_sample_social_agegroup_anova_tables")
tab_dfs(anova_list, file = myfile3, digits = 3, show.rownames = TRUE)

########################## Bar charts ##########################################
#set up list with names of emmeans
list <- c("emmean1", "emmean2")

#set up list with plot titles
titles <- c("Negative Affect","Positive Affect")

#create function for making plots
myplot <- function(data, title){
  ggplot(summary(data), aes(x = physical_social_environment, y = emmean, fill = sample)) +
    facet_wrap(~ age_group)+ theme_light() +
    geom_bar(stat="identity",width = 0.6, position="dodge",color = "black" ,size = 0.5) +
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
    scale_fill_manual("sample", values = c("Pre-lockdown" = "#FFFFFF", "Lockdown" = "#808080"))+
    scale_y_continuous(
      breaks = c(-1, 0, 1),
      label = c("-1", "0", "1"),
      limits = c(-1,1))
}

#call function for list of emmeans and store each one
for(i in seq_along(list)){
  bar <- paste("bar",i, sep="")
  b <- myplot(get(list[i]), titles[i])
  assign(bar, b)
}

# add facet wrap text to bar1
bar1 <- bar1 + theme(strip.text.x = element_text(size = 8, color = "black", face = "bold"),
                     strip.background = element_rect(fill = "gray89"))

# add x-axis labels to bar2
physical_social_env.bar.labs <- c("Alone\n", "Not\nInteracting", "Interacting\n")

bar2 <- bar2 + theme(axis.text.x=element_text(size = 8, color = "black", face = "bold"),
                     axis.ticks.length.x = unit(3, "pt"),
                     axis.ticks.x = element_line(size = 1.5 ))+
  scale_x_discrete(labels= physical_social_env.bar.labs)

# put together all plots using patchwork
all_plots <- bar1/bar2 + plot_layout(guides = "collect")&
  geom_hline(yintercept = 0, size = 0.2)  & theme(legend.margin=margin(0,10,10,10),
                                                  legend.position = 'bottom', 
                                                  legend.key.size = unit(1,"line"))

# save all plots as tiff or PDF
ggsave(
  "affect_pre_post_plots.tiff",
  all_plots, units = "cm",
  width = 11.4,
  height = 10,
  dpi = 1000, 
)

ggsave(
  "affect_pre_post_plots.pdf",
  all_plots, units = "cm",
  width = 11.4,
  height = 10,
  dpi = 1000, 
)

########################## Post hoc comparisons ################################
# set name of new txt file for saving results of post hoc comparisons
fp2 <- "lmm_pca_affect_by_sample_social_agegroup_centered_age_posthoc.txt"

# Model 1
# two-way interaction between age group and social environment
# Does age group moderate the social environment effect?
# to understand interaction, obtain simple effects (differences of predicted values)
emm1.age.physical_social_environment = emmeans(model1, specs = pairwise ~ physical_social_environment|age_group, type = "response")
emm1.age.physical_social_environment.contrasts <- emm1.age.physical_social_environment$contrasts %>%
  rbind()%>% summary(infer = TRUE)

cat("Probing 2-way interaction between agegroup and social envir. for model 1:\n", file = fp2, append = TRUE)
capture.output(emm1.age.physical_social_environment.contrasts,file = fp2, append = TRUE)
cat("\n\n\n", file = fp2, append = TRUE)

# Model 2
# two-way interaction between sample and social environment
# Does sample moderate the social environment effect?
# to understand interaction, obtain simple effects (differences of predicted values)
# then compare the simple effects of social environment between samples
emm2.physical_social_environment.by.sample = emmeans(model2, specs = pairwise ~ physical_social_environment|sample, type = "response")
emm2.physical_social_environment.by.sample.contrast <- emm2.physical_social_environment.by.sample$contrasts %>%
  rbind()%>% summary(infer = TRUE)

# shows that effect of social environment is smaller during lockdown on positive affect
# use contrast of contrasts to test the difference of differences formally
# first save emmeans for sample and physical_social_environment (6 in total)
emmean2.sample.physical_social_environment.specs <- c("sample", "physical_social_environment")
emmean2.sample.physical_social_environment<- emmeans(model2, specs= emmean2.sample.physical_social_environment.specs, type = "response")

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

# alone- interacting contrast of contrasts

emm2.alone.interacting.contrast.of.contrasts <- contrast(emmean2.sample.physical_social_environment, 
                                                         method = list("(before.alone - before.interacting) - (during.alone - during.interacting) " 
                                                         = (before.alone - before.interacting)-(during.alone - during.interacting) ), adjust ='bonferroni' )%>% summary(infer = TRUE)

# not interacting - interacting contrast of contrasts
emm2.not.interacting.interacting.contrast.of.contrasts <- contrast(emmean2.sample.physical_social_environment,
                                                          method = list("(before.not.interacting - before.interacting) - (during.not.interacting - during.interacting) " 
                                                          = (before.not.interacting - before.interacting)-(during.not.interacting - during.interacting) ), adjust ='bonferroni' )%>% summary(infer = TRUE)

cat("Probing 2-way interaction between Sample and social envir. for model 2 (positive affect):\n", file = fp2, append = TRUE)
capture.output(emm2.physical_social_environment.by.sample.contrast,file = fp2, append = TRUE)
cat("\n\n\n", file = fp2, append = TRUE)
capture.output(emm2.alone.interacting.contrast.of.contrasts,file = fp2, append = TRUE)
cat("\n\n\n", file = fp2, append = TRUE)
capture.output(emm2.not.interacting.interacting.contrast.of.contrasts,file = fp2, append = TRUE)
cat("\n\n\n", file = fp2, append = TRUE)

# Model 2
# three-way interaction 
#12 pairwise comparisons
emm2.age.physical_social_environment.sample = emmeans(model2, specs = pairwise ~physical_social_environment|age_group|sample, type = "response")
emm2.age.physical_social_environment.sample.contrasts <-  emm2.age.physical_social_environment.sample$contrasts %>%
  rbind()%>% summary(infer = TRUE)

# use emmean2 to run contrast of contrasts (already set in model loop)
# assign a 1 to emmean interest and a 0 to the other emmeans
before.alone.young <- c(1,0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0 )
during.alone.young <- c(0,1, 0, 0, 0, 0, 0, 0, 0, 0, 0,0 )

before.not.interacting.young <- c(0,0, 1, 0, 0, 0, 0, 0, 0, 0, 0,0 )
during.not.interacting.young <- c(0,0, 0, 1, 0, 0, 0, 0, 0, 0, 0,0 )

before.interacting.young <- c(0,0, 0, 0, 1, 0, 0, 0, 0, 0, 0,0 )
during.interacting.young <- c(0,0, 0, 0, 0, 1, 0, 0, 0, 0, 0,0 )

before.alone.old <- c(0,0, 0, 0, 0, 0, 1, 0, 0, 0, 0,0 )
during.alone.old <- c(0,0, 0, 0, 0, 0, 0, 1, 0, 0, 0,0 )

before.not.interacting.old <- c(0,0, 0, 0, 0, 0, 0, 0, 1, 0, 0,0 )
during.not.interacting.old <- c(0,0, 0, 0, 0, 0, 0, 0, 0, 1, 0,0 )

before.interacting.old <- c(0,0, 0, 0, 0, 0, 0, 0, 0, 0, 1,0 )
during.interacting.old <- c(0,0, 0, 0, 0, 0, 0, 0, 0, 0, 0,1 )

# run contrast of contrasts for young and old
# lockdown changed effect of social environment on positive affect for young PS only:
fac2.contrast.of.contrasts.young.alone.interacting <- contrast(emmean2, method = list("(before.alone.young-before.interacting.young) - (during.alone.young - during.interacting.young) " 
                                                              = (before.alone.young-before.interacting.young) - (during.alone.young - during.interacting.young) ))%>% summary(infer = TRUE)


fac2.contrast.of.contrasts.young.interacting.not.interacting <- contrast(emmean2, method = list("(before.not.interacting.young - before.interacting.young) - (during.not.interacting.young - during.interacting.young) " 
                                                                                          = (before.not.interacting.young -before.interacting.young) - (during.not.interacting.young- during.interacting.young) ))%>% summary(infer = TRUE)


fac2.contrast.of.contrasts.old.alone.interacting <- contrast(emmean2, method = list("(before.alone.old-before.interacting.old) - (during.alone.old - during.interacting.old) " 
                                                                    = (before.alone.old-before.interacting.old) - (during.alone.old - during.interacting.old) ))%>% summary(infer = TRUE)

fac2.contrast.of.contrasts.old.interacting.not.interacting <- contrast(emmean2, method = list("(before.not.interacting.old - before.interacting.old) - (during.not.interacting.old - during.interacting.old) " 
                                                                                              = (before.not.interacting.old -before.interacting.old) - (during.not.interacting.old- during.interacting.old) ))%>% summary(infer = TRUE)


cat("Probing 3-way interaction for model 2 (positive affect):\n", file = fp2, append = TRUE)
capture.output(emm2.age.physical_social_environment.sample.contrasts,file = fp2, append = TRUE)
cat("\n\n\n", file = fp2, append = TRUE)
capture.output(fac2.contrast.of.contrasts.young.alone.interacting,file = fp2, append = TRUE)
cat("\n\n\n", file = fp2, append = TRUE)
capture.output(fac2.contrast.of.contrasts.young.interacting.not.interacting,file = fp2, append = TRUE)
cat("\n\n\n", file = fp2, append = TRUE)
capture.output(fac2.contrast.of.contrasts.old.alone.interacting,file = fp2, append = TRUE)
cat("\n\n\n", file = fp2, append = TRUE)
capture.output(fac2.contrast.of.contrasts.old.interacting.not.interacting,file = fp2, append = TRUE)
cat("\n\n\n", file = fp2, append = TRUE)

############################## Assumptions #####################################
models = c(model1, model2)
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

# for just saving out model 1's residual plot for supplementary materials
tiff("resid_affect_model_1.tiff", units="in", width=5, height=5, res=600)
plot(fitted(model1),resid(model1),xlim=c(-1.5,1.5), ylim=c(-3,3),
                     xlab="Fitted values", ylab="Residuals",main="Residual plot for model 1")
dev.off()
