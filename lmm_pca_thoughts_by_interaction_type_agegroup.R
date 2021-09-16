"This script runs Linear Mixed Models with Interaction type and age-group
as predictors in the lockdown sample. 

Reads in experience-sampling data (see Mendeley Data, doi: 10.17632/xv6dv2drm8.1)
Selects lockdown sample.
Sets up fixed factors: Interaction type (4 levels), Age-group (2 levels)
Sets up random factors: Participant id and day_number
Uses loop to run all 5 models and create bar charts.
Saves all results to results folder"

######################### load libraries #######################################

library(plyr) # data manipulation
library(tidyverse) # data manipulation
library(lme4) # for linear mixed models
library(lmerTest) # for p-values for lme4
library(emmeans) # predicted means and post-hoc comparisons
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
                      na.strings=c(" ","", "NA", "nan"))

# remove rows with missing values for variables of interest.
# this also removes all pre-lockdown data
df1 <- dataframe %>% drop_na(fac1_both_samples, fac2_both_samples,
                             fac3_both_samples, fac4_both_samples,
                             fac5_both_samples,
                             age_group, virtual_social_environment)



###################### Fixed Factors ###########################################
## Interaction type

# create new Interaction type variable with 4 levels using physical & virtual
# social environment responses
df1 <- mutate(df1,
                  interaction = case_when(
                    physical_social_environment == "Alone" & virtual_social_environment == "Alone" ~ "No interaction",
                    physical_social_environment == "Around people but NOT interacting with them" & virtual_social_environment == "Around people but NOT interacting with them" ~ "No interaction",
                    physical_social_environment == "Around people but NOT interacting with them" & virtual_social_environment == "Alone" ~ "No interaction",
                    physical_social_environment == "Alone" & virtual_social_environment == "Around people but NOT interacting with them" ~ "No interaction",
                   (physical_social_environment == "Alone"| physical_social_environment == "Around people but NOT interacting with them") & virtual_social_environment == "Around people and interacting with them" ~ "Virtual interaction",
                   (virtual_social_environment == "Alone"| virtual_social_environment == "Around people but NOT interacting with them") & physical_social_environment == "Around people and interacting with them" ~ "Physical interaction",
                   physical_social_environment == "Around people and interacting with them" & virtual_social_environment == "Around people and interacting with them" ~ "Interaction both",
                  ))    


# for checking it is re-coded as expected:
df2 <- select(df1, physical_social_environment, virtual_social_environment, interaction)


# specify interaction type as a factor for modeling below.
df1$interaction <- as.factor(df1$interaction) 
# Change order of levels
df1$interaction <- factor(df1$interaction, levels = c("Physical interaction", 
                                                "Virtual interaction",
                                                "No interaction",
                                                "Interaction both" 
                                                ), 
                       ordered = FALSE) #set order to False.
# check levels
levels(df1$interaction)

## age-group
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

############################ Set up for results ################################

#set file name for lmer text output
fp = "lmm_pca_thoughts_by_interaction_type_agegroup.txt"

#set current directory to results folder to write output to results/R text file below
setwd("")

results_dir <- ""

################## Interaction type by age group counts ########################

#create table of interaction counts by age group and save to results folder
table <- with(df1, table(interaction, age_group))
write.table(table, file = "interaction_type_by_agegroup_counts.csv", sep = ",", quote = FALSE, row.names = T)

####################### Setting up for LMMS ####################################

#set contrasts, lmer test limit and sattherthwaite approximation
options(contrasts = c("contr.sum","contr.poly")) # IMPORTANT
options("contrasts") 
emm_options(lmerTest.limit = 1865, lmer.df = "satterthwaite")

############################## Models ##########################################
# set up list of dependent variables for model loop below
dv <- c("fac1_both_samples", "fac2_both_samples", "fac3_both_samples",
        "fac4_both_samples", "fac5_both_samples")

# run all 5 models using each DV from list using loop
for(i in 1:length(dv)){
  model <- paste("model",i, sep="") # create name of model (e.g. model1)
  summ <- paste("summary",i, sep = "") # create name of summary (e.g. summary1)
  an <- paste("anova",i, sep = "") # create name of anova (e.g anova1)
  emmean <- paste("emmean",i, sep = "") # create name of emmean (e.g. emmean1)
  emmean.specs <- c("interaction", "age_group") # set specs for emmeans
  
  #run model
  m <- lmer(as.formula(paste(dv[i],"~ interaction * age_group +(1|participant_id/day_number)")), data=df1) 
  s <- summary(m) # create summary
  a <- anova(m) # create anova
  e <- emmeans(m, specs = emmean.specs, type = "response") # create emmeans
  
  assign(model,m) # assign model to model name
  assign(summ,s) # assign summary to summary name
  assign(an, a) # assign anova to anova name
  assign(emmean, e) # assign emmeans to emmean name
  
  #save output to txt file
  capture.output(s,file = fp, append = TRUE)
  cat("\n\n\n", file = fp, append = TRUE)
  capture.output(a,file = fp, append = TRUE)
  cat("\n\n\n", file = fp, append = TRUE)
  capture.output(e,file = fp, append = TRUE)
  cat("\n\n\n", file = fp, append = TRUE)
} 

#combine all model summaries into one table for supplementary
myfile2 <- file.path(results_dir,"interaction_type_agegroup_summary_tables")
tab_model(model1,model2,model3,model4,model5, file = myfile2,
          show.r2 = FALSE,
          show.stat = TRUE, show.icc = FALSE,
          show.re.var = TRUE)

#combine all anova summaries in one document for supplementary 
anova_list <- list(anova1, anova2, anova3, anova4, anova5)
myfile3 <- file.path(results_dir,"interaction_type_agegroup_anova_tables")
tab_dfs(anova_list, file = myfile3, digits = 3, show.rownames = TRUE)

############################# BAR CHARTS #######################################
# set up list with names of emmeans for bar graphs
list <- c("emmean1", "emmean2","emmean3", "emmean4", "emmean5")

#set up list with plot titles
titles <- c("Future-directed problem-solving",
            "Pleasant engagement",
            "Episodic social",
            "Imagery",
            "Detailed task focus")

# function for making plots
myplot <- function(data, title){
  ggplot(summary(data), aes(x = interaction, y = emmean, fill = age_group)) +
  theme_bw() +
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
    scale_fill_manual("age_group", values = c("Younger" = "#FFFFFF",
                                              "Older" = "#808080"))+ 
    scale_y_continuous(
      breaks = c(-1, 0, 1),
      label = c("-1", "0", "1"),
      limits = c(-1,1)
    )
}

#call function for list of emmeans and store each bar chart (bar1, bar2, etc)
for(i in seq_along(list)){
  bar <- paste("bar",i, sep="")
  b <- myplot(get(list[i]), titles[i])
  assign(bar, b)
}


# add x-axis labels to plot 5
interaction.bar.labs <- c("Physical\ninteraction", "Virtual\ninteraction",
                          "No\ninteraction","Interaction\nboth")

bar5 <- bar5 + theme(axis.text.x=element_text(size = 8, color = "black", face = "bold"),
                     axis.title.x = element_text(size = 8, color = "black", face = "bold"),
                     axis.ticks.length.x = unit(3, "pt"),
                     axis.ticks.x = element_line(size = 1.5 ))+
  scale_x_discrete(labels= interaction.bar.labs)

# add all 5 plots together using patchwork package
all_plots <- (bar1/bar2/bar3/bar4/bar5) + plot_layout(guides = "collect")&
  geom_hline(yintercept = 0, size = 0.2)  & theme(legend.margin=margin(0,0,0,0),
                                                  legend.position = 'bottom', 
                                                  legend.key.size = unit(1,"line"), 
                                                  axis.title.x = element_blank(),
                                                  plot.title = element_text(margin = margin(t =1, b = 1)))

# save all_plots to results folder in tiff or pdf format
ggsave(
  "interaction_type_agegroup_bar.tiff",
  all_plots, units = "cm",
  width = 8.7,
  height = 15,
  dpi = 1000, 
)

ggsave(
  "interaction_type_agegroup_bar.pdf",
  all_plots, units = "cm",
  width = 8.7,
  height = 15,
  dpi = 1000, 
)


######################### Post hoc comparisons #################################
# create new txt file for post hoc comparisons
fp2 <- "lmm_pca_thoughts_by_interaction_type_agegroup_posthoc.txt"

# Model 1
# sig. effect of interaction type
emm1.interaction <- emmeans(model1, specs = pairwise ~ interaction, type = "response")
emm1.interaction.contrasts <- emm1.interaction$contrasts %>%
  rbind()%>% summary(infer = TRUE)

# save to output
cat("Probing main effect of interaction for model 1:\n", file = fp2, append = TRUE)
capture.output(emm1.interaction.contrasts,file = fp2, append = TRUE)
cat("\n\n\n", file = fp2, append = TRUE)

# Model 3
# sig. effect of interaction type
emm3.interaction <- emmeans(model3, specs = pairwise ~ interaction, type = "response")
emm3.interaction.contrasts <- emm3.interaction$contrasts %>%
  rbind()%>% summary(infer = TRUE)

# save to output
cat("Probing main effect of interaction for model 3:\n", file = fp2, append = TRUE)
capture.output(emm3.interaction.contrasts,file = fp2, append = TRUE)
cat("\n\n\n", file = fp2, append = TRUE)

# Model 4
# sig. effect of interaction type (doesn't pass bonferroni cut-off)
emm4.interaction <- emmeans(model4, specs = pairwise ~ interaction, type = "response")
emm4.interaction.contrasts <- emm4.interaction$contrasts %>%
  rbind()%>% summary(infer = TRUE)

# save to output
cat("Probing main effect of interaction for model 4:\n", file = fp2, append = TRUE)
capture.output(emm4.interaction.contrasts,file = fp2, append = TRUE)
cat("\n\n\n", file = fp2, append = TRUE)

# sig. 2-way interaction between interaction and age group
# (doesn't pass bonferroni cut-off)
# Are age group estimates significantly different between interactions?
emm4.interaction.age <- emmeans(model4, specs = pairwise ~ interaction|age_group, type = "response")
emm4.interaction.age.contrasts <- emm4.interaction.age$contrasts %>%
  rbind()%>% summary(infer = TRUE)

# save to output
cat("Probing 2-way interaction between agegroup and interaction type for model 4:\n", file = fp2, append = TRUE)
capture.output(emm4.interaction.age.contrasts,file = fp2, append = TRUE)
cat("\n\n\n", file = fp2, append = TRUE)

# Model 5
# sig. effect of interaction type
emm5.interaction <- emmeans(model5, specs = pairwise ~ interaction, type = "response")
emm5.interaction.contrasts <- emm5.interaction$contrasts %>%
  rbind()%>% summary(infer = TRUE)

# save to output
cat("Probing main effect of interaction for model 5:\n", file = fp2, append = TRUE)
capture.output(emm5.interaction.contrasts,file = fp2, append = TRUE)
cat("\n\n\n", file = fp2, append = TRUE)

# 2-way interaction between interaction and age group
# Are age group estimates significantly different between interactions?
emm5.interaction.age <- emmeans(model5, specs = pairwise ~ interaction|age_group, type = "response")
emm5.interaction.age.contrasts <- emm5.interaction.age$contrasts %>%
  rbind()%>% summary(infer = TRUE)

# save to output
cat("Probing 2-way interaction between agegroup and interaction (not vs interacting) for model 5:\n", file = fp2, append = TRUE)
capture.output(emm5.interaction.age.contrasts,file = fp2, append = TRUE)
cat("\n\n\n", file = fp2, append = TRUE)


###################### ASSUMPTIONS #############################################
# run and save diagnostic plots
models = c(model1, model2, model3, model4, model5)
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