# pre vs during lockdown ESQ analysis
## Repository description
This repo contains all of the scripts used in the analysis reported in Mckeown et al. (2021): "The impact of social isolation and changes in work patterns on ongoing thought during the first COVID-19 lockdown in the United Kingdom."

For data used in these analyses, see: Mckeown et al (2021): "The impact of social isolation and changes in work patterns on ongoing thought during the first COVID-19 lockdown in the UK.”, Mendeley Data, http://dx.doi.org/10.17632/n3wz7y8mhs.1

There are 9 R scripts in total:
1. lmm_pca_thoughts_by_sample_social_agegroup_centered_age
- This script compares PCA thought components between 1) samples, 2) social environments and 3) age groups and includes age group mean-centered age as a nuissance covariate.
2. lmm_pca_thoughts_by_primary_activity_agegroup
- This script compares PCA thought components between 1) primary activities and 2) age groups in the lockdown sample.
3. lmm_pca_thoughts_by_interaction_type_agegroup
- This script compares PCA thought components between 1) interaction type (virtual vs physical) and 2) age groups in the lockdown sample. 
4. lmm_pca_thoughts_by_sample_social_agegroup_centered_age_with_affect_covariates
- This script compares PCA thought components between 1) samples, 2) social environments and 3) age groups and includes two affect components and age group mean-centered age as nuissance covariates.
5. lmm_pca_affect_by_sample_social_agegroup_centered_age
- This script compares PCA affect components between 1) samples, 2) social environments and 3) age groups and includes age group mean-centered age as a nuissance covariate.
6. lmm_pca_thoughts_by_sample_social_agegroup_centered_age_limit_age_range
- This script compares PCA thought components between 1) samples, 2) social environments and 3) age groups and includes age group mean-centered age as a nuissance covariate, while also limiting young age-group range to 18-27 in both samples. 
7. social_env_barchart_anovo_location_piechart
- This script creates location pie chart and social environment bar chart presented in figure 1 and calculates ANOVA comparing 'alone' percentage between 1) sample and 2) age groups.
8. correspondence_between_pca_thought_solutions_scatterplots
- This script creates scatterplots showing correspondence between PCA thought solutions applied to both samples together and separately. 
9. correspondence_between_pca_affect_solutions_scatterplots
- This script creates scatterplots showing correspondence between PCA affect solutions applied to both samples together and separately. 
