# packages
library("dplyr")
library("ggplot2")
library("stringr")
library("betareg")
library("lmtest")
library("geosphere")
library("tidyr")
library("cocor")
library("olsrr")
# library(BayesFactor)
# library(pastecs)
# library(ggplot2)
# library("Hmisc")



# set up our directories
home_dir <- 'C:/Users/dakot/Dropbox/PC/Documents/Trees/Analysis'
setwd(home_dir)
data<-read.csv("City_Data_Diversity_Enviroment_Social.csv")
env_PCAscores<-read.csv("Environmental_PCA_Scores.csv")
park_vs_urban<-read.csv("Parks_vs_Urban_Areas_Biodiversity_Native.csv")

####################################################################
####################################################################
####################################################################
###
###  PCA of environmental data
###
####################################################################
####################################################################
####################################################################
## subset only the environmental columns from our data frame
env_data<-data[c(which(colnames(data)=="Precip_Coldest_Quarter"):which(colnames(data)=="Annual_Mean_Temp"))]

## label the rows by city
rownames(env_data)<-data$filename_city

## conduct PCA
pca<-princomp(env_data, cor = TRUE, scores = TRUE)

## check and save the scores
pca$scores
pca_scores=as.data.frame(pca$scores)
pca_scores$filename_city<-rownames(pca_scores)
write.csv(pca_scores,"Environmental_PCA_Scores.csv")

## check and save loadings for the first four
pca$loadings[,1:ncol(pca$loadings)]
write.csv(as.data.frame(pca$loadings[,1:ncol(pca$loadings)])%>%
            arrange(-abs(Comp.1)), "Environmental_PCA_Loadings.csv")

## check and save proportion of variance explained by first 4
proportions_variance<-(pca$sdev^2/sum(pca$sdev^2))
write.csv(proportions_variance,"Environmental_PCA_ProportionVariance.csv")

# #### code for later
# round(proportions_variance[[1]]*100,1)
# Variance_Explained_First_Four<-sum(proportions_variance[1:4])

## merge PCA with whole dataframe for modelling
all_data<-merge(pca_scores,data)

head(all_data)

####################################################################
####################################################################
####################################################################
###
###  Effective Species Count Across Cities
###
####################################################################
####################################################################
####################################################################

# let's find the lowest AIC based on all possible predictors

# linear model with all predictors
model1<-lm(effective_species~
             Comp.1*Comp.2+
             city_age_2021* tree_city_USA+
             # log transform number trees because it is right skewed
             log(number_trees,10),
           data=all_data)
summary(model1)


# identify outliers

ols_plot_cooksd_bar(model1)

# 33 is worst, also 22, 30, 33, 56
outlier_city_1<-
  all_data[33,"filename_city"]


# remove first outlier, Miami
model2<-lm(effective_species~
             Comp.1*Comp.2+
             city_age_2021* tree_city_USA+
             # log transform number trees because it is right skewed
             log(number_trees,10),
           data=all_data%>%
             filter(filename_city!=outlier_city_1))

# identify outliers
ols_plot_cooksd_bar(model2)


# only one major outlier, 22
outlier_city_2<-
  all_data[22,"filename_city"]

# remove second outlier, Honolulu
model3<-lm(effective_species~
             Comp.1*Comp.2+
             city_age_2021* tree_city_USA+
             # log transform number trees because it is right skewed
             log(number_trees,10),
           data=all_data%>%
             filter(filename_city!=outlier_city_1)%>%
             filter(filename_city!=outlier_city_2))

# check for any major outliers
ols_plot_cooksd_bar(model3)

# no major outliers; therefore, continue
k<-ols_step_all_possible(model3)
plot(k)

## 8, 29, 64, 100
k[8,]$aic
k[8,]$predictors
k[29,]$aic
k[29,]$predictors
k[64,]$aic
k[64,]$predictors
k[100,]$aic
k[100,]$predictors

## best model has COmp1*Comp2, and log number trees and tree-city_USA
model_op1<-lm(effective_species~
                 Comp.1:Comp.2+
                 Comp.1+
                 # log transform number trees because it is right skewed
                 log(number_trees,10),
               data=all_data)

ols_plot_cooksd_bar(model_op1)

# remove outliers and run best model
model_op1_no_outliers<-lm(effective_species~
                            Comp.1:Comp.2+
                            Comp.1+
                            # log transform number trees because it is right skewed
                            log(number_trees,10),
                          data=all_data%>%
                             filter(filename_city!="Miami")%>%
                            filter(filename_city!="Honolulu"))
ols_plot_cooksd_bar(model_op1_no_outliers)

####
model_op2<-lm(effective_species~
                             Comp.1:Comp.2+
                             # log transform number trees because it is right skewed
                             log(number_trees,10),
                           data=all_data)

####
model_op2_no_outliers<-lm(effective_species~
                Comp.1:Comp.2+
                # log transform number trees because it is right skewed
                log(number_trees,10),
              data=all_data%>%
                filter(filename_city!="Miami")%>%
                filter(filename_city!="Honolulu"))

####
model_op3<-lm(effective_species~
                Comp.1:Comp.2+
                Comp.1+
                tree_city_USA+
                # log transform number trees because it is right skewed
                log(number_trees,10),
              data=all_data)

####
model_op3_no_outliers<-lm(effective_species~
                Comp.1:Comp.2+
                Comp.1+
                tree_city_USA+
                # log transform number trees because it is right skewed
                log(number_trees,10),
              data=all_data%>%
                filter(filename_city!="Miami")%>%
                filter(filename_city!="Honolulu"))

####
model_op4<-lm(effective_species~
                Comp.1:Comp.2+
                Comp.1+Comp.2+
                tree_city_USA+
                # log transform number trees because it is right skewed
                log(number_trees,10),
              data=all_data)

####
model_op4_no_outliers<-lm(effective_species~
                            Comp.1:Comp.2+
                            Comp.1+Comp.2+
                            tree_city_USA+
                            # log transform number trees because it is right skewed
                            log(number_trees,10),
                          data=all_data%>%
                            filter(filename_city!="Miami")%>%
                            filter(filename_city!="Honolulu"))


### Four models to report
models<-list(model_op1,
             model_op1_no_outliers,
             model_op2,
             model_op2_no_outliers,
             model_op3,
             model_op3_no_outliers,
             model_op4,
             model_op4_no_outliers)

### get coefficients and p values for each
results<-list()

for (i in 1:length(models)){
  mymodel<-models[[i]]
  coefs<-rownames(as.data.frame(summary(mymodel)$coef))
  results[[i]]<-matrix(NA,nrow=length(coefs),ncol=7)
  colnames(results[[i]])<-c(
                            "AIC","AdjR2","equation",
                            "coef","estimate",
                       "confint","pvalue")
  results[[i]][1,"equation"]<-paste(mymodel$call[2])
  results[[i]][1,"AIC"]<-round(AIC(mymodel),2)
  results[[i]][1,"AdjR2"]<-round(summary(mymodel)$ adj.r.squared,2)
  
  for (j in 1:length(coefs)){
    # coef
    results[[i]][j,"coef"]<-coefs[j]
    # estimate
    results[[i]][j,"estimate"]<-round(summary(mymodel)$coef[coefs[j],"Estimate"],2)
    # confidence interval
    right<-round(summary(mymodel)$coef[coefs[j],"Estimate"]+
                  2*summary(mymodel)$coef[coefs[j],"Std. Error"],
                2)
    left<-round(summary(mymodel)$coef[coefs[j],"Estimate"]-
                   2*summary(mymodel)$coef[coefs[j],"Std. Error"],
                 2)
    
    results[[i]][j,"confint"]<-paste0("[",left,", ", right,"]")
    # p value
    results[[i]][j,"pvalue"]<-signif(summary(mymodel)$coef[coefs[j],"Pr(>|t|)"],2)
  }
}


results_pretty<-do.call(rbind, results)

results_pretty<-as.data.frame(results_pretty)

results_pretty[(results_pretty$coef=="(Intercept)"), "pvalue"] <- ""
results_pretty[(results_pretty$coef=="(Intercept)"), "confint"] <- ""
results_pretty[(results_pretty$coef=="(Intercept)"), "estimate"] <- ""
results_pretty[(results_pretty$coef=="(Intercept)"), "coef"] <- ""
results_pretty[is.na(results_pretty)]<-""
results_pretty
write.csv(results_pretty,"ModelResults_Effecive_Species.csv")


###################################
###################################
##
## Max abundance single species vs. 
## effective species count correlation
## plot residuals
##
###################################
###################################

# test for correlation
cor.test (all_data$max_abundance_most_common_species, 
          all_data$effective_species, method = "p")


####################################################################
####################################################################
####################################################################
###
###  Percent Native Across Cities
###
####################################################################
####################################################################
####################################################################



beta1<-betareg(percent_native_trees~
                Comp.1+
                Comp.2+
                Comp.1:Comp.2,
                # log(number_trees)+
                # tree_city_USA+
                # city_age_2021+
                # tree_city_USA:city_age_2021,
                 data=all_data3)
summary(beta1)
AIC(beta1)


lrtest(beta1,beta2)



beta2<-betareg(percent_native_trees~
                 Comp.1+
                 Comp.2+
                 Comp.1:Comp.2+
               log(number_trees),
               # tree_city_USA+
               # city_age_2021+
               # tree_city_USA:city_age_2021,
               data=all_data)

lrtest(beta1,beta2)
# big difference adding number trees

beta3<-betareg(percent_native_trees~
                 Comp.1+
                 Comp.2+
                 Comp.1:Comp.2+
                 # log(number_trees),
               tree_city_USA,
               # city_age_2021+
               # tree_city_USA:city_age_2021,
               data=all_data)

lrtest(beta1,beta3)
# no difference adding tree city.

beta4<-betareg(percent_native_trees~
                 Comp.1+
                 Comp.2+
                 Comp.1:Comp.2+
                 # log(number_trees),
                 # tree_city_USA,
               city_age_2021,
               # tree_city_USA:city_age_2021,
               data=all_data)

lrtest(beta1,beta4)
# big difference adding city age

beta5<-betareg(percent_native_trees~
                 Comp.1+
                 Comp.2+
                 Comp.1:Comp.2+
                 # log(number_trees)
                 # city_age_2021,
               tree_city_USA:city_age_2021,
               data=all_data)

lrtest(beta5,beta4)
# again tree city makes no difference

beta6<-betareg(percent_native_trees~
                 Comp.1+
                 Comp.2+
                 Comp.1:Comp.2+
                 log(number_trees)+
                 city_age_2021,
               data=all_data)

lrtest(beta1,beta6)


beta7<-betareg(percent_native_trees~
                 Comp.1+
                 Comp.2+
                 log(number_trees)+
                 city_age_2021,
               data=all_data)

lrtest(beta6,beta7)
# environment interaction term is making no difference

beta8<-betareg(percent_native_trees~
                 Comp.1+
                 Comp.2+
                 log(number_trees)+
                 state_number_native_species/
                 state_number_all_observed_species+
                 city_age_2021,
               data=all_data)

lrtest(beta8,beta7)
summary(beta8)

beta_final<-betareg(percent_native_trees~
                 Comp.1+
                 Comp.2+
                 log(number_trees)+
                 city_age_2021,
               data=all_data)


summary(beta_final)
AIC(beta_final)
plot(beta_final)
beta_final$pseudo.r.squared

## notice one crazy outlier-- Honolulu
beta_final_nooutlier<-betareg(percent_native_trees~
                      Comp.1+
                      Comp.2+
                      log(number_trees)+
                      city_age_2021,
                    data=all_data%>%
                      filter(filename_city!="Honolulu"))

summary(beta_final_nooutlier)
AIC(beta_final_nooutlier)
plot(beta_final_nooutlier)
beta_final_nooutlier$pseudo.r.squared

### Two models to report
models<-list(beta_final,
             beta_final_nooutlier)

### get coefficients and p values for each
results<-list()

for (i in 1:length(models)){
  mymodel<-models[[i]]
  coefs<-rownames(as.data.frame(summary(mymodel)$coef))
  results[[i]]<-matrix(NA,nrow=length(coefs),ncol=9)
  colnames(results[[i]])<-c(
    "AIC","Pseudo_R2","loglik",
    "coef","estimate",
    "confint","odds_ratio",
    "confint_odds_ratio","pvalue")
  results[[i]][1,"AIC"]<-round(AIC(mymodel),2)
  results[[i]][1,"Pseudo_R2"]<-round(summary(mymodel)$pseudo.r.squared,2)
  results[[i]][1,"loglik"]<-round(summary(mymodel)$loglik,2)
  
  for (j in 1:length(coefs)){
    # coef
    results[[i]][j,"coef"]<-coefs[j]
    # estimate
    results[[i]][j,"estimate"]<-signif(summary(mymodel)$coef$mean[coefs[j],"Estimate"],2)
    results[[i]][j,"odds_ratio"]<-round(exp(summary(mymodel)$coef$mean[coefs[j],"Estimate"]),4)
    
    # confidence interval
    right<-signif(summary(mymodel)$coef$mean[coefs[j],"Estimate"]+
                   2*summary(mymodel)$coef$mean[coefs[j],"Std. Error"],
                 2)
    left<-signif(summary(mymodel)$coef$mean[coefs[j],"Estimate"]-
                  2*summary(mymodel)$coef$mean[coefs[j],"Std. Error"],
                2)
    
    results[[i]][j,"confint"]<-paste0("[",left,", ", right,"]")
    
    right_OR<-signif(exp(summary(mymodel)$coef$mean[coefs[j],"Estimate"]+
                    2*summary(mymodel)$coef$mean[coefs[j],"Std. Error"]),
                  4)
    left_OR<-signif(exp(summary(mymodel)$coef$mean[coefs[j],"Estimate"]-
                   2*summary(mymodel)$coef$mean[coefs[j],"Std. Error"]),
                 4)
    
    results[[i]][j,"confint_odds_ratio"]<-paste0("[",left_OR,", ", right_OR,"]")
    
    # p value
    results[[i]][j,"pvalue"]<-signif(summary(mymodel)$coef$mean[coefs[j],"Pr(>|z|)"],2)
  }
}


results_pretty<-do.call(rbind, results)

results_pretty<-as.data.frame(results_pretty)

results_pretty[(results_pretty$coef=="(Intercept)"), "pvalue"] <- ""
results_pretty[(results_pretty$coef=="(Intercept)"), "confint"] <- ""
results_pretty[(results_pretty$coef=="(Intercept)"), "estimate"] <- ""
results_pretty[(results_pretty$coef=="(Intercept)"), "confint_odds_ratio"] <- ""
results_pretty[(results_pretty$coef=="(Intercept)"), "odds_ratio"] <- ""
results_pretty[(results_pretty$coef=="(Intercept)"), "coef"] <- ""
results_pretty[is.na(results_pretty)]<-""
results_pretty

write.csv(results_pretty, "ModelResults_Percent_Native.csv")

###################################
###################################
##
## Biodiversity all vs biodiversity native
##
###################################
###################################

# test for correlation
cor.test (all_data$native_effective_species, 
          all_data$effective_species, method = "p")


####################################################################
####################################################################
####################################################################
###
###  Similarity Scores
###
####################################################################
####################################################################
####################################################################

####################################################################
###  Get data in long form with similarity, not difference, metrics
####################################################################
setwd(home_dir)

comparison_native<-read.csv("ChiSquare_Distances_Species_Composition_NATIVE.csv")
comparison_nonnative<-read.csv("ChiSquare_Distances_Species_Composition_NONNATIVE.csv")
comparison_all<-read.csv("ChiSquare_Distances_Species_Composition_ALL.csv")

# get native only results in long form
results_long_native<-gather(comparison_native,"city2","chisq_distance",-X)
colnames(results_long_native)<-c("city1","city2","chisq_distance_NATIVE")

# get non-native only results in long form
results_long_nonnative<-gather(comparison_nonnative,"city2","chisq_distance",-X)
colnames(results_long_nonnative)<-c("city1","city2","chisq_distance_NONNATIVE")

# Get all results in long form
results_long<-gather(comparison_all,"city2","chisq_distance",-X)
colnames(results_long)<-c("city1","city2","chisq_distance")

# with all results, add a number of other important columns
results_long_noNA<-results_long[complete.cases(results_long), ]
chisq_distances_DF<-results_long_noNA%>%
  left_join(all_data%>%
              dplyr::select(filename_city,state,Lat,Long)%>%
              rename(state1=state,
                     Lat1=Lat,
                     Long1=Long),
            by=c("city1"="filename_city"))%>%
  left_join(all_data%>%
              dplyr::select(filename_city,state,Lat,Long)%>%
              rename(state2=state,
                     Lat2=Lat,
                     Long2=Long),
            by=c("city2"="filename_city"))%>%
  mutate(samestate=ifelse(state1==state2,"yes","no"))%>%
  mutate(hawaii=ifelse(city1=="Honolulu","yes",ifelse(city2=="Honolulu","yes","no")))
# get actual distances between cities
chisq_distances_DF$distance<- distHaversine(chisq_distances_DF[ ,c("Long1", "Lat1")],
                                            chisq_distances_DF[ ,c("Long2", "Lat2")])


env_PCA_distances<-env_PCAscores%>%
  dplyr::select(X,Comp.1,Comp.2)

# get environmental distances
env_dist_results<-matrix(nrow=nrow(chisq_distances_DF),ncol=3)
colnames(env_dist_results)<-c("city1","city2","enviro_dist")

for (i in 1:nrow(chisq_distances_DF)) {
  city1<-chisq_distances_DF$city1[i]
  city2<-chisq_distances_DF$city2[i]
  city1_comp1<-(env_PCA_distances%>%filter(X==city1))$Comp.1
  city1_comp2<-(env_PCA_distances%>%filter(X==city1))$Comp.2
  city2_comp1<-(env_PCA_distances%>%filter(X==city2))$Comp.1
  city2_comp2<-(env_PCA_distances%>%filter(X==city2))$Comp.2
  env_dist_results[i,"enviro_dist"]<-sqrt((city1_comp1-city2_comp1)^2+
                                            (city1_comp2-city2_comp2)^2)
  env_dist_results[i,"city1"]<-city2
  env_dist_results[i,"city2"]<-city1
}


env_dist_results<-as.data.frame(env_dist_results)
env_dist_results$enviro_dist<-as.numeric(env_dist_results$enviro_dist)


nrow(chisq_distances_DF)
nrow(env_dist_results)

chisq_distances_DF["enviro_dist"]<-env_dist_results$enviro_dist

head(chisq_distances_DF)

max_env_integer<-round(max(chisq_distances_DF$enviro_dist),0)

## now, merge back in the native only and nonnative only results
full_chisq_distances_DF<-chisq_distances_DF%>%
  left_join(results_long_native)%>%
  left_join(results_long_nonnative)%>%
  # create columns for similarity, rather than distance
  mutate(chisq_similarity=1-chisq_distance)%>%
  mutate(chisq_similarity_NATIVE=1-chisq_distance_NATIVE)%>%
  mutate(chisq_similarity_NONNATIVE=1-chisq_distance_NONNATIVE)%>%
  # normalized environmental similarity
  mutate(enviro_similarity=1-(enviro_dist-min(enviro_dist))/max(enviro_dist)-min(enviro_dist))%>%
  # get differences 
  mutate(all_minus_native=chisq_similarity-chisq_similarity_NATIVE)%>%
  mutate(all_minus_nonnative=chisq_similarity-chisq_similarity_NONNATIVE)%>%
  mutate(nonnative_minus_native=chisq_similarity_NONNATIVE-chisq_similarity_NATIVE)

  

head(full_chisq_distances_DF)
summary(full_chisq_distances_DF)

write.csv(full_chisq_distances_DF,"Chisq_comparisons_speciescomposition.csv")

####################################################################
###  Statistics
####################################################################

## calculate correlation coefficients
native_env_cor<-cor(full_chisq_distances_DF$enviro_similarity, full_chisq_distances_DF$chisq_similarity_NATIVE)  
all_env_cor<-cor(full_chisq_distances_DF$enviro_similarity, full_chisq_distances_DF$chisq_similarity)
native_all_cor<-cor(full_chisq_distances_DF$chisq_similarity_NATIVE, full_chisq_distances_DF$chisq_similarity)

# test for significant correlations    
cor.test (full_chisq_distances_DF$enviro_similarity, full_chisq_distances_DF$chisq_similarity_NATIVE, method = "p")
cor.test (full_chisq_distances_DF$enviro_similarity, full_chisq_distances_DF$chisq_similarity, method = "p")

# use cocor web interface to compare these dependent, overlapping correlations
cocor_output<-cocor.dep.groups.overlap(r.jk=native_env_cor, r.jh=all_env_cor, r.kh=native_all_cor,
                         n=1951, alternative="two.sided", alpha=0.05,
                         conf.level=0.95, null.value=0)

# extract Z value and p value
cocor_output@pearson1898$statistic
cocor_output@pearson1898$p.value


# check that difference is normal
shapiro.test(full_chisq_distances_DF$nonnative_minus_native)
plot(density(full_chisq_distances_DF$all_minus_native))

# check the difference between all and native only
t.test(full_chisq_distances_DF$chisq_distance_NATIVE,
       full_chisq_distances_DF$chisq_distance,
       paired = TRUE, alternative = "two.sided")

wilcox.test(full_chisq_distances_DF$chisq_distance_NATIVE,
            full_chisq_distances_DF$chisq_distance,
            paired = TRUE, alternative = "two.sided")

ttest$statistic



####################################################################
####################################################################
####################################################################
###
###  Park vs. Urban Forests
###
####################################################################
####################################################################
####################################################################

########### percent native

# check that difference is normal
shapiro.test(park_vs_urban$percent_native_park-park_vs_urban$percent_native_urban)
plot(density(park_vs_urban$percent_native_park-park_vs_urban$percent_native_urban))

# test difference
tt<-t.test(park_vs_urban$percent_native_park,
       park_vs_urban$percent_native_urban,
       paired = TRUE, alternative = "two.sided")

wilcox.test(park_vs_urban$percent_native_park,
            park_vs_urban$percent_native_urban,
            paired = TRUE, alternative = "two.sided")

paste0("t=",signif(tt$statistic,2),", ",
       "p=",signif(tt$p.value,2), ", ",
       "95% CI=[",signif(tt$conf.int[1],2),", ",signif(tt$conf.int[2],2),"]", ", ",
       "mean diff. = ", signif(tt$estimate,2))

########### max abundance

# check that difference is normal
shapiro.test(park_vs_urban$max_abundance_most_common_species_park-park_vs_urban$max_abundance_most_common_species_urban)
plot(density(park_vs_urban$max_abundance_most_common_species_park-park_vs_urban$max_abundance_most_common_species_urban))

# test difference
tt<-t.test(park_vs_urban$max_abundance_most_common_species_park,
       park_vs_urban$max_abundance_most_common_species_urban,
       paired = TRUE, alternative = "two.sided")

wilcox.test(park_vs_urban$max_abundance_most_common_species_park,
            park_vs_urban$max_abundance_most_common_species_urban,
            paired = TRUE, alternative = "two.sided")

paste0("t=",signif(tt$statistic,2),", ",
       "p=",signif(tt$p.value,2), ", ",
       "95% CI=[",signif(tt$conf.int[1],2),", ",signif(tt$conf.int[2],2),"]", ", ",
       "mean diff. = ", signif(tt$estimate,2))


########### biodiversity

# check that difference is normal
shapiro.test(park_vs_urban$effective_species_park-park_vs_urban$effective_species_urban )
plot(density(park_vs_urban$effective_species_park-park_vs_urban$effective_species_urban ))

# test difference
tt<-t.test(park_vs_urban$effective_species_park,
       park_vs_urban$effective_species_urban ,
       paired = TRUE, alternative = "two.sided")

wilcox.test(park_vs_urban$effective_species_park,
            park_vs_urban$effective_species_urban ,
            paired = TRUE, alternative = "two.sided")

paste0("t=",signif(tt$statistic,2),", ",
       "p=",signif(tt$p.value,2), ", ",
       "95% CI=[",signif(tt$conf.int[1],3),", ",signif(tt$conf.int[2],3),"]", ", ",
       "mean diff. = ", signif(tt$estimate,3))





