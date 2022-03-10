###
### does location_type correlate with condition?
###

# packages
library("dplyr")
library("ggplot2")
library("stringr")
library("beepr")
library("arm")
library("glm2")

# set up our directories
home_dir <- 'C:/Users/dakot/Dropbox/PC/Documents/Trees/Analysis'
dir_rawsheets <- 'C:/Users/dakot/Dropbox/PC/Documents/Trees/Analysis/Final Spreadsheets'

setwd(dir_rawsheets)

# ## make a blank list to fill it in with dataframes
# list<-list()

all_files<-list.files(pattern = ".csv$")

## make a blank list to fill it in with dataframes
list<-list()
list_nolocationtype<-list()

for (i in 1:length(all_files)) {
  data<-read.csv(all_files[i])
  mycity<-strsplit(all_files[i],"_")[[1]][1]
  ## CONDITION
  if (!(sum(is.na(data$condition))-length(data$condition)==0)) {
    ## NATIVE
    if (!(sum(is.na(data$native))-length(data$native)==0)) {
      ## LOCATION TYPE
      if (!(sum(is.na(data$location_type))-length(data$location_type)==0)) {
        summary<-data%>%
          dplyr::select(condition,native,location_type,scientific_name)%>%
          # group_by(condition,native)%>%
          # summarize(n=n())%>%
          # ungroup()%>%
          filter(condition!="")%>%
          filter(!is.na(condition))%>%
          filter(condition!="NA")%>%
          filter(condition!=" ")%>%
          filter(native!="")%>%
          filter(native!="<null>")%>%
          filter(!is.na(native))%>%
          filter(native!='no_info')%>%
          filter(location_type!="no_info")%>%
          filter(location_type!="")%>%
          filter(location_type!="<null>")%>%
          filter(location_type!="<NA>")%>%
          filter(!is.na(location_type))%>%
          mutate_if(is.logical, as.character)
        if (nrow(summary)>0){
          if (length(unique(summary$condition)>1)){
            if (length(unique(summary$location_type))>1){
              list[[mycity]]<-summary
            }
            if (!(length(unique(summary$location_type))>1)){
              summary<-summary%>%dplyr::select(-location_type)
              list_nolocationtype[[mycity]]<-summary
            }
            }
          }
      }
    }
  }
}


beep()

length(list)
names(list)
length(list_nolocationtype)
names(list_nolocationtype)

results<-matrix(nrow=length(list)+
                  length(list_nolocationtype)+
                  length(list_noDBH),ncol=10)
colnames(results)<-c("city",
                     "native_coef", "native_pval","native_CI_left","native_CI_right",
                     "location_type_coef", "location_type_pval","location_type_CI_left","location_type_CI_right",
                     "model_pval") 

head(model_data)

ggplot() + geom_bar(stat="count",data=model_data,aes(x=condition_numeric,fill=location_type))

for(i in 1:length(list)) {
  data<-list[[i]]
  
  results[i, "city"]<-names(list)[i]
  
  model_data<-data%>%
    mutate(condition_numeric=ifelse(condition=="excellent",1,
                                    ifelse(condition=="good",1,
                                           ifelse(condition=="fair",1,
                                                  ifelse(condition=="poor",0,
                                                         ifelse(condition=="dead",0,
                                                                ifelse(condition=="dead/dying",0,"NA")
                                                         ))))))
  
   model_data$condition_numeric<-as.numeric(model_data$condition_numeric)
   model_data$native<-as.factor(model_data$native)
   model_data$location_type<-as.factor(model_data$location_type)
   
  # 
  # (model_data%>%group_by(location_type)%>%summarize(n=n()))
  # model_data%>%group_by(condition_numeric,native)%>%summarize(n=n())

  ## if we didn't filter everything out...
  if (nrow(model_data>0)) { 
  
    mylogit <- glm2(condition_numeric~ 
                          native + 
                         location_type,
                   data = model_data, family = binomial(link = "logit"),
                   maxit=25)
    
    summary(mylogit)
    #coefficients
    #coef(mylogit)
    results[i, "native_coef"]<-coef(mylogit)[['nativeTRUE']]
    results[i, "native_CI_left"]<- confint(mylogit)["nativeTRUE",1]
    results[i, "native_CI_right"]<- confint(mylogit)["nativeTRUE",2]
    results[i, "location_type_coef"]<-coef(mylogit)[['location_typegreen_space']]
    results[i, "location_type_CI_left"]<- confint(mylogit)["location_typegreen_space",1]
    results[i, "location_type_CI_right"]<- confint(mylogit)["location_typegreen_space",2]
    #p values
    #coef(summary(mylogit))[,4]
    results[i, "native_pval"]<-coef(summary(mylogit))[,4][['nativeTRUE']]
    results[i, "location_type_pval"]<-coef(summary(mylogit))[,4][['location_typegreen_space']]
    #confint(mylogit)
    ## does model fit significantly better than an empty model?
    results[i, "model_pval"]<-with(mylogit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
    
    
  }


}

predict(mylogit,type = "response")

for(i in (length(list)+1):(length(list)+length(list_nolocationtype))) {
  data<-list_nolocationtype[[i-length(list)]]
  
  results[i, "city"]<-names(list_nolocationtype)[i-length(list)]
  
  model_data<-data%>%
    mutate(condition_numeric=ifelse(condition=="excellent",1,
                                    ifelse(condition=="good",1,
                                           ifelse(condition=="fair",1,
                                                  ifelse(condition=="poor",0,
                                                         ifelse(condition=="dead",0,
                                                                ifelse(condition=="dead/dying",0,"NA")
                                                         ))))))
  
  model_data$condition_numeric<-as.numeric(model_data$condition_numeric)
  # 
  # (model_data%>%group_by(location_type)%>%summarize(n=n()))
  # model_data%>%group_by(condition_numeric,native)%>%summarize(n=n())
  
  ## if we didn't filter everything out...
  if (nrow(model_data>0)) { 
    
    mylogit <- glm(condition_numeric~ native,
                   data = model_data, family = "binomial",maxit=25)
    
    summary(mylogit)
    #coefficients
    #coef(mylogit)
    results[i, "native_coef"]<-coef(mylogit)[['nativeTRUE']]
    #p values
    #coef(summary(mylogit))[,4]
    results[i, "native_pval"]<-coef(summary(mylogit))[,4][['nativeTRUE']]
    #confint(mylogit)

    ## does model fit significantly better than an empty model?
    results[i, "model_pval"]<-with(mylogit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
    
  }
  
}








for(i in (length(list)+length(list_nolocationtype)+1):
          (length(list)+length(list_nolocationtype)+length(list_noDBH))) {
  data<-list_noDBH[[i-length(list)-length(list_nolocationtype)]]
  
  results[i, "city"]<-names(list_noDBH)[i-length(list)-length(list_nolocationtype)]
  
  model_data<-data%>%
    mutate(condition_numeric=ifelse(condition=="excellent",1,
                                    ifelse(condition=="good",1,
                                           ifelse(condition=="fair",1,
                                                  ifelse(condition=="poor",0,
                                                         ifelse(condition=="dead",0,
                                                                ifelse(condition=="dead/dying",0,"NA")
                                                         ))))))
  
  model_data$condition_numeric<-as.numeric(model_data$condition_numeric)
  # 
  # (model_data%>%group_by(location_type)%>%summarize(n=n()))
  # model_data%>%group_by(condition_numeric,native)%>%summarize(n=n())
  
  ## if we didn't filter everything out...
  if (nrow(model_data>0)) { 
    
    mylogit <- glm(condition_numeric~ native + 
                     location_type,
                   data = model_data, family = "binomial",maxit=25)
    
    summary(mylogit)
    #coefficients
    #coef(mylogit)
    results[i, "native_coef"]<-coef(mylogit)[['nativeTRUE']]
    results[i, "location_type_coef"]<-coef(mylogit)[['location_typegreen_space']]
    #p values
    #coef(summary(mylogit))[,4]
    results[i, "native_pval"]<-coef(summary(mylogit))[,4][['nativeTRUE']]
    results[i, "location_type_pval"]<-coef(summary(mylogit))[,4][['location_typegreen_space']]
    #confint(mylogit)
    ## does model fit significantly better than an empty model?
    results[i, "model_pval"]<-with(mylogit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
    
  }
  
}

results
beep()






#################################################
##
## Location Type and Native Status NO DBH -- NO FILES
##
#################################################

#################################################
##
## Native Status and DBH NO Location Type -- NO FILES
##
#################################################


## make a blank list to fill it in with dataframes
list<-list()

for (i in 1:length(all_files)) {
  data<-read.csv(all_files[i])
  ## CONDITION
  if (!(sum(is.na(data$condition))-length(data$condition)==0)) {
    ## NATIVE
    if (!(sum(is.na(data$native))-length(data$native)==0)) {
      ## LOCATION TYPE
      if ((sum(is.na(data$location_type))-length(data$location_type)==0)) {
        ## DIAMETER BREAST HEIGHT
        if (!(sum(is.na(data$diameter_breast_height_CM))-length(data$diameter_breast_height_CM)==0)) {
          mycity<-strsplit(all_files[i],"_")[[1]][1]
          summary<-data%>%
            dplyr::select(condition,native,location_type,diameter_breast_height_CM,scientific_name)%>%
            # group_by(condition,native)%>%
            # summarize(n=n())%>%
            # ungroup()%>%
            filter(condition!="")%>%
            filter(!is.na(condition))%>%
            filter(condition!="NA")%>%
            filter(condition!=" ")%>%
            filter(native!="")%>%
            filter(native!="<null>")%>%
            filter(!is.na(native))%>%
            filter(native!='no_info')%>%
            filter(location_type!="no_info")%>%
            filter(location_type!="")%>%
            filter(location_type!="<null>")%>%
            filter(location_type!="<NA>")%>%
            filter(!is.na(location_type))%>%
            filter(diameter_breast_height_CM!="")%>%
            filter(diameter_breast_height_CM!="<null>")%>%
            # smaller than 40 ft diameter
            filter(diameter_breast_height_CM<1219.2)%>%
            filter(!is.na(diameter_breast_height_CM))%>%
            mutate_if(is.logical, as.character)
          if (nrow(summary)>0){
            if (length(unique(summary$location_type))>1){
              if (length(unique(summary$condition)>1)){
                list[[mycity]]<-summary
              }
            }
          }
        }
      }
    }
  }
}

beep()

length(list)


results<-matrix(nrow=length(list),ncol=14)
colnames(results)<-c("city",
                     "native_coef", "native_pval","native_CI_left","native_CI_right",
                     "location_type_coef", "location_type_pval","location_type_CI_left","location_type_CI_right",
                     "DBH_coef", "DBH_pval","DBH_CI_left","DBH_CI_right",
                     "model_pval") 


for(i in 1:length(list)) {
  data<-list[[i]]
  
  results[i, "city"]<-names(list)[i]
  
  model_data<-data%>%
    mutate(condition_numeric=ifelse(condition=="excellent",1,
                                    ifelse(condition=="good",1,
                                           ifelse(condition=="fair",1,
                                                  ifelse(condition=="poor",0,
                                                         ifelse(condition=="dead",0,
                                                                ifelse(condition=="dead/dying",0,"NA")
                                                         ))))))
  
  # model_data$condition_numeric<-as.numeric(model_data$condition_numeric)
  # 
  # (model_data%>%group_by(location_type)%>%summarize(n=n()))
  # model_data%>%group_by(condition_numeric,native)%>%summarize(n=n())
  
  ## if we didn't filter everything out...
  if (nrow(model_data>0)) { 
    
    mylogit <- glm(condition_numeric~ native + 
                     location_type + 
                     diameter_breast_height_CM,
                   data = model_data, family = "binomial",maxit=25)
    
    summary(mylogit)
    #coefficients
    #coef(mylogit)
    results[i, "native_coef"]<-coef(mylogit)[['nativeTRUE']]
    results[i, "location_type_coef"]<-coef(mylogit)[['location_typegreen_space']]
    results[i, "DBH_coef"]<-coef(mylogit)[['diameter_breast_height_CM']]
    #p values
    #coef(summary(mylogit))[,4]
    results[i, "native_pval"]<-coef(summary(mylogit))[,4][['nativeTRUE']]
    results[i, "location_type_pval"]<-coef(summary(mylogit))[,4][['location_typegreen_space']]
    results[i, "DBH_pval"]<-coef(summary(mylogit))[,4][['diameter_breast_height_CM']]
    #confint(mylogit)
    ## does model fit significantly better than an empty model?
    results[i, "model_pval"]<-with(mylogit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
    
    
  }
  
  
}

results
beep()



















#################################################
##
## Percent Native vs Condition
##
#################################################

## make a blank list to fill it in with dataframes
list<-list()

for (i in 1:length(all_files)) {
  data<-read.csv(all_files[i])
  ## limit to files where condition is not always NA
  if (!(sum(is.na(data$condition))-length(data$condition)==0)) {
    ## limit to files where native is not always NA
    if (!(sum(is.na(data$native))-length(data$native)==0)) {
      mycity<-strsplit(all_files[i],"_")[[1]][1]
      summary<-data%>%
        select(condition,native)%>%
        group_by(condition,native)%>%
        summarize(n=n())%>%
        ungroup()%>%
        filter(condition!="")%>%
        filter(!is.na(condition))%>%
        filter(condition!="NA")%>%
        filter(condition!=" ")%>%
        filter(native!="")%>%
        filter(native!="<null>")%>%
        filter(!is.na(native))%>%
        mutate_if(is.logical, as.character)
      list[[mycity]]<-summary
    }
  }
}

beep()

length(list)

native_cond_data<-bind_rows(list, .id = "city")

head(native_cond_data)

native_cond_model<-native_cond_data%>%
  filter(native!='no_info')%>%
  mutate(condition_numeric=ifelse(condition=="excellent",5,
                                  ifelse(condition=="good",4,
                                         ifelse(condition=="fair",3,
                                                ifelse(condition=="poor",2,
                                                       ifelse(condition=="dead",1,
                                                              ifelse(condition=="dead/dying",1,"NA")
                                                       ))))))
plot<-native_cond_model%>%
  group_by(condition_numeric,native)%>%
  summarize(sum=sum(n))

plot3<-native_cond_model%>%
  group_by(city,condition_numeric,native)%>%
  summarize(sum=sum(n))

native_cond_model$condition_numeric<-as.numeric(native_cond_model$condition_numeric)

plot2<-native_cond_model%>%
  group_by(filename_city,native)%>%
  summarize(mean_condition=mean(condition_numeric))

ggplot(plot)+geom_col(aes(x=condition_numeric,y=sum,fill=native),position="fill")+
  labs(title=("1 is Dead/Dying, 5 is Excellent"))
setwd(home_dir)
ggsave("Condition_vs_Native_All.png", dpi=600,height=3,width=5,units="in")


ggplot(plot3)+geom_col(aes(x=condition_numeric,y=sum,fill=native),position="fill")+
  facet_wrap(facets=c('city'))+
  labs(title=("1 is Dead/Dying, 5 is Excellent"))

setwd(home_dir)
ggsave("Condition_vs_Native_byCity.png", dpi=600,height=8,width=8,units="in")



#################################################
##
## Condition of Most Common Species
##
#################################################
setwd(dir_rawsheets)
list=list()
for (i in 1:length(all_files)) {
  
  mycity_name<-strsplit(all_files[i],"_")[[1]][1]
  mycity<-read.csv(all_files[i])%>%
    ### need to label genus only rows by adding "sp."
    mutate(wordcount = stringr::str_count(scientific_name, ' ') + 1)%>%
    mutate(scientific_name = ifelse(wordcount == 1, 
                                    ## TRUE
                                    ## but do not paste for blanks
                                    ifelse(scientific_name=="",
                                           "",
                                           paste(scientific_name,"sp.")),
                                    ## FALSE
                                    scientific_name))%>%
    mutate(condition_numeric=ifelse(condition=="excellent",5,
                                    ifelse(condition=="good",4,
                                           ifelse(condition=="fair",3,
                                                  ifelse(condition=="poor",2,
                                                         ifelse(condition=="dead",1,
                                                                ifelse(condition=="dead/dying",1,"NA")
                                                         ))))))
  mycity$condition_numeric<-as.numeric( mycity$condition_numeric)
  mycity<-mycity%>%
    filter(!(is.na(condition_numeric)))
  if (!(is.na(sum(mycity$condition_numeric)))){
  ### get 10 most common species
    species_table<-mycity%>%
      filter(scientific_name!="")%>%
      filter(condition_numeric!="NA")%>%
      group_by(scientific_name,native)%>%
      summarize(n=n(),mean_condition=mean(condition_numeric),median_condition=median(condition_numeric))
    species_table
  list[[mycity_name]] <-species_table
  }
}

beep()

length(list)




cond_by_species<-bind_rows(list, .id = "city")%>%
  group_by(city,scientific_name)%>%
  arrange(city,-n)
head(cond_by_species)


##########
##
## right now we're still including Genus sp. type species. should we exclude them?
##
############
top_ten_species<-cond_by_species %>% 
  arrange(desc(n)) %>% 
  group_by(city) %>% slice(1:10)%>%
  arrange(city)%>%
  mutate(top_ten="top_ten_species")%>%
  select(city,scientific_name,top_ten)

alldata<-left_join(cond_by_species,top_ten_species)%>%
  mutate(top_ten=ifelse(is.na(top_ten),"less_common",top_ten))

ggplot(data=alldata)+geom_jitter(aes(x=top_ten,y=mean_condition,color=top_ten),alpha=0.1)+
  geom_boxplot(aes(x=top_ten,y=mean_condition),alpha=0)

setwd(home_dir)
ggsave("Condition_vs_SpeciesCommonness.png", dpi=600,height=4,width=6,units="in")


ggplot(data=alldata%>%
         group_by(median_condition,top_ten)%>%
         summarize(sum=n()))+geom_col(aes(x=median_condition,y=sum,fill=top_ten),position="fill")+
  labs(title=("1 is Dead/Dying, 5 is Excellent"))


setwd(home_dir)
ggsave("Condition_vs_SpeciesCommonness_Barplot.png", dpi=600,height=4,width=6,units="in")

alldata%>%View()

write.csv(alldata,"Condition_by_Species_and_City.csv")

##########################################
##
##
## Condition of all trees in a city colored by native
##
##
##########################################


## limit to more than 30 of a given species
# dat<-cond_by_species%>%
#   filter(n>30)

dat<-cond_by_species
head(dat)

ggplot(dat%>%filter(native!="no_info"))+
  geom_jitter(aes(y=mean_condition,x=native,color=native),alpha=0.5)+
  geom_boxplot(aes(y=mean_condition,x=native),alpha=0)+
  facet_wrap("city")

setwd(home_dir)
ggsave("Condition_vs_Native_ByCity.png", dpi=600,height=8,width=8,units="in")

ggplot(dat%>%filter(native!="no_info"))+
  geom_jitter(aes(y=mean_condition,x=native,color=native),alpha=0.5)+
  geom_boxplot(aes(y=mean_condition,x=native),alpha=0)
  
setwd(home_dir)
ggsave("Condition_vs_Native.png", dpi=600,height=4,width=6,units="in")

###############
##
## species that are present in both native and non native form with mroe than 100 trees
##
###############
dat2<-cond_by_species%>%
  filter(n>100)%>%
  filter(native!="no_info")%>%
  group_by(scientific_name,native)%>%
  summarize(trees=sum(n),mean_condition_overall=mean(mean_condition))%>%
  # summarize(trees=sum(n),median_condition_overall=median(median_condition))%>%
  group_by(scientific_name)%>% filter(n()>1)%>%
  arrange(scientific_name)%>%
  select(-trees)%>%
  # # spread(.,native,median_condition_overall)
  tidyr::spread(.,native,mean_condition_overall)%>%
  mutate(native_minus_non_mean_condition=`TRUE`-`FALSE`)%>%
  select(scientific_name,native_minus_non_mean_condition)
  

ggplot(dat2)+ geom_density(aes(x=native_minus_non_mean_condition))+
  theme_classic()

setwd(home_dir)
ggsave("Condition_vs_NativeMinusNon.png", dpi=600,height=4,width=4,units="in")

  
dat3<-cond_by_species%>%
  filter(n>100)%>%
  filter(native!="no_info")%>%
  group_by(scientific_name,native)%>%
  summarize(trees=sum(n),mean_condition_overall=mean(mean_condition))%>%
  # summarize(trees=sum(n),median_condition_overall=median(median_condition))%>%
  group_by(scientific_name)%>% filter(n()>1)%>%
  arrange(scientific_name)

dat3$scientific_name <- factor(dat3$scientific_name, levels=dat2$scientific_name[order(dat2$native_minus_non_mean_condition)], ordered=TRUE)

ggplot(dat3)+
  geom_boxplot(aes(x=native,y=mean_condition_overall))+
  geom_jitter(aes(x=native,y=mean_condition_overall,color=native))
setwd(home_dir)
ggsave("Condition_vs_Native_SpeciesWithBoth.png", dpi=600,height=8,width=8,units="in")


ggplot(dat3,aes(x=native,y=mean_condition_overall,group=1))+
  geom_point(aes(color=native))+
  geom_line(aes(x=native,y=mean_condition_overall))+
  facet_wrap("scientific_name")


setwd(home_dir)
ggsave("Condition_vs_Native_SpeciesWithBoth_bySpecies.png", dpi=600,height=8,width=12,units="in")


















##################
##
## Scratch work
##
##################



ggplot(dat)+geom_point(aes(x=trees,y=mean_condition_overall))
  
cond_by_species%>%
  filter(scientific_name=="Platanus acerifolia")

    ### add list of specified colors
    # mutate(colors=c("#5e4fa2", "#3288bd","#66c2a5","#abdda4","#e6f598",
    #                 "#fee08b","#fdae61","#f46d43","#d53e4f","#9e0142"))%>%
    # add_row(.,scientific_name="Other",
    #         ##  count number other species
    #         n= nrow(mycity%>%filter(!(scientific_name %in% mycity$scientific_name))),
    #         colors="black")
  











library(lme4)
mixed_model = lmer(condition_numeric ~ native + (1 | city), data = native_cond_model)
summary(mixed_model)

summary(lm(data=native_cond_model,condition_numeric~native+filename_city))

## make a blank list to fill it in with dataframes
list<-list()
setwd(dir_rawsheets)

for (i in 1:length(all_files)) {
  data<-read.csv(all_files[i])
  if ("condition" %in% colnames(data)) {
    if ('native' %in% colnames (data)) {
      mycity<-strsplit(all_files[i],"_")[[1]][1]
      summary<-data%>%
        select(condition,native)%>%
        group_by(condition,native)%>%
        summarize(n=n())%>%
        ungroup()%>%
        filter(condition!="")%>%
        filter(!is.na(condition))%>%
        filter(condition!="NA")%>%
        filter(condition!=" ")%>%
        filter(native!="")%>%
        filter(native!="<null>")%>%
        filter(!is.na(native))%>%
        mutate_if(is.logical, as.character)
      list[[mycity]]<-summary
    }
  }
}

beep()

length(list)

native_cond_data<-bind_rows(list, .id = "city")




