## add most common genera and abundance

############################################################
##
## City Trees Project
##
############################################################

##############################
##
## Analysis: Raw Datasheets
##
##############################

# load packages
library("dplyr")
library("ggplot2")
library("stringr")
library("maps")
library("totalcensus")
library("raster")
library("mapdata")
library("beepr")
library(taxize)
library("analogue")
library(geosphere)

# set up our directories
home_dir <- 'C:/Users/dakot/Dropbox/PC/Documents/Trees/Analysis'
dir_rawsheets <- 'C:/Users/dakot/Dropbox/PC/Documents/Trees/Analysis/Final Spreadsheets'
dir_figures <- 'C:/Users/dakot/Dropbox/PC/Documents/Trees/Figures'
setwd(home_dir)

# read basic files
city_info<-read.csv("city_information.csv")
social_data<-read.csv("Social_Data_Treecity_Ages.csv")
bonap_native<-read.csv("BONAP_native_taxa_uniquebinomials.csv")
bonap_all<-read.csv("BONAP_all_species_uniquebinomials.csv")

## make a blank list to fill it in with dataframes
LIST_NATIVE<-list()

setwd(dir_rawsheets)
all_files<-list.files(pattern = ".csv$")

##################################
###
### Get list of native or not within each city, and counts
###
##################################
for (i in 1:length(all_files)) {
  data<-read.csv(all_files[i])
  if ("scientific_name" %in% colnames(data)) {
    ### stuff
    summary<-data%>%
      #filter(nonmatch==FALSE)%>%
      dplyr::select(scientific_name,state,city,native)%>%
      filter(scientific_name!="")%>%
      group_by(scientific_name,native)%>%
      summarise(n=n())
    
    ############
    ###
    ### Label as "unique" genus-only rows where there are not other species of that genus
    ###
    ############
    
    ## count words in species name
    summary$word_count<-str_count(summary$scientific_name, '\\w+')
    
    ## make list of genera
    genusonlylist<-summary%>%filter(word_count==1)
    speciesonlylist<-summary%>%filter(word_count==2)
    genusonlylist$unique_genus="NA"
    genusonlylist
    
    for (j in 1:nrow(genusonlylist)){
      ## select each genus-only scientific name
      genus=genusonlylist$scientific_name[j]
      ## generate regex search term
      genus_at_start=paste0("^",genus," ",sep="")
      ## count other instances in full dataset of species who share that genus 
      count_genus<-sum(str_count(summary$scientific_name, pattern = genus_at_start))
      ## label with unique or duplicate
      ifelse(count_genus==0,genusonlylist$unique_genus[j]<-"unique",genusonlylist$unique_genus[j]<-"duplicate")
    }
    speciesonlylist<-speciesonlylist%>%
      mutate(unique_genus="species")
    ## bind the "unique" rows back to the dataset
    # genus_to_keep<-genusonlylist%>%filter(unique_genus=="unique")%>%
    #   dplyr::select(-unique_genus)
    alldata<-rbind(speciesonlylist,genusonlylist)%>%
      dplyr::select(-word_count)
    LIST_NATIVE[[all_files[i]]]<-alldata
  }
}
beep()
length(LIST_NATIVE)
LIST_NATIVE[[1]]

#############################
###
### for every city, calculate measures of diversity for whole city population and
### measures of diversity for native trees
###
#############################
setwd(home_dir)

### get list of state names
state_list<-unique(bonap_native$state)

### view city specific information
head(city_info)

## prepare output file
diversity_output<-matrix(nrow=length(LIST_NATIVE),ncol=37)
colnames(diversity_output)<-c("filename_city",
                              "city_pretty",
                              "state",
                              "most_common_species",
                              "most_common_genus",
                              "Lat",
                              "Long",
                              "population",
                              "population_rank",
                              "number_trees",
                              "number_species",
                              "number_trees_filtered",
                              "Shannon_Wiener_Index",
                              "effective_species",
                              "max_abundance_most_common_species",
                              "max_abundance_most_common_genus",
                              "native_number_species",
                              "native_number_trees",
                              "native_Shannon_Wiener_Index",
                              "native_effective_species",
                              "number_trees_known_native_status",
                              "percent_native_trees",
                              "state_number_all_observed_species",
                              "state_number_native_species",
                              "shared_species_city_statenative",
                              "beta_diversity_city_statenative",
                              "nonnative_trees_NO_native_congener",
                              "nonnative_trees_YES_native_congener",
                              "nonnative_species_NO_native_congener",
                              "nonnative_species_YES_native_congener",
                              "nonnative_trees_PERCENTWITH_native_congeners",
                              "nonnative_species_PERCENTWITH_native_congeners",
                              "effective_species_park",
                              "effective_species_urban",
                              "percent_native_park",
                              "percent_native_urban",
                              "state_land_area_km2")


## calculate diversity measures
for (i in 1:length(LIST_NATIVE)) {
  data_all<-LIST_NATIVE[[i]]
  ##############################################
  ##
  ## All trees: Diversity Measures
  ##
  ##############################################
  data<-data_all%>%
    # keep only genera for which they are unique (to get accurate diversity measures)
    filter(unique_genus!="duplicate")%>%
    # get frequency values
    mutate(frequency=n/sum(data_all$n))%>%
    # prepare for shannon_wiener
    mutate(freq_ln_freq=-1*frequency*log(frequency))
  ## filename
  diversity_output[i,"filename_city"]<-strsplit(names(LIST_NATIVE)[i], '_')[[1]][1]
  ## number of species
  diversity_output[i,"number_species"]<-nrow(data)
  ## number of trees included (with the limitation of excluding nonmatches, genuses duplicated elsewhere, etc)
  diversity_output[i,"number_trees_filtered"]<-sum(data$n)
  ## shannon weiner
  shannon_wiener <- sum(data$freq_ln_freq)
  diversity_output[i,"Shannon_Wiener_Index"]<-shannon_wiener
  ## exp (shannon wiener)
  diversity_output[i,"effective_species"]<-exp(shannon_wiener)
  ## most common species
  diversity_output[i,"most_common_species"]<-data$scientific_name[which.max(data$frequency)]
  diversity_output[i,"max_abundance_most_common_species"]<-max(data$frequency)
  ## most common genus
  ## build off of all genera, not just unique genera
  data_genera<-data_all ## this includes ALL genera
  data_genera$genus<-str_split_fixed(data_genera$scientific_name,' ',n=2)[,1]
  data_genera<-data_genera%>%group_by(genus)%>%
    summarize(count=sum(n))
  data_genera<-data_genera%>%
    mutate(frequency=count/sum(data_genera$count))
  diversity_output[i,"most_common_genus"]<-data_genera$genus[which.max(data_genera$frequency)]
  diversity_output[i,"max_abundance_most_common_genus"]<-max(data_genera$frequency)
  ##############################################
  ##
  ## Native data only: Diversity Measures
  ##
  ##############################################
  native_data_all<-data_all%>%
    filter(native==TRUE)
  native_data<-native_data_all%>%
    # get frequency values
    mutate(frequency=n/sum(native_data_all$n))%>%
    # prepare for shannon_wiener
    mutate(freq_ln_freq=-1*frequency*log(frequency))
  # number native species
  diversity_output[i,"native_number_species"]<-nrow(native_data)
  ## number of native trees included (with the limitation of excluding nonmathches, genuses duplicated elsewhere, etc)
  diversity_output[i,"native_number_trees"]<-sum(native_data$n)
  ## shannon weiner
  shannon_wiener_native <- sum(native_data$freq_ln_freq)
  diversity_output[i,"native_Shannon_Wiener_Index"]<-shannon_wiener_native
  ## exp (shannon wiener)
  diversity_output[i,"native_effective_species"]<-exp(shannon_wiener_native)
  ## Exclude species for which we have "no info"
  status_known_data<-data_all%>%
    filter(native!="no_info")
  ## total trees for which we know native status
  diversity_output[i,"number_trees_known_native_status"]<-sum(status_known_data$n)
  ##############################################
  ##
  ## Beta Diversity between City All Species and State Native Species
  ##
  ##############################################
  ## get list of native species in state
  mycity_filename<-strsplit(names(LIST_NATIVE)[i], '_')[[1]][1]
  mystate<-(city_info%>%filter(filename_city==mycity_filename))$state
  mycitypretty<-(city_info%>%filter(filename_city==mycity_filename))$city_pretty
  Lat<-(city_info%>%filter(filename_city==mycity_filename))$Lat
  Long<-(city_info%>%filter(filename_city==mycity_filename))$Long
  Population_Rank<-(city_info%>%filter(filename_city==mycity_filename))$rank
  Population<-(city_info%>%filter(filename_city==mycity_filename))$Population
  state_land_area<-(city_info%>%filter(filename_city==mycity_filename))$land_area_km2
  native_state_list<-(bonap_native%>%filter(state==mystate))$scientific_name
  ## save pretty city name
  diversity_output[i,"city_pretty"]<-mycitypretty
  ## calculate how many species are shared
  shared_count<- sum(data_all$scientific_name %in% native_state_list)
  ## save @ native species in that city's state
  state_native_count<-length(native_state_list)
  diversity_output[i,"state_number_native_species"]<-state_native_count
  ## save # species shared between city and native state
  diversity_output[i,"shared_species_city_statenative"]<-shared_count
  ## save state name
  diversity_output[i,"state"]<-mystate
  ## save state number all observed species
  all_state_list<-(bonap_all%>%filter(state==mystate))$scientific_name
  state_all_count<-length(all_state_list)
  diversity_output[i,"state_number_all_observed_species"]<-state_all_count
  ## save other city info
  diversity_output[i,"population"]<-Population
  diversity_output[i,"population_rank"]<-Population_Rank
  diversity_output[i,"Lat"]<-Lat
  diversity_output[i,"Long"]<-Long
  diversity_output[i,"state_land_area_km2"]<-state_land_area
  ##############################################
  ##
  ## Do non-native species have a native congener among that
  ## state's list of native species?
  ##
  ##############################################
  # get list of all native genera with counts of species within genus
  native_congeners<-as.data.frame(str_split_fixed(native_state_list,' ',n=2)[,1])%>%
    `colnames<-`(c("genus"))%>%
    group_by(genus)%>%
    summarize(number_native_congeners=n())
  # get list of nonnative genera with counts
  congener_values <- data_all%>%
    filter(native==FALSE)%>%
    mutate(genus=str_split_fixed(scientific_name,' ',n=2)[,1])%>%
    group_by(genus)%>%
    ## count how many of each genus-- potentially useful later if we want more detailed info about each city
    summarize(number_nonnative=sum(n))%>%
    ## label each row by whether it has a native congener
    mutate(native_congener=(genus %in% native_congeners$genus))%>%
    ## count number non-native species, and number non-native trees, that have a native congenerer
    group_by(native_congener)%>%
    summarize(trees=sum(number_nonnative),species=n())
  diversity_output[i,"nonnative_trees_NO_native_congener"]<-(congener_values%>%filter(native_congener==FALSE))$trees
  diversity_output[i,"nonnative_trees_YES_native_congener"]<-(congener_values%>%filter(native_congener==TRUE))$trees
  diversity_output[i,"nonnative_species_NO_native_congener"]<-(congener_values%>%filter(native_congener==FALSE))$species
  diversity_output[i,"nonnative_species_YES_native_congener"]<-(congener_values%>%filter(native_congener==TRUE))$species
  ## count number non-native trees that have a native congener
  ##############################################
  ##
  ## Get total # all trees
  ##
  ##############################################
  unfiltered_data<-read.csv(paste0("Final Spreadsheets/",names(LIST_NATIVE)[i],sep=''))
  diversity_output[i,"number_trees"]<-nrow(unfiltered_data)
}

beep()
DIVERSITY<-as.data.frame(diversity_output)
# DIVERSITY[,25]
## convert relevant columns to numeric
DIVERSITY[,6:32] <- sapply(DIVERSITY[,6:32],as.numeric)

##############################################
##
## Calculate percent native
##
##############################################
DIVERSITY$percent_native_trees<-DIVERSITY$native_number_trees/DIVERSITY$number_trees_known_native_status

##############################################
##
## Calculate beta diversity comparing city to the list of native species
## from that state
##
##############################################
DIVERSITY$beta_diversity_city_statenative<-
  DIVERSITY$number_species  + 
  DIVERSITY$state_number_native_species -
  2*DIVERSITY$shared_species_city_statenative 

##############################################
##
## Calculate percentage of non-native trees, and species, that have native congeners
## from that state
##
##############################################

## trees
DIVERSITY$nonnative_trees_PERCENTWITH_native_congeners<-
  # yes / (yes + no)
  DIVERSITY$nonnative_trees_YES_native_congener/(DIVERSITY$nonnative_trees_YES_native_congener+
                                                   DIVERSITY$nonnative_trees_NO_native_congener)
### species
DIVERSITY$nonnative_species_PERCENTWITH_native_congeners<-
  # yes / (yes + no)
  DIVERSITY$nonnative_species_YES_native_congener/(DIVERSITY$nonnative_species_YES_native_congener+
                                                     DIVERSITY$nonnative_species_NO_native_congener)
head(DIVERSITY)
tail(DIVERSITY)

####################################################################################
##
##
## STATE
## save a spreadsheet of all states
##
##
####################################################################################

setwd(home_dir)

native<-bonap_native%>%
  group_by(state,code)%>%
  summarize(state_number_native_species=n())

all_by_state<-bonap_all%>%
  group_by(state,code)%>%
  summarize(state_number_all_observed_species=n())%>%
  full_join(native)

write.csv(all_by_state,"State_Specific_Data.csv",row.names=FALSE, fileEncoding = "UTF-8")


#####################################################
###
### Write city-specific data file
###
####################################################

setwd(home_dir)

# write.csv(DIV,"Q1_Q5_diversity_by_city_ALL_and_NATIVE.csv",row.names=FALSE, fileEncoding = "UTF-8")
write.csv(DIVERSITY,"City_Specific_Data.csv",row.names=FALSE, fileEncoding = "UTF-8")

#############################################################################
#############################################################################
#############################################################################
#############################################################################
###
### BioClim Analysis
###
#############################################################################
#############################################################################
#############################################################################
#############################################################################

## get data
env <- getData("worldclim", var="bio", res=2.5)
env

### label the variables
bioclim_names <-
  c(
    "Annual_Mean_Temp",
    "Mean_Diurnal_Range",
    "Isothermality",
    "Temp_Seasonality",
    "Max_Temp_Warmest Month",
    "Min_Temp_Coldest_Month",
    "Temp_Annual_Range",
    "Mean_Temp_Wettest_Quarter",
    "Mean_Temp_Driest_Quarter",
    "Mean_Temp_Warmest_Quarter",
    "Mean_Temp_Coldest_Quarter",
    "Annual_Precip",
    "Precip_Wettest_Month",
    "Precip_Driest_Month",
    "Precip_Seasonality",
    "Precip_Wettest_Quarter",
    "Precip_Driest_Quarter",
    "Precip_Warmest_Quarter",
    "Precip_Coldest_Quarter"
  )

names(env) <- bioclim_names

### Crop to continental US

cont_USA_long<-c(-124.848974,-66.885444)
cont_USA_lat<-c(49.384358, 24.396308)

buff <- 1   #a buffer of one degree around the raster

xmin <- cont_USA_long[1] - buff
xmax <- cont_USA_long[2] + buff
ymin <- cont_USA_lat[2] - buff
ymax <- cont_USA_lat[1] + buff

e <- extent(xmin, xmax, ymin, ymax)

envcrop <- crop(env, e)

### test plot
# plot(envcrop[[1]], main = "Annual Mean Temperature")
# map(
#   'state',
#   xlim = c(xmin, xmax),
#   ylim = c(ymin, ymax),
#   fill = F,
#   add = T
# )

### Crop to Hawaii

Hawaii_long<-c(-178.334698,-154.806773)
Hawaii_lat<-c(28.402123, 18.910361)

buff <- 1   #a buffer of one degree around the raster

xmin <- Hawaii_long[1] - buff
xmax <- Hawaii_long[2] + buff
ymin <- Hawaii_lat[2] - buff
ymax <- Hawaii_lat[1] + buff

e <- extent(xmin, xmax, ymin, ymax)

envcrop_Hawaii <- crop(env, e)
### test plot
# plot(envcrop_Hawaii[[1]], main = "Annual Mean Temperature")
# map(
#   'state',
#   xlim = c(xmin, xmax),
#   ylim = c(ymin, ymax),
#   fill = F,
#   add = T
# )


### Get All Environmental Data

env_all <- data.frame(rasterToPoints(env)) %>%
  rename(.,Long=x,Lat=y)
beep()
head(env_all)

### assign environmental values to each city

ENV_LIST=list()
for (i in 1:nrow(DIVERSITY)){
  env_data_mycity<-env_all%>%
    filter(abs(Long - DIVERSITY$Long[i]) == min(abs(Long - DIVERSITY$Long[i])))%>%
    filter(abs(Lat - DIVERSITY$Lat[i]) == min(abs(Lat - DIVERSITY$Lat[i])))%>%
    rename(.,Env_Long=Long,Env_Lat=Lat)
  ENV_LIST[[DIVERSITY$filename_city[i]]]<-env_data_mycity
}

env_by_city<-bind_rows(ENV_LIST, .id = "filename_city")


### merge with our data
##

alldata<-DIVERSITY%>%
  left_join(env_by_city,by="filename_city")

write.csv(alldata,"City_Data_Diversity_Enviroment.csv",row.names=FALSE, fileEncoding = "UTF-8")


################################################
###
### Merge with Social data
### for example tree city USA age
###
################################################

alldata_social<-alldata%>%
  left_join(social_data,by="filename_city")


write.csv(alldata_social,"City_Data_Diversity_Enviroment_Social.csv",
          row.names=FALSE, fileEncoding = "UTF-8")


head(alldata_social)


beep()


######################
##
## WEIGHTED DIFFERENCES-- Bray Curtis is bad and chi-square is good
##
## https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.12776
##
######################

###########################################
##
## Add Dice-Sorenson between cities
## Add Dice-Sorenson between state native populations
## get Ben's code
##
##
###########################################

citycount<-length(all_files)

# citycount=5

results_all<-matrix(nrow=citycount,ncol=citycount)
colnames(results_all)<-alldata_social$filename_city
rownames(results_all)<-alldata_social$filename_city

results_native<-results_all
results_nonnative<-results_all
results_euclidean_all<-results_all
results_euclidean_native<-results_all
results_euclidean_nonnative<-results_all
results_dice_sorenson<-results_all
results_dice_sorenson_all<-results_all
results_dice_sorenson_native<-results_all
results_dice_sorenson_nonnative<-results_all

# going to do comparisons for all species, and also for only native and only non-native
native_status<-c("TRUE","FALSE")
file_label<-c("native","nonnative","allspecies")

for (m in 1:3){
  for (i in 1:citycount){
    for (j in 1:citycount) {
        # get only below the diagonal (no need to compare cities twice)
      if (j<i) {
        # read in files
        x_full<-read.csv(all_files[i])
        y_full<-read.csv(all_files[j])
        
        # filter species 
        ### when m is 1, run native only
        ### when m is 2, run non-native only
        ### when m is 3, run full file
        if (m<3){
          x_full<-x_full%>%
            filter(native==native_status[m])
          y_full<-y_full%>%
            filter(native==native_status[m])
        }

        # summarize them to get species frequencies
        x<-x_full%>%group_by(scientific_name)%>%
          summarize(n=n())%>%
          filter(scientific_name!='')%>%
          mutate(X=n/sum(n))%>%
          dplyr::select(-n)
        y<-y_full%>%group_by(scientific_name)%>%
          summarize(n=n())%>%
          filter(scientific_name!='')%>%
          mutate(Y=n/sum(n))%>%
          dplyr::select(-n)
        
        comparison<-x%>%
          full_join(y)%>%
          replace(is.na(.), 0)%>%
          mutate(chisq_diff=(X-Y)^2/(X+Y))%>%
          mutate(euclidean_diff=(X-Y)^2)
        
        # calculate comparison metrics
        elements_in_common<-length(intersect(x$scientific_name,
                                             y$scientific_name))
        total_elements<-nrow(x)+nrow(y)
        chi_squared_distance<-sum(comparison$chisq_diff)/2
        euclidean_distance<-sqrt(sum(comparison$euclidean_diff))
        dice_sorenson_value<-2*elements_in_common/total_elements
        
        # save results
        if (m==1) {
          results_native[i,j]<-chi_squared_distance
          results_euclidean_native[i,j]<-euclidean_distance
          results_dice_sorenson_native[i,j]<-dice_sorenson_value
        }
        
        if (m==2) {
          results_nonnative[i,j]<-chi_squared_distance
          results_euclidean_nonnative[i,j]<-euclidean_distance
          results_dice_sorenson_nonnative[i,j]<-dice_sorenson_value
        }
        
        if (m==3) {
          results_all[i,j]<-chi_squared_distance
          results_euclidean_all[i,j]<-euclidean_distance
          results_dice_sorenson_all[i,j]<-dice_sorenson_value
        }
      }
    }
  }
}

beep()


write.csv(results_all, paste0(home_dir,"/ChiSquare_Distances_Species_Composition_ALL.csv"))
write.csv(results_native, paste0(home_dir,"/ChiSquare_Distances_Species_Composition_NATIVE.csv"))
write.csv(results_nonnative, paste0(home_dir,"/ChiSquare_Distances_Species_Composition_NONNATIVE.csv"))

write.csv(results_euclidean_all, paste0(home_dir,"/Euclidean_Distances_Species_Composition_ALL.csv"))
write.csv(results_euclidean_native, paste0(home_dir,"/Euclidean_Distances_Species_Composition_NATIVE.csv"))
write.csv(results_euclidean_nonnative, paste0(home_dir,"/Euclidean_Distances_Species_Composition_NONNATIVE.csv"))

write.csv(results_dice_sorenson_all, paste0(home_dir,"/DiceSorenson_Scores_Species_Composition_ALL.csv"))
write.csv(results_dice_sorenson_native, paste0(home_dir,"/DiceSorenson_Scores_Species_Composition_NATIVE.csv"))
write.csv(results_dice_sorenson_nonnative, paste0(home_dir,"/DiceSorenson_Scores_Species_Composition_NONNATIVE.csv"))


##########################
##
## Get Dice Sorenson scores for state populations
## of trees, all versus native
##
##
##########################
setwd(home_dir)
BONAP_all<-read.csv("BONAP_all_species_uniquebinomials.csv")
BONAP_native<-read.csv("BONAP_native_taxa_uniquebinomials.csv")

state_list<-unique(BONAP_all$state)

state_count<-length(state_list)

results_state_ALL<-matrix(nrow=state_count,ncol=state_count)
colnames(results_state_ALL)<-state_list
rownames(results_state_ALL)<-state_list

results_state_NATIVE<-results_state_ALL

for (i in 1:state_count){
  for (j in 1:state_count) {
    
    if (j<i) {
      # BONAP_ALL
      x<-BONAP_all%>%
        filter(state==state_list[i])
      y<-BONAP_all%>%
        filter(state==state_list[j])

      # dice sorenson
      elements_in_common<-length(intersect(x$scientific_name,
                                           y$scientific_name))
      total_elements<-nrow(x)+nrow(y)
      
      results_state_ALL[i,j]<-2*elements_in_common/total_elements

      # BONAP_NATIVE
      x<-BONAP_native%>%
        filter(state==state_list[i])
      y<-BONAP_native%>%
        filter(state==state_list[j])
      
      # dice sorenson
      elements_in_common<-length(intersect(x$scientific_name,
                                           y$scientific_name))
      total_elements<-nrow(x)+nrow(y)
      
      results_state_NATIVE[i,j]<-2*elements_in_common/total_elements
      
    }
    
  }
}

write.csv(results_state_ALL, paste0(home_dir,"/DiceSorenson_Scores_States_ALL.csv"))
write.csv(results_state_NATIVE, paste0(home_dir,"/DiceSorenson_Scores_States_NATIVE.csv"))


# results<-read.csv(paste0(home_dir,"/ChiSquare_Distances_Species_Composition.csv"))
# results_euclidean<-read.csv(paste0(home_dir,"/Euclidean_Distances_Species_Composition.csv"))
# 
# ## convert to long form
# results_long<-gather(results,"city2","chisq_distance",-X)
# 
# colnames(results_long)<-c("city1","city2","chisq_distance")
# 
# results_long_noNA<-results_long[complete.cases(results_long), ]
# 
# head(results_long_noNA)
# 
# results_long_noNA%>%
#   arrange(-chisq_distance)
# 
# chisq_distances_DF<-results_long_noNA%>%
#   left_join(alldata_social%>%
#               dplyr::select(filename_city,state,Lat,Long)%>%
#               rename(state1=state,
#                      Lat1=Lat,
#                      Long1=Long),
#             by=c("city1"="filename_city"))%>%
#   left_join(alldata_social%>%
#               dplyr::select(filename_city,state,Lat,Long)%>%
#               rename(state2=state,
#                      Lat2=Lat,
#                      Long2=Long),
#             by=c("city2"="filename_city"))%>%
#   mutate(samestate=ifelse(state1==state2,"yes","no"))%>%
#   mutate(hawaii=ifelse(city1=="Honolulu","yes",ifelse(city2=="Honolulu","yes","no")))
# 
# # get actual distances between cities
# chisq_distances_DF$distance<- distHaversine(chisq_distances_DF[ ,c("Long1", "Lat1")],
#                                             chisq_distances_DF[ ,c("Long2", "Lat2")])
# 
# # 
# 
# # get "distances" for environmental similarity between PCA 1 and PCA2
# %>%mutate(enviro_dist=sqrt((Comp.1-Comp)))
# 
# 
#  ## exploratory plots
# ggplot(data=chisq_distances_DF)+
#   geom_point(aes(y=chisq_distance,x=distance,color=samestate,shape=hawaii),
#              alpha=0.75)
# 
# ggplot(data=chisq_distances_DF%>%
#          filter(hawaii=="no"))+
#   geom_point(aes(y=chisq_distance,x=distance,color=hawaii),
#              alpha=0.75)
# 
# 
# 
# ggplot(data=chisq_distances_DF)+geom_jitter(aes(y=chisq_distance,x=samestate))
# 
# ggplot(data=chisq_distances_DF)+
#   geom_histogram(aes(y=..density..,x=chisq_distance,fill=samestate),
#                  alpha=0.75,
#                  position="identity")
# 
# 
# 
# chisq_distances_DF$distance<- distHaversine(chisq_distances_DF[ ,c("Long1", "Lat1")],
#                 chisq_distances_DF[ ,c("Long2", "Lat2")])
# 
# head(chisq_distances_DF)
# 
# 
# 
# ggplot(data=chisq_distances_DF)+
#   geom_point(aes(y=chisq_distance,x=distance,color=samestate),
#                  alpha=0.75)
# 
# 
# 
# 
# 
# 
# 
# x<-read.csv(all_files[i])%>%group_by(scientific_name)%>%
#   summarize(n=n())%>%
#   filter(scientific_name!='')%>%
#   mutate(X=n/sum(n))%>%
#   dplyr::select(-n)
# y<-read.csv(all_files[j])%>%group_by(scientific_name)%>%
#   summarize(n=n())%>%
#   filter(scientific_name!='')%>%
#   mutate(Y=n/sum(n))%>%
#   dplyr::select(-n)
# 
# comparison<-x%>%
#   full_join(y)%>%
#   replace(is.na(.), 0)%>%
#   mutate(diff=(X-Y)^2/(X+Y))
# 
# chi_squared_distance<-sum(comparison$diff)/2
# 
# chi_squared_distance




# check
# sum(comparison$relative_abundance_X)
# sum(comparison$relative_abundance_Y)

# to run without genus only rows

# distance(comparison,method="chi.squ")

# check






################################################################################
###
### NOTES
###
################################################################################


########################################
##
##
##
## Why use effective species #?
##
##
#########################################

### theoretical background
###  http://www.loujost.com/Statistics%20and%20Physics/Diversity%20and%20Similarity/EffectiveNumberOfSpecies.htm
###  http://www.loujost.com/Statistics%20and%20Physics/Diversity%20and%20Similarity/Diversity%20of%20a%20single%20community.htm
###  http://www.loujost.com/Statistics%20and%20Physics/Diversity%20and%20Similarity/How%20to%20compare%20the%20diversities%20of%20two%20communities.htm
###
### Shannon indices are best because they are not biased toward common or rare species
### you should alwys convert diversity indices to effective # of species
### see links above for more details.