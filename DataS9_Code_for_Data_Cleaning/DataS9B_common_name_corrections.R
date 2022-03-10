library("dplyr")
library("stringr")
library('beepr')

### at end of code, you can find the code we used to generate reference
### scientific names given common names

home_dir <- 'C:/Users/dakot/Documents/Trees/Data Cleaning'
dir_rawsheets <- 'C:/Users/dakot/Documents/Trees/Data Cleaning/Sheets_Taxize_Name_Corrections'
## specify a path for corrected sheets
path_common <- 'C:/Users/dakot/Documents/Trees/Data Cleaning/Sheets_Common_Names_Corrected'



########################
##
##  edit files
##
########################
setwd(home_dir)

## read in overall manually checked lookup file
### see commented code below for how we got the look up table
common_name_lookup_all <-read.csv('Common_Name_Lookup_Table_28Jan2021.csv')
## if duplicates, prefer Ben Goulet-Scott's manually converted name rather than
## the automatic name.
common_name_lookup<-distinct(common_name_lookup_all,common_name,.keep_all=TRUE)%>%
  select(common_name,scientific_name)
common_name_lookup%>%
  filter(common_name=="Austrian pine")
## read in list of non matches
non_matches <- read.csv('Non_Matches_Taxize.csv')


##########################################
##
## Pair all common names with scientific names
##
##########################################

setwd(dir_rawsheets)
all_files<-list.files(pattern = ".csv$")
# 
# files_to_edit_common<-read.csv('Common_Names_Files_To_Edit.csv')$X0
# files_to_edit_common

for (j in 1:length(all_files)) {
  data<-read.csv(all_files[j])
  cityname <- strsplit(all_files[j], '_')[[1]][1]
  ##############################
  ##
  ## no common_name column
  ##
  ##############################
  if (!("common_name" %in% colnames(data))) {
    print(paste(all_files[[j]],"has no common_name column"))
  }
  ##############################
  ##
  ## common_name AND scientific_name column
  ##
  ##############################
  if("common_name" %in% colnames(data)) {
    if("scientific_name" %in% colnames(data)) {
      print(paste(all_files[[j]],"has both scientific_name and common_name columns"))
    }
    ##############################
    ##
    ## ONLY common_name columns
    ##
    ##############################
    if (!("scientific_name" %in% colnames(data))) {
      print(paste("I looked up scientific_name for common_name in", all_files[[j]]))
      ## add column based on our lookup table
      ## first, join data with lookup table
      data<-data%>%
        left_join(common_name_lookup)
    }
  }
  ## make column to label non-matches
  data$nonmatch=data$scientific_name %in% non_matches$scientific_name
  ### add column for whether it is genus only.
  data$word_count_name <- sapply(strsplit(data$scientific_name, " "), length)
  data$genus_only <- ifelse(data$word_count_name==1,"genus_only","no")
  write.csv(data,paste(path_common,'/',cityname,'_edited_taxize_common.csv',sep=''),
              row.names=FALSE, fileEncoding = "UTF-8")
    
}

beep()





########################
##
##
## Make reference table out of all sheets that have BOTH common name AND scientific name
## This code is commented because we 
##
##
########################

# list_common_names <- list()
# 
# for (k in 1:length(all_files)) {
#   data<-read.csv(all_files[k])
# 
#   if (("scientific_name" %in% colnames(data))) {
#     if (("common_name" %in% colnames(data))) {
#       ## add column based on our lookup table
#       ## first, join data with lookup table
#       ref_data<-data%>%select(common_name,scientific_name)%>%
#         group_by(common_name,scientific_name)%>%
#         summarize(n=n())%>%
#         arrange(-n)%>%
#         mutate(file=all_files[k])
#       list_common_names[[k]]<-ref_data
#     }
#   }
# }
# 
# 
# within_data_ref_common_names<-do.call(rbind, list_common_names)%>%
#   filter(common_name!='')%>%
#   arrange(common_name,-n)
# head(within_data_ref_common_names)
# 
# View(within_data_ref_common_names)
# 
# final_within_data_lookup<-distinct(within_data_ref_common_names,common_name,.keep_all=TRUE)
# View(final_within_data_lookup)
# write.csv(final_within_data_lookup,paste(home_dir,'/Common_Name_Within_Data_Lookup.csv',sep=''),
# row.names=FALSE, fileEncoding = "UTF-8")








###########################
##
##
### Generate reference scientific names
### given common names only. 
### this code is commented because we
### manually reviewed the suggestions
##
##
###########################

# dir <- 'C:/Users/dakot/Documents/Trees Collaboration/Analysis'
# setwd(dir)
# 
# non_matches <-read.csv('Non_Matches_Taxize.csv')
# 
# common_names<-read.csv("Common_Names_Counts.csv")%>%
#   select(-X)
# head(common_names)
# 
# ##################
# ## ITIS
# ##################
# results<-comm2sci(common_names$common_name,db = "itis")
# beep()
# 
# list_to_df <- function(listfordf){
#   if(!is.list(listfordf)) stop("it should be a list")
# 
#   df <- list(list.element = listfordf)
#   class(df) <- c("tbl_df", "data.frame")
#   attr(df, "row.names") <- .set_row_names(length(listfordf))
# 
#   if (!is.null(names(listfordf))) {
#     df$name <- names(listfordf)
#   }
# 
#   df
# }
# 
# df_version<-list_to_df(results)
# 
# 
# comm2sci<-matrix(nrow=length(results),ncol=4)
# for (i in  1:length(results)){
#   comm2sci[i,1]<-common_names$common_name[i]
#   comm2sci[i,2]<-results[[i]][1]
#   comm2sci[i,3]<-results[[i]][2]
#   comm2sci[i,4]<-results[[i]][3]
# }
# comm2sci<-as.data.frame(comm2sci)
# colnames(comm2sci)<-c("common_name","ITIS_scientific_name_1","ITIS_scientific_name_2",
#                       "ITIS_scientific_name_3")
# head(comm2sci)
# # write.csv(comm2sci,"ITIS_comm2sci_results.csv",row.names=FALSE, fileEncoding = "UTF-8")
# 
# common_reference_ITIS<-read.csv("ITIS_comm2sci_results.csv")
# head(common_reference_ITIS)
# 
# common_reference_internal_all<-read.csv("Common_Names_Reference.csv")
# head(common_reference_internal_all)
# common_reference_internal<-common_reference_internal_all%>%
#   group_by(common_name,scientific_name)%>%
#   summarise(n=sum(count))%>%
#   #summarise(n=n())%>%
#   arrange(-n)%>%
#   ungroup()%>%
#   distinct(common_name,.keep_all = TRUE)%>%
#   select(common_name,scientific_name)
# 
# common_reference_external<-read.csv("EcoSpeciesList_20July2018_esxmUKb.csv")%>%
#   select(common_name,scientific_name)%>%
#   rename(EcoSpeciesList_scientific_name=scientific_name)
# head(common_reference_external)
# 
# common_name_lookup<-common_names%>%
#   left_join(common_reference_internal)%>%
#   left_join(common_reference_external)%>%
#   left_join(common_reference_ITIS)%>%
#   arrange(-count)
# head(common_name_lookup)
# write.csv(common_name_lookup, "Common_Name_Replacements.csv",row.names=FALSE, fileEncoding = "UTF-8")







