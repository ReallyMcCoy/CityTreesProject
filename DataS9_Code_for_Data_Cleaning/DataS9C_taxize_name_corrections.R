############
####
#### https://ropensci.org/tutorials/taxize_tutorial/
####
############


library("taxize")
library("dplyr")
library("stringr")
library('beepr')

home_dir <- 'C:/Users/dakot/Documents/Trees/Data Cleaning'

# where are the unedited sheets
dir_rawsheets <- 'C:/Users/dakot/Documents/Trees/Data Cleaning/Sheets_Data_Misspellings_Corrected'

## specify a path for our newly edited files
path_taxize <-'C:/Users/dakot/Documents/Trees/Data Cleaning/Sheets_Taxize_Name_Corrections'


######## 
# read in needed files
########

setwd(home_dir)

### read in data frames
allnames_bycity <- read.csv('Species_By_City.csv')
## need to have filename be "City_edited_common.csv"

allnames <- read.csv('Species_Counts.csv')

## for purposes of matching, remove "sp." from the genus only rows
# allnames<-allnames%>%
#  mutate(scientific_name=str_replace(allnames$scientific_name, " sp.$", ""))


###############
###############
###
### use taxize to check all the names
###
###############
###############

################################
## full scientific name checks
################################
name_check <- gnr_resolve(sci = allnames$scientific_name,
            best_match_only = TRUE)%>%
  arrange(.,by=score,ascending=FALSE)

original<-name_check
beep()

name_check<-name_check%>%
  # as we did before, remove the "x" indicating cultivars
  mutate(matched_name=str_replace(matched_name, " x ", " "))%>%
  mutate(matched_name=str_replace(matched_name, " X ", " "))%>%
  mutate(matched_name=str_replace(matched_name, " x ", " "))%>%
  mutate(matched_name=str_replace(matched_name, "X ", ""))%>%
  #mutate(matched_name=str_replace(matched_name, "x ", ""))%>%
  #mutate(matched_name=str_replace(matched_name, " x", ""))%>%
  mutate(matched_name=str_replace(matched_name, "× ", ""))%>%
  mutate(matched_name=str_replace(matched_name, " ×", ""))


### first two words only
name_check$final_name<-paste(word(name_check$matched_name, 1,1, sep=" "),
                                 word(name_check$matched_name, 2,2, sep=" "),
                                 sep=" ")

## remove NA from genus only names
name_check<-name_check%>%
  mutate(final_name=str_replace(name_check$final_name, " NA", ""))%>%
## create column for whether it is a perfect match or not
  mutate(match = if_else(user_supplied_name == matched_name, 
                         'match', 'non-match'))

## count words
#sapply(strsplit(str1, " "),length)
#df$total <- str_count(df$df, '\\s+')+1

# PERFECT matches
perfect_matches <- name_check %>%
  filter(match=="match")
perfect_matches
write.csv(perfect_matches,"Perfect_Matches_Taxize.csv")

# PARTIAL Matches
partial_matches<-name_check %>% 
  filter(match!="match")%>%
  left_join(.,allnames,by=c("final_name"="scientific_name"))%>%
  arrange(-count)
partial_matches
write.csv(partial_matches,"Partial_Matches_Taxize.csv")

## NON-MATCHES 
allnames$any_match=allnames$scientific_name %in% name_check$user_supplied_name
non_matches<-allnames%>%
  filter(any_match=="FALSE")
non_matches
write.csv(non_matches,"Non_Matches_Taxize.csv")




###########################
##
##
## BONAP
##
##
###########################

# dir <- 'C:/Users/dakot/Documents/Trees Collaboration'
# setwd(dir)
# 
# BONAP <- read.csv("BONAP_native_taxa.csv")
# 
# #### too big to do all at once,
# #### so split into groups of 5000.
# 
# # make a sequence of our range cutoffs
# seq<-seq(0, nrow(BONAP), by = 5000)
# seq<-append(seq, nrow(BONAP), after = length(seq))
# 
# list = list()
# for (i in 1:(length(seq)-1)) {
#   list[[i]] <- gnr_resolve(sci = BONAP$scientific_name[seq[i]:seq[i+1]],
#                          best_match_only = TRUE)
# }
# beep()
# 
# all_BONAP_check<-do.call(rbind, list)
# write.csv(all_BONAP_check,'BONAP_name_check.csv')
# 
# 
# BONAP$any_match=BONAP$scientific_name %in% all_BONAP_check$user_supplied_name
# BONAP_non_matches<-BONAP%>%
#   filter(any_match=="FALSE")
# write.csv(BONAP_non_matches,"BONAP_Non_Matches.csv")

############################################
##
##
## REPLACE bad names with edited matches
##
##
############################################
# dir <- 'C:/Users/dakot/Documents/Trees Collaboration'
# setwd(dir)
# 
# states <- read.csv("State_Abbreviations.csv")
# 
# citylist<- read.csv("Cities.csv")
# citylist

setwd(dir_rawsheets)


# edited_files
allnames_bycity

#### find the city files that have the names that need to be edited
#### make a dataframe with all the edits that need to be made, with a column
#### for the file name
edits_to_make<-merge(partial_matches,allnames_bycity,by.x='user_supplied_name',
      by.y='scientific_name')

## get a list of the files we need to read and edit
## and merge it with the list of all files
all_files <- data.frame(list.files(pattern = ".csv$"))
colnames(all_files)<-c('file')
files_to_edit <- data.frame(unique(edits_to_make$file))
colnames(files_to_edit)<-c('file')
files_to_edit['edit']<-'yes'
files_to_edit<-all_files%>%
  full_join(.,files_to_edit,keep.all=TRUE)
files_to_edit[is.na(files_to_edit)] <- "no"


## make all the edits suggested by taxize
for (j in 1:nrow(files_to_edit)) {
  data<-read.csv(files_to_edit$file[j])
  if ("scientific_name" %in% colnames(data)) {
    ## make column to label non-matches
    data$nonmatch=data$scientific_name %in% non_matches$scientific_name
    ### add column for whether it is genus only.
    data$word_count_name <- sapply(strsplit(data$scientific_name, " "), length)
    data$genus_only <- ifelse(data$word_count_name==1,"genus_only","no")
  }
  cityname <- strsplit(files_to_edit$file[j], '_')[[1]][1]
  ### if file needs to be edited, edit it
  if (files_to_edit$edit[j]=="yes") {
    edits<-edits_to_make%>%
      filter(file==files_to_edit$file[j])
    for (i in 1:nrow(edits)) {
      data<-data %>%
      mutate(scientific_name=replace(scientific_name, 
                                 scientific_name==edits$user_supplied_name[i],
                                 edits$final_name[i]))
    }
    ## label genus-only columns
    
    write.csv(data,paste(path_taxize,'/',cityname,'_edited_taxize.csv',sep=''),
                         row.names=FALSE, fileEncoding = "UTF-8")
  }
  # if file doesn't need to be edited, just write it to a new CSV
  if (files_to_edit$edit[j]=="no") {
    write.csv(data,paste(path_taxize,'/',cityname,'_edited_taxize.csv',sep=''),
                         row.names=FALSE, fileEncoding = "UTF-8")
  }
}

beep()

########
## edit non mathes
########


# nonmatchedits<-read.csv('C:/Users/dakot/Documents/Trees Collaboration/Analysis/manual_nonmatch_replacements.csv')
# head(nonmatchedits)
# head(non_matches)
# 
# write.csv(non_matches%>%
#   left_join(nonmatchedits)%>%
#   arrange(replacement_coded),
#   "delete_soon.csv")



