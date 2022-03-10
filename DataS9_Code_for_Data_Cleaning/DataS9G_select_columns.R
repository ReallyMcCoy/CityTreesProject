library("dplyr")
library("stringr")
library('beepr')


home_dir <- 'C:/Users/dakot/Documents/Trees/Data Cleaning'
dir_rawsheets <- 'C:/Users/dakot/Documents/Trees/Data Cleaning/Sheets_Coordinates_Fixed'
## specify a dir for corrected sheets
dir_edited<- 'C:/Users/dakot/Documents/Trees/Data Cleaning/Sheets_Columns_Selected'


setwd(home_dir)

# make character vector of GOOD columns
columns_list <- read.csv("Column_Headers_Final.csv")
good_columns<-columns_list$column_name



setwd(dir_rawsheets)
all_files<-list.files(pattern = ".csv$")

## we will be removing weird lat long points
cont_USA_long<-c(-124.848974,-66.885444)
cont_USA_lat<-c(49.384358, 24.396308)
buff <- 1   #a buffer of one degree
xmin <- cont_USA_long[1] - buff
xmax <- cont_USA_long[2] + buff
ymin <- cont_USA_lat[2] - buff
ymax <- cont_USA_lat[1] + buff

for (i in 1:length(all_files)) {
  # read data for that city
  data <- read.csv(all_files[i])
  # get cityname
  mycity <- strsplit(all_files[i], '_')[[1]][1]
  # get statename
  mystate <- data$state[1]
  # subset by the columns in our list
  final_data<-data%>% 
    select(any_of(good_columns))%>%
    ## add a greater metro area column
    mutate(greater_metro=mycity)
  ## add blank columns for any column names with no data, to make all sheets same dimension
  for (j in 1:length(good_columns)) {
    if (!(good_columns[j] %in% colnames(final_data))) {
      final_data<-final_data%>%
        mutate(!!(good_columns[j]) := 'NA')
    }
  }
  
  ## remove lat long points that are outside continental USA for non-Honolulu files
  if (mycity!="Honolulu") {
    final_data<-final_data%>%
      mutate(latitude_coordinate = ifelse(latitude_coordinate < ymin | latitude_coordinate > ymax, '', latitude_coordinate))%>%
      mutate(longitude_coordinate = ifelse(longitude_coordinate < xmin | longitude_coordinate > xmax, '', longitude_coordinate))
  }
  if (dim(final_data)[2]!=length(good_columns)){
    print(paste0("There's a problem with ",mycity))
  }
  ## write CSV
  write.csv(final_data,paste(dir_edited,'/',mycity,'_Final_',Sys.Date(),'.csv',sep=''),
              row.names=FALSE, fileEncoding = "UTF-8")
}
  
beep()

###################
#######
##### LA
##### LA is too long, so we need to merge the four documents into 1. We did that and saved it as one.
#######

setwd(dir_edited)
###################
losangeles<-list.files(pattern = "LosAn*")
losangeles
LA<-read.csv(losangeles[1])
for (i in 2:length(losangeles)) {
  LA<-rbind(LA,read.csv(losangeles[i]))
}

head(LA)
LA_edit_greater_metro<-LA%>%
  mutate(greater_metro=gsub(1,"",greater_metro))%>%
  mutate(greater_metro=gsub(2,"",greater_metro))%>%
  mutate(greater_metro=gsub(3,"",greater_metro))%>%
  mutate(greater_metro=gsub(4,"",greater_metro))
head(LA_edit_greater_metro)
write.csv(LA_edit_greater_metro,paste(dir_edited,'/','LosAngeles','_Final_',Sys.Date(),'.csv',sep=''),row.names=FALSE, fileEncoding = "UTF-8")
sum_LA<-nrow(read.csv(losangeles[1]))+nrow(read.csv(losangeles[2]))+nrow(read.csv(losangeles[3]))+nrow(read.csv(losangeles[4]))
original_LA<-nrow(LA_edit_greater_metro)

#############
##
## Delete old LA files
##
#############
if (sum_LA == original_LA) {
  print("everything worked!")
  for (j in 1:length(losangeles)) {
    fn<-losangeles[j]
    if (file.exists(fn)) {
    #Delete file if it exists
     file.remove(fn)
    }
  }
}
beep()
