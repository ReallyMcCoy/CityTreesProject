library("dplyr")
library("stringr")
library('beepr')



home_dir <- 'C:/Users/dakot/Documents/Trees/Data Cleaning'
dir_rawsheets <- 'C:/Users/dakot/Documents/Trees/Data Cleaning/Sheets_BONAP_Native_Assignments'
## specify a dir for corrected sheets
dir_edited<- 'C:/Users/dakot/Documents/Trees/Data Cleaning/Sheets_Coordinates_Fixed'
dir_fixed_sheets<- 'C:/Users/dakot/Documents/Trees/Data Cleaning/Sheets_Coordinates_Fixed/Fixed_Sheets'

setwd(home_dir)

## read list of cities to be fixed


setwd(dir_fixed_sheets)
all_files<-list.files(pattern = ".csv$")
cities_to_fix<- matrix(nrow=length(all_files),ncol=1)

for (i in 1:length(all_files)){
  cities_to_fix[i,1]<-str_split(all_files[i],"_")[[1]][1]
}

cities_to_fix<-as.vector(cities_to_fix)

setwd(dir_rawsheets)
raw_files<-list.files(pattern = ".csv$")
raw_files

# check if file is in list to fix
for (i in 1:length(raw_files)) {
  setwd(dir_rawsheets)
  data<-read.csv(raw_files[i])
  mycity<-str_split(raw_files[i],"_")[[1]][1]
  mycity

# if so, merge with lat long
  if (mycity %in% cities_to_fix) {
    setwd(dir_fixed_sheets)
    find_fixed_file<-list.files(pattern= paste0(mycity,"*",sep=""))
    # make sure you didn't find two files
    if (length(find_fixed_file) > 1) {print ("We have a problem")}
    fixed_data<-read.csv(find_fixed_file[1])
    fixed_coordinates<-fixed_data%>%
      dplyr::select(address,longitude_coordinate,latitude_coordinate)%>%
      rename(., address_2 = address)
    ## bind new rows 
    final_sheet<-cbind(data,fixed_coordinates)
    head(final_sheet)
    # check that address 2 column matches address column
    if(!(identical(final_sheet[['address']],final_sheet[['address_2']]))){
      print("problem: addresses don't match")}
    # check that final sheet lengths are the same
    if (nrow(final_sheet)!=nrow(data)){print("problem: lengths don't match")}
  } 
  # if not, save original data as final sheet
  if (!(mycity %in% cities_to_fix)){
    final_sheet<-data
  }
  
# write to csv
  write.csv(final_sheet,paste(dir_edited,'/',mycity,
                              '_edited_taxize_common_native_coordinates',
                              '.csv',sep=''),
            row.names=FALSE, fileEncoding = "UTF-8")

}
beep()
