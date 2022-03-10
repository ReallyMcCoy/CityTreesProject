library("dplyr")
library("stringr")
library('beepr')
library(ggplot2)
#library(maps)
#library(scatterpie)
#library(totalcensus)

home_dir <- 'C:/Users/dakot/Documents/Trees/Data Cleaning'
dir_rawsheets <- 'C:/Users/dakot/Documents/Trees/Data Cleaning/Sheets_Common_Names_Corrected'
## specify a path for corrected sheets
dir_BONAP<- 'C:/Users/dakot/Documents/Trees/Data Cleaning/Sheets_BONAP_Native_Assignments'
path <-'C:/Users/dakot/Documents/Trees/Data Cleaning//Sheets_BONAP_Native_Assignments'


setwd(home_dir)

BONAP_ALL <- read.csv("BONAP_native_taxa_uniquebinomials.csv")

states <- read.csv("State_Abbreviations.csv")

non_matching_names <-read.csv('Non_Matches_Taxize.csv')


setwd(dir_rawsheets)
all_files<-list.files(pattern = ".csv$")
output <- matrix(ncol=4, nrow=length(all_files))

for (i in 1:length(all_files)) {
  # read data for that city
  data <- read.csv(all_files[i])
  # get cityname
  mycity <- strsplit(all_files[i], '_')[[1]][1]
  # get statename
  mystate <- data$state[1]
  # filter BONAP native plants to be our state
  BONAP<-BONAP_ALL%>%filter(state==mystate)
  # check whether each tree is native to the state
  # label in "native" column
  if ("scientific_name" %in% colnames(data)) {
    data$native=data$scientific_name %in% BONAP$scientific_name
    ## label cases of no knowledge
    data<-data %>%
      mutate(native = ifelse(scientific_name == "", "no_info", native))%>%
      mutate(native = ifelse(genus_only == "genus_only", "no_info", native))%>%
      mutate(native = ifelse(nonmatch == "TRUE", "no_info", native))
    ## write CSV
    write.csv(data,paste(path,'/',mycity,'_edited_taxize_common_native.csv',sep=''),
                         row.names=FALSE, fileEncoding = "UTF-8")
    ## now store variables
    output[i,1]=mycity
    output[i,2]=mystate
    # not native
    trimmeddata<-data%>%
      filter(.,native!='no_info')
    output[i,3]=length(trimmeddata$native[trimmeddata$native==FALSE])
    # native
    output[i,4]=length(trimmeddata$native[trimmeddata$native==TRUE])
  }
  # save new CSV
  if (!("scientific_name" %in% colnames(data))) {
    print(allfiles[i],"has no scientific names")
  }
  # get proportion native and save to results
}
beep()



output <- data.frame(output)
colnames(output)<-c('city','state','non_native','native')
output$native<-as.numeric(output$native)
output$non_native<-as.numeric(output$non_native)

output<-output%>%
  filter(native!='NA')%>%
  mutate(.,percent_native=native/(native+non_native))
output
#write.csv(output,"delete_soon.csv")


# ####################################
# ##
# ## MAP
# ##
# ####################################
# 
# dir <- 'C:/Users/dakot/Documents/Trees Collaboration/Analysis'
# setwd(dir)
# 
# city_coords<-read.csv("1000-largest-us-cities-by-population-with-geographic-coordinates.csv")
# head(city_coords)
# 
# 
# native_final<-read.csv('Native_Plotting.csv')
# native_final
# 
# ## merge actual data with that city data
# to_plot<-native_final%>%
#   left_join(.,city_coords,by=c('city','state'))%>%
#   filter(.,state!='Hawaii')%>%
#   filter(.,native!='NA')%>%
#   filter(.,Lat!='NA')
# 
# #write.csv(output,"delete.csv")
# 
# #load us map data
# all_states <- map_data("state")
# 
# #plot all states with ggplot
# p <- ggplot()
# 
# ## plot
# p <- p + geom_polygon( data=all_states, aes(x=long, y=lat, group = group),fill="grey95",colour="grey75" )
# p <- p + geom_point( data=to_plot, aes(x=Long, y=Lat, color = percent_native),size=3) + 
#   scale_size(name="Population")+theme_classic()+
#   scale_color_gradient(low = "black", high = "green")+
#   labs(color = "Percent Native",title="Native Trees in US Cities")+
#   theme(plot.title = element_text(hjust = 0.5))
# 
# ## remove axes
# p+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
#          axis.text.y=element_blank(),axis.ticks=element_blank(),
#          axis.title.x=element_blank(),
#          axis.title.y=element_blank(),
#          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
#          panel.grid.minor=element_blank(),plot.background=element_blank())
# 
# # save
# ggsave("Percent_Native_plot.png",width=8,height=4,units="in")
# 

