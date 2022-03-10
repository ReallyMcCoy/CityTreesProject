# packages
library("dplyr")
library("ggplot2")
library("beepr")
library("stringr")
library("betareg")
library("lmtest")



# set up our directories
home_dir <- 'C:/Users/dakot/Documents/Trees/Analysis'
dir_cluster_stats_sheets <- 'C:/Users/dakot/Documents/Trees/Analysis/Cluster Stats Sheets'
sheets_dir <- 'C:/Users/dakot/Documents/Trees/Analysis/Clustered CSVs'
setwd(sheets_dir)

#############################################
##
##
##
#############################################

files<-list.files(pattern="*.csv")

cluster_scores_results<-matrix(ncol=6,nrow=length(files))
colnames(cluster_scores_results)<-c("city","median","mean","max","min","number_clusters")

# start at 47
for (k in 47:length(files)) {
  ## save city name
  mycity_name<-strsplit(files[k],"_")[[1]][1]
  
  # get data
  data<-read.csv(files[k])
  
  # get cluster statistics
  cluster_stats<-data%>%
    dplyr::select(cluster,scientific_name)%>%
    filter(cluster!=-1)%>%
    filter(!(is.na(cluster)))%>%
    group_by(cluster,scientific_name)%>%
    summarize(n=n())%>%
    # get frequency values
    # note that it automatically considers sum(n) to be the sum just of our group
    mutate(frequency=n/sum(n))%>%
    # prepare for shannon_wiener
    mutate(freq_ln_freq=-1*frequency*log(frequency))%>%
    #calculate shannon_wiener
    ungroup()%>%
    group_by(cluster)%>%
    summarize(shannon_wiener=sum(freq_ln_freq),trees=sum(n),species=n_distinct(scientific_name))%>%
    # calculate effective species
    mutate(effective_species=exp(shannon_wiener))%>%
    mutate(median_effective_species=NA)%>%
    mutate(median_shannon_wiener=NA)%>%
    mutate(median_species_count=NA)
  
  
  
  
  for (j in 1:nrow(cluster_stats)){
  
    # randomly sample 500 of each cluster size and calculate the same values
    cluster_size<-cluster_stats$trees[j]
    
    results<-matrix(nrow=100,ncol=4)
    colnames(results)<-c("shannon_wiener","trees","species_count","effective_species")
    
    for (i in 1:100){
      stats<-sample_n(data, cluster_size)%>%
        dplyr::select(scientific_name)%>%
        group_by(scientific_name)%>%
        summarize(n=n())%>%
        # get frequency values
        # note that it automatically considers sum(n) to be the sum just of our group
        mutate(frequency=n/sum(n))%>%
        # prepare for shannon_wiener
        mutate(freq_ln_freq=-1*frequency*log(frequency))%>%
        #calculate shannon_wiener
        ungroup()%>%
        summarize(shannon_wiener=sum(freq_ln_freq),trees=sum(n),species=n_distinct(scientific_name))%>%
        # calculate effective species
        mutate(effective_species=exp(shannon_wiener))
      # save all results
      results[i,"shannon_wiener"]<-stats$shannon_wiener
      results[i,"trees"]<-stats$trees
      results[i,"species_count"]<-stats$species
      results[i,"effective_species"]<-stats$effective_species
    }
    
    cluster_stats$median_effective_species[j] <-median(results[,'effective_species'])
    cluster_stats$median_shannon_wiener[j] <-median(results[,'shannon_wiener'])
    cluster_stats$median_species_count[j] <-median(results[,'species_count'])
  }
  
  beep()
  head(cluster_stats)
  
  cluster_stats_full<-cluster_stats%>%
    mutate(difference=(median_effective_species -effective_species )/median_effective_species)
  
  write.csv(cluster_stats_full,paste(dir_cluster_stats_sheets,
                                     '/',mycity_name,'_clusterstats_',Sys.Date(),'.csv',
                                     sep=''),
            row.names=FALSE, fileEncoding = "UTF-8")
  
  cluster_city_summary<-cluster_stats_full%>%
    summarize(median=median(difference),max=max(difference),min=min(difference),mean=mean(difference),number_clusters=n())
  
  
  
  cluster_scores_results[k,'city']<-mycity_name
  cluster_scores_results[k,'median']<-cluster_city_summary$median
  cluster_scores_results[k,'mean']<-cluster_city_summary$mean
  cluster_scores_results[k,'max']<-cluster_city_summary$max
  cluster_scores_results[k,'min']<-cluster_city_summary$min
  cluster_scores_results[k,'number_clusters']<-cluster_city_summary$number_clusters
  
}

beep()
head(cluster_scores_results)


##################
##
## get the format we want
##
####################

setwd(dir_cluster_stats_sheets)

stats_files<-list.files(pattern="*.csv")

final_stats<-matrix(ncol=8,nrow=length(stats_files))
colnames(final_stats)<-c("filename_city","median","mean","max","min","IQR_1","IQR_2","number_clusters")

std <- function(x) sd(x)/sqrt(length(x))

for (h in 1:length(stats_files)){

mycity_name<-strsplit(stats_files[h],"_")[[1]][1]

# get ratio of observed / expected
ratio<-(read.csv(stats_files[h])%>%mutate(ratio=(effective_species/median_effective_species)))$ratio
final_stats[h,'min']<-summary(ratio)[[1]]
final_stats[h,'max']<-summary(ratio)[[6]]
final_stats[h,'IQR_1']<-summary(ratio)[[2]]
final_stats[h,'median']<-summary(ratio)[[3]]
final_stats[h,'mean']<-summary(ratio)[[4]]
final_stats[h,'IQR_2']<-summary(ratio)[[5]]
final_stats[h,'number_clusters']<-length(ratio)
final_stats[h,'filename_city']<- mycity_name
}

final_stats<-as.data.frame(final_stats)%>%arrange(median)
final_stats[,c(2:8)]<-lapply(final_stats[,c(2:8)],as.numeric)

write.csv(final_stats,paste0(home_dir,"/clustering_results_final.csv",sep=""),
          row.names=FALSE, fileEncoding = "UTF-8")

# p<- ggplot(data=final_stats,aes(x=reorder(city,median),y=median*100)) 
# 
# p + geom_point()+
#   #geom_line()+
#   geom_errorbar(aes(ymin=IQR_1*100, ymax=IQR_2*100), width=.1)+
#   theme_classic()+
#   xlab("City") + ylab("Observed/Expected\nEffective Species Per Cluster") +
#   theme(axis.text.x=element_text(size=8,angle=45,hjust=1),
#         axis.text.y=element_text(size=10),
#         axis.title=element_text(size=10))+
#   coord_cartesian(ylim=c(0,130))+
#   geom_abline(slope=0,intercept=100,linetype="longdash", color = "purple", size=0.5)
# 
# setwd(home_dir)
# ggsave("Clustering_By_Species.pdf", dpi=600,height=3,width=7,units="in")
