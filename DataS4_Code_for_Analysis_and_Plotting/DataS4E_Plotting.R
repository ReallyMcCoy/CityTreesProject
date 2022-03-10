# load packages
library("tidyr")
library("dplyr")
library("ggplot2")
library("maps")
library("raster")
library("mapdata")
library("beepr")
library("ggrepel")
library("colorspace")
library("usmap")
# library("geosphere")

# set up our directories
home_dir <- 'C:/Users/dakot/Dropbox/PC/Documents/Trees/Analysis'
dir_rawsheets <- 'C:/Users/dakot/Dropbox/PC/Documents/Trees/Analysis/Final Spreadsheets'
dir_figures <- 'C:/Users/dakot/Dropbox/PC/Documents/Trees/Figures'
dir_figure_diversity <- 'C:/Users/dakot/Dropbox/PC/Documents/Trees/Figures/DiversityFigure'
dir_figure_native <- 'C:/Users/dakot/Dropbox/PC/Documents/Trees/Figures/NativeFigure'
dir_cluster_stats_sheets <- 'C:/Users/dakot/Dropbox/PC/Documents/Trees/Analysis/Cluster Stats Sheets'
dir_figure_clustering <- 'C:/Users/dakot/Dropbox/PC/Documents/Trees/Figures/ClusteringFigure'
dir_figure_comparisons <- 'C:/Users/dakot/Dropbox/PC/Documents/Trees/Figures/ComparisonsFigure'
dir_figure_parkurban <- 'C:/Users/dakot/Dropbox/PC/Documents/Trees/Figures/ParkUrbanFigure'

setwd(home_dir)

# read basic files
city_info<-read.csv("city_information.csv")
alldata_social<-read.csv("City_Data_Diversity_Enviroment_Social.csv")
families <-read.csv("families_most_common_species.csv")
env_PCAscores<-read.csv("Environmental_PCA_Scores.csv")
env_PCAload<-read.csv("Environmental_PCA_Loadings.csv")
env_PCAvar<-read.csv("Environmental_PCA_ProportionVariance.csv")
final_cluster_stats<-read.csv("clustering_results_final.csv")
comparison_chisq<-read.csv("Chisq_comparisons_speciescomposition.csv")
park_vs_urban<-read.csv("Parks_vs_Urban_Areas_Biodiversity_Native.csv")

# save colors
native_low_color<-"#fff7bc"
native_high_color<-"#d95f0e"
effectivespecies_low_color<-"#e0ecf4"
effectivespecies_high_color<-"#8856a7"

#load us map data
all_states <- map_data("state")

all_data<-merge(env_PCAscores%>%dplyr::select(filename_city,Comp.1,Comp.2),alldata_social)

head(all_data)



#############################################################################
#############################################################################
#############################################################################
#############################################################################
###
### Plotting
###
#############################################################################
#############################################################################
#############################################################################
#############################################################################

##################################################
##
## A) Number of trees per city on a map
##
##################################################

############# main map
dat_no_Hawaii<-alldata_social%>%filter(state!="Hawaii")

ggplot(data=dat_no_Hawaii,
       aes(x=Long,y=Lat)) +  geom_polygon( data=all_states, aes(x=long, y=lat, group = group),
                                           fill="white",colour="grey80" )  +
  coord_equal() +
  xlab("Longitude") + ylab("Latitude")+
  geom_point(aes(color = log(number_trees,10)),
             shape=19,
             size=2,alpha=0.8)+
  geom_point(aes(x=Long,y=Lat),
             shape = 1,size = 2,alpha=0.25,colour = "black")+
  # geom_text_repel(aes(label=filename_city),
  #                 min.segment.length = 0.5,
  #                 size=3) +
  scale_color_gradient(low = "white",
                       high = "grey25",
                       name="Log(number trees)")+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.background = element_blank())

ggsave(paste0(dir_figures,"/OverviewFigure/Overview_USA_NumberTrees.pdf",sep=""),
       device=cairo_pdf,height=6,width=7,units="in")


## california inset
dat_only_California<-alldata_social%>%filter(state=="California")

ggplot(data=dat_only_California,
       aes(x=Long,y=Lat)) +  geom_polygon( data=all_states%>%
                                             filter(region=="california"), aes(x=long, y=lat, group = group),
                                           fill="white",colour="grey80" )  +
  coord_equal() +
  xlab("Longitude") + ylab("Latitude")+
  geom_point(aes(color = log(number_trees,10)),
             shape=19,
             size=2,alpha=0.8)+
  geom_point(aes(x=Long,y=Lat),
             shape = 1,size = 2,alpha=0.25,colour = "black")+
  scale_color_gradient(low = "white",
                       high = "grey25",
                       # force colorbar to be the same
                       limits=c(log(min(dat_no_Hawaii$number_trees),10),
                                log(max(dat_no_Hawaii$number_trees),10)),
                       name="log(number trees)")+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.background = element_blank())

ggsave(paste0(dir_figures,"/OverviewFigure/Overview_USA_NumberTrees_California.pdf",sep=""),
       device=cairo_pdf,height=6,width=7,units="in")

## Hawaii Inset
Hawaii_dat<-alldata_social%>%
  filter(state=="Hawaii")

transformed_data <- usmap_transform(data.frame(cbind(Hawaii_dat["Long"],Hawaii_dat["Lat"])))

Hawaii_dat_plot<-merge(Hawaii_dat,transformed_data)


plot_usmap(include = c("HI"),color="grey80")+
  geom_point(data = Hawaii_dat_plot, 
             aes(x = Long.1, y = Lat.1, color=log(number_trees,10)),
             size=2)+
  geom_point(data = Hawaii_dat_plot, 
             aes(x = Long.1, y = Lat.1),
             shape=1,size=2,alpha=0.25,color="black")+
  scale_color_gradient(low = "white",
                       high = "grey25",
                       # force colorbar to be the same
                       limits=c(log(min(dat_no_Hawaii$number_trees),10),
                                log(max(dat_no_Hawaii$number_trees),10)),
                       name="log(number trees)")+
  theme(legend.position="none")

ggsave(paste0(dir_figures,"/OverviewFigure/Overview_USA_NumberTrees_Hawaii.pdf",sep=""),
       device=cairo_pdf,height=0.85,width=0.85,units="in")


################################################
###
### Overview of Dataset
###
################################################

dat_no_Hawaii<-alldata_social%>%filter(state!="Hawaii")

ggplot(data=dat_no_Hawaii,
       aes(x=Long,y=Lat)) +  geom_polygon( data=all_states, aes(x=long, y=lat, group = group),
                                           fill="white",colour="grey80" )  +
  coord_equal() +
  xlab("Longitude") + ylab("Latitude")+
  geom_point(aes(color = effective_species),
             shape=19,
             size=2,alpha=0.8)+
  geom_point(aes(x=Long,y=Lat),
             shape = 1,size = 2,alpha=0.25,colour = "black")+
  # geom_text_repel(aes(label=filename_city),
  #                 min.segment.length = 0.5,
  #                 size=3) +
  scale_color_gradient(low = effectivespecies_low_color,
                       high = effectivespecies_high_color,
                       name="effective species")+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.background = element_blank())

dat_only_California<-alldata_social%>%filter(state=="California")

ggplot(data=dat_only_California,
       aes(x=Long,y=Lat)) +  geom_polygon( data=all_states%>%
                                             filter(region=="california"), aes(x=long, y=lat, group = group),
                                           fill="white",colour="grey80" )  +
  coord_equal() +
  xlab("Longitude") + ylab("Latitude")+
  geom_point(aes(color = effective_species),
             shape=19,
             size=2,alpha=0.8)+
  geom_point(aes(x=Long,y=Lat),
             shape = 1,size = 2,alpha=0.25,colour = "black")+
  scale_color_gradient(low = effectivespecies_low_color,
                       high = effectivespecies_high_color,
                       # force colorbar to be the same
                       limits=c(min(dat_no_Hawaii$effective_species),
                                max(dat_no_Hawaii$effective_species)),
                       name="effective species")+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.background = element_blank())

# setwd(paste(dir_figures,"/OverviewFigure",sep=''))
ggsave(paste0(dir_figures,"/OverviewFigure/Overview_USA_Number_Trees.pdf",sep=""),
       device=cairo_pdf,height=6,width=7,units="in")

# get california inset
dat_only_California<-alldata_social%>%filter(state=="California")

ggplot(data=dat_only_California,
       aes(x=Long,y=Lat)) +  geom_polygon( data=all_states%>%
                                             filter(region=="california"), aes(x=long, y=lat, group = group),
                                           fill="white",colour="grey80" )  +
  coord_equal() +
  xlab("Longitude") + ylab("Latitude")+
  geom_point(aes(color = effective_species),
             shape=19,
             size=2,alpha=0.8)+
  geom_point(aes(x=Long,y=Lat),
             shape = 1,size = 2,alpha=0.25,colour = "black")+
  scale_color_gradient(low = effectivespecies_low_color,
                       high = effectivespecies_high_color,
                       # force colorbar to be the same
                       limits=c(min(dat_no_Hawaii$effective_species),
                                max(dat_no_Hawaii$effective_species)),
                                name="effective species")+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.background = element_blank())

# setwd(paste(dir_figures,"/OverviewFigure",sep=''))
# ggsave("Overview_California.pdf",device=cairo_pdf,height=8,width=8,units="in")
ggsave(paste0(dir_figures,"/OverviewFigure/Overview_California.pdf",sep=""),
       device=cairo_pdf,height=8,width=8,units="in")

## get more breaks on the legend

#https://stackoverflow.com/questions/47335132/how-can-i-force-ggplot-to-show-more-levels-on-the-legend


## Hawaii


Hawaii_dat<-alldata_social%>%
  filter(state=="Hawaii")

#df<-data.frame(cbind(Hawaii_dat["Long"],Hawaii_dat["Lat"]))

transformed_data <- usmap_transform(data.frame(cbind(Hawaii_dat["Long"],Hawaii_dat["Lat"])))

Hawaii_dat_plot<-merge(Hawaii_dat,transformed_data)


plot_usmap(include = c("HI"),color="grey80")+
  geom_point(data = Hawaii_dat_plot, 
             aes(x = Long.1, y = Lat.1, color=effective_species),
             size=2)+
  geom_point(data = Hawaii_dat_plot, 
             aes(x = Long.1, y = Lat.1),
             shape=1,size=2,alpha=0.25,color="black")+
  scale_color_gradient(low = effectivespecies_low_color,
                       high = effectivespecies_high_color,
                       # force colorbar to be the same
                       limits=c(min(dat_no_Hawaii$effective_species),
                                max(dat_no_Hawaii$effective_species)),
                       name="effective species")+
  theme(legend.position="none")


# setwd(paste(dir_figures,"/OverviewFigure",sep=''))
# ggsave("Overview_Hawaii.pdf",device=cairo_pdf,height=0.85,width=0.85,units="in")
ggsave(paste0(dir_figures,"/OverviewFigure/Overview_Hawaii.pdf",sep=""),
       device=cairo_pdf,height=0.85,width=0.85,units="in")


################################################
###
### Example City PLot for OVerview Fig
###
################################################
setwd(dir_rawsheets)
files<-list.files(pattern="Pitt*")
files[1]
mycity_name<-strsplit(files[1],"_")[[1]][1]
mycity<-read.csv(files[1])%>%
    ### need to label genus only rows by adding "sp."
    mutate(wordcount = stringr::str_count(scientific_name, ' ') + 1)%>%
    mutate(scientific_name = ifelse(wordcount == 1, 
                                    ## TRUE
                                    ## but do not paste for blanks
                                    ifelse(scientific_name=="",
                                           "",
                                           paste(scientific_name,"sp.")),
                                    ## FALSE
                                    scientific_name))
  
### get 10 most common species
species_table<-mycity%>%
    filter(scientific_name!="")%>%
    dplyr::select(scientific_name)%>%
    group_by(scientific_name)%>%
    summarize(n=n())%>%
    arrange(-n) %>% 
    slice_head(n=10)%>%
    ### add list of specified colors
    mutate(colors=c("#5e4fa2", "#3288bd","#66c2a5","#abdda4","#e6f598",
                    "#fee08b","#fdae61","#f46d43","#d53e4f","#9e0142"))%>%
    add_row(.,scientific_name="Other",
            ##  count number other species
            n= nrow(mycity%>%filter(!(scientific_name %in% mycity$scientific_name))),
            colors="black")
  
### set up colors
species_colors <- as.character(species_table$colors)
names(species_colors) <- species_table$scientific_name
  
### set up dataframes to plot
plot_mycity<-mycity%>%
    mutate(scientific_name = ifelse(scientific_name %in% species_table$scientific_name,
                                    scientific_name,
                                    "Other"))
  
plot_mycity$scientific_name <- factor(plot_mycity$scientific_name, 
                                        levels = species_table$scientific_name)
  
## for lat long city plots
plot_mycity_city<-plot_mycity%>%
    filter(latitude_coordinate!=0)%>%
    filter(longitude_coordinate!=0)%>%
    filter(!(abs(latitude_coordinate - median(latitude_coordinate)) > 4*sd(latitude_coordinate))) %>%
    filter(!(abs(longitude_coordinate - median(longitude_coordinate)) > 4*sd(longitude_coordinate)))
  
## for city plots
ggplot(data=plot_mycity_city)+
      geom_point(aes(x=longitude_coordinate,y=latitude_coordinate),color="black",
                 size=0.1)+
      geom_point(data=plot_mycity_city%>%
                   filter(scientific_name!="Other"),
                 aes(x=longitude_coordinate,y=latitude_coordinate,color=scientific_name),
                 size=0.1)+
      scale_color_manual(values=species_colors)+
      theme_classic()+
      theme(axis.line=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            panel.background=element_blank(),
            panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            plot.background=element_blank(),
            legend.position = "none",
            legend.background = element_blank())+
      ## add map coordinates
      coord_quickmap()

setwd(paste(dir_figures,"/OverviewFigure",sep=''))
ggsave("Cityplot_Pittsburgh_Overview.pdf",device=cairo_pdf,height=3.5,width=3.5,units="in")

################################################
###
### Zoom In ON Particular Street Area
###
################################################
ggplot(data=plot_mycity_city)+
  geom_point(aes(x=longitude_coordinate,y=latitude_coordinate),color="black",
             size=0.05)+
  geom_point(data=plot_mycity_city%>%
               filter(scientific_name!="Other"),
             aes(x=longitude_coordinate,y=latitude_coordinate,color=scientific_name),
             size=0.05)+
  scale_color_manual(values=species_colors)+
  theme_classic()+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        legend.position = "none",
        legend.background = element_blank())+
  ## add map coordinates
  coord_quickmap(
    xlim=c(-80.03,-80.005),
    ylim=c(40.449,40.47)
                 )

setwd(paste(dir_figures,"/OverviewFigure",sep=''))
ggsave("Cityplot_Pittsburgh_Zoom.pdf",device=cairo_pdf,height=4,width=4,units="in")

ggplot(data=plot_mycity_city)+
  geom_point(aes(x=longitude_coordinate,y=latitude_coordinate),color="black",
             size=0.05)+
  geom_point(data=plot_mycity_city%>%
               filter(scientific_name!="Other"),
             aes(x=longitude_coordinate,y=latitude_coordinate,color=scientific_name),
             size=0.05)+
  scale_color_manual(values=species_colors)+
  theme_classic()+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        legend.position = "none",
        legend.background = element_blank())+
coord_quickmap(
  xlim=c(-79.94,-79.955),
  ylim=c(40.45,40.475)
)

setwd(paste(dir_figures,"/OverviewFigure",sep=''))
ggsave("Cityplot_Pittsburgh_Zoom2.pdf",device=cairo_pdf,height=4,width=4,units="in")


### make bar plot
  
number_other_trees<-nrow(plot_mycity%>%filter(scientific_name=="Other"))
  
ggplot(data=plot_mycity%>%
           filter(scientific_name!="Other"))+
    geom_bar(aes(x=scientific_name,fill=scientific_name))+
    # geom_bar(aes(x=forcats::fct_infreq(scientific_name),fill=scientific_name))+
    scale_fill_manual(values=species_colors[names(species_colors) != "Other"],
                      name="Species")+
    theme_classic()+ theme(text = element_text(size = 12),
                           # axis.text.x = element_blank(),
                           axis.text.x = element_text(size=12),
                           axis.title.x = element_text(size=12),
                           axis.text.y  = element_text(size = 12,
                                                    face="italic"),
                           axis.title.y = element_blank(),
                           legend.position = "none")+
    labs(x="Most Common Species", 
         y="Count")+
    annotate("text",  x=Inf, y = Inf, 
             label = paste0("Other species:\n", format(number_other_trees, nsmall=1, big.mark=","), " trees"), 
             vjust=1.3, hjust=1,size=3.5)+
    coord_flip()+
   scale_y_continuous(breaks=c(0,1500,3000))

setwd(paste(dir_figures,"/OverviewFigure",sep=''))
ggsave("Cityplot_Pittsburgh_CommonSpecies.pdf",device=cairo_pdf,height=3.25,width=3.1,units="in")





#################################################################################
#################################################################################
#################################################################################
#################################################################################
###
### Percent Native versus Environment
###
#################################################################################
#################################################################################
#################################################################################
#################################################################################

## get environmental data
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

########################################################3
##
# Crop to continental US
##
########################################################

cont_USA_long<-c(-124.848974,-66.885444)
cont_USA_lat<-c(49.384358, 24.396308)

buff <- 1   #a buffer of one degree around the raster

xmin <- cont_USA_long[1] - buff
xmax <- cont_USA_long[2] + buff
ymin <- cont_USA_lat[2] - buff
ymax <- cont_USA_lat[1] + buff

e <- extent(xmin, xmax, ymin, ymax)

envcrop <- crop(env, e)
plot(envcrop[[1]], main = "Annual Mean Temperature")
map(
  'state',
  xlim = c(xmin, xmax),
  ylim = c(ymin, ymax),
  fill = F,
  add = T
)

#######################################################3
##
# Crop to Hawaii
##
########################################################

Hawaii_long<-c(-178.334698,-154.806773)
Hawaii_lat<-c(28.402123, 18.910361)

buff <- 1   #a buffer of one degree around the raster

xmin <- Hawaii_long[1] - buff
xmax <- Hawaii_long[2] + buff
ymin <- Hawaii_lat[2] - buff
ymax <- Hawaii_lat[1] + buff

e <- extent(xmin, xmax, ymin, ymax)

envcrop_Hawaii <- crop(env, e)
plot(envcrop_Hawaii[[1]], main = "Annual Mean Temperature")

# get the background USA environmental variable of interest
df <- data.frame( rasterToPoints( envcrop[[15]] )) %>%
  rename(.,Long=x,Lat=y)
head(df)

# check which variable we are testing
colnames(df)[3]

# plot
ggplot( df ) + geom_tile(aes(x=Long,y=Lat,fill=Precip_Seasonality))+
  #scale_fill_gradientn( colors=c('#018571','#80cdc1','#f5f5f5','#dfc27d','#a6611a'))+
  scale_fill_gradientn( colors=c('grey99','grey75','grey50','grey25','black'),
                        name="seasonality of\nprecipitation")+
  coord_equal() + xlab("Longitude") + ylab("Latitude") +
  geom_polygon( data=all_states, aes(x=long, y=lat, group = group),
                fill=NA,colour="white" )+
  geom_point(data=alldata_social%>%filter(state!="Hawaii"),aes(x=Long,y=Lat,color=percent_native_trees),
             size=2)+
  # add black borders to the points
  geom_point(data=alldata_social%>%filter(state!="Hawaii"),aes(x=Long,y=Lat),
             shape = 1,size = 2,alpha=0.25,colour = "black")+
  scale_color_gradient(low = native_low_color,
                       high = native_high_color,
                       name="percent\nnative trees")+
  # scale_colour_gradientn(colours = colorspace::diverge_hcl(4,
  #                                                          palette = "Purple-Green"),
  #                        name="percent\nnative trees")+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.background = element_blank())

ggsave(paste0(dir_figure_native, "/Precip_Seasonality_vs_Percent_Native.pdf",sep=""),
       useDingbats=FALSE,height=4,width=5,units="in")

##################################
##
## Good plot of Bioclim PCA vs. percent native
##
##################################

bioclim_PCA<-merge(rename(env_PCAscores,"filename_city"="X"),alldata_social)
env_PCAvar[1,2]

ggplot(data=all_data)+geom_point(aes(x=Comp.1,y=Comp.2,color=percent_native_trees*100),size=2)+
  scale_colour_gradient(low = native_low_color, 
                        high = native_high_color,
                        name="Percent\nNative\nTrees")+
  geom_point(aes(x=Comp.1,y=Comp.2,color=percent_native_trees*100),
             shape = 1,size = 2,
             alpha=0.25,
             #stroke=0.5,
             colour = "black")+
  theme_classic()+ theme(text = element_text(size = 10),
                         axis.text = element_text(size = 10),
                         legend.text=element_text(size=10),
                         legend.title=element_text(size=10))+
  # stat_ellipse(aes(x=Comp.1,y=Comp.2,color = percent_native_trees > 0.25),level=0.85)+
  labs(x=paste("PC1 (",round(env_PCAvar[1,2]*100,1),"% var.)",sep=""), 
       y=paste("PC2 ( ",round(env_PCAvar[2,2]*100,1),"% var.)",sep=""))+
  theme(legend.position = "none")

ggsave(paste0(dir_figure_native,"/Environmental_PCA.pdf",sep=""), 
       dpi=600,useDingbats=FALSE,width=2.75,height=2.75,units="in")


##################################
##
## biodiversity of native trees vs. biodiversity overall
##
##################################
ggplot(data=alldata_social)+
  geom_smooth(aes(y=native_effective_species,x=effective_species),method="lm",
              color="grey55",fill="grey85")+
  geom_point(aes(y=native_effective_species,x=effective_species,color=percent_native_trees*100),size=1.5)+
  scale_colour_gradient(low = native_low_color, 
                        high = native_high_color,
                        name="Percent\nNative\nTrees")+

  geom_point(aes(y=native_effective_species,x=effective_species,color=percent_native_trees*100),
             shape = 1,size = 1.5,
             alpha=0.25,
             #stroke=0.5,
             colour = "black")+
  theme_classic()+ theme(text = element_text(size = 10),
                         axis.text = element_text(size = 10),
                         legend.text=element_text(size=10),
                         legend.title=element_text(size=10))+
  # stat_ellipse(aes(x=Comp.1,y=Comp.2,color = percent_native_trees > 0.25),level=0.85)+
  labs(y="biodiversity of native trees\n(effective species)", 
       x="biodiversity of all trees\n(effective species)")+
  theme(legend.position = "none")

ggsave(paste0(dir_figure_native,"/BiodiversityAll_vs_BiodiversityNative.pdf",sep=""), dpi=600,useDingbats=FALSE,
       width=2.5,height=2.5,units="in")



##################################
##
## native congeners for nonnative trees
##
##################################
ggplot(data=alldata_social)+
  geom_point(aes(y=percent_native_trees*100,x=nonnative_trees_PERCENTWITH_native_congeners*100,color=percent_native_trees*100),size=1.5)+
  scale_colour_gradient(low = native_low_color, 
                        high = native_high_color,
                        name="Percent\nNative\nTrees")+
  geom_point(aes(y=percent_native_trees*100,x=nonnative_trees_PERCENTWITH_native_congeners*100,color=percent_native_trees*100),
             shape = 1,size = 1.5,
             alpha=0.25,
             #stroke=0.5,
             colour = "black")+
  theme_classic()+ theme(text = element_text(size = 10),
                         axis.text = element_text(size = 10),
                         legend.text=element_text(size=10),
                         legend.title=element_text(size=10))+
  # stat_ellipse(aes(x=Comp.1,y=Comp.2,color = percent_native_trees > 0.25),level=0.85)+
  labs(y="percent native trees", 
       x="percent non-native trees\nwith a native congener")+
  theme(legend.position = "none")

ggsave(paste0(dir_figure_native,"/PercentNative_vs_NonNativeCongeners.pdf",sep=""), dpi=600,useDingbats=FALSE,
       width=2.5,height=2.5,units="in")

## https://geocompr.github.io/post/2019/ggplot2-inset-maps/

#################################################################################
#################################################################################
#################################################################################
#################################################################################
###
### Diversity Figure
###
#################################################################################
#################################################################################
#################################################################################
#################################################################################


################################################
###
### (A) Metric of Effective Specives vs. max abundance
###
################################################
ggplot(data=alldata_social)+
  geom_point(aes(effective_species,
                 max_abundance_most_common_species*100,
                 color=effective_species),
             size=2)+
  geom_hline(yintercept=10,linetype="longdash", size=0.5,color="black")+
  scale_color_gradient(low = effectivespecies_low_color,
                       high = effectivespecies_high_color,
                       name="effective\nspecies")+
  theme_classic()+ 
  theme(text = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size=10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size=10),
        legend.text=element_text(size=10),
        legend.title=element_text(size=10))+
  labs(x="effective species", 
       y="abundance of most\ncommon species (%)")+
  theme(legend.position="none")

setwd(dir_figure_diversity)
ggsave("Effective_Species_vs_Max_Abundance.pdf",height=2.5,width=3,units="in")

################################################
###
### (A) Max abundance species vs max abundance genus
###
################################################
ggplot(data=alldata_social)+
  geom_point(aes(max_abundance_most_common_genus*100,
                 max_abundance_most_common_species*100,
                 color=effective_species),
             size=2)+
  scale_color_gradient(low = effectivespecies_low_color, 
                       high = effectivespecies_high_color,
                       name="effective\nspecies")+
  geom_segment(x=0,y=10,aes(xend=20, yend=10),
               linetype="longdash", size=0.5,color="black") +
  geom_segment(x=20,y=0,aes(xend=20, yend=10),
               linetype="longdash", size=0.5,color="black") +
  theme_classic()+ 
  theme(text = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size=10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size=10),
        legend.text=element_text(size=10),
        legend.title=element_text(size=10))+
  labs(x="abundance of most common genus (%)", 
       y="abundance of most\ncommon species (%)")+
  theme(legend.position="right")

setwd(dir_figure_diversity)
ggsave("Max_Abundance_Species_vs_Max_Abundance_Genus.pdf",height=2.5,width=4,units="in")

################################################
###
### (A) effective species vs. temp seasonality
###
################################################
ggplot(data=all_data)+
  geom_smooth(aes(Temp_Seasonality,effective_species),method="lm",
              color="grey55",fill="grey85")+
  geom_point(aes(Temp_Seasonality,effective_species),
                 color="black",
             size=1)+
  # scale_color_gradient(low = effectivespecies_low_color, 
  #                      high = effectivespecies_high_color,
  #                      name="effective\nspecies")+
  theme_classic()+ 
  theme(text = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size=10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size=10),
        legend.text=element_text(size=10),
        legend.title=element_text(size=10))+
  labs(x="temperature seasonality", 
       y="effective species")+
  theme(legend.position="none")
setwd(dir_figure_diversity)
ggsave("Effective_Species_vs_Temp_Seasonality.pdf",height=2.5,width=2.5,units="in")


################################################
###
### histogram of effective species counts
###
################################################
ggplot(data=alldata_social)+
  geom_histogram(aes(x=effective_species),
                 binwidth=4,fill="grey55")+
  coord_cartesian(xlim=c(0,75))+
  theme_classic()+ 
  theme(text = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size=10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size=10),
        legend.text=element_text(size=10),
        legend.title=element_text(size=10))+
  labs(x="effective species")+
  theme(legend.position="none")

setwd(dir_figure_diversity)
ggsave("Effective_species_histogram.pdf",height=2.5,width=2.5,units="in")


################################################
###
### (C) Map of most common genus in each city 
###
################################################

genera<-dat_no_Hawaii%>%
  dplyr::select(most_common_genus,max_abundance_most_common_genus)%>%
  group_by(most_common_genus)%>%
  summarize(n=n())%>%
  filter(n>1)
genera_list<-genera$most_common_genus
genera


genera_plotting<-dat_no_Hawaii%>%
  mutate(genus_label=ifelse(most_common_genus %in% genera_list,"",most_common_genus))%>%
  mutate(genus_color=ifelse(most_common_genus %in% genera_list,most_common_genus,"other"))

genera_table<-genera_plotting%>%
  group_by(genus_color)%>%
  summarize(n=n())%>%
  mutate(colors=c("#d53e4f",
                  "#fc8d59",
                  "#de77ae",
                  "black",
                  "#99d594",
                  "#fee08b",
                  
                  "#542788",
                  "#3288bd"
                  ))


genera_colors <- as.character(genera_table$colors)
names(genera_colors)<-genera_table$genus_color
genera_colors

#### All states
ggplot(data=genera_plotting,
       aes(x=Long,y=Lat)) +  
  geom_polygon( data=all_states, aes(x=long, y=lat, group = group),
                                           fill="white",colour="grey80" )  +
  coord_equal() +
  xlab("Longitude") + ylab("Latitude")+
  geom_point(aes(color = genus_color),
             shape=19,
             size=1.5,alpha=0.8)+
  scale_color_manual(values=genera_colors)+
  # geom_text_repel(aes(label=genus_label),
  #                 min.segment.length = 0.01,
  #                 size=3,max.overlaps = 20) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.background = element_blank())

setwd(dir_figure_diversity)
# ggsave("LABELLED_Most_Common_Genus.pdf",device=cairo_pdf,height=6,width=6,units="in")
ggsave("Most_Common_Genus.pdf",device=cairo_pdf,height=6,width=6,units="in")



#### California inset

ggplot(data=genera_plotting%>%
         filter(state=="California"),
       aes(x=Long,y=Lat)) +  
  geom_polygon( data=all_states%>%
                  filter(region=="california"), aes(x=long, y=lat, group = group),
                fill="white",colour="grey80" )  +
  coord_equal() +
  xlab("Longitude") + ylab("Latitude")+
  geom_point(aes(color = genus_color),
             shape=19,
             size=1.5,alpha=0.8)+
  scale_color_manual(values=genera_colors)+
  # geom_text_repel(aes(label=genus_label),
  #                 min.segment.length = 0.01,
  #                 size=3,max.overlaps = 20) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.background = element_blank())


setwd(dir_figure_diversity)
# ggsave("California_LABELS_Most_Common_Genus.pdf",device=cairo_pdf,height=6,width=6,units="in")
ggsave("California_Most_Common_Genus.pdf",device=cairo_pdf,height=6,width=6,units="in")


#### Hawaii inset
Hawaii_dat<-alldata_social%>%
  filter(state=="Hawaii")

#df<-data.frame(cbind(Hawaii_dat["Long"],Hawaii_dat["Lat"]))

transformed_data <- usmap_transform(data.frame(cbind(Hawaii_dat["Long"],Hawaii_dat["Lat"])))

Hawaii_dat_plot<-merge(Hawaii_dat,transformed_data)


plot_usmap(include = c("HI"),color="grey80")+
  geom_point(data = Hawaii_dat_plot, 
             aes(x = Long.1, y = Lat.1), color="black",
             size=1.5)+
  theme(legend.position="none")

setwd(dir_figure_diversity)
ggsave("Hawaii_Most_Common_Genus.pdf",device=cairo_pdf,height=1,width=1,units="in")


################################################
###
### (C) Most common species
###
################################################


common_species<-alldata_social%>%
  dplyr::select(most_common_species,Lat,Long,city_pretty,filename_city)%>%
  group_by(most_common_species)%>%
  summarize(n=n())
# %>%
#   filter(n>2)

common_species_list<-common_species$most_common_species


common_species_plotting_1<-alldata_social%>%
  dplyr::select(most_common_species,Lat,Long,city_pretty,state,filename_city)%>%
  mutate(species_label=ifelse(most_common_species %in% common_species_list,"",most_common_species))%>%
  mutate(species_color=ifelse(most_common_species %in% common_species_list,most_common_species,"other"))%>%
  left_join(.,families)

family_list_plot<-common_species_plotting_1%>%
  group_by(family)%>%
  summarize(n=n())%>%
  arrange(-n)%>%
  mutate(family=ifelse(n>1,family,"Other"))%>%
  group_by(family)%>%
  summarize(n=sum(n))

common_species_plotting<-common_species_plotting_1%>%
  mutate(family=ifelse(family %in% family_list_plot$family,family,"Other"))


usa <- map_data("usa")

#### All Named families
for (i in 1:nrow(family_list_plot)) {
  ggplot(data=common_species_plotting%>%
           filter(state!="Hawaii")%>%
           filter(family==family_list_plot$family[i]),
         aes(x=Long,y=Lat)) +  
    geom_polygon( data=usa, aes(x=long, y=lat, group = group),
                  fill="white",colour="grey75" )  +
    coord_equal() +
    xlab("Longitude") + ylab("Latitude")+
    geom_point(aes(color = most_common_species),
               shape=19,
               size=2,alpha=0.5)+
    # scale_color_manual(values=genera_colors)+
    scale_color_discrete_sequential(palette = "Plasma")+
    # geom_text_repel(aes(label=most_common_species,color=most_common_species),
    #                 fontface="italic",
    #                 size=3)+
    labs(title=paste0(family_list_plot$family[i]))+
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          title=element_text(size=10),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank(),
          legend.position = "right")+
    scale_colour_brewer(palette = "Set1")
  
  
  setwd(dir_figure_diversity)
  # ggsave("LABELLED_Most_Common_Genus.pdf",device=cairo_pdf,height=6,width=6,units="in")
  ggsave(paste0(family_list_plot$family[i],"_Species_Map.pdf",sep=''),
         device=cairo_pdf,height=2,width=5,units="in")
}


1+1



#################################################################################
#################################################################################
#################################################################################
#################################################################################
###
### CLUSTERING FIGURE
###
#################################################################################
#################################################################################
#################################################################################
#################################################################################

################
## first, cluster scores
################

# merge stats with effective species
final_cluster_stats
clusterdata_2<-alldata_social%>%
  dplyr:: select(filename_city, city_pretty, 
                 effective_species,number_trees,city_age_2021,
                 tree_city_USA , tree_city_age_2021,max_abundance_most_common_species,
                 max_abundance_most_common_genus)%>%
  right_join(.,final_cluster_stats)

p<- ggplot(data=clusterdata_2,aes(x=reorder(city_pretty,median),y=median*100)) 

p + geom_point(size=1)+
  #geom_line()+
  geom_errorbar(aes(ymin=IQR_1*100, ymax=IQR_2*100), width=.1)+
  theme_classic()+
  xlab("\ncity") + ylab("observed/expected\neffective species per cluster (%)") +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_text(size=10),
        axis.title=element_text(size=10))+
  coord_cartesian(ylim=c(0,130))+
  geom_abline(slope=0,intercept=100,linetype="longdash", color = "purple", size=0.5)

ggsave(paste0(dir_figure_clustering,"/Clustering_By_Species.pdf",sep=""), dpi=600,
       height=2.4,width=4.5,units="in")


################
## second, cluster scores vs effective species counts
################

### max abundance species
p<- ggplot(data=clusterdata_2,aes(x=max_abundance_most_common_species*100,y=100*median)) 

p + geom_point(size=1)+
  theme_classic()+
  xlab("abundance of most\ncommon species (%)") + ylab("clustering score")+
  theme(axis.text.x=element_text(size=10),
      axis.text.y=element_text(size=10),
      axis.title=element_text(size=10))+
  geom_vline(xintercept=10,linetype="longdash", color = "purple", size=0.5)

ggsave(paste0(dir_figure_clustering,"/Clustering_Vs_MaxAbundanceSpecies.pdf",sep=""), dpi=600,
       height=2.6,width=2.5,units="in")

### max abundance genus
p<- ggplot(data=clusterdata_2,aes(x=max_abundance_most_common_genus*100,y=100*median)) 

p + geom_point(size=1)+
  theme_classic()+
  xlab("abundance of most\ncommon genus (%)") + ylab("clustering score")+
theme(axis.text.x=element_text(size=10),
      axis.text.y=element_text(size=10),
      axis.title=element_text(size=10))+
  geom_vline(xintercept=20,linetype="longdash", color = "purple", size=0.5)

ggsave(paste0(dir_figure_clustering,"/Clustering_Vs_MaxAbundanceGenus.pdf",sep=""), dpi=600,
       height=2.6,width=2.5,units="in")


## effective species
p<- ggplot(data=clusterdata_2,aes(x=effective_species,y=100*median)) 

p + geom_point(size=1)+
  theme_classic()+
  ylab("observed/expected\neffective species per cluster (%)")  + xlab("effective species\n")+
theme(axis.text.x=element_text(size=10),
      axis.text.y=element_text(size=10),
      axis.title=element_text(size=10))+
  coord_cartesian(ylim=c(0,130))

ggsave(paste0(dir_figure_clustering,"/Clustering_Vs_EffectiveSpecies.pdf",sep=""), dpi=600,
       height=2.6,width=2.8,units="in")


## city age
p<- ggplot(data=clusterdata_2,aes(x=city_age_2021,y=100*median)) 

p + geom_point(size=1)+
  theme_classic()+
  ylab("clustering score") + xlab("city age\n")+
  theme(axis.text.x=element_text(size=10),
        axis.text.y=element_text(size=10),
        axis.title=element_text(size=10))

ggsave(paste0(dir_figure_clustering,"/Clustering_Vs_CityAge.pdf",sep=""), dpi=600,
       height=2.6,width=2.5,units="in")




##################################################################
#################################################################################
#################################################################################
#################################################################################
#################################################################################
###
### SIMILARITY FIGURE
###
#################################################################################
#################################################################################
#################################################################################
#################################################################################

head(comparison_chisq)

# all data
ggplot(data=comparison_chisq,
       aes(y=chisq_similarity,x=enviro_similarity))+
  geom_point(color="#9ecae1",
             alpha=0.5,size=1)+
  theme_classic()+
  geom_smooth(method = "lm",color='#08519c',size=2)+
  theme(axis.text.x=element_text(size=10),
        axis.text.y=element_text(size=10),
        axis.title=element_text(size=10),
        title=element_text(size=10))+
  xlab("environmental similarity")+ylab("species community similarity")+
  # ggtitle("All Species")+
  coord_cartesian(ylim=c(0,0.9))

ggsave(paste0(dir_figure_comparisons,
              "/ChiSquareDistance_vs_Environmental_Distance.pdf",
              sep=""), 
       dpi=600,
       height=2.5,width=2.5,units="in")

# native only
ggplot(data=comparison_chisq,
       aes(y=chisq_similarity_NATIVE,x=enviro_similarity))+
  geom_point(color="#a1d99b",
             alpha=0.5,size=1)+
  theme_classic()+
  geom_smooth(method = "lm",color="#006d2c",size=1.5)+
  theme(axis.text.x=element_text(size=10),
        axis.text.y=element_text(size=10),
        axis.title=element_text(size=10),
        title=element_text(size=10))+
  xlab("environmental similarity")+ylab("species community similarity")+
  # ggtitle("Native Species")+
  coord_cartesian(ylim=c(0,0.9))+ 
  scale_x_continuous(breaks=seq(0, 1, by = .5))

ggsave(paste0(dir_figure_comparisons,
              "/ChiSquareDistance_vs_Environmental_Distance_NATIVE.pdf",
              sep=""), 
       dpi=600,
       height=2.5,width=2.5,units="in")


# non-native only
ggplot(data=comparison_chisq,
       aes(y=chisq_similarity_NONNATIVE,x=enviro_similarity))+
  geom_point(color="black",
             alpha=0.5,size=1)+
  theme_classic()+
  geom_smooth(method = "lm")+
  theme(axis.text.x=element_text(size=10),
        axis.text.y=element_text(size=10),
        axis.title=element_text(size=10),
        title=element_text(size=10))+
  xlab("environmental similarity")+ylab("species community similarity")+
  ggtitle("Non-native Species")+
  coord_cartesian(ylim=c(0,0.9))+ 
  scale_x_continuous(breaks=seq(0, 1, by = .5))

ggsave(paste0(dir_figure_comparisons,
              "/ChiSquareDistance_vs_Environmental_Distance_NONNATIVE.pdf",
              sep=""), 
       dpi=600,
       height=2.5,width=2.5,units="in")

# all with expected trendline based on native species only
ggplot(data=comparison_chisq,
       aes(y=chisq_similarity,x=enviro_similarity))+
  geom_point(color="#9ecae1",
             alpha=0.5,size=1)+
  theme_classic()+
  geom_smooth(method = "lm",color='#08519c',size=1.5)+
  theme(axis.text.x=element_text(size=10),
        axis.text.y=element_text(size=10),
        axis.title=element_text(size=10),
        title=element_text(size=10))+
  xlab("environmental similarity")+ylab("species community similarity")+
  # ggtitle("All Species")+
  coord_cartesian(ylim=c(0,0.9))+
    # add expected trendline based on native species only
    geom_smooth(aes(y=chisq_similarity_NATIVE,x=enviro_similarity), method = "lm",
                color="#006d2c",size=1.5)+ 
  scale_x_continuous(breaks=seq(0, 1, by = .5))

ggsave(paste0(dir_figure_comparisons,
              "/ChiSquareDistance_vs_Environmental_Distance_LINES.pdf",
              sep=""), 
       dpi=600,
       height=2.5,width=2.5,units="in")


## density plot of all minus native
ggplot(data=comparison_chisq)+
  geom_histogram(aes(x=all_minus_native),binwidth = 0.01,
                 fill="grey60")+
  theme_classic()+
  theme(axis.text.x=element_text(size=10),
        axis.text.y=element_text(size=10),
        axis.title=element_text(size=10),
        title=element_text(size=10))+
  # here paste in the calculated difference in means
  geom_vline(xintercept=0.06777371,linetype = "longdash")

ggsave(paste0(dir_figure_comparisons,
              "/ChiSquareDistance_vs_Environmental_Distance_ALL-MINUS-NATIVE.pdf",
              sep=""), 
       dpi=600,
       height=2.5,width=3,units="in")




#################################################################
#################################################################################
#################################################################################
#################################################################################
#################################################################################
###
###  Park versus Urban Figure
###
#################################################################################
#################################################################################
#################################################################################
#################################################################################

park_urban_plot<-park_vs_urban%>%
  mutate(diff_nat=percent_native_park-percent_native_urban)%>%
  mutate(diff_bio=effective_species_park-effective_species_urban)%>%
  mutate(diff_abun=max_abundance_most_common_species_park -
            max_abundance_most_common_species_urban)%>%
  mutate(park_more_nat=ifelse(diff_nat>0,"yes","no"))%>%
  mutate(park_more_bio=ifelse(diff_bio>0,"yes","no"))%>%
  mutate(park_lower_max_abund=ifelse(diff_abun<0,"yes","no"))
  
## for main figure, plot both distributions
ggplot(park_urban_plot) + 
  # geom_histogram(aes(x = effective_species_park,y=..density..),binwidth = 10,
  #                fill = "darkorchid3",alpha=0.5) +
  stat_density(aes(x = effective_species_park),
               # geom="line",lwd = 1.2,
               fill = "darkorchid3",
               bw=7,alpha=0.5)+
  # geom_histogram(aes(x = effective_species_urban,y=..density..),binwidth = 10,
  #                fill = "dodgerblue3",alpha=0.5) +
  stat_density(aes(x = effective_species_urban),
               # geom="line",lwd = 1.2,
               fill = "dodgerblue3",
               bw=7,alpha=0.5)+
  theme_classic()+
  xlab("effective species")+
  theme(axis.text.x=element_text(size=10),
        axis.text.y=element_text(size=10),
        axis.title=element_text(size=10),
        title=element_text(size=10))
ggsave(paste0(dir_figure_diversity,
              "/Park_vs_Urban_EffectiveSpecies.pdf",
              sep=""), 
       dpi=600,
       height=2.5,width=2.5,units="in")

## scratch work
ggplot(park_urban_plot) + 
  geom_histogram(aes(x = effective_species_park,y=..density..),binwidth = 7,
                 fill = "darkorchid3",alpha=0.5) +
  geom_histogram(aes(x = effective_species_urban,y=..density..),binwidth = 7,
                 fill = "dodgerblue3",alpha=0.5) +
  theme_classic()+
  xlab("effective species")+
  theme(axis.text.x=element_text(size=10),
        axis.text.y=element_text(size=10),
        axis.title=element_text(size=10),
        title=element_text(size=10))

### plot differences as histograms and density plots

### difference in biodiversity
ggplot(park_urban_plot, aes(x = diff_bio)) + 
  geom_histogram(aes(y=..density..),binwidth = 4,
                 fill = "grey85") +

  stat_density(geom="line",lwd = 1.2,
               colour = "darkorchid3",
               bw=3)+
  theme_classic()+
  xlab("effective species (count)\n(park trees - urban trees)")+
  theme(axis.text.x=element_text(size=10),
        axis.text.y=element_text(size=10),
        axis.title=element_text(size=10),
        title=element_text(size=10))

ggsave(paste0(dir_figure_parkurban,
              "/EffectiveSpecies_Difference.pdf",
              sep=""), 
       dpi=600,
       height=2.5,width=2.5,units="in")

### difference in percent native
ggplot(park_urban_plot, aes(x = diff_nat*100)) + 
  geom_histogram(aes(y=..density..),binwidth = 4,
                 fill = "grey85") +
  stat_density(geom="line",lwd = 1.2,
               colour = "black",
               bw=4)+
  theme_classic()+
  xlab("percent native (%)\n(park trees - urban trees)")+
  theme(axis.text.x=element_text(size=10),
        axis.text.y=element_text(size=10),
        axis.title=element_text(size=10),
        title=element_text(size=10))

ggsave(paste0(dir_figure_parkurban,
              "/PercentNative_Difference.pdf",
              sep=""), 
       dpi=600,
       height=2.5,width=2.5,units="in")

### difference in max abundance
ggplot(park_urban_plot, aes(x = diff_abun*100)) + 
  geom_histogram(aes(y=..density..),binwidth = 5,
                 fill = "grey85") +
  stat_density(geom="line",lwd = 1.2,
               colour = "darkorchid3",
               bw=5)+
  theme_classic()+
  xlab("max. species abundance (%)\n(park trees - urban trees)")+
  theme(axis.text.x=element_text(size=10),
        axis.text.y=element_text(size=10),
        axis.title=element_text(size=10),
        title=element_text(size=10))

ggsave(paste0(dir_figure_parkurban,
              "/MaxSpeciesAbundance_Difference.pdf",
              sep=""), 
       dpi=600,
       height=2.5,width=2.5,units="in")


###### 
## biodiversity comparison
######
ggplot(data=park_urban_plot%>%
         filter(filename_city!="Honolulu")%>%
         left_join(all_data%>%dplyr::select(filename_city,Lat,Long)),
       aes(x=Long,y=Lat)) +  
  geom_polygon( data=usa, aes(x=long, y=lat, group = group),
                fill="white",colour="grey75" )  +
  coord_equal() +
  xlab("Longitude") + ylab("Latitude")+
  geom_point(aes(color = park_more_nat),
             shape=19,
             size=2)+
  scale_color_manual(values=c("dodgerblue2","darkorchid3"))+
  labs(title="Percent Native")+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        title=element_text(size=10),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        legend.position = "bottom")

ggsave(paste0(dir_figure_parkurban,
              "/Native_Map.pdf",
              sep=""), 
       dpi=600,
       height=2.5,width=2.5,units="in")

###### 
## biodiversity comparison
######
ggplot(data=park_urban_plot%>%
         filter(filename_city!="Honolulu")%>%
         left_join(all_data%>%dplyr::select(filename_city,Lat,Long)),
       aes(x=Long,y=Lat)) +  
  geom_polygon( data=usa, aes(x=long, y=lat, group = group),
                fill="white",colour="grey75" )  +
  coord_equal() +
  xlab("Longitude") + ylab("Latitude")+
  geom_point(aes(color = park_more_bio),
             shape=19,
             size=2)+
  scale_color_manual(values=c("darkorchid3","dodgerblue2"))+
  labs(title="Biodiversity (Effective Species)")+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        title=element_text(size=10),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        legend.position = "bottom")

ggsave(paste0(dir_figure_parkurban,
              "/EffectiveSpecies_Map.pdf",
              sep=""), 
       dpi=600,
       height=2.5,width=2.5,units="in")

###### 
## max abundance
######
ggplot(data=park_urban_plot%>%
         filter(filename_city!="Honolulu")%>%
         left_join(all_data%>%dplyr::select(filename_city,Lat,Long)),
       aes(x=Long,y=Lat)) +  
  geom_polygon( data=usa, aes(x=long, y=lat, group = group),
                fill="white",colour="grey75" )  +
  coord_equal() +
  xlab("Longitude") + ylab("Latitude")+
  geom_point(aes(color = park_lower_max_abund),
             shape=19,
             size=2)+
  scale_color_manual(values=c("dodgerblue2","darkorchid3"))+
  labs(title="Biodiversity (Max. Abundance)")+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        title=element_text(size=10),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        legend.position = "bottom")

ggsave(paste0(dir_figure_parkurban,
              "/MaxAbundance_Map.pdf",
              sep=""), 
       dpi=600,
       height=2.5,width=2.5,units="in")
