library(tidycensus)
library(dplyr)
library(ggplot2)
library(sf)
library(raster)
options(tigris_use_cache = TRUE)

## insert your own key
census_api_key(key="YOUR KEY HERE")

# set up our directories
home_dir <- 'C:/Users/dakot/Dropbox/PC/Documents/Trees/Analysis'
dir_rawsheets <- 'C:/Users/dakot/Dropbox/PC/Documents/Trees/Analysis/Final Spreadsheets'

setwd(home_dir)

dc<-read.csv("./Final Spreadsheets/WashingtonDC_Final_2021-09-22.csv")



## set figure parameters
graph_theme<-  theme(axis.line=element_blank(),
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
                     legend.text=element_text(size=10),
                     legend.title=element_text(size=10),
                     legend.position = "top",
                     legend.background = element_blank())



# https://walker-data.com/census-r/mapping-census-data-with-r.html
dc_income <- get_acs(
  geography = "tract", 
  variables = "B19013_001",
  state = "DC", 
  geometry = TRUE
)

dc_income
df<-as.data.frame(dc_income)


str(dc_income)

dc_income$geometry

plot(dc_income["estimate"])

ggplot(data = dc_income, aes(fill = estimate)) + 
  geom_sf()+
  scale_fill_distiller(palette = "RdPu", 
                       direction = 1,
                       na.value = "white") + 
  theme_classic()

###################################################
###################################################
###################################################
##
## Washington, DC biodiversity per census tract
##
###################################################
###################################################
###################################################

# https://gis.stackexchange.com/questions/282750/identify-polygon-containing-point-with-r-sf-package
## select all points
pnts<-(dc%>%
  select(longitude_coordinate,latitude_coordinate))

# transform to appropriate coordinate system
pnts_sf <- do.call("st_sfc",c(lapply(1:nrow(pnts), 
                                     function(i) {st_point(as.numeric(pnts[i, ]))}), list("crs" = 4326))) 

pnts_trans <- st_transform(pnts_sf, 2163) # apply transformation to pnts sf
tt1_trans <- st_transform(dc_income, 2163)      # apply transformation to polygons sf

# intersect and extract census tract name
pnts$region <- apply(st_intersects(tt1_trans, pnts_trans, sparse = FALSE), 2, 
                     function(col) { 
                       tt1_trans[which(col), ]$NAME
                     })
pnts

## join the census tract labels with the original dataframe

nrow(dc)
nrow(pnts)

dc_labeled<-cbind(dc,pnts%>%select(region))

nrow(dc_labeled)

head(dc_labeled)

# calculate species richness by census tract
dc_data<-dc_labeled%>%
  filter(!is.na(scientific_name))%>%
  filter(!is.na(region))%>%
  group_by(region)%>%
  summarize(unique_species = n_distinct(scientific_name))

dc_data2<-dc_labeled%>%
  filter(!is.na(scientific_name))%>%
  filter(!is.na(region))%>%
  select(region,scientific_name)%>%
  group_by(region,scientific_name)%>%
  summarise(n = n()) %>%
  mutate(frequency = n / sum(n))%>%
  mutate(freq_ln_freq=-1*frequency*log(frequency))%>%
  ungroup()%>%
  group_by(region)%>%
  mutate(effective_species=exp(sum(freq_ln_freq)))%>%
  mutate(species_richness=n())%>%
  select(region,species_richness,effective_species)%>%
  distinct()

summary(dc_data2)

## bind this with our polygon
dc_income$species_richness <- dc_data2$species_richness[match(dc_income$NAME, dc_data2$region)]
dc_income$effective_species <- dc_data2$effective_species[match(dc_income$NAME, dc_data2$region)]

### plot 

water_income <- st_read("https://opendata.arcgis.com/datasets/db65ff0038ed4270acb1435d931201cf_24.geojson") %>%
  st_transform(st_crs(dc_income)) 

water_income_sp <- as(water_income, "Spatial")

## income
ggplot(data = dc_income, aes(fill = estimate/1000)) + 
  geom_sf()+
  scale_fill_distiller(palette = "RdPu", 
                       direction = 1,
                       na.value = "white") + 
  labs(fill="income ($1,000)")+
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5))+
  theme_classic()+
  graph_theme

ggsave("../Figures/PossibleAnalysesFigure/Income.pdf",
       width=2.5,height=4,
       units="in",
       useDingbats=FALSE)

## effective species
ggplot(data = dc_income, aes(fill = effective_species)) + 
  geom_sf()+
  scale_fill_distiller(palette = "BuPu", 
                       direction = 1,
                       na.value = "black") + 
  theme_classic()+
  labs(fill="effective species")+
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5))+
  graph_theme

ggsave("../Figures/PossibleAnalysesFigure/Effective_Species.pdf",
       width=2.5,height=4,
       units="in",
       useDingbats=FALSE)

## species richness
ggplot(data = dc_income, aes(fill = species_richness)) + 
  geom_sf()+
  scale_fill_distiller(palette = "YlGn", 
                       direction = 1,
                       na.value = "black") + 
  theme_classic()+
  labs(fill="species richness")+
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5))+
  graph_theme

ggsave("../Figures/PossibleAnalysesFigure/Species_Richness.pdf",
       width=2.5,height=4,
       units="in",
       useDingbats=FALSE)


## species richness
ggplot(data = dc_income) + 
  geom_sf(fill="NA")+
  theme_classic()+
  graph_theme

ggsave("../Figures/PossibleAnalysesFigure/Outlines.pdf",
       width=2.5,height=4,
       units="in",
       useDingbats=FALSE)

## move legend, name legend, remove axes, remove axis ticks and everything.


# 
# Blues, BuGn, BuPu, GnBu, Greens, Greys, Oranges, OrRd, PuBu, PuBuGn, PuRd, 
# Purples, RdPu, Reds, YlGn, YlGnBu, YlOrBr, YlOrRd


###################################################
###################################################
###################################################
##
## Heat islands by census tract
##
###################################################
###################################################
###################################################

# https://www.danliden.com/post/heat-map-02132021/
# https://www.katiejolly.io/blog/2019-08-28/nyt-urban-heat

## downloaded DC open data CC=by-4.0
#https://opendata.dc.gov/documents/DCGIS::land-surface-temperature-july-2018/about

landsat_dc_july18 <- raster("LST_F_20180708.tif") # saved the downloaded files in a data/ folder

# water features in the city
water <- st_read("https://opendata.arcgis.com/datasets/db65ff0038ed4270acb1435d931201cf_24.geojson") %>%
  st_transform(st_crs(landsat_dc_july18)) # use the same coordinate reference system as the landsat data

## set up data
temp_spdf <- as(landsat_dc_july18, "SpatialPointsDataFrame") # create spatialpoints dataframe
temp_df <- as_tibble(temp_spdf) # convert that to a plain tibble
colnames(temp_df) <- c("value", "x", "y")

# make water feature information a spatial object
water_sp <- as(water, "Spatial")



## plot temperature 
ggplot() +
  geom_raster(data = temp_df, aes(x = x, y = y,  fill = value), 
              interpolate = TRUE) +
  geom_polygon(data = water_sp, aes(x = long, y = lat, group = group), 
               color = "white", fill = "white") +
  coord_equal() + 
  scale_fill_distiller(palette = "YlOrRd", 
                     direction = 1,
                     na.value = "black",
                     limits=c(70,95)
                     ) + 
  labs(fill=expression("temperature " ( degree*F)))+
  theme_classic()+
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5))+
  graph_theme
  
ggsave("../Figures/PossibleAnalysesFigure/Temperature.pdf",
       width=3,height=5,
       units="in",
       useDingbats=FALSE)







