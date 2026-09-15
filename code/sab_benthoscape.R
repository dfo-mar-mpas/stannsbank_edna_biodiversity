#load libraries 
library(tidyverse)
library(sf)
library(ggspatial)
library(lubridate)
library(patchwork)

s2_as_sf = FALSE

source("https://raw.githubusercontent.com/dfo-mar-mpas/MCRG_functions/refs/heads/main/code/trim_img_ws.R")

#projections
latlong <- 4326
utm <- 32620 

#Load St. Anns Bank polygon
sab_zones <- read_sf("data/shapefiles/SAB_boundary_zones_2017.shp")%>%
  st_transform(latlong)

#just he outside shape with no zones
sab  <- sab_zones%>%
  st_transform(utm)%>%
  st_buffer(0.0025)%>% #this is small buffer that the vertex coordinate rounding issue
  st_union()%>% #gets rid of the zones
  st_transform(latlong)%>%
  st_as_sf()

benthoscape <- read_sf("data/shapefiles/benthoscape.shp")%>%
  st_transform(latlong)%>%
  mutate(class = Assigned_c,
         class = gsub("A - ","",class), #clean up the classification labels
         class = gsub("Asp - ","",class),
         class = gsub("B - ","",class),
         class = gsub("C - ","",class),
         class = gsub("D - ","",class),
         class = gsub("E - ","",class),
         class = gsub("F - ","",class),
         class = gsub("cobblles","cobbles",class))%>%
      st_make_valid()

#load the banks shapefile (this is a bathymetry derived product for 40m and shallower)
sab_banks <- read_sf("data/shapefiles/sab_banks_40m.shp") %>%
              st_transform(latlong) %>%
              st_make_valid() %>%
              st_polygonize() %>% #these were derived from bathymetry so the 'shape' file is actually a line. 
              st_make_valid() 

sab_banks_benthoscape <- sab_banks%>%
                         st_intersection(benthoscape) #this will assign the benthoscape to the 'banks' in this case they are both completely 'Till with coraline algae'

#load a high resolution coastline for St. Anns Bank (general coast most proximate to St Anns Bank)
sab_coast_hr <- read_sf("data/shapefiles/SAB_hr_coast.shp")%>%
                st_transform(latlong)

#depth contour (250m)
contour_250 <- read_sf("data/shapefiles/contour_250_fine.shp")%>%
               st_transform(latlong)

#read in the sample coordinates and clean them up
sab_edna_meta <- read.csv("data/SAB_eDNA_AllSamplesCoordinates_2022-2026.csv")%>%
                 filter(!(is.na(Longitude)|is.na(Latitude)),#keeping only those with coordinates
                        !grepl("blank",tolower(samplingStation)))%>% #get rid of sample planks
                 mutate(eventDate = mdy(eventDate),
                        year  = year(eventDate),
                        month = month(eventDate), 
                        day   = day(eventDate),
                        lat = as.numeric(Latitude),
                        lon = as.numeric(Longitude)*-1,# negative longitude for decimal degrees west
                        unique_id = paste(day,month,year,lat,lon,sep="-"))%>%
                 distinct(unique_id,.keep_all = TRUE)%>%
                 st_as_sf(coords=c("lon","lat"),crs=latlong,remove=FALSE)%>% #note that this is the projection used with these decimal degrees
                 st_join(benthoscape%>%dplyr::select(class))%>% #assign points a 'benthoscape'
                 st_join(sab_banks%>%dplyr::select(name))%>% #assign points to a bank if they on one
                 mutate(distance_to_scatarie = st_distance(sab_banks%>%filter(name=="Scatarie Bank")), #calcualte the distance to the respective bank polygons
                        distance_to_curdo = st_distance(sab_banks%>%filter(name=="Curdo Bank")),
                        distance_to_scatarie = as.numeric(distance_to_scatarie)/1000, #convert to km
                        distance_to_curdo = as.numeric(distance_to_curdo)/1000)%>%
                 rename(bank=name)%>%
                dplyr::select(-unique_id,Latitude,Longitude)
                 

#save the outputs
if(!file.exists("output/sab_edna_meta_processed.csv")){
  
  write.csv(x = sab_edna_meta%>%st_drop_geometry(),file = "output/sab_edna_meta_processed.csv")
  
}

#Set plotting limits
plot_lims <- sab%>%
            st_transform(utm)%>%
            st_buffer(20*1000)%>% #buffer in km as 'utm' is in metres 
            st_transform(latlong)%>%
            st_bbox()

plot_lims_banks <- sab_banks%>%
                  st_transform(utm)%>%
                  st_buffer(5*1000)%>% #buffer in km as 'utm' is in metres 
                  st_transform(latlong)%>%
                  st_bbox()


#Make map just of Curdo Bank (zoomed in)
curdo_plot_bound <- sab_banks%>%
  filter(name=="Curdo Bank")%>%
  st_transform(utm)%>%
  st_buffer(0.5*1000)%>%
  st_transform(latlong)%>%
  st_bbox()

curdo_box <- curdo_plot_bound%>%st_as_sfc()

  p1_curdo <- ggplot()+
              geom_sf(data=benthoscape,aes(fill=class),alpha=0.75)+
              geom_sf(data=sab_banks_benthoscape%>%filter(name=="Curdo Bank"),aes(fill=class),linewidth=0.9,col="black")+
              geom_sf(data=sab_edna_meta,aes(shape=factor(year)),fill="white",size=2)+
              theme_bw()+
              theme(axis.text=element_blank(),
                    plot.margin = margin(0, 0, 0, 0),
                    plot.title = element_text(size=10,vjust=-2),
                    legend.position = "none")+
              coord_sf(xlim=curdo_plot_bound[c(1,3)],ylim=curdo_plot_bound[c(2,4)],expand=0)+
              labs(title="Curdo Bank",fill="")+
              annotation_scale(location="br")+
              scale_shape_manual(values= 21:25) #open shapes
    
    

  #Make map just of Scatarie Bank (zoomed in)
  scat_plot_bound <- sab_banks%>%
    filter(name=="Scatarie Bank")%>%
    st_transform(utm)%>%
    st_buffer(0.5*1000)%>%
    st_transform(latlong)%>%
    st_bbox()
  
  scat_box <- curdo_plot_bound%>%st_as_sfc()
  
  
  p1_scat <- ggplot()+
    geom_sf(data=benthoscape,aes(fill=class),alpha=0.75)+
    geom_sf(data=sab_banks_benthoscape%>%filter(name=="Scatarie Bank"),aes(fill=class),linewidth=0.9,col="black")+
    geom_sf(data=sab_edna_meta,aes(shape=factor(year)),fill="white",size=2)+
    theme_bw()+
    theme(axis.text=element_blank(),
          plot.margin = margin(0, 0, 0, 0),
          plot.title = element_text(size=10,vjust=-2),
          legend.position = "none")+
    coord_sf(xlim=scat_plot_bound[c(1,3)],ylim=scat_plot_bound[c(2,4)],expand=0)+
    labs(title="Scatarie Bank",fill="")+
    annotation_scale(location="br")+
    scale_shape_manual(values= 21:25) #open shapes
  
  
#make a plot of the banks together
  p_banks <- ggplot() +
    geom_sf(data = benthoscape, aes(fill = class), alpha = 0.7) +
    geom_sf(data = sab_banks_benthoscape %>% filter(name == "Scatarie Bank"), 
            aes(fill = class), linewidth = 0.9, col = "black") +
    geom_sf(data = sab_zones, fill = NA) +
    geom_sf(data = sab_edna_meta, aes(shape = factor(year)), fill = "white", size = 1.5) +
    scale_shape_manual(values = 21:25) +
    labs(fill = "Benthoscape Class", shape = "Sample Year") +
    coord_sf(xlim = plot_lims_banks[c(1, 3)], ylim = plot_lims_banks[c(2, 4)], expand = 0) +
    annotation_scale(location = "tl") +
    theme_bw() + # Must come BEFORE custom theme settings
    theme(
      plot.margin = margin(5, 5, 5, 5),
      legend.box = "vertical",
      legend.title = element_text(size = 9, face = "bold"),
      legend.text = element_text(size = 8),
      legend.key.size = unit(0.4, "cm")
    ) +
    guides(
      shape = guide_legend(
        position = "bottom",
        direction = "horizontal",
        title.position = "top",
        nrow = 1
      ),
      fill = guide_legend(
        position = "right",
        ncol = 1
      )
    )
  
  #now combine all into one plot
  
  p_banks_configured <- p_banks + # top plot (or row)
    theme(
      legend.position = "right",
      legend.box = "vertical"
    ) +
    guides(
      fill = guide_legend(order = 1),
      shape = guide_legend(
        order = 2,
        position = "bottom",
        direction = "horizontal",
        title.position = "top",
        nrow = 1
      )
    )
  
  
  p_bottom <- (p1_curdo + p1_scat) & #bottom plot (or row)
    theme(
      legend.position = "none",
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank()
    )
  
  
  combined_plot <- p_banks_configured / p_bottom + 
    plot_layout(
      heights = c(2, 1),
      guides = "keep" # Preserves p_banks's internal legend locations
    )
  
 
  ggsave(
    "output/sab_edna_samples_bethoscape_banks.jpg",
    combined_plot,
    width = 8,
    height = 5,
    units = "in",
    dpi = 300
  )
