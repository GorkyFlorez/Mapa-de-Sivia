#  Cargamos las Librerias ----------------------------------------------------------
library(sf)
library(ggplot2)
library(tidyverse)
library(ggnewscale)
library(raster)
library(extrafont)      # custom font
library(hrbrthemes)     # to use import_roboto_condensed()
library(ggthemes)
library(elevatr)
library(ggspatial)
library(tmap)
library(ggpubr)
# Cargammos los SHp del Peru ---------------------------------------------------------------
DISTRITOS = st_read("SHP/DISTRITOS.shp")  %>% st_as_sf()
DISTRIT <- st_transform(DISTRITOS ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
Huan     <- subset(DISTRIT, PROVINCIA  == "HUANTA")
SIVIA     <- subset(Huan , DISTRITO  == "SIVIA")

Peru          <- getData('GADM', country='Peru', level=1) %>% st_as_sf()
Per           <- getData('GADM', country='Peru', level=2) %>% st_as_sf()
Perrr         <- getData('GADM', country='Peru', level=3) %>% st_as_sf()
Pais_Elev     <- getData('alt', country='Peru')
Bolivia           <- getData('GADM', country='Bolivia', level=0) %>% st_as_sf()
Brazil             <- getData('GADM', country='Brazil', level=0) %>% st_as_sf()
Chile              <- getData('GADM', country='Chile', level=0) %>% st_as_sf()
Ecuador            <- getData('GADM', country='Ecuador', level=0) %>% st_as_sf()

Ayacuch      <- subset(Per, NAME_1  == "Ayacucho")
Ayacucho      <- subset(Perrr, NAME_1  == "Ayacucho")
Huanta        <- subset(Perrr, NAME_2  == "Huanta")

Huant         <- subset(Perrr, NAME_3  == "Huanta")
Cusco_xy      <- cbind(Cusco, st_coordinates(st_centroid(Cusco$geometry)))

Per=ggplot()+
  geom_sf(data = Peru, fill=NA, color="black")+
  geom_sf(data=Bolivia, fill=NA, color="black", size=0.2)+
  geom_sf(data=Brazil, fill=NA, color="black", size=0.2)+
  geom_sf(data=Chile, fill=NA, color="black", size=0.2)+
  geom_sf(data=Ecuador, fill=NA, color="black", size=0.2)+
  geom_sf(data=Ayacucho , fill="red", color="red", size=0.2)+
  coord_sf(xlim = c(-81.3307, -67), ylim = c(-18.3518 ,-0.03747),expand = FALSE)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  annotate(geom = "text", x = -80, y = -1, hjust = 0, vjust = 1, 
           label = "Ecuador",size = 2, family="serif", color = "black",  fontface="italic")+
  annotate(geom = "text", x = -68, y = -15, hjust = 0, vjust = 1, angle = 90,
           label = "Bolivia",size = 2, family="serif", color = "black",  fontface="italic")+
  annotate(geom = "text", x = -70, y = -8, hjust = 0, vjust = 1, 
           label = "Brasil",size = 2, family="serif", color = "black",  fontface="italic")+
  annotate(geom = "text", x = -73, y = -1, hjust = 0, vjust = 1, 
           label = "Colombia",size = 2, family="serif", color = "black",  fontface="italic")+
  ggspatial::annotation_scale(location = "br",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book", text_col="black")

Peru.grob  <- ggplotGrob(Per)

Ayagg=ggplot()+
  geom_sf(data = Peru, fill="gray90", color="black")+
  geom_sf(data=Ayacuch , fill="#faedcd", color="black", size=0.5)+
  geom_sf(data=Huanta , fill="red", color="red", size=0.5)+
  geom_sf_text(data = Peru, aes(label = NAME_1), 
                family="serif", color = "black",  fontface="italic", box.padding = unit(0.9, "lines"), size = 3, face = "bold",color = 'black',
                 point.padding = unit(0.9, "lines"))+
  
  coord_sf(xlim = c(-78, -72.5), ylim = c(-16.3 ,-12))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  annotation_custom(Peru.grob, xmin = -78.3, xmax = -75, ymin =-16.3, ymax=-13.6)

Ayagg.grob  <- ggplotGrob(Ayagg)

Mapa =ggplot()+
  geom_sf(data=Huan  , fill="gray80", color="white", size=0.5)+
  geom_sf(data=SIVIA  , fill="red", color="red", size=0.5)+
  geom_sf_text(data = Huan , aes(label = DISTRITO ), 
                family="serif", color = "black",  fontface="italic", box.padding = unit(0.9, "lines"), 
                size = 3, face = "bold",color = 'black',
                point.padding = unit(0.9, "lines"))+
  coord_sf(xlim = c(-74.8, -73.82992), ylim = c(-13.3 ,-12.16788))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  annotate(geom = "text", x = -74.2, y = -12.2, hjust = 0, vjust = 1, 
           label = "DISTRITO SIVIA",size = 5, family="serif", color = "black",  fontface="italic")+
  annotation_custom(Ayagg.grob , xmin = -74.82, xmax = -74.2, ymin =-13.34, ymax=-12.8)+
  ggspatial::annotation_scale(location = "br",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book", text_col="black")+
  ggspatial::annotation_north_arrow(
    location = "tl", which_north = "true",
    pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey10", "white"),
      line_col = "grey10", text_family = "ArcherPro Book" , text_col="black"))

ggsave(plot = Mapa ,"Mapa/Mapa de siviaa.png", 
       units = "cm", width = 12.5,height = 15, dpi = 1500) 
