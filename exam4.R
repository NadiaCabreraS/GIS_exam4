library(sf)
library(here)
library (tidyverse)
library(janitor)
library(countrycode)
library(ggplot2)
library (dplyr)
library (tmap)



# Read the csv and select the data we need -> Then calculate the difference between years 2019 and 2010

df_gii<- read.csv("HDR25_Composite_indices_complete_time_series.csv", 
                  fileEncoding = "latin1",
                  na.strings = "n/a") %>%
  select(country, iso3, gii_2010, gii_2019) %>%
  mutate(diff2019_10 = gii_2010 - gii_2019 )
head(df_gii)

# Revisamos las capas del gpkg
st_layers(here("World_Countries_(Generalized)_8414823838130214587.gpkg"))

# Load the gpkg, check that the CRS is correct (WGS84)
worldmap <- st_read(here("World_Countries_(Generalized)_8414823838130214587.gpkg"))%>%
  clean_names()
head(worldmap)
plot(worldmap)
st_crs(worldmap)
#Using Country code, we match the ISO codes between our map and our data set, then we merge the files using left join.

gii_map <- worldmap %>%
  mutate(iso= countrycode(iso, origin = "iso2c", destination = "iso3c")) %>%
  left_join(.,df_gii, by=c("iso" = "iso3")) %>%
  st_transform(., crs = 54030)
  
plot(gii_map["diff2019_10"])


#GGPLOT HISTOGRAM


ggplot(gii_map) +
  geom_sf(aes(fill = diff2019_10), color = "grey70", size = 0.15) +
  scale_fill_gradient2(
    name = "Variation of GII",
    low = "#FFD700",    # amarillo (bajos)
    mid = "white",      # 0
    high = "#18392B",   # verde (altos)
    midpoint = 0,
    limits=NULL,
    na.value = "grey90"
  ) +
  coord_sf(crs = "ESRI:54030", expand = FALSE) +
  labs(
    title = "Difference in Gender inequality index 2010 - 2019",
    caption = "Source: Human Development Report (UNDP)"
  ) +
  theme_test() 


gii_map_noSAU <- gii_map %>%
  filter(iso != "SAU")

ggplot(gii_map_noSAU) +
  geom_sf(aes(fill = diff2019_10), color = "grey70", size = 0.15) +
  scale_fill_gradient2(
    name = "Variation of GII",
    low = "#FFD700",    # amarillo (bajos)
    mid = "white",      # 0
    high = "#18392B",   # verde (altos)
    midpoint = 0,
    limits = range(gii_map_noSAU$diff2019_10, na.rm = TRUE),
    na.value = "grey90"
  ) +
  coord_sf(crs = "ESRI:54030", expand = FALSE) +
  labs(
    title = "Difference in Gender inequality index 2010 - 2019",
    caption = "Source: Human Development Report (UNDP)"
  ) +
  theme_test() 



# set up the basic histogram
gghist <- ggplot(df_gii, 
                 aes(x=diff2019_10)) + 
  geom_histogram(color="black", 
                 fill="white",
                 bins=60)
  labs(title="Distribution of change in the GII (2010–2019)", 
       x="Variation of GII",
       y="Frequency")
# add a vertical line to the hisogram showing mean tempearture
gghist + geom_vline(aes(xintercept=mean(diff2019_10, 
                                        na.rm=TRUE)),
                    color="blue", 
                    linetype="dashed", 
                    size=1)+
  theme(plot.title = element_text(hjust = 0.5))

#GGPLOT MAP (using de 0 a arriba y de 0 a abajo)

#Dinamic map

tmap_mode("view")
tm_shape(gii_map) + 
  # add polygon layer
  tm_polygons(fill="diff2019_10",
              fill.scale= tm_scale_intervals (values="carto.blu_grn",
                                             style="jenks"),
              fill_alpha = 0.9,
          
              fill.legend = tm_legend(title = "Difference in inequality index", 
                                      size = 0.4))+
  tm_basemap(server = "OpenStreetMap") +
  tm_compass(type = "arrow", position = c("left", "bottom")) + 
  tm_scalebar(position = c("left", "bottom"))+
  tm_title("Difference in Gender inequality index 2010 - 2019", 
           size = 1,
           position = c("center", "top"))
  

