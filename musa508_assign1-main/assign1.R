library(tidyverse)
library(tidycensus)
library(tidytransit)
library(tigris)
library(gridExtra)
library(sf)

source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")

# ACS variables, geography is Minneapolis-St. Paul urbanized area
twincities <- urban_areas(cb=TRUE) %>%
  filter(UACE10 == "57628") %>% 
  select(geometry)

getACSyr <- function(yearACS) {
  temp<-get_acs(geography = 'tract',
                year = yearACS,
                variables = c("B25026_001","B25058_001","B08301_001","B08301_010"),
                state = 27,
                geometry = TRUE,
                output = "wide") %>%
    mutate(year = yearACS) %>%
    mutate(MedRent = B25058_001E) %>% 
    mutate(PopDens = B25026_001E / st_area(geometry)) %>% 
    mutate(PctTransit = B08301_010E / B08301_001E) %>% 
    select(!(B25026_001E:B08301_010M))
  
  st_join(temp, twincities, join=st_within, left=FALSE) %>% 
    st_transform('EPSG:2812')
}

tracts09 <- getACSyr(2009)
tracts19 <- getACSyr(2019)

lehdwac08 <- read_csv("data/mn_wac_S000_JT00_2008.csv") %>% 
  select(w_geocode,C000) %>%
  mutate(GEOID = str_sub(w_geocode,1,11)) %>%
  group_by(GEOID) %>% 
  summarise(jobs=sum(C000))
lehdwac18 <- read_csv("data/mn_wac_S000_JT00_2018.csv") %>% 
  select(w_geocode,C000) %>%
  mutate(GEOID = str_sub(w_geocode,1,11)) %>%
  group_by(GEOID) %>% 
  summarise(jobs=sum(C000))

tracts09 <- tracts09 %>% left_join(lehdwac08,by="GEOID") %>% 
  mutate(JobDens = jobs / st_area(geometry))
tracts19 <- tracts19 %>% left_join(lehdwac18,by="GEOID") %>% 
  mutate(JobDens = jobs / st_area(geometry))

allTracts <- rbind(tracts09,tracts19)

# Rail stops and centroid buffers

metrotransit <- read_gtfs("https://svc.metrotransit.org/mtgtfs/gtfs.zip")

service_id <- filter(metrotransit$calendar, monday==1) %>% pull(service_id)
route_id <- filter(metrotransit$routes, route_id==c('901','902')) %>% pull(route_id)
railStops <- filter_stops(metrotransit,service_id,route_id) %>% 
  group_by(stop_name) %>% slice(1) %>% stops_as_sf(2812)
railLines <- get_route_geometry(gtfs_as_sf(metrotransit), route_id, service_id)

railBuffers <- rbind(
  st_buffer(railStops,2640) %>% 
    mutate(Legend = "Buffer") %>% 
    dplyr::select(Legend),
  st_union(st_buffer(railStops,2640)) %>% 
    st_sf() %>% 
    mutate(Legend = "Unionized Buffer")
)

# ggplot() +
#   geom_sf(data=railBuffers) +
#   geom_sf(data=railStops, show.legend = "point") +
#   facet_wrap(~Legend)

buffer <- filter(railBuffers, Legend=="Unionized Buffer")

# select TOD tracts by buffer, centroid method

allTracts.group <- 
  rbind(
    st_centroid(allTracts)[buffer,] %>%
      st_drop_geometry() %>%
      left_join(allTracts) %>%
      st_sf() %>%
      mutate(TOD = "TOD"),
    st_centroid(allTracts)[buffer, op = st_disjoint] %>%
      st_drop_geometry() %>%
      left_join(allTracts) %>%
      st_sf() %>%
      mutate(TOD = "Non-TOD")) %>%
  mutate(MedRent.inf = ifelse(year == 2009, MedRent * 1.275, MedRent))

allTracts.rings <-
  st_join(st_centroid(dplyr::select(allTracts, GEOID, year)), 
          multipleRingBuffer(st_union(railStops), 47520, 2640)) %>%
  st_drop_geometry() %>%
  left_join(dplyr::select(allTracts, GEOID, MedRent, year), 
            by=c("GEOID"="GEOID", "year"="year")) %>%
  st_sf() %>%
  na.omit() %>%
  mutate(distance = distance / 5280)#convert to miles

Summary_allTracts.rings <-
  st_drop_geometry(allTracts.rings)%>%
  group_by(year,distance)%>%
  summarise(Rent=mean(MedRent),na.rm=T)

ggplot(data=Summary_allTracts.rings,aes(x=distance,y=Rent,color=year))+
  geom_line()+
  labs(title='rent as function of distance to subway station')

# ggplot

palette5 <- c("#f0f9e8","#bae4bc","#7bccc4","#43a2ca","#0868ac")

# Define TOD and non-TOD tracts
ggplot() +
  geom_sf(data=allTracts.group, aes(fill = TOD), color="transparent") +
  geom_sf(data=railLines, aes(color = route_id),size=1) +
  facet_wrap(~year) +
  scale_color_manual(values = c('blue', 'darkgreen'),
                     labels = c('Blue Line', 'Green Line')) + 
  mapTheme() +
  labs(title = "Census tracts by transit-oriented development classification",
       subtitle = "Minneapolis-St. Paul urbanized area, MN-WI",
       caption = "Note: Green Line did not open until 2014")

# Step 2: Four indicator variables
plotStep2 <- function(indicator, indTitle) {
  ggplot() +
    geom_sf(data=allTracts.group, aes(fill = q5(eval(parse(text=indicator)))), color="transparent") +
    geom_sf(data=buffer, color = "red", fill = "transparent") +
    scale_fill_manual(values = palette5,
                      labels = qBr(allTracts.group, indicator),
                      name = paste0(indTitle,"\n(Quintile breaks)")) +
    labs(title = paste0(indTitle,", 2009-2019"),
         subtitle = "Red border = tracts 1/2 mile from light rail stations") + 
    mapTheme() + 
    facet_wrap(~year)
}

plotStep2("PopDens","Population density")
plotStep2("MedRent.inf", "Median rent")
plotStep2("PctTransit", "Transit mode share (%)")

# grid.arrange(i1,i2,i3,i4,nrow=2)



