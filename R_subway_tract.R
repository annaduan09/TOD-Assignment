# We're looking at the relationship between TOD, rent, and crime rate in Boston, MA
# within the time frame of 2012-2018

#################################### Libraries##################################
install.packages("tidyverse")
library(tidyverse)
install.packages("tidycensus")
library(tidycensus)
library(sf)
library(kableExtra)

options(scipen=999)
options(tigris_class = "sf")

######################DEFINE MULTIPLERINGBUFFER FUNCTION########################

multipleRingBuffer <- function(inputPolygon, maxDistance, interval) 
{
  #create a list of distances that we'll iterate through to create each ring
  distances <- seq(0, maxDistance, interval)
  #we'll start with the second value in that list - the first is '0'
  distancesCounter <- 2
  #total number of rings we're going to create
  numberOfRings <- floor(maxDistance / interval)
  #a counter of number of rings
  numberOfRingsCounter <- 1
  #initialize an otuput data frame (that is not an sf)
  allRings <- data.frame()
  
  #while number of rings  counteris less than the specified nubmer of rings
  while (numberOfRingsCounter <= numberOfRings) 
  {
    #if we're interested in a negative buffer and this is the first buffer
    #(ie. not distance = '0' in the distances list)
    if(distances[distancesCounter] < 0 & distancesCounter == 2)
    {
      #buffer the input by the first distance
      buffer1 <- st_buffer(inputPolygon, distances[distancesCounter])
      #different that buffer from the input polygon to get the first ring
      buffer1_ <- st_difference(inputPolygon, buffer1)
      #cast this sf as a polygon geometry type
      thisRing <- st_cast(buffer1_, "POLYGON")
      #take the last column which is 'geometry'
      thisRing <- as.data.frame(thisRing[,ncol(thisRing)])
      #add a new field, 'distance' so we know how far the distance is for a give ring
      thisRing$distance <- distances[distancesCounter]
    }
    
    
    #otherwise, if this is the second or more ring (and a negative buffer)
    else if(distances[distancesCounter] < 0 & distancesCounter > 2) 
    {
      #buffer by a specific distance
      buffer1 <- st_buffer(inputPolygon, distances[distancesCounter])
      #create the next smallest buffer
      buffer2 <- st_buffer(inputPolygon, distances[distancesCounter-1])
      #This can then be used to difference out a buffer running from 660 to 1320
      #This works because differencing 1320ft by 660ft = a buffer between 660 & 1320.
      #bc the area after 660ft in buffer2 = NA.
      thisRing <- st_difference(buffer2,buffer1)
      #cast as apolygon
      thisRing <- st_cast(thisRing, "POLYGON")
      #get the last field
      thisRing <- as.data.frame(thisRing$geometry)
      #create the distance field
      thisRing$distance <- distances[distancesCounter]
    }
    
    #Otherwise, if its a positive buffer
    else 
    {
      #Create a positive buffer
      buffer1 <- st_buffer(inputPolygon, distances[distancesCounter])
      #create a positive buffer that is one distance smaller. So if its the first buffer
      #distance, buffer1_ will = 0. 
      buffer1_ <- st_buffer(inputPolygon, distances[distancesCounter-1])
      #difference the two buffers
      thisRing <- st_difference(buffer1,buffer1_)
      #cast as a polygon
      thisRing <- st_cast(thisRing, "POLYGON")
      #geometry column as a data frame
      thisRing <- as.data.frame(thisRing[,ncol(thisRing)])
      #add teh distance
      thisRing$distance <- distances[distancesCounter]
    }  
    
    #rbind this ring to the rest of the rings
    allRings <- rbind(allRings, thisRing)
    #iterate the distance counter
    distancesCounter <- distancesCounter + 1
    #iterate the number of rings counter
    numberOfRingsCounter <- numberOfRingsCounter + 1
  }
  
  #convert the allRings data frame to an sf data frame
  allRings <- st_as_sf(allRings)
}

############################Map/Plot Formatting#################################
mapTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 16,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    strip.text.x = element_text(size = 14))
}

plotTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 16,colour = "black"),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line("grey80", size = 0.1),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    strip.background = element_rect(fill = "grey80", color = "white"),
    strip.text = element_text(size=12),
    axis.title = element_text(size=12),
    axis.text = element_text(size=10),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic"),
    legend.text = element_text(colour = "black", face = "italic"),
    strip.text.x = element_text(size = 14)
  )
}

# Quintile Map Breaks
qBr <- function(df, variable, rnd) {
  if (missing(rnd)) {
    as.character(quantile(round(df[[variable]],0),
                          c(.01,.2,.4,.6,.8), na.rm=T))
  } else if (rnd == FALSE | rnd == F) {
    as.character(formatC(quantile(df[[variable]]), digits = 3),
                 c(.01,.2,.4,.6,.8), na.rm=T)
  }
}

q5 <- function(variable) {as.factor(ntile(variable, 5))}

# Palette
palette5 <- c("#f0f9e8","#bae4bc","#7bccc4","#43a2ca","#0868ac")

####################################get data####################################

# Input API key
census_api_key("d9ebfd04caa0138647fbacd94c657cdecbf705e9", install = TRUE, overwrite = TRUE)

rm(sub_16_5, sub_17_5)
# Median Rent, MHHINC, Population, Bachelor's, No. Vehicle (home owner, renter), Households (owner, renter)
## projection: (NAD 1983 StatePlane Massachusetts Mainland FIPS 2001 Feet)
tracts10 <-  
  get_acs(geography = "tract", variables = c("B25058_001E", "B19013_001E", "B01003_001E", "B06009_005E", 
                                             "B25044_003E", "B25044_010E", "B07013_002E", "B07013_003E"), 
          year=2010, state=25, county=025, geometry=T) %>% 
  st_transform('ESRI:102686')

#BC https://en.wikipedia.org/wiki/MBTA_subway 
#BC I think our focus will be mainly on silver line, which started on 2002

tracts18 <-  
  get_acs(geography = "tract", variables = c("B25058_001E", "B19013_001E", "B01003_001E", "B06009_005E", 
                                             "B25044_003E", "B25044_010E", "B07013_002E", "B07013_003E"), 
          year=2018, state=25, county=025, geometry=T) %>% 
  st_transform('ESRI:102686')


###############################plot data 10########################################
###################################################################################

# rent
tracts10_rent <-
  tracts10 %>%
  filter(variable == "B25058_001")

plot(tracts10_rent[,4])

# household income
tracts10_income <-
  tracts10 %>%
  filter(variable == "B19013_001")

plot(tracts10_income[,4])

# population
tracts10_pop <-
  tracts10 %>%
  filter(variable == "B01003_001")

plot(tracts10_pop[,4])

# Bachelor's
tracts10_bach <-
  tracts10 %>%
  filter(variable == "B06009_005")

plot(tracts10_bach[,4])

# No. Vehicle (home owner)
tracts10_noCarOwner <-
  tracts10 %>%
  filter(variable == "B25044_003")

plot(tracts10_noCarOwner[,4])

# No. Vehicle (renter)
tracts10_noCarRenter <-
  tracts10 %>%
  filter(variable == "B25044_010")

plot(tracts10_noCarRenter[,4])

# No. Owner-occupied households
tracts10_ownerHH <-
  tracts10 %>%
  filter(variable == "B07013_002")

plot(tracts10_ownerHH[,4])

# No. Renter-occupied households
tracts10_renterHH <-
  tracts10 %>%
  filter(variable == "B07013_003")

plot(tracts10_renterHH[,4])


##############################long form to wide form###############################
tracts10 <- 
  tracts10 %>%
  dplyr::select( -NAME, -moe) %>%
  spread(variable, estimate) %>%
  dplyr::select(-geometry) %>%
  rename(Rent = B25058_001, 
         MedHHInc = B19013_001,
         Population = B01003_001, 
         Bachelor = B06009_005,
         NoVehicle_hmow = B25044_003, 
         NoVehicle_hmre = B25044_010,
         Households_hmow = B07013_002,
         Households_hmre = B07013_003)

st_drop_geometry(tracts10)[1:3,]
###################################mutate##########################################
tracts10 <- 
  tracts10 %>%
  mutate(pctBach = ifelse(Population > 0, Bachelor / Population, 0),
         pctNoVehicle = ifelse(Households_hmow + Households_hmre > 0, 
                               (NoVehicle_hmow + NoVehicle_hmre) / 
                                  (Households_hmow + Households_hmre),0),
         year = "2010") %>%
  dplyr::select(-Households_hmow,-Households_hmre,-NoVehicle_hmow, -NoVehicle_hmre, -Bachelor)


###############################plot data 19########################################
###################################################################################
# rent
tracts18_rent <-
  tracts18 %>%
  filter(variable == "B25058_001")

plot(tracts18_rent[,4])

PlotRent <- 
  ggplot() +
  geom_sf(data = tracts18_rent, aes(fill = q5(estimate))) +
  scale_fill_manual(values = palette5,
                    labels = qBr(tracts18_rent, "estimate"),
                    name = "Rent\n(Quintile Breaks)") +
  labs(title = "Median Contract Rent", subtitle = "Boston; 2018") +
  mapTheme() + theme(plot.title = element_text(size=22))
# household income
tracts18_income <-
  tracts18 %>%
  filter(variable == "B19013_001")

plot(tracts18_income[,4])

# population
tracts18_pop <-
  tracts18 %>%
  filter(variable == "B01003_001")

plot(tracts18_pop[,4])

# Bachelor's
tracts18_bach <-
  tracts18 %>%
  filter(variable == "B06009_005")

plot(tracts18_bach[,4])

# No. Vehicle (home owner)
tracts18_noCarOwner <-
  tracts18 %>%
  filter(variable == "B25044_003")

plot(tracts18_noCarOwner[,4])

# No. Vehicle (renter)
tracts18_noCarRenter <-
  tracts18 %>%
  filter(variable == "B25044_010")

plot(tracts18_noCarRenter[,4])

##############################long form to wide form###############################
tracts18 <- 
  tracts18 %>%
  dplyr::select( -NAME, -moe) %>%
  spread(variable, estimate) %>%
  dplyr::select(-geometry) %>%
  rename(Rent = B25058_001, 
         MedHHInc = B19013_001,
         Population = B01003_001, 
         Bachelor = B06009_005,
         NoVehicle_hmow = B25044_003, 
         NoVehicle_hmre = B25044_010,
         Households_hmow = B07013_002,
         Households_hmre = B07013_003)

st_drop_geometry(tracts10)[1:3,]
###################################mutate##########################################
tracts18 <- 
  tracts18 %>%
  mutate(pctBach = ifelse(Population > 0, Bachelor / Population, 0),
         pctNoVehicle = ifelse(Households_hmow + Households_hmre > 0, 
                               (NoVehicle_hmow + NoVehicle_hmre) / 
                                 (Households_hmow + Households_hmre),0),
         year = "2018") %>%
  dplyr::select(-Households_hmow,-Households_hmre,-NoVehicle_hmow, -NoVehicle_hmre, -Bachelor)

###################################bind 2009 and 2018#############################
allTracts <- rbind(tracts10,tracts18)

#################################transit data######################################
##################################################################################
#mbta_node <- st_transform(st_crs(tracts10))
#  rbind(
#    st_read("https://docs.digital.mass.gov/api/3/action/package_show?id=89711539-cf8a-4386-bf91-a1110c541da0") %>% 
#      mutate(LINE = "SILVER") %>%
#      select(STATION, LINE),#2002
#    st_read("https://docs.digital.mass.gov/api/3/action/package_show?id=89711539-cf8a-4386-bf91-a1110c541da0") %>%
#      mutate(LINE ="RED") %>%
#      select(STATION, LINE),#1929,1912
#    st_read("https://docs.digital.mass.gov/api/3/action/package_show?id=89711539-cf8a-4386-bf91-a1110c541da0") %>% 
#     mutate(LINE = "ORANGE") %>%
#      select(STATION, LINE),#1901
#    st_read("https://docs.digital.mass.gov/api/3/action/package_show?id=89711539-cf8a-4386-bf91-a1110c541da0") %>% 
#      mutate(LINE = "GREEN") %>%
#      select(STATION, LINE),#1897
#    st_read("https://docs.digital.mass.gov/api/3/action/package_show?id=89711539-cf8a-4386-bf91-a1110c541da0") %>%
#      mutate(LINE ="BLUE") %>%#1904
#      select(STATION, LINE)) %>% 

mbta_node <- st_read("/Users/annaduan/Documents/GitHub/TOD-Assignment/mbta_node.geojson") %>% st_transform(st_crs(tracts17)) 

ggplot() + 
  geom_sf(data=st_union(tracts18)) +
  geom_sf(data=mbta_node, aes(colour = line), show.legend = "point", size= 2) +
  scale_colour_manual(values = c("orange","blue","red","green","purple","gray","yellow","pink","dark blue","dark green")) +
  labs(title="Subway Stops", subtitle="Boston, MA", caption="Figure 1.1") +
  mapTheme()
 
####################################set buffer##################################
mbtaBuffers <- 
  rbind(
    st_buffer(mbta_node, 2640) %>% #in feet
      mutate(Legend = "Buffer") %>%
      dplyr::select(Legend),
    st_union(st_buffer(mbta_node, 2640)) %>% #union buffer
      st_sf() %>%
      mutate(Legend = "Unioned Buffer"))


#The resulting 'small multiple' map is only possible when data is organized in long form.
ggplot() +
  geom_sf(data=mbtaBuffers) +
  geom_sf(data=mbta_node, show.legend = "point") +
  facet_wrap(~Legend) +  #wrap by years and make small multiple plots
  labs(caption = "Figure 1.2") +
  mapTheme()


#############################multi-ring buffer##################################

mbtamultibuffers <- multipleRingBuffer(mbtaBuffers, 10, 0.5)

ggplot() + geom_sf(data = mbtamultibuffers, aes(fill = distance))

#############################spatial operation##################################

buffer <- filter(mbtaBuffers, Legend=="Unioned Buffer")

selectCentroids <-
  st_centroid(tracts18)[buffer,] %>%
  st_drop_geometry() %>%
  left_join(dplyr::select(tracts18, GEOID)) %>%
  st_sf() %>%
  dplyr::select(TotalPop) %>%
  mutate(Selection_Type = "Select by Centroids")


###########################indicator map########################################

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
  mutate(MedRent.inf = ifelse(year == "2009", MedRent * 1.14, MedRent))
#need to change here!!!
##################
#################
##MedRent?

###########################TOD Indicator Tables################################

allTracts.Summary <- 
  st_drop_geometry(allTracts.group) %>%
  group_by(year, TOD) %>%
  summarize(Rent = mean(MedRent, na.rm = T),
            Population = mean(TotalPop, na.rm = T),
            Percent_White = mean(pctWhite, na.rm = T),
            Percent_Bach = mean(pctBachelors, na.rm = T),
            Percent_Poverty = mean(pctPoverty, na.rm = T))

kable(allTracts.Summary) %>%
  kable_styling() %>%
  footnote(general_title = "\n",
           general = "Table 2.2")

########change to long form######
allTracts.Summary %>%
  unite(year.TOD, year, TOD, sep = ": ", remove = T) %>%
  gather(Variable, Value, -year.TOD) %>%
  mutate(Value = round(Value, 2)) %>%
  spread(year.TOD, Value) %>%
  kable() %>%
  kable_styling() %>%
  footnote(general_title = "\n",
           general = "Table 2.3")


###########################TOD Indicator Plots##################################

allTracts.Summary %>%
  gather(Variable, Value, -year, -TOD) %>%
  ggplot(aes(year, Value, fill = TOD)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~Variable, scales = "free", ncol=5) +
    scale_fill_manual(values = c("#bae4bc", "#0868ac")) +
    labs(title = "Indicator differences across time and space") +
    plotTheme() + theme(legend.position="bottom")

# Examining six submarkets

Downtown <-
  st_intersection(
    st_buffer(filter(mbta_node, line == "SILVER"), 2640) %>% st_union(),
    st_buffer(filter(mbta_node, line == "RED"&"GREEN/RED"&"ORANGE/RED"), 2640) %>% st_union(),
    st_buffer(filter(mbta_node, line == "GREEN"&"GREEN/RED"&"GREEN/ORANGE"&"BLUE/GREEN"), 2640) %>% st_union(),
    st_buffer(filter(mbta_node, line == "ORANGE"&"ORANGE/RED"&"BLUE/ORANGE"), 2640) %>% st_union(),
    st_buffer(filter(mbta_node, line == "BLUE"&"BLUE/GREEN"&"BLUE/ORANGE"), 2640) %>% st_union()) %>%
  st_sf() %>%
  mutate(Submarket = "Downtown")

SILVER_LINE <-
  st_buffer(filter(mbta_node, line == "SILVER"), 2640) %>% st_union() %>%
  st_sf() %>%
  st_difference(Downtown) %>%
  mutate(Submarket = "El")

RED_LINE <-
  st_buffer(filter(mbta_node, line == "RED"&"GREEN/RED"&"ORANGE/RED"), 2640) %>% st_union() %>%
  st_sf() %>%
  st_difference(Downtown) %>%
  mutate(Submarket = "Broad Street")

GREEN_LINE <-
  st_buffer(filter(mbta_node, line == "GREEN"&"GREEN/RED"&"GREEN/ORANGE"&"BLUE/GREEN"), 2640) %>% st_union() %>%
  st_sf() %>%
  st_difference(Downtown) %>%
  mutate(Submarket = "Broad Street")

ORANGE_LINE <-
  st_buffer(filter(mbta_node, line == "ORANGE"&"ORANGE/RED"&"BLUE/ORANGE"), 2640) %>% st_union() %>%
  st_sf() %>%
  st_difference(Downtown) %>%
  mutate(Submarket = "Broad Street")

BLUE_LINE <-
  st_buffer(filter(mbta_node, line == "BLUE"&"BLUE/GREEN"&"BLUE/ORANGE"), 2640) %>% st_union() %>%
  st_sf() %>%
  st_difference(Downtown) %>%
  mutate(Submarket = "Broad Street")

sixMarkets <- rbind(SILVER_LINE, RED_LINE, GREEN_LINE, ORANGE_LINE, BLUE_LINE, Downtown)

# You can then bind these buffers to tracts and map them or make small multiple plots

allTracts.sixMarkets <-
  st_join(st_centroid(allTracts), sixMarkets) %>%
  st_drop_geometry() %>%
  left_join(allTracts) %>%
  mutate(Submarket = replace_na(Submarket, "Non-TOD")) %>%
  st_sf() 
#spread goes long to wide, gather opposite



####################################Crime Data############################################


