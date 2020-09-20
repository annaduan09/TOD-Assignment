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

#BC mine: 0e3cc3910723434685f1e4c5df2ac519c0790ba5
rm(sub_16_5, sub_17_5)
# Tracts, Median Rent, MHHINC, Population, Bachelor's, No. Vehicle (home owner, renter), Households (owner, renter)
## projection (NAD 1983 StatePlane Massachusetts Mainland FIPS 2001 Feet)
tracts10 <-  
  get_acs(geography = "tract", variables = c("B25058_001E", "B19013_001E", "B01003_001E", "B06009_005E", 
                                             "B25044_003E", "B25044_010E", "B07013_002E", "B07013_003E"), 
          year=2010, state=25, county=025, geometry=T) %>% 
  st_transform('ESRI:102686')

#BC not sure if we do the year 2012 and 2019 or?
#AD the professor said to do 2010 and 2019

#BC https://en.wikipedia.org/wiki/MBTA_subway 
#BC I think our focus will be mainly on silver line, which started on 2002
#AD Sounds good

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

RentPlot <- 
  ggplot() +
  geom_sf(data = tracts10_rent, aes(fill = q5(estimate))) +
  scale_fill_manual(values = palette5,
                    labels = qBr(tracts10_rent, "estimate"),
                    name = "Rent\n(Quintile Breaks)") +
  labs(title = "Median Contract Rent", subtitle = "Boston; 2010") +
  mapTheme() + theme(plot.title = element_text(size=22))
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
         households_hmow = B07013_002,
         households_hmre = B07013_003)

st_drop_geometry(tracts10)[1:3,]
###################################mutate##########################################
tracts10 <- 
  tracts10 %>%
  mutate(pctBach = ifelse(Population > 0, Bachelor / Population, 0),
         pctNoVehicle = ifelse(households_hmow + households_hmre > 0, 
                               (NoVehicle_hmow + NoVehicle_hmre) / 
                                  (households_hmow + households_hmre),0),
         year = "2010") %>%
  dplyr::select(-householdshmow,-householdshmre,-geometry)


###############################plot data 19########################################
###################################################################################
##BC: I haven't changed the year below
##AD: I changed it
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
         households_hmow = B07013_002,
         households_hmre = B07013_003)

st_drop_geometry(tracts10)[1:3,]
###################################mutate##########################################
tracts18 <- 
  tracts18 %>%
  mutate(pctBach = ifelse(Population > 0, Bachelor / Population, 0),
         pctNoVehicle = ifelse((households_hmow + households_hmre) > 0, 
                               (NoVehicle_hmow + NoVehicle_hmre) / 
                                 (households_hmow + households_hmre),0),
         year = "2018") %>%
  dplyr::select(-householdshmow,-householdshmre,-geometry)


#################################transit data######################################
##################################################################################
mbta_node <- st_transform(st_crs(tracts10))
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


mbta_node_sf <- st_as_sf(mbta_node, coords = c("Lon", "Lat"), crs = 4326) %>%
  st_transform('ESRI:102728')

ggplot() + 
  geom_sf(data=st_union(tracts10)) +
  geom_sf(data=mbta_node, aes(colour = line), show.legend = "point", size= 2) +
  scale_colour_manual(values = c("orange","blue")) +
  labs(title="Subway Stops", subtitle="Boston, MA", caption="Figure 2.5") +
  mapTheme()
 

###########################muti-ring buffer#######################################
square <-  
  st_sfc(st_polygon(list(cbind(c(0,3,3,0,0),c(0,0,3,3,0))))) %>%
  st_sf()

#Run the function:
  
  buffers <- multipleRingBuffer(square, 10, 1)
#Plot the result:
  
  ggplot() + geom_sf(data = buffers, aes(fill = distance))
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


