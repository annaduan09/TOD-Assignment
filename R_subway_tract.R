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

######################DEFINE MULTIPLE RING BUFFER FUNCTION########################

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
      #add the distance
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

############################Plot Formatting#################################
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
    strip.text.x = element_text(size = 10))
}

plotTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 10,colour = "black"),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line("grey80", size = 0.1),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    strip.background = element_rect(fill = "grey80", color = "white"),
    strip.text = element_text(size=10),
    axis.title = element_text(size=10),
    axis.text = element_text(size=10),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic"),
    legend.text = element_text(colour = "black", face = "italic"),
    strip.text.x = element_text(size = 10)
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
palette5 <- c("#b3cde0","#6497b1","#005b96","#03396c","#011f4b")

####################################ACS DATA####################################

# Input API key
census_api_key("d9ebfd04caa0138647fbacd94c657cdecbf705e9", install = TRUE, overwrite = TRUE)

# Median Rent, MHHINC, Population, Bachelor's, No. Vehicle (home owner, renter), Households (owner, renter occupied)
## projection: (NAD 1983 StatePlane Massachusetts Mainland FIPS 2001 Feet)
oritracts10 <-  
  get_acs(geography = "tract", variables = c("B25058_001E", "B19013_001E", "B01003_001E", "B06009_005E", 
                                             "B25044_003E", "B25044_010E", "B07013_002E", "B07013_003E"), 
          year=2010, state=25, county=025, geometry=T) %>% 
  st_transform('ESRI:102686')

oritracts18 <-  
  get_acs(geography = "tract", variables = c("B25058_001E", "B19013_001E", "B01003_001E", "B06009_005E", 
                                             "B25044_003E", "B25044_010E", "B07013_002E", "B07013_003E"), 
          year=2018, state=25, county=025, geometry=T) %>% 
  st_transform('ESRI:102686')


###############################plot 2010 data########################################

# rent
tracts10_rent <-
  oritracts10 %>%
  filter(variable == "B25058_001")

plot(tracts10_rent[,4])

# household income
tracts10_income <-
  oritracts10 %>%
  filter(variable == "B19013_001")

plot(tracts10_income[,4])

# population
tracts10_pop <-
  oritracts10 %>%
  filter(variable == "B01003_001")

plot(tracts10_pop[,4])

# Bachelor's
tracts10_bach <-
  oritracts10 %>%
  filter(variable == "B06009_005")

plot(tracts10_bach[,4])

# No. Vehicle (home owner)
tracts10_noVehicleOwner <-
  oritracts10 %>%
  filter(variable == "B25044_003")

plot(tracts10_noVehicleOwner[,4])

# No. Vehicle (renter)
tracts10_noVehicleRenter <-
  oritracts10 %>%
  filter(variable == "B25044_010")

plot(tracts10_noVehicleRenter[,4])

# No. Owner-occupied households
tracts10_ownerHH <-
  oritracts10 %>%
  filter(variable == "B07013_002")

plot(tracts10_ownerHH[,4])

# No. Renter-occupied households
tracts10_renterHH <-
  oritracts10 %>%
  filter(variable == "B07013_003")

plot(tracts10_renterHH[,4])


##############################long form to wide form###############################
#have to change the original file to oritracts because we change the variables names here, then if we go back 
#to run the plots, it won't work because the variabes are no long "B...."

tracts10 <- 
  oritracts10 %>%
  dplyr::select( -NAME, -moe) %>%
  spread(variable, estimate) %>%
  dplyr::select(-geometry) %>%
  rename(Rent = B25058_001, 
         medHHInc = B19013_001,
         population = B01003_001, 
         bachelor = B06009_005,
         noVehicle_hmow = B25044_003, 
         noVehicle_hmre = B25044_010,
         Households_hmow = B07013_002,
         Households_hmre = B07013_003)

st_drop_geometry(tracts10)[1:3,]

#GEOID population bachelor Households_hmow Households_hmre medHHInc noVehicle_hmow noVehicle_hmre Rent
#1 25025000100       3620      429            1084            2339    55179             55            207 1266
#2 25025000201       3636      834            1235            2212    68010             37            221 1346
#3 25025000202       3899      680            1573            2267    54151             58            206 1223
###################################mutate##########################################
#the variables are selected from the table above
tracts10 <- 
  tracts10 %>%
  mutate(pctBach = ifelse(population > 0, bachelor / population, 0),
         pctNoVehicle = ifelse(Households_hmow + Households_hmre > 0, 
                               (noVehicle_hmow + noVehicle_hmre) / 
                                  (Households_hmow + Households_hmre),0),
         year = "2010") %>%
  dplyr::select(-Households_hmow,-Households_hmre,-noVehicle_hmow,-noVehicle_hmre,-bachelor)
#select the rest of the variables we don't use
#the results are now in data tracts10 :)

###############################plot data 18########################################
###################################################################################
# rent
tracts18_rent <-
  oritracts18 %>%
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
  oritracts18 %>%
  filter(variable == "B19013_001")

plot(tracts18_income[,4])

# population
tracts18_pop <-
  oritracts18 %>%
  filter(variable == "B01003_001")

plot(tracts18_pop[,4])

# Bachelor's
tracts18_bach <-
  oritracts18 %>%
  filter(variable == "B06009_005")

plot(tracts18_bach[,4])

# No. Vehicle (home owner)
tracts18_noVehicleOwner <-
  oritracts18 %>%
  filter(variable == "B25044_003")

plot(tracts18_noVehicleOwner[,4])

# No. Vehicle (renter)
tracts18_noVehicleRenter <-
  oritracts18 %>%
  filter(variable == "B25044_010")

plot(tracts18_noCarRenter[,4])

##############################long form to wide form###############################
tracts18 <- 
  oritracts18 %>%
  dplyr::select( -NAME, -moe) %>%
  spread(variable, estimate) %>%
  dplyr::select(-geometry) %>%
  rename(Rent = B25058_001, 
         medHHInc = B19013_001,
         population = B01003_001, 
         bachelor = B06009_005,
         noVehicle_hmow = B25044_003, 
         noVehicle_hmre  = B25044_010,
         Households_hmow = B07013_002,
         Households_hmre = B07013_003)

st_drop_geometry(tracts18)[1:3,]

#GEOID population bachelor Households_hmow Households_hmre medHHInc noVehicle_hmow noVehicle_hmre Rent
#1 25025000100       5324     1193            1244            4026    79597             12            310 1787
#2 25025000201       3991     1498            1087            2834    88424             37            142 1956
#3 25025000202       4272      967            1143            3046    83000             23            192 1475
###################################mutate##########################################
tracts18 <- 
  tracts18 %>%
  mutate(pctBach = ifelse(population > 0, bachelor / population, 0),
         pctNoVehicle = ifelse(Households_hmow + Households_hmre > 0, 
                               (noVehicle_hmow + noVehicle_hmre) / 
                                 (Households_hmow + Households_hmre),0),
         year = "2018") %>%
  dplyr::select(-Households_hmow,-Households_hmre,-noVehicle_hmow,-noVehicle_hmre,-bachelor)

###################################bind 2010 and 2018#############################
#in order to combine, we need to have exactly the same column names 
allTracts <- rbind(tracts10,tracts18)

##################################Remove non-Boston tracts######################
bosTracts <- c("25025010405", "25025010404", "25025010801", "25025010702", "25025010204", 
"25025010802", "25025010104", "25025000703", "25025000504", "25025000704", "25025010103", 
"25025000803", "25025980300", "25025120201", "25025120104", "25025110607", "25025000302", 
"25025000301", "25025140400", "25025140300", "25025140201", "25025140202", "25025140102", 
"25025130402", "25025130300", "25025130200", "25025130100", "25025120700", "25025120600", 
"25025120500", "25025120400", "25025110601", "25025110502", "25025110501", "25025110401", 
"25025101102", "25025101101", "25025101002", "25025101001", "25025100900", "25025100800", 
"25025100601", "25025100500", "25025100400", "25025100300", "25025981300", "25025981201", 
"25025990101", "25025981501", "25025981700", "25025981800", "25025100200", "25025100100", 
"25025092400", "25025092300", "25025092200", "25025092000", "25025091900", "25025091800", 
"25025091700", "25025091600", "25025981100", "25025140105", "25025980700", "25025120105", 
"25025120301", "25025071201", "25025091001", "25025091500", "25025091400", "25025091300", 
"25025091200", "25025091100", "25025090700", "25025090600", "25025090400", "25025090300", 
"25025090200", "25025980101", "25025040801", "25025010203", "25025110403", "25025110201", 
"25025981000", "25025090100", "25025082100", "25025082000", "25025081900", "25025081800", 
"25025081700", "25025081500", "25025081400", "25025081300", "25025110103", "25025110301", 
"25025140106", "25025010701", "25025010408", "25025000503", "25025081200", "25025081100", 
"25025080900", "25025080801", "25025080601", "25025080500", "25025080401", "25025080300", 
"25025071101", "25025070900", "25025140107", "25025130404", "25025130406", "25025120103", 
"25025100700", "25025100603", "25025092101", "25025061101", "25025070800", "25025070700", 
"25025070600", "25025070500", "25025070300", "25025070200", "25025070101", "25025061200", 
"25025061000", "25025060800", "25025060700", "25025080100", "25025060301", "25025090901", 
"25025060101", "25025981502", "25025060600", "25025060400", "25025060200", "25025051200", 
"25025050700", "25025050600", "25025050500", "25025050400", "25025050300", "25025050200", 
"25025040300", "25025040200", "25025030500", "25025981600", "25025051101", "25025051000", 
"25025030400", "25025030300", "25025030200", "25025030100", "25025020200", "25025010600", 
"25025010500", "25025010300", "25025000802", "25025000701", "25025050101", "25025050901", 
"25025060501", "25025981202", "25025040100", "25025040600", "25025000602", "25025000601", 
"25025000502", "25025000402", "25025000401", "25025000202", "25025000201", "25025040401", 
"25025020303", "25025070402", "25025020302", "25025020301", "25025020101", "25025081001", 
"25025010403", "25025000100")

allTractsBos <- 
  subset(allTracts, GEOID %in% bosTracts)


#################################transit data######################################
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

mbtaNode <- st_read("/Users/annaduan/Documents/GitHub/TOD-Assignment/mbta_node.geojson") %>% st_transform(st_crs(allTractsBos)) 
#mbtaNode <- st_read("E:/Upenn/CPLN508/TOD-Assignment/mbta_node.geojson") %>% st_transform(st_crs(allTractsBos)) 

############################################Exclude MBTA stops outside Boston############################################
bosStations <-
  mbtaNode %>%
  filter(station != "Alewife" & station != "Assembly" & station != "Beachmont" & station !=  "Beaconsfield" & station !=  "Bellingham Square" & station !=  "Box District" & station !=  
         "Braintree" & station !=  "Brandon Hall" & station !=  "Brookline Village" & station !=  "Brookline Hills" & station !=  "Capen Street" & station !=  "Central" & station !=  
         "Central Avenue" & station !=  "Chelsea" & station !=  "Chestnut Hill" & station !=  "Coolidge Corner" & station !=  "Davis" & station !=  "Dean Road" & station !=  "Eastern Avenue" & station !=  
         "Eliot" & station !=  "Englewood Avenue" & station !=  "Fairbanks Street" & station !=  "Harvard" & station !=  "Hawes Street" & station !=  "Kendall/MIT" & station !=  "Kent Street" & station !=  "Longwood" & station !=  
         "Malden Center" & station !=  "Milton" & station !=  "Newton Centre" & station !=  "Newton Highlands" & station !=  "North Quincy" & station !=  "Oak Grove" & station !=  "Porter" & station !=  
         "Quincy Adams" & station !=  "Quincy Center" & station !=  "Revere Beach" & station !=  "Riverside" & station !=  "Saint Marys Street" & station !=  "Saint Paul Street" & station !=  
         "Summit Avenue" & station !=  "Tappan Street" & station !=  "Valley Road" & station !=  "Waban" & station !=  "Washington Square" & station !=  "Wellington" & station !=  "Wollaston" & station !=  
         "Wonderland" & station !=  "Woodland" & station !=  "Bellingham Sq"  & station !=  "Eastern Ave"  & station !=  "Brandon Hall" & station != "Summit Ave/Winchester St" & station != "Lechmere")

# Station plot
ggplot() + 
  geom_sf(data=st_union(tracts10)) +
  geom_sf(data=bosStations, aes(colour = line), show.legend = "point", size= 2) +
  scale_colour_manual(values = c("blue","dark green","brown","green","purple","black","yellow","pink","red","gray")) +
  labs(title="Subway Stops", subtitle="Boston, MA", caption="Figure 1.1") +
  mapTheme()
 
####################################set buffer##################################
bosBuffers <- 
  rbind(
    st_buffer(bosStations, 2640) %>% #in feet
      mutate(Legend = "Buffer") %>%
      dplyr::select(Legend),
    st_union(st_buffer(bosStations, 2640)) %>% #union buffer
      st_sf() %>%
      mutate(Legend = "Unioned Buffer"))

#The resulting 'small multiple' map is only possible when data is organized in long form.
ggplot() +
  geom_sf(data=st_union(allTractsBos)) +
  geom_sf(data=bosBuffers) +
  geom_sf(data=bosStations, show.legend = "point") +
  facet_wrap(~Legend) +  #wrap by years and make small multiple plots
  labs(caption = "Figure 1.2") +
  mapTheme()

#############################spatial operation##################################
buffer <- filter(bosBuffers, Legend=="Unioned Buffer")

selectCentroids <-
  st_centroid(allTractsBos)[buffer,] %>%
    st_drop_geometry() %>%
    left_join(dplyr::select(allTractsBos, GEOID)) %>%
    st_sf() %>%
    dplyr::select(year, population, Rent) %>%
    mutate(Selection_Type = "Select by Centroids")

ggplot(selectCentroids)+
  geom_sf(data = st_union(allTractsBos))+
  geom_sf(aes(fill = q5(population))) +
  scale_fill_manual(values = palette5,
                    labels = qBr(selectCentroids, "population"),
                    name = "Population\n(Quintile Breaks)") +
  labs(title = "Total Population", subtitle = "Boston; 2010") +
  facet_wrap(~Selection_Type)+
  mapTheme() + 
  theme(plot.title = element_text(size=22))



###map the centroids to show population change
#?what to do if the maps in 2010 and 2018 are different? should we combine the two
#?or just present the two seperately?
##AD: I think it's fine - it still works for facet wrap
######################10#########################
selectCentroids10 <-
  st_centroid(tracts10)[buffer,] %>%
    st_drop_geometry() %>%
    left_join(dplyr::select(tracts10, GEOID)) %>%
    st_sf() %>%
    dplyr::select(year, population, Rent) %>%
    mutate(Selection_Type = "Select by Centroids")

selectCentroids10$Rent <- as.numeric(selectCentroids10$Rent)
#PLOT POPULATION
ggplot(selectCentroids10)+
  geom_sf(data = st_union(allTractsBos))+
  geom_sf(aes(fill = q5(population))) +
  scale_fill_manual(values = palette5,
                    labels = qBr(selectCentroids10, "population"),
                    name = "Popluation\n(Quintile Breaks)") +
  labs(title = "Total Population", subtitle = "Boston; 2010") +
  facet_wrap(~Selection_Type)+
  mapTheme() + 
  theme(plot.title = element_text(size=22))


#########################18########################
selectCentroids18 <-
  st_centroid(tracts18)[buffer,] %>%
  st_drop_geometry() %>%
  left_join(dplyr::select(tracts18, GEOID)) %>%
  st_sf() %>%
  dplyr::select(year, population, Rent) %>%
  mutate(Selection_Type = "Select by Centroids")

#PLOT POPULATION
ggplot(selectCentroids18)+
  geom_sf(data = st_union(allTractsBos))+
  geom_sf(aes(fill = q5(population))) +
  scale_fill_manual(values = palette5,
                    labels = qBr(selectCentroids18, "population"),
                    name = "Popluation\n(Quintile Breaks)") +
  labs(title = "Total Population", subtitle = "Boston; 2018") +
  facet_wrap(~Selection_Type)+
  mapTheme() + 
  theme(plot.title = element_text(size=22))

###########################INDICATOR MAPS########################################
#In other words, $100 in 2010 is equivalent in purchasing power to about $115.21 in 2018
allTracts.group <- 
  rbind(
    st_centroid(allTractsBos)[buffer,] %>%
      st_drop_geometry() %>%
      left_join(allTractsBos) %>%
      st_sf() %>%
      mutate(TOD = "TOD"),
    st_centroid(allTractsBos)[buffer, op = st_disjoint] %>%
      st_drop_geometry() %>%
      left_join(allTractsBos) %>%
      st_sf() %>%
      mutate(TOD = "Non-TOD")) %>%
  mutate(Rent.inf = ifelse(year == "2010", Rent * 1.1521, Rent))

################10~18###############################

#1: Population Map
ggplot(allTracts.group)+
  geom_sf(data = st_union(allTractsBos))+
  geom_sf(aes(fill = q5(population))) +
  geom_sf(data = buffer, fill = "transparent", color = "red")+
  scale_fill_manual(values = palette5,
                    labels = qBr(allTracts.group, "population"),
                    name = "Population\n(Quintile Breaks, TOD in Red)") +
  labs(title = "Total Population 2010-2018", subtitle = "Boston") +
  facet_wrap(~year)+
  mapTheme() + 
  theme(plot.title = element_text(size=22))

#2: Bachelor's Map
ggplot(allTracts.group)+
  geom_sf(data = st_union(allTractsBos))+
  geom_sf(aes(fill = q5(pctBach))) +
  geom_sf(data = buffer, fill = "transparent", color = "red")+
  scale_fill_manual(values = palette5,
                    labels = qBr(allTracts.group, "pctBach"),
                    name = "% Adults >25 Years with Bachelor's\n(Quintile Breaks, TOD in Red)") +
  labs(title = "Share of Adults with Bachelor's Degree 2010-2018", subtitle = "Boston") +
  facet_wrap(~year)+
  mapTheme() + 
  theme(plot.title = element_text(size=22))

#3: Income Map 
ggplot(allTracts.group)+
  geom_sf(data = st_union(allTractsBos))+
  geom_sf(aes(fill = q5(medHHInc))) +
  geom_sf(data = buffer, fill = "transparent", color = "red")+
  scale_fill_manual(values = palette5,
                    labels = qBr(allTracts.group, "medHHInc"),
                    name = "Median HH Income\n(Quintile Breaks, TOD in Red)") +
  labs(title = "Median Household Income 2010-2018", subtitle = "Boston") +
  facet_wrap(~year)+
  mapTheme() + 
  theme(plot.title = element_text(size=22))

#4: No Vehicle Map
ggplot(allTracts.group)+
  geom_sf(data = st_union(allTractsBos))+
  geom_sf(aes(fill = q5(pctNoVehicle))) +
  geom_sf(data = buffer, fill = "transparent", color = "red")+
  scale_fill_manual(values = palette5,
                    labels = qBr(allTracts.group, "pctNoVehicle"),
                    name = "% Households without Vehicle\n(Quintile Breaks, TOD in Red)") +
  labs(title = "Share of Households without a Vehicle 2010-2018", subtitle = "Percentage") +
  facet_wrap(~year)+
  mapTheme() + 
  theme(plot.title = element_text(size=22))

#5: Rent Map
ggplot(allTracts.group)+
  geom_sf(data = st_union(allTractsBos))+
  geom_sf(aes(fill = q5(Rent.inf))) +
  geom_sf(data = buffer, fill = "transparent", color = "red")+
  scale_fill_manual(values = palette5,
                    labels = qBr(allTracts.group, "Rent.inf"),
                    name = "Rent\n(Quintile Breaks, TOD in Red)") +
  labs(title = "Median Rent 2010-2018", subtitle = "Real Dollars") +
  facet_wrap(~year)+
  mapTheme() + 
  theme(plot.title = element_text(size=22))

###########################TOD Indicator Tables################################

allTracts.Summary <- 
  st_drop_geometry(allTracts.group) %>%
  group_by(year, TOD) %>%
  summarize(Rent = mean(Rent, na.rm = T),
            Population = mean(population, na.rm = T),
            pctBach = mean(pctBach, na.rm = T),
            pctNoVehicle = mean(pctNoVehicle, na.rm = T),
            MedHHInc = mean(medHHInc, na.rm = T))

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

downtown <-
  st_intersection(
    st_buffer(filter(bosStations, line == "ORANGE"), 2640) %>% st_union(),
    st_buffer(filter(bosStations, line == "RED"), 2640) %>% st_union(),
    st_buffer(filter(bosStations, line == "SILVER"), 2640) %>% st_union()) %>%
  st_sf() %>%
  mutate(Submarket = "downtown")

blue <-
  st_buffer(filter(bosStations, line == "BLUE"), 2640) %>% st_union() %>%
  st_sf() %>%
  st_difference(downtown) %>%
  mutate(Submarket = "blue")

orange <-
  st_buffer(filter(bosStations, line == "ORANGE"), 2640) %>% st_union() %>%
  st_sf() %>%
  st_difference(downtown) %>%
  mutate(Submarket = "orange")

red <-
  st_buffer(filter(bosStations, line == "RED"), 2640) %>% st_union() %>%
  st_sf() %>%
  st_difference(downtown) %>%
  mutate(Submarket = "red")

silver <-
  st_buffer(filter(bosStations, line == "SILVER"), 2640) %>% st_union() %>%
  st_sf() %>%
  st_difference(downtown) %>%
  mutate(Submarket = "silver")

green <-
  st_buffer(filter(bosStations, line == "GREEN"), 2640) %>% st_union() %>%
  st_sf() %>%
  st_difference(downtown) %>%
  mutate(Submarket = "green")

sixMarkets <- rbind(downtown, silver, red, green, orange, blue)

#problem: can't see downtown on the map?
##AD: silver is covering it, i'm not sure why we aren't able to get rid of the overlap..
ggplot() + 
  geom_sf(data=st_union(allTractsBos)) +
  geom_sf(data=silver, aes(colour = Submarket), show.legend = "point", size= 1) +
  scale_colour_manual(values = c("blue","black","green","orange","red","gray")) +
  labs(title="six submarkets", subtitle="Boston, MA", caption="Figure 1.3") +
  mapTheme()

# You can then bind these buffers to tracts and map them or make small multiple plots

allTracts.sixMarkets <-
  st_join(st_centroid(allTractsBos), sixMarkets) %>%
  st_drop_geometry() %>%
  left_join(allTractsBos) %>%
  mutate(Submarket = replace_na(Submarket, "Non-TOD")) %>%
  st_sf() 

ggplot() +
  geom_sf(data=st_union(allTracts.sixMarkets)) +
  geom_sf(data = allTracts.sixMarkets, aes(fill = q5(Rent))) +
  facet_wrap(~Submarket + year) + 
  scale_fill_manual(values = palette5,
                    labels = qBr(allTracts.sixMarkets, "Rent"),
                    name = "Rent\n(Quintile Breaks)") +
  labs(title = "Rent", subtitle = "Boston") +
  mapTheme() + theme(plot.title = element_text(size=18))


#spread goes long to wide, gather opposite
allTracts.sixMarkets.Summary <- 
  st_drop_geometry(allTracts.sixMarkets) %>%
  group_by(year, Submarket) %>%
  summarize(Rent = mean(Rent, na.rm = T),
            Population = mean(population, na.rm = T),
            pctBach = mean(pctBach, na.rm = T),
            pctNoVehicle = mean(pctNoVehicle, na.rm = T),
            MedHHInc = mean(medHHInc, na.rm = T))

kable(allTracts.sixMarkets.Summary) %>%
  kable_styling() %>%
  footnote(general_title = "\n",
           general = "Table 2.3")

########change to long form######
allTracts.sixMarkets.Summary %>%
  unite(year.Submarket, year, Submarket, sep = ": ", remove = T) %>%
  gather(Variable, Value, -year.Submarket) %>%
  mutate(Value = round(Value, 2)) %>%
  spread(year.Submarket, Value) %>%
  kable() %>%
  kable_styling() %>%
  footnote(general_title = "\n",
           general = "Table 2.3")


###########################TOD Indicator Plots##################################

allTracts.sixMarkets.Summary %>%
  gather(Variable, Value, -year, -Submarket) %>%
  ggplot(aes(year, Value, fill = Submarket)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Variable, scales = "free", ncol=5) +
  scale_fill_manual(values = c("#335BFF", "#000000", "#34D539", "#F9FD0A", "#FF7000", "#FF0000", "#CDCDCD")) +
  labs(title = "Indicator differences across time and space") +
  plotTheme() + theme(legend.position="bottom")


###############################graduate map#########################################
centectroi10_pop <- st_centroid(selectCentroids10[,1])
centectroi18_pop <- st_centroid(selectCentroids18[,1])
troi_pop <- rbind(selectCentroids10[,1:2], selectCentroids18[,1:2]) 
oitroi_pop.group <- st_centroid(troi_pop)
centectroi10_re <- st_centroid(selectCentroids10[,2])
centectroi18_re <- st_centroid(selectCentroids18[,2])
troi_rent <- rbind(selectCentroids10[,c(1,3)], selectCentroids18[,c(1,3)]) 
oitroi_rent.group <- st_centroid(troi_rent)

ggplot() + 
  geom_sf(data=st_union(allTracts.group)) +
  geom_sf(data = buffer, fill = "white") + 
  geom_sf(data = oitroi_pop.group, aes(size = population), shape = 21, 
      fill = "lightblue", alpha = 1, show.legend = "point") + 
  scale_size_continuous(range = c(0.1, 6))+
  facet_wrap(~year)+
  labs(title = "Population 2010-2018") +
  mapTheme() + 
  theme(plot.title = element_text(size=22))

ggplot() + 
  geom_sf(data=st_union(allTractsBos)) +
  geom_sf(data = buffer, fill = "white") + 
  geom_sf(data = oitroi_rent.group, aes(size = Rent), shape = 21, 
          fill = "pink", alpha = 1, show.legend = "point") + 
  scale_size_continuous(range = c(0.1, 6))+
  facet_wrap(~year)+
  labs(title = "Rent 2010-2018") +
  mapTheme() + 
  theme(plot.title = element_text(size=22))


#############################multi-ring buffer##################################

multiBuffers <- multipleRingBuffer(bosStations, 10, 0.5)
#buffernum <- subset(multiBuffers, distance = num)
# for 2010
rent_dis10 <- data.frame(
  distance = double(),
  meanrent = double(),
  stringsAsFactors=FALSE
)

for (i in seq(0.5,10, by = 0.5)){
  
  a <- st_union(st_buffer(bosStations, i*5280)) %>% st_sf()
  b <-
    st_centroid(trants10)[a,] %>%
    st_drop_geometry() %>%
    left_join(dplyr::select(tracts10, GEOID)) %>%
    st_sf() %>%
    dplyr::select(Rent)
  b$Rent <- as.numeric(b$Rent)
  c <- mean(b[["Rent"]],na.rm=TRUE)
  d <- data.frame(distance=i, meanrent=c, stringsAsFactors=FALSE)
  rent_dis10 <- rbind(rent_dis10, d)
#  rent_dis10 %>% add_row(distance = i, meanrent = c)
  i <- i + 0.5
}


# for 2018
rent_dis18 <- data.frame(
  distance = double(),
  meanrent = double(),
  stringsAsFactors=FALSE
)

for (i in seq(0.5,10, by = 0.5)){
  
  a <- st_union(st_buffer(bosStations, i*5280)) %>% st_sf()
  b <-
    st_centroid(tracts18)[a,] %>%
    st_drop_geometry() %>%
    left_join(dplyr::select(tracts18, GEOID)) %>%
    st_sf() %>%
    dplyr::select(Rent)
  b$Rent <- as.numeric(b$Rent)
  c <- mean(b[["Rent"]],na.rm=TRUE)
  d <- data.frame(distance=i, meanrent=c, stringsAsFactors=FALSE)
  rent_dis18 <- rbind(rent_dis18, d)
  #  rent_dis10 %>% add_row(distance = i, meanrent = c)
  i <- i + 0.5
}

##?maybe we should edit the color and size of the lines?

ggplot(rent_dis10, aes(x=distance, y=meanrent)) +
  geom_line(arrow = arrow())+
  geom_point()
ggplot(rent_dis18, aes(x=distance, y=meanrent)) +
  geom_line(arrow = arrow())+
  geom_point()

#if we have moe data for rent

ggplot(df3, aes(x=dose, y=len, group=supp, color=supp)) + 
  geom_errorbar(aes(ymin=len-sd, ymax=len+sd), width=.1, 
                position=position_dodge(0.05)) +
  geom_line() + geom_point()+
  scale_color_brewer(palette="Paired")+theme_minimal()

#from help
# Setting line type vs colour/size
# Line type needs to be applied to a line as a whole, so it can
# not be used with colour or size that vary across a line
x <- seq(0.01, .99, length.out = 100)
df <- data.frame(
  x = rep(x, 2),
  y = c(qlogis(x), 2 * qlogis(x)),
  group = rep(c("a","b"),
              each = 100)
)
p <- ggplot(df, aes(x=x, y=y, group=group))
# These work
p + geom_line(linetype = 2)
p + geom_line(aes(colour = group), linetype = 2)
p + geom_line(aes(colour = x))
# But this doesn't
should_stop(p + geom_line(aes(colour = x), linetype=2))
####################################Crime Data############################################
crime2012 <- st_read("/Users/annaduan/Documents/GitHub/TOD-Assignment/Crime2012.geojson") %>% st_transform(st_crs(allTractsBos))
crime2012_sf <- st_as_sf(crime2012, coords = c("Location"), crs = 4326) %>%
  st_transform('ESRI:102686')
#are we doing 2012-2019 in crime data?
# AD: I think so
#crime2012 <- st_read("E:/Upenn/CPLN508/TOD-Assignment/Crime2012.geojson") %>% st_transform(st_crs(allTractsBos))
#crime2019 <- st_read("E:/Upenn/CPLN508/TOD-Assignment/Crime2019.geojson") %>% st_transform(st_crs(allTractsBos))


