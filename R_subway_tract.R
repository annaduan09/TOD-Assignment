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
# Tracts, Median Rent, MHHINC, Population, GRad/Prof Degree, No. Vehicle (home owner, renter)
## projection (NAD 1983 StatePlane Massachusetts Mainland FIPS 2001 Feet)
tracts10 <-  #BC tracts12 maybe?
  get_acs(geography = "tract", variables = c("B25058_001E", "B19013_001E", "B01003_001E", "B20004_006E", 
                                             "B25044_003E", "B25044_010E"), 
          year=2010, state=25, county=025, geometry=T) %>% 
  st_transform('ESRI:102686')

#BC not sure if we do the year 2012 and 2019 or?
#BC https://en.wikipedia.org/wiki/MBTA_subway 
#BC I think our focus will be mainly on silver line, which started on 2002

tracts19 <-  
  get_acs(geography = "tract", variables = c("B25058_001E", "B19013_001E", "B01003_001E", "B20004_006E", 
                                             "B25044_003E", "B25044_010E"), 
          year=2019, state=25, county=025, geometry=T) %>% 
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
# to be filled

# population
# to be filled 

# GRad/Prof Degree
# BC: should we use bachelor degree instead of...?
# to be filled

# No. Vehicle (home owner)
# to be filled

# No. Vehicle (renter)
# to be filled

##############################long form to wide form###############################
tracts10 <- 
  tracts10 %>%
  dplyr::select( -NAME, -moe) %>%
  spread(variable, estimate) %>%
  dplyr::select(-geometry) %>%
  rename(Rent = B25058_001, 
         MedHHInc = B19013_001,
         Population = B01003_001, 
         GPDegree = B20004_006,
         Vehicle_hmow = B25044_003, 
         Vehicle_hmre = B25044_010)

st_drop_geometry(tracts10)[1:3,]

###############################plot data 19########################################
###################################################################################
