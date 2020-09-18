# We're looking at the relationship between TOD, rent, and crime rate in Boston, MA
# within the time frame of 2012-2018
# Projected CRS: NAD_1983_StatePlane_Massachusetts_Mainland_FIPS_2001_Feet

# Libraries
library(tidyverse)
library(tidycensus)
library(sf)
library(kableExtra)

options(scipen=999)
options(tigris_class = "sf")

# Map/Plot Formatting
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

# Input API key
census_api_key("d9ebfd04caa0138647fbacd94c657cdecbf705e9", install = TRUE, overwrite = TRUE)

# Tracts, Median Rent, MHHINC, Population, GRad/Prof Degree, No Vehicle (home owner, renter)
## projection (NAD 1983 StatePlane Massachusetts Mainland FIPS 2001 Feet)
tracts10 <-  
  get_acs(geography = "tract", variables = c("B25058_001E", "B19013_001E", "B01003_001E", "B20004_006E", 
                                             "B25044_003E", "B25044_010E"), 
                year=2010, state=25, county=025, geometry=T) %>% 
  st_transform('ESRI:102686')

# Tester plots

totalPop10 <-
  tracts10 %>%
  filter(variable == "B01003_001")

plot(totalPop10)

NetPop <- 
  ggplot() +
  geom_sf(data = totalPop10, aes(fill = q5(estimate))) +
  scale_fill_manual(values = palette5,
                    labels = qBr(totalPop10, "estimate"),
                    name = "Population\n(Quintile Breaks)") +
  labs(title = "Total Population", subtitle = "Boston; 2010") +
  mapTheme() + theme(plot.title = element_text(size=22))

# Spread Tables
tracts10 <- 
  tracts10 %>%
  dplyr::select( -NAME, -moe) %>%
  spread(variable, estimate) %>%
  dplyr::select(-geometry) %>%
  rename(MedianRent = B25058_001, 
         MedHHInc = B19013_001,
         Population = B01003_001, 
         GradorProfDegree = B20004_006,
         NoVehicleOwner = B25044_003, 
         NoVehicleRenter = B25044_010)

st_drop_geometry(tracts10)[1:3,]

# 
