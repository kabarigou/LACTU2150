
library(sf)       # Handle spatial data (shapefiles and geometries)
library(dplyr)    # Manipulate and transform datasets
library(ggplot2)  # Create static maps and visualizations
library(tmap)     # Create thematic maps, including interactive maps

# Shapefiles for Belgium can be downloaded from:
# https://www.geo.be/catalog/details/1cd00540-b0ca-11ec-a863-186571a04de2?l=fr

## --- 1. Load geographical data -----------------------------------------------

# Read the shapefile containing the geographical boundaries of Belgian
# postal districts. Each observation represents a geographical polygon.
be_pc <- st_read("shapefiles/postaldistrict_3812.shp", quiet = TRUE)

# Convert coordinates to WGS84 (longitude/latitude), a widely used
# coordinate reference system for geographical mapping.
be_pc <- st_transform(be_pc, crs = 4326)

# Display variable names to identify the postal-code column.
names(be_pc)


## --- 2. Create population data ------------------------------------------------

# Option A (real data): import population figures from Statbel.
# pop <- read.csv("statbel_population_by_postcode.csv")

# Option B (simulated data): generate a random population for each
# unique postal code, so that the example can run without external data.
set.seed(42)  # Ensure reproducibility of the simulated data

pop <- be_pc %>%
  st_drop_geometry() %>%   # Remove geographical information
  distinct(postcode) %>%  # Keep one observation per postal code
  mutate(pop = round(runif(n(), 500, 40000)))  # Generate random population


## --- 3. Combine geographical and population data -----------------------------

# Join population figures to the geographical data using postal codes.
# Each postal code receives its corresponding population value.
be_map <- left_join(
  be_pc,
  pop,
  by = c("postcode" = "postcode"),
  relationship = "many-to-one"
)


# Calculate the area of each polygon in km².
be_map$area_km2 <- as.numeric(st_area(be_map)) / 1e6

# Calculate population density (inhabitants per km²).
# Density is a rate that accounts for differences in geographical area.
be_map$density <- be_map$pop / be_map$area_km2


## --- 4. Classify population density ------------------------------------------

# Divide postal districts into three density classes using quantiles:
# - Low: bottom 20% of density values
# - Average: middle 60%
# - High: top 20%
be_map$dens_class <- cut(
  be_map$density,
  breaks = quantile(be_map$density, c(0, 0.2, 0.8, 1), na.rm = TRUE),
  right = FALSE,
  include.lowest = TRUE,
  labels = c("low", "average", "high")
)


## --- 5. Create a static map using ggplot2 ------------------------------------

# Draw geographical polygons and colour them according to their
# population-density class.
ggplot(be_map) +
  geom_sf(aes(fill = dens_class), colour = "black", size = 0.1) +
  scale_fill_brewer(palette = "Blues", na.value = "white") +
  labs(
    title = "Population density by postal code (Belgium)",
    fill = "inhab./km2"
  ) +
  theme_bw()


## --- 6. Create a thematic map using tmap -------------------------------------

## --- 6. Create a thematic map using tmap ----------------------------

# Create a thematic map with postal districts coloured by density class.
my_map <- tm_shape(be_map) +
  tm_borders(col = "black") +
  tm_fill(
    col = "dens_class",
    style = "cat",
    palette = "Blues",
    colorNA = "white"
  )

# Display the map
my_map

# Convert the map into an interactive Leaflet map.
tmap_leaflet(my_map)
