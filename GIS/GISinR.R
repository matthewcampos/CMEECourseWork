library(raster)
library(sf)
library(viridis)
library(units)
rm(list=ls())

pop_dens <- data.frame(n_km2 = c(260,67,151,4500,133), country = c("England", "Scotland", "Wales", "London", "Northern Ireland"))
pop_dens

# Create coordinates  for each country 
# -  this is a list of sets of coordinates forming the edge of the polygon. 
# - note that they have to _close_ (have the same coordinate at either end)
scotland <- rbind(c(-5, 58.6), c(-3, 58.6), c(-4, 57.6), 
                  c(-1.5, 57.6), c(-2, 55.8), c(-3, 55), 
                  c(-5, 55), c(-6, 56), c(-5, 58.6))
england <- rbind(c(-2,55.8),c(0.5, 52.8), c(1.6, 52.8), 
                 c(0.7, 50.7), c(-5.7,50), c(-2.7, 51.5), 
                 c(-3, 53.4),c(-3, 55), c(-2,55.8))
wales <- rbind(c(-2.5, 51.3), c(-5.3,51.8), c(-4.5, 53.4),
               c(-2.8, 53.4),  c(-2.5, 51.3))
ireland <- rbind(c(-10,51.5), c(-10, 54.2), c(-7.5, 55.3),
                 c(-5.9, 55.3), c(-5.9, 52.2), c(-10,51.5))

# Convert these coordinates into feature geometries
# - these are simple coordinate sets with no projection information
scotland <- st_polygon(list(scotland))
england <- st_polygon(list(england))
wales <- st_polygon(list(wales))
ireland <- st_polygon(list(ireland))

# Combine geometries into a simple feature column
uk_eire <- st_sfc(wales, england, scotland, ireland, crs=4326) #sfc allows us to set the coordinate reference system 
plot(uk_eire, asp=1)

uk_eire_capitals <- data.frame(long= c(-0.1, -3.2, -3.2, -6.0, -6.25),
                               lat=c(51.5, 51.5, 55.8, 54.6, 53.30),
                               name=c('London', 'Cardiff', 'Edinburgh', 'Belfast', 'Dublin'))
uk_eire_capitals <- st_as_sf(uk_eire_capitals, coords=c('long','lat'), crs=4326)

st_pauls <- st_point(x=c(-0.098056, 51.513611)) #sets coordinates 
london <- st_buffer(st_pauls, 0.25) #buffer creates polygon

england_no_london <- st_difference(england, london) #order matters as we want england that differs from london
lengths(scotland)
lengths(england_no_london)
plot(england_no_london, asp=1)

wales <- st_difference(wales, england)
a <- st_sfc(wales,england,crs = 4326)
plot(wales,asp=1)
plot(a)

# A rough polygon that includes Northern Ireland and surrounding sea.
# - not the alternative way of providing the coordinates
ni_area <- st_polygon(list(cbind(x=c(-8.1, -6, -5, -6, -8.1), y=c(54.4, 56, 55, 54, 54.4))))
northern_ireland <- st_intersection(ireland, ni_area)
eire <- st_difference(ireland, ni_area) #difference of ireland from NI so removes NI
plot(eire)

# Combine the final geometries
uk_eire <- st_sfc(wales, england_no_london, scotland, london, northern_ireland, eire, crs=4326)
plot(uk_eire, asp=1)

# Make the UK into a single feature 
uk_country <- st_union(uk_eire[-6]) #feature is a set of vector GIS geometries that represent a spatial unit we are interested in
print(uk_country) #[-6] gets rid of the 6th vector 
print(uk_eire)
plot(uk_country, asp=1)
# Plot them
par(mfrow=c(1, 2), mar=c(3,3,1,1))
plot(uk_eire, asp=1, col=rainbow(6))
plot(st_geometry(uk_eire_capitals), add=TRUE) #adds captials to uk_eire
plot(uk_country, asp=1, col='lightblue')

# add features to vector geometries
uk_eire <- st_sf(name=c('Wales', 'England','Scotland', 'London', 
                        'Northern Ireland', 'Eire'),
                 geometry=uk_eire) #st_sf creates sf which extends df with a sf column and can add attributes
plot(uk_eire, asp=1)

uk_eire$capital <- c('London', 'Edinburgh', 'Cardiff', NA, 'Belfast', 'Dublin') #order matters depending on df_sf table

uk_eire <- merge(uk_eire, pop_dens, by.x='name', by.y='country', all.x=TRUE)
print(uk_eire)

uk_eire_centroids <- st_centroid(uk_eire)
st_coordinates(uk_eire_centroids)

uk_eire$area <- st_area(uk_eire)
uk_eire$length <- st_length(uk_eire)
print(uk_eire)
plot(uk_eire[1], asp=1)
uk_eire$area <- set_units(uk_eire$area, 'km^2') # You can change units in a neat way
uk_eire$length <- set_units(uk_eire$length, 'km')
# will not allow errors in unit conversion
uk_eire$area <- set_units(uk_eire$area, 'kg')
#convert length to numeric
uk_eire$length <- as.numeric(uk_eire$length)
print(uk_eire)
st_distance(uk_eire)
st_distance(uk_eire_centroids)

plot(uk_eire['n_km2'], asp=1)
uk_eire$log_n_km2 <- log10(uk_eire$n_km2)
plot(uk_eire['log_n_km2'], asp=1)
plot(uk_eire['n_km2'], asp=1, logz=TRUE)

# British National Grid (EPSG:27700)
uk_eire_BNG <- st_transform(uk_eire, 27700)
st_bbox(uk_eire)
st_bbox(uk_eire_BNG)
# UTM50N (EPSG:32650)
uk_eire_UTM50N <- st_transform(uk_eire, 32650)
par(mfrow=c(1, 3), mar=c(3,3,1,1))
plot(st_geometry(uk_eire), asp=1, axes=TRUE, main='WGS 84')
plot(st_geometry(uk_eire_BNG), axes=TRUE, main='OSGB 1936 / BNG')
plot(st_geometry(uk_eire_UTM50N), axes=TRUE, main='UTM 50N')

# Set up some points separated by 1 degree latitude and longitude from St. Pauls
st_pauls <- st_sfc(st_pauls, crs=4326)
one_deg_west_pt <- st_sfc(st_pauls - c(1, 0), crs=4326) # near Goring
one_deg_north_pt <-  st_sfc(st_pauls + c(0, 1), crs=4326) # near Peterborough
st_distance(st_pauls, one_deg_west_pt)
st_distance(st_pauls, one_deg_north_pt)
st_distance(st_transform(st_pauls, 27700), st_transform(one_deg_west_pt, 27700))
#fix london 
london_bng <- st_buffer(st_transform(st_pauls, 27700), 25000) #buffer creates separate polygon around features and transform changes crs
england_not_london_bng <- st_difference(st_transform(st_sfc(england, crs=4326), 27700), london_bng)
others_bng <- st_transform(st_sfc(eire, northern_ireland, scotland, wales, crs=4326), 27700)
corrected <- c(others_bng, london_bng, england_not_london_bng)
par(mar=c(3,3,1,1))
plot(corrected, main='25km radius London', axes=TRUE)

# Create an empty raster object covering UK and Eire
uk_raster_WGS84 <- raster(xmn=-11,  xmx=2,  ymn=49.5, ymx=59, 
                          res=0.5, crs="+init=EPSG:4326")
hasValues(uk_raster_WGS84)


# Add data to the raster: just the number 1 to number of cells
values(uk_raster_WGS84) <- seq(length(uk_raster_WGS84))
par(mfrow=c(1,1))
plot(uk_raster_WGS84)
plot(st_geometry(uk_eire), add=TRUE, border='black', lwd=2, col='#FFFFFF44') #true adds existing vector data to existing map

m <- matrix(c(1, 1, 3, 3,
              1, 2, 4, 3,
              5, 5, 7, 8,
              6, 6, 7, 7), ncol=4, byrow=TRUE)
square <- raster(m)

# Maximum values
square_agg_max <- aggregate(square, fact=2, fun=max)
values(square_agg_max)

# Average values
square_agg_mean <- aggregate(square, fact=2, fun=mean)
values(square_agg_mean)

# Modal values for categories
square_agg_modal <- aggregate(square, fact=2, fun=modal)
values(square_agg_modal) ## The bottom left cell has a modal value of 6 even though there is no mode: there are two 5s and two 6s. You can use `first` and `last` to specify which value gets chose but there is no mode.

# Copy parents
square_disagg <- disaggregate(square, fact=2)
# Interpolate
square_disagg_interp <- disaggregate(square, fact=2, method='bilinear')

# make two simple `sfc` objects containing points in  the
# lower left and top right of the two grids
uk_pts_WGS84 <- st_sfc(st_point(c(-11, 49.5)), st_point(c(2, 59)), crs=4326)
uk_pts_BNG <- st_sfc(st_point(c(-2e5, 0)), st_point(c(7e5, 1e6)), crs=27700)

#  Use st_make_grid to quickly create a polygon grid with the right cellsize
uk_grid_WGS84 <- st_make_grid(uk_pts_WGS84, cellsize=0.5)
uk_grid_BNG <- st_make_grid(uk_pts_BNG, cellsize=1e5)

# Reproject BNG grid into WGS84
uk_grid_BNG_as_WGS84 <- st_transform(uk_grid_BNG, 4326)

# Plot the features
plot(uk_grid_WGS84, asp=1, border='grey', xlim=c(-13,4))
plot(st_geometry(uk_eire), add=TRUE, border='darkgreen', lwd=2)
plot(uk_grid_BNG_as_WGS84, border='red', add=TRUE)

# Create the target raster
uk_raster_BNG <- raster(xmn=-200000, xmx=700000, ymn=0, ymx=1000000,
                        res=100000, crs='+init=EPSG:27700')
uk_raster_BNG_interp <- projectRaster(uk_raster_WGS84, uk_raster_BNG, method='bilinear')
uk_raster_BNG_ngb <- projectRaster(uk_raster_WGS84, uk_raster_BNG, method='ngb')
# compare the values in the top row
round(values(uk_raster_BNG_interp)[1:9], 2)
## [1]    NA 31.36 30.02 29.87 30.91 33.14 36.56 41.17    NA

values(uk_raster_BNG_ngb)[1:9]
## [1] NA 29 33 36 39 43 46 50 NA

par(mfrow=c(1,3), mar=c(1,1,2,1))
plot(uk_raster_BNG_interp, main='Interpolated', axes=FALSE, legend=FALSE)
plot(uk_raster_BNG_ngb, main='Nearest Neighbour',axes=FALSE, legend=FALSE)

# Create the target raster 
uk_20km <- raster(xmn=-200000, xmx=650000, ymn=0, ymx=1000000, 
                  res=20000, crs='+init=EPSG:27700')

# Rasterizing polygons
uk_eire_poly_20km  <- rasterize(as(uk_eire_BNG, 'Spatial'), uk_20km, field='name')
plot(uk_eire_poly_20km)

st_agr(uk_eire_BNG) <- 'constant' #depends on what data you are rasterising i.e. data of coordinates is constant but not temperature distributions

# Rasterizing lines
uk_eire_BNG_line <- st_cast(uk_eire_BNG, 'LINESTRING')
plot(uk_eire_BNG_line)
uk_eire_line_20km <- rasterize(as(uk_eire_BNG_line, 'Spatial'), uk_20km, field='name')
plot(uk_eire_line_20km)

# Rasterizing points 
# - This isn't quite as neat. You need to take two steps in the cast and need to convert 
#   the name factor to numeric.
uk_eire_BNG_point <- st_cast(st_cast(uk_eire_BNG, 'MULTIPOINT'), 'POINT')
uk_eire_BNG_point$name <- as.numeric(uk_eire_BNG_point$name)
uk_eire_point_20km <- rasterize(as(uk_eire_BNG_point, 'Spatial'), uk_20km, field='name')

# Plotting those different outcomes
par(mfrow=c(1,3), mar=c(1,1,1,1))
plot(uk_eire_poly_20km, col=viridis(6, alpha=0.5), legend=FALSE, axes=FALSE)
plot(st_geometry(uk_eire_BNG), add=TRUE, border='grey')

plot(uk_eire_line_20km, col=viridis(6, alpha=0.5), legend=FALSE, axes=FALSE)
plot(st_geometry(uk_eire_BNG), add=TRUE, border='grey')

plot(uk_eire_point_20km, col=viridis(6, alpha=0.5), legend=FALSE, axes=FALSE)
plot(st_geometry(uk_eire_BNG), add=TRUE, border='grey')

# rasterToPolygons returns a polygon for each cell and returns a Spatial object
poly_from_rast <- rasterToPolygons(uk_eire_poly_20km)
poly_from_rast <- as(poly_from_rast, 'sf')

require(rgeos)
# but can be set to dissolve the boundaries between cells with identical values
poly_from_rast_dissolve <- rasterToPolygons(uk_eire_poly_20km, dissolve=TRUE)
poly_from_rast_dissolve <- as(poly_from_rast_dissolve, 'sf')

# rasterToPoints returns a matrix of coordinates and values.
points_from_rast <- rasterToPoints(uk_eire_poly_20km)
points_from_rast <- st_as_sf(data.frame(points_from_rast), coords=c('x','y'))

# Plot the outputs - using key.pos=NULL to suppress the key and
# reset=FALSE to avoid plot.sf altering the par() options
par(mfrow=c(1,3), mar=c(1,1,1,1))
plot(poly_from_rast['layer'], key.pos = NULL, reset = FALSE)
plot(poly_from_rast_dissolve, key.pos = NULL, reset = FALSE)
plot(points_from_rast, key.pos = NULL, reset = FALSE)

st_write(uk_eire, 'data/uk_eire_WGS84.shp')
st_write(uk_eire_BNG, 'data/uk_eire_BNG.shp')

st_write(uk_eire, 'data/uk_eire_WGS84.geojson')
## Writing layer `uk_eire_WGS84' to data source `data/uk_eire_WGS84.geojson' using driver `GeoJSON'
## Writing 6 features with 5 fields and geometry type Polygon.
st_write(uk_eire, 'data/uk_eire_WGS84.gpkg')
## Updating layer `uk_eire_WGS84' to data source `data/uk_eire_WGS84.gpkg' using driver `GPKG'
## Writing 6 features with 5 fields and geometry type Polygon.

# Save a GeoTiff
writeRaster(uk_raster_BNG_interp, 'data/uk_raster_BNG_interp.tif')
# Save an ASCII format file: human readable text. 
# Note that this format does not contain the projection details!
writeRaster(uk_raster_BNG_ngb, 'data/uk_raster_BNG_ngb.asc', format='ascii')

# Load a vector shapefile
ne_110 <- st_read('data/ne_110m_admin_0_countries/ne_110m_admin_0_countries.shp')
## Reading layer `ne_110m_admin_0_countries' from data source `data/ne_110m_admin_0_countries/ne_110m_admin_0_countries.shp' using driver `ESRI Shapefile'
## Simple feature collection with 177 features and 94 fields
## geometry type:  MULTIPOLYGON
## dimension:      XY
## bbox:           xmin: -180 ymin: -90 xmax: 180 ymax: 83.64513
## epsg (SRID):    4326
## proj4string:    +proj=longlat +datum=WGS84 +no_defs

# Also load some WHO data on 2016 life expectancy
# see: http://apps.who.int/gho/athena/api/GHO/WHOSIS_000001?filter=YEAR:2016;SEX:BTSX&format=csv
life_exp <- read.csv(file = "data/WHOSIS_000001.csv")

# Generate two stacked plots with narrow margins
par(mfrow=c(2,1), mar=c(1,1,1,1))

# The first plot is easy
plot(ne_110['GDP_MD_EST'],  asp=1, main='Global GDP', logz=TRUE, key.pos=4)

#Thend for the second we need to merge the data
ne_110 <- merge(ne_110, life_exp, by.x='ISO_A3_EH', by.y='COUNTRY', all.x=TRUE)
# Create a sequence of break values to use for display
bks <- seq(50, 85, by=0.25)
# Plot the data
plot(ne_110['Numeric'], asp=1, main='Global 2016 Life Expectancy (Both sexes)',
     breaks=bks, pal=viridis, key.pos=4)

# Read in Southern Ocean example data
so_data <- read.csv('data/Southern_Ocean.csv', header=TRUE)
head(so_data)

# Convert the data frame to an sf object
so_data <- st_as_sf(so_data, coords=c('long', 'lat'), crs=4326)
head(so_data)

etopo_25 <- raster('data/etopo_25.tif')
print(etopo_25)
plot(etopo_25)

bks <- seq(-10000, 6000, by=250)
land_cols  <- terrain.colors(24)
sea_pal <- colorRampPalette(c('darkslateblue', 'steelblue', 'paleturquoise'))
sea_cols <- sea_pal(40)
plot(etopo_25, axes=FALSE, breaks=bks, col=c(sea_cols, land_cols), 
     axis.args=list(at=seq(-10000, 6000, by=2000), lab=seq(-10,6,by=2)))

tmax <- getData('worldclim', download=TRUE, path='data', var='tmax', res=10)
print(tmax)
# ... and the data for those layers has been saved in our data folder.
dir('data/wc10')

# scale the data
tmax <- tmax / 10
# Extract  January and July data and the annual maximum by location.
tmax_jan <- tmax[[1]]
tmax_jul <- tmax[[7]]
tmax_max <- max(tmax)
# Plot those maps
par(mfrow=c(3,1), mar=c(2,2,1,1))
bks <- seq(-500, 500, length=101)
pal <- colorRampPalette(c('lightblue','grey', 'firebrick'))
cols <- pal(100)
ax.args <- list(at= seq(-500, 500, by=100))
plot(tmax_jan, col=cols, breaks=bks, axis.args=ax.args, main='January maximum temperature')
plot(tmax_jul, col=cols, breaks=bks, axis.args=ax.args, main='July maximum temperature')
plot(tmax_max, col=cols, breaks=bks, axis.args=ax.args, main='Annual maximum temperature')



