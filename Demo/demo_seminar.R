#### DOWNLOAD AND INSTALL PACKAGE FROM GITLAB ####

library(devtools)
library(git2r)
library(getPass)

#uname<- "Your GITLAB username"
uname <- "Daniel.Frisinghelli"

devtools::install_git("https://gitlab.inf.unibz.it/REMSEN/CubeR",
                      credentials = git2r::cred_user_pass(uname, getPass::getPass()))

##################################################

#### LOAD PACKAGE TO ENVIRONMENT #################
library(CubeR)

# list all functions in the package
ls("package:CubeR")

##################################################

#### DEMONSTRATION OF USAGE ######################

# Get capabilities: List all coverages on the server
capabs <- getCoverage()
print(capabs)

# Select a coverage
coverage <- capabs[7]
print(coverage)

#### METADATA ####

# To work with a coverage, you need its metadata
# Functions to retrieve metadata:
ls("package:CubeR")[1:7]

# To retrieve metadata of any kind you need different urls
# Therefore the url creator provided by the package
ls("package:CubeR")[8]

# The metadata base url is:
meta_url <- createWCS_URLs(type = "Meta")

##################

# Now you can use the metadata functions
# Retrieve information about selected coverage

# Get timestamps
times <- coverage_get_timestamps(meta_url, coverage)

# timestamps of image
print(times)

# number of available images
print(length(times))

# Required metadata to perform an analysis

# Reference coordinate System: determined via EPSG identifier
# EPSG url:
EPSG_url <- createWCS_URLs(type = "Coords")
EPSG_id <- coord_sys_reference <- coverage_get_coordinate_reference(meta_url, EPSG_url, coverage)
EPSG <- "WGS 84 / UTM zone 32N"
print(paste0("EPSG identifier ", EPSG_id, " = ", EPSG))

# Coordinate System: format of coordinates (Lat, Easting, ...)
coord_sys <- coverage_get_coordsys(meta_url, coverage)
print(coord_sys)

# Bands/channels
bands <- coverage_get_bands(meta_url, coverage)
print(bands)

# Image resolution in pixels
resolution <- coverage_get_resolution(meta_url, coverage)
print(resolution)

# Bounding Box: (xmin, xmax, ymin, ymax)
BB <- coverage_get_bounding_box(meta_url, coverage)
print(BB)

# Therefore image size in pixels is
im_size <- c((as.numeric(BB[2])-as.numeric(BB[1]))/resolution,
             (as.numeric(BB[4])-as.numeric(BB[3]))/resolution)
print(im_size)

#### ANALYSIS ####

# GET PIXEL HISTORY OF A PIXEL

# Need url to process coverage
pixel_url <- createWCS_URLs(type = "Pixel")

# Define pixel (center of image)
BB_num <- as.numeric(BB)
coords <- as.character(c(BB_num[1]+(BB_num[2]-BB_num[1])/2, BB_num[3]+(BB_num[4]-BB_num[3])/2))

# date = NULL: total timeseries, subsetting with date = c("date_min", "date_max")
x11()
pxl_hst_1 <- pixel_history(coverage, coord_sys, bands[2], coords, date = NULL, meta_url, pixel_url)

# Multiple bands possible
x11()
pxl_hst_5 <- pixel_history(coverage, coord_sys, bands[2:7], coords, date = NULL, meta_url, pixel_url)

# GET NORMALIZED DIFFERENCE HISTORY OF A PIXEL

# Define two bands:
Red <- bands[4]
print(Red)

NIR <- bands[8]
print(NIR)

# History of NDVI in this case
x11()
norm_dff_hist <- norm_diff_hist(coverage, coord_sys, coords, NIR, Red, date = NULL, meta_url,
                                pixel_url)

# GET AN IMAGE

# Define a slice: slice of 1000x1000 pixels around center of image
slice_E <- as.character(c(as.numeric(coords[1])-5000, as.numeric(coords[1])+5000))
slice_N <- as.character(c(as.numeric(coords[2])-5000, as.numeric(coords[2])+5000))

# Define a timestamp
date = times[1]

# image (as RasterLayer class)
img <- image_from_coverage(coverage, coord_sys, slice_E, slice_N, date, EPSG_id, res_eff = 1, format = "tiff",
                    bands = bands[8])
print(img)
print(img[[1]]@data@values)

# res_eff scales the ouput raster: 1 == originial resolution, 2 == scaled to half of original resolution, 3 == ...
# results in faster computation and visualization, especially for large slicing
img_scld <- image_from_coverage(coverage, coord_sys, slice_E, slice_N, date, EPSG_id, res_eff = 2, format = "tiff",
                           bands = bands[8])
print(img)
print(img_scld)

# visualize raster img
install.packages("leaflet")
install.packages("rgdal")
library(leaflet)
library(rgdal)
lflt <- leaflet() %>% addTiles() %>% addRasterImage(img[[1]], colors = gray.colors(10))
lflt
lflt %>% clearImages() %>% addRasterImage(img_scld[[1]], colors = gray.colors(10))

# multiple bands possible
img_m <- image_from_coverage(coverage, coord_sys, slice_E, slice_N, date, EPSG_id, res_eff = 1, format = "tiff",
                           bands = bands[2:4])
print(img_m)

# GET NORMALIZED DIFFERENCE IMAGE
nd_img <- norm_diff_raster(coverage, coord_sys, slice_E, slice_N, date, EPSG_id, res_eff = 1, format = "tiff",
                           NIR, Red)
print(nd_img)

# visualize
NDVI_colormap <- colorRampPalette(c("blue","cadetblue","azure","grey","brown","yellow","green"))

lflt %>% clearImages() %>% addRasterImage(nd_img[[1]], colors = NDVI_colormap(255)) %>%
  addLegend("bottomleft", colors = NDVI_colormap(11), labels = seq(-1,1,by = 2/10))

