#### DOWNLOAD AND INSTALL PACKAGE FROM GITLAB ####

library(devtools)
library(git2r)
library(getPass)

#uname<- "Your GITLAB username"
uname <- "Daniel.Frisinghelli"

devtools::install_git("https://gitlab.inf.unibz.it/REMSEN/CubeR",
                      credentials = git2r::cred_user_pass(uname, getPass::getPass()))

devtools::install_git("https://gitlab.inf.unibz.it/REMSEN/MonalisR",
                      credentials = git2r::cred_user_pass(uname, getPass::getPass()))

library(CubeR) # OPEN PACKAGE IN PACKAGES

##################################################




#### USECASE: CONNECT TO SERVICE #################

# Get capabilities: List all coverages on the server
capabs <- getCoverage()
print(capabs)

# Select a coverage
coverage <- capabs[7]
print(coverage)

##################################################




#### USECASE: METADATA ###########################

### Example 1: query bounding box ###

# Bounding Box: (xmin, xmax, ymin, ymax)
BB <- coverage_get_bounding_box(coverage = coverage)
print(BB)



### Example 2: timestamps ###

# Get timestamps
times <- coverage_get_timestamps(coverage = coverage)

print(times) # timestamps of image
print(length(times)) # number of available images



### Example 3: bands ###
bands <- coverage_get_bands(coverage = coverage)
print(bands)

##################################################




#### INPUTS FOR ANALYSIS #########################
library(leaflet)
library(rgdal)

coord_sys <- coverage_get_coordsys(coverage = coverage)
BB_num <- as.numeric(BB)
coords <- as.character(c(BB_num[1]+(BB_num[2]-BB_num[1])/2, BB_num[3]+(BB_num[4]-BB_num[3])/2))
slice_E <- as.character(c(as.numeric(coords[1])-5000, as.numeric(coords[1])+5000))
slice_N <- as.character(c(as.numeric(coords[2])-5000, as.numeric(coords[2])+5000))
date = times[1]
EPSG_id <- coverage_get_coordinate_reference(coverage = coverage)
NDVI_colormap <- colorRampPalette(c("blue","cadetblue","azure","grey","brown","yellow","green"))
##################################################




#### USECASE: ANALYSIS ###########################

### Example 1: pixel history ###
x11()
pxl_hst_1 <- pixel_history(coverage, coord_sys, bands[2], coords, date = NULL)

# Multiple bands possible
x11()
pxl_hst_5 <- pixel_history(coverage, coord_sys, bands[2:7], coords, date = NULL)




### Example 2: normalized difference history ###
NIR <- bands[8]
Red <- bands[4]

# History of NDVI in this case
x11()
norm_dff_hist <- norm_diff_hist(coverage, coord_sys, coords, NIR, Red, date = NULL)



### Example 3: get an image ###
img <- image_from_coverage(coverage, coord_sys, slice_E, slice_N, date, EPSG_id, res_eff = 1, format = "tiff",
                           bands = bands[8])

lflt <- leaflet() %>% addTiles() %>% addRasterImage(img[[1]], colors = gray.colors(10))
lflt



### Example 4: get normalized difference raster ###
nd_img <- norm_diff_raster(coverage, coord_sys, slice_E, slice_N, date, EPSG_id, res_eff = 1, format = "tiff",
                           NIR, Red)

lflt %>% clearImages() %>% addRasterImage(nd_img[[1]], colors = NDVI_colormap(255)) %>%
  addLegend("bottomleft", colors = NDVI_colormap(11), labels = seq(-1,1,by = 2/10))

##################################################




#### COMBINING THINGS ############################

# UTM 32N coordinates of vimes1500
East <- "620812.02"
North <- "5171499.36"

# S2A timeseries
coords <- c(East, North)
tstmp_value <- norm_diff_hist(coverage, coord_sys, coords, NIR, Red, date = NULL, plot = FALSE)

tstmp <- as.Date(tstmp_value[,1], origin = "1970-01-01")
value <- tstmp_value[,2]

p<-plot(tstmp,value,type="o", lwd = 2, xlab="Date", ylab="Normalized difference", ylim = c(-1,1), cex.axis = 1.2, cex.lab = 1.2)
p<-legend("topright", inset = .02,legend=paste0(NIR," - ",Red), pch=15)
p<-title(paste0("Normalized Difference between ", NIR, " and ", Red))

# In-situ timeseries


##################################################







