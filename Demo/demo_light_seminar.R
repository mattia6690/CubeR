#### DOWNLOAD AND INSTALL PACKAGE FROM GITLAB ####

library(devtools)
library(git2r)
library(getPass)

#uname<- "Your GITLAB username"
uname <- "Mattia.Rossi"

devtools::install_git("https://gitlab.inf.unibz.it/REMSEN/CubeR",
                      credentials = git2r::cred_user_pass(uname, getPass::getPass()))

library(CubeR) # OPEN PACKAGE IN PACKAGES

##################################################


#### USECASE: CONNECT TO SERVICE #################

# Get capabilities: List all coverages on the server
capabs <- getCapability()
print(capabs)

# Select a coverage
coverage <- capabs[length(capabs)]
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
library("magrittr")
library("stringr")

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
x11(12,8, xpos = - 400, ypos = 100)
pxl_hst_1 <- pixel_history(coverage, coord_sys, bands[3], coords, date = NULL,plot=F)

# Multiple bands possible
x11(12,8, xpos = - 400, ypos = 100)
pxl_hst_5 <- pixel_history(coverage, coord_sys, bands[2:7], coords, date = NULL,plot=T)




### Example 2: normalized difference history ###
NIR <- bands[8]
Red <- bands[4]

# History of NDVI in this case
x11(12,8, xpos = - 400, ypos = 100)
norm_dff_hist <- norm_diff_pixel(coverage, coord_sys, coords, band1=Red, band2=NIR, date = NULL)



### Example 3: get an image ###
img <- image_from_coverage(coverage, slice_E, slice_N, date, res_eff = 1, format = "tiff",
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

# time range
s <- "2016-01-01"
e <- "2016-12-31"

# S2A timeseries
coords <- c(East, North)
tstmp_value <- norm_diff_hist(coverage, coord_sys, coords, NIR, Red, date = c(s,e), plot = FALSE)

tstmp <- as.Date(tstmp_value[,1], origin = "1970-01-01")
value <- tstmp_value[,2]

# In-situ timeseries
#devtools::install_git("https://gitlab.inf.unibz.it/earth_observation_public/MonalisR")


devtools::install_git("https://gitlab.inf.unibz.it/REMSEN/MonalisR",
                      credentials = git2r::cred_user_pass(uname, getPass::getPass()))
library(MonalisR)

s <- paste0(s, " 00:00")
e <- paste0(e, " 00:00")

mnls_down<-downloadMonalisa(foi="vimes1500",datestart = s,dateend = e)
time<-mnls_down[[1]]$Timestamp %>% str_split(.," ") %>% lapply(.,"[[",2) %>% do.call(rbind,.)

mnls_vimes1500<-mnls_down[[1]][which(time=="10:30:00"),]

# plot data
x11(12,8, xpos = - 400, ypos = 100)
plot(tstmp,value,type="o", lwd = 2, xlab="Date", ylab="Normalized difference", ylim = c(-1,1),
     cex.axis = 1.2, cex.lab = 1.2)
par(new = TRUE)
plot(mnls_vimes1500$Timestamp, mnls_vimes1500$`Normalized Difference Vegetation Index (average)`,
     type = "o", col = "red", xlab = "", ylab = "", axes = FALSE, lwd = 2)
p<-title(paste0("Normalized Difference between ", NIR, " and ", Red))

p<-legend("topright", inset = .02,legend=c("Sentinel 2: B8A - B04", "In situ: NDVI (avg)"),
          col = c("black","red"), pch=15)

##################################################







