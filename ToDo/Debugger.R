# Function to check if library 'mypgk' is available
loadandinstall <- function(mypkg) {if (!is.element(mypkg, installed.packages()[,1]))
{install.packages(mypkg, repos = "http://cran.r-project.org/")}; library(mypkg, character.only=TRUE)}

# Initialize packages
loadandinstall("httr")
loadandinstall("stringr")
loadandinstall("urltools")
loadandinstall("raster")
loadandinstall("rgdal")
loadandinstall("xml2")
loadandinstall("png")
loadandinstall("tiff")
loadandinstall("jpeg")
loadandinstall("grid")
loadandinstall("leaflet")
loadandinstall("mapview")
loadandinstall("shiny") 

request ="http://10.8.244.147:8080/rasdaman/ows?&SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCoverage&COVERAGEID=S2A_L2A_RGB_UTM32&SUBSET=DATE(%222015-06-27T00:00:00.000Z%22)&FORMAT=image/png"
res <- GET(request)
bin <- content(res, "raw")
format = "tiff"
to_img <- get(paste0("read",toupper(format)))
img <- to_img(bin)
coord_sys = c("E","N","DATE")
slice_E = c("600000","610000")
slice_N = c("5190020", "5200020")
ras_ext <- extent(c(as.numeric(slice_E), as.numeric(slice_N)))
raster_brick <- brick(img)
ref_Id = "32632"
proj4string(raster_brick) <- CRS(paste0("+init=epsg:",ref_Id))
extent(raster_brick) <- ras_ext
res_eff = 10
ras<-raster_brick
ras_aggregate <- aggregate(ras, fact=res_eff, expand = FALSE)
print(ras_aggregate)

leaflet() %>% addTiles() %>% addRasterImage(ras_aggregate$layer.1, color = gray.colors(255), opacity = 0.8) %>% 
  


  
# epsg32632<- leafletCRS(
#   crsClass="L.Proj.CRS.TMS",
#   code='EPSG:32632',
#   proj4def='+init=epsg:32632 proj=utm +zone=32 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0,0,0,0 +units=m +no_defs',
#   resolutions = c(
#     8192, 4096, 2048, 1024, 512, 256, 128, 64, 32, 16, 8, 4, 2, 1, 0.5),
#   origin = c(500000.00, 4649776.22), bounds = c(166021.4431, 0.0000, 833978.5569, 9329005.1825))






