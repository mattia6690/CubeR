# Source RasdamanFunctions.R
source("RasdamanFunctions.R")

##### global urls #####

# capabilities
capab_url = "http://10.8.244.147:8080/rasdaman/ows?SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCapabilities"
xml_capabils <- xml2::read_xml(capab_url)

# coverage choices to be available in the app
coverages <- xml_text(xml2::xml_find_all(xml_capabils, ".//wcs:CoverageId")) %>% .[8:(length(.)-2)]

# coverage metadata
desc_url <- "http://10.8.244.147:8080/rasdaman/ows?SERVICE=WCS&VERSION=2.0.1&REQUEST=DescribeCoverage&COVERAGEID="

# coverage coordinate reference system
coord_url <- "http://10.8.244.147:8080/def/crs/EPSG/0/"

# coverage time axis reference
time_url <- "http://10.8.244.147:8080/def/crs/OGC/0/"

# process coverage query url
pixel_url <- 'http://10.8.244.147:8080/rasdaman/ows?SERVICE=WCS&VERSION=2.0.1&REQUEST=ProcessCoverages&QUERY='

# get coverage query url
tiff_url <- 'http://10.8.244.147:8080/rasdaman/ows?&SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCoverage&COVERAGEID='
  
#######################

##### output format ###

formats = c("tiff", "png", "jpeg")

#######################

#### Colorramps for additive color mixing #####

red <- colorRampPalette(c("black","red"))
green <- colorRampPalette(c("black","green"))
blue <- colorRampPalette(c("black","blue"))
NDVI_palette <- colorRampPalette(c("blue","cadetblue","azure","grey","brown","yellow","green"))
NDSI_palette <- colorRampPalette(c("green", "grey", "azure"))

###############################################
