#' @title Plot the MONALISA Stations
#' @description Function to spatially plot the MONALISA Stations and the respective FOI
#' to a Leaflet plot for better visualization.
#' @param db optional DB table; If empty the standard db file fill be generated
#' @import leaflet
#' @import magrittr
#' @import tibble
#' @import dplyr
#' @examples
#' ## Standard Plotting wothout Database
#' MonalisaPlot()
#' @export

MonalisaPlot<- function(db=NULL){
  
  if(is.null(db)) db<-getDataBase()
  coords<-db$station$geometry$coordinates %>% do.call(rbind,.) %>% as.tibble %>% select(.,-V3)
  name<-db$station$properties$label
  stats<-cbind.data.frame(coords,name)
  
  m<-leaflet() %>% addTiles() %>% addMarkers(
    lng=stats$V1 %>% as.character %>% as.numeric,
    lat=stats$V2 %>% as.character %>% as.numeric,
    popup=paste("FOI:",stats$name)
  )
  return(m)
  
}
