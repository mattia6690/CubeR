# Pixel history of "bands" (coverage bands) for a given point determined "coords"
pixel_history <- function(coverage, coord_sys, bands, coords, date=NULL){

  #### INPUTS ######################################################################################################
  # -------------------------------------------------------------------------------------------------------------- #
  # coverage: name of the coverage; class: [character]                                                             #
  # example: "S2_L2A_T32TPS_20M"                                                                                   #
  #                                                                                                                #
  # coord_sys: coordinate system; class: [character];                                                              #
  # format: (X,Y,Time)                                                                                             #
  # example: c("E", "N", "DATE")                                                                                   #
  #                                                                                                                #
  # bands: coverage bands to calculate and visualize pixel history; class: [character]                             #
  # example: bands = c("AOT", "B02","B03", "B04", ..., "B8A", ... )                                                #
  #                                                                                                                #
  # coords: coordinates of the location of interest; class: [character]                                            #
  # format: c(X,Y)                                                                                                 #
  # example:  c("610000", "5150000")                                                                               #
  #                                                                                                                #
  # date: date range; class: [character, Date]                                                                     #
  # format: "%yyyy-%mm-%dd"                                                                                        #
  # example: c("2015-12-31", "2016-12-31")                                                                         #
  # default: NULL; If date is per default NULL, the pixel history for all available timestamps is computed         #
  # -------------------------------------------------------------------------------------------------------------- #
  ##################################################################################################################

  # Get all available timestamps
  times<-coverage_get_timestamps(desc_url,coverage)

  # Check if a daterange is desired or if full extent is required
  if(is.null(date)){

    # convert to Date
    times <- times %>% as.Date()

    # dates for the query
    start_date_mod <- times[1]
    end_date_mod <- times[length(times)]

  } else {

    # dates for the query
    start_date_mod = date[1]
    end_date_mod = date[2]

    # Restrict all dates (times) to the desired daterange
    times <- times[times >= start_date_mod & times <= end_date_mod] %>% as.Date()

  }

  # Get number of input bands
  bands_len <- length(bands)

  # Error Handling if no band is selected
  if(bands_len == 0){

    p <- plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
    p <- text(1,1, "Can't visualize pixel history for selected coverage, reason: No band selected!", cex = 2)

  } else {

    # List of pixel histories for each band
    responses = list()

    # List of maximum values of pixel history for each band
    max_res = list()

    # Initiate loop to create a query for every input band
    for(k in 1:bands_len){

      # Construct the Query
      query <-str_c('for c in ( ',coverage,' ) return encode( c.',bands[k],'[',coord_sys[1],'(',coords[1],'),',coord_sys[2],'(',coords[2],
                    '),', coord_sys[3], '("', start_date_mod,'":"', end_date_mod,'")],"csv")')


      # Create the Request
      query_encode  <- urltools::url_encode(query)
      request       <- paste(pixel_url, query_encode, collapse = NULL, sep="")
      r             <- GET(request)

      # Get content
      res <- content(r, "text")

      # parse text content to vector
      num <- str_replace_all(res,"\\{","") %>% str_replace_all(.,"\\}","") %>% str_replace_all(.,"\"","") %>%
        str_split(.,",") %>% unlist() %>% str_split(.," ") %>% unlist() %>% as.numeric()

      # write to list
      responses[[k]] = num

      # Get maximum value
      max_res[[k]] = max(num)
    }

    # Get absolute maximum value for plotting
    abs_max <- max(na.omit(unlist(max_res)))

    # plot first entry in the list "responses" to create a plot environment to add other entries to
    p <- plot(times,responses[[1]], type="o", col = rainbow(1)[1], lwd = 2, xlab="Date", ylab="Single channels", ylim = c(0,abs_max),
              cex.axis = 1.2, cex.lab = 1.2)

    # Check how many bands to plot
    if(length(responses) == 1){

      # Add legend
      p<-legend("topright", inset = .02,legend=c(bands), col = c(rainbow(length(responses))), pch=15)

    } else{

      # Initiate loop to plot all input bands
      for(l in 2:length(responses)){
        p <- lines(times, responses[[l]], type = "o", col = rainbow(l)[l], lwd = 2)

      }

      # Add legend
      p<-legend("topright", inset = .02,legend=c(bands), col = c(rainbow(length(responses))), pch=15)

    }

  }

  return(p)

  #### OUTPUT ######################################################
  # -------------------------------------------------------------- #
  # p: pixel history plot; class: [NULL]; graphics object          #
  # -------------------------------------------------------------- #
  ##################################################################

}
