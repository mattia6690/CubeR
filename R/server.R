shinyServer(server <- function(input,output){
  
  ##### COORDINATE SYSTEM ###################################
  
  coordinate_system <- reactive({
    coords <- coverage_get_coordsys(desc_url, input$coverage)
    return(coords)
  })
  
  system_reference <- reactive({
    reference_Id <- coordsys_reference(desc_url, coord_url, input$coverage)
    
    sys_xml <- xml2::read_xml(paste0(coord_url,reference_Id))
    
    system <- xml_find_first(sys_xml, ".//gml:name") %>%  xml_text()
    
    return(system)
    
  })
  
  output$system <- renderPrint({
    coordinate_system()
  })
  
  output$reference <- renderPrint({
    system_reference()
  })
  
  bands <- reactive({
    sat_bands <- coverage_get_bands(desc_url, input$coverage)
    return(sat_bands)
  })
  
  output$bands <- renderText({
    bands()
  })
  
  ###########################################################
  
  #---------------------------------------------------------#
  
  ##### DEFAULT SETTINGS ####################################
  
  default_coords <- reactive({
    sys <- coordinate_system()
    ref <- system_reference()
    
    if(sys[1] == "Lat"){
      def_X = "46.498295"
      def_Y = "11.354758"
      
      def_coords = c(def_X, def_Y)
      
    } else if(sys[1] == "E"){
      
      def_X = "680688"
      def_Y = "5152108"
      
      def_coords = c(def_X, def_Y)
      
    } else {
      
      def_Y = "4100696.116"
      def_X = "2209644.577"
      
      def_coords = c(def_X, def_Y)
      
    }
    
    return(def_coords)
    
  })
  
  output$defcoords <- renderUI({
    
    div(style = "display:inline-block",
        textInput(inputId = "coordX", label = coordinate_system()[1], value = default_coords()[1]),
        textInput(inputId = "coordY", label = coordinate_system()[2], value = default_coords()[2])
    )
    
  })
  
  default_time <- reactive({
    def_time <- coverage_get_temporal_extent(desc_url, input$coverage)
    
    def_time <- as.Date(def_time)
    
    return(def_time)
  })
  
  output$deftime <- renderUI({
    
    dateRangeInput(inputId = "date", label = h2("Time period"), start = default_time()[1] + 1,
                   end = default_time()[1] + 31)
    
  })
  
  date_process <- reactive({
    
    if(input$date[2] > default_time()[2]){
      e_date = default_time()[2] - 1
    } else {
      e_date = input$date[2]
    }
    
    if(input$date[1] < default_time()[1]){
      s_date = default_time()[1] + 1
    } else {
      s_date = input$date[1]
    }
    
    dates_proc = c(s_date,e_date)
    
    return(dates_proc)
    
  })
  
  # Get bounding box for error handling
  bounding_box <- reactive({
    
    bb <- coverage_get_bounding_box(desc_url, input$coverage)
    
    return(bb)
    
  })
      
  # set default slice of coverage to the upper left corner of the image
  default_slicing <- reactive({
    
    bounds <- bounding_box()
    
    if(coordinate_system()[1] == "E" | coordinate_system()[1] == "Y"){
      
      to_x <- as.numeric(bounds) %>% .[1] %>% + 10000 %>% as.character()
      from_y <- as.numeric(bounds) %>% .[4] %>% - 10000 %>% as.character()
      slicing <- c(bounds[1], to_x, from_y, bounds[4])
      
    } else if(coordinate_system()[1] == "Lat"){
      
      to_x <- as.numeric(bounds) %>% .[1] %>% + 10 %>% as.character()
      from_y <- as.numeric(bounds) %>% .[4] %>% - 10 %>% as.character()
      slicing <- c(bounds[1], to_x, from_y, bounds[4])
    }
    
    return(slicing)
    
  })
  
  slicing_process <- reactive({
    
    if(input$sliceX_from < bounding_box()[1]){
      sliceX_from = bounding_box()[1]
    } else {
      sliceX_from = input$sliceX_from
    }
    
    if(input$sliceX_to > bounding_box()[2]){
      sliceX_to = bounding_box()[2]
    } else {
      sliceX_to = input$sliceX_to
    }
    
    if(input$sliceY_from < bounding_box()[3]){
      sliceY_from = bounding_box()[3]
    } else {
      sliceY_from = input$sliceY_from
    }
    
    if(input$sliceY_to > bounding_box()[4]){
      sliceY_to = bounding_box()[4]
    } else {
      sliceY_to = input$sliceY_to
    }
    
    slicing_error_handled <- c(sliceX_from, sliceX_to, sliceY_from, sliceY_to)
    
    return(slicing_error_handled)
    
  })
  
  output$default_slice_X_header <- renderUI({
    
    h3(coordinate_system()[1])
    
  })
  
  output$default_slice_Y_header <- renderUI({
    
    h3(coordinate_system()[2])
    
    
  })
  
  output$slice_fromX <- renderUI({
    textInput(inputId = "sliceX_from", label = "From", value = default_slicing()[1], width = "150px")
  })
  
  output$slice_toX <- renderUI({
    textInput(inputId = "sliceX_to", label = "To", value = default_slicing()[2], width = "150px")
  })
  
  output$slice_fromY <- renderUI({
    textInput(inputId = "sliceY_from", label = "From", value = default_slicing()[3], width = "150px")
  })
  
  output$slice_toY <- renderUI({
    textInput(inputId = "sliceY_to", label = "To", value = default_slicing()[4], width = "150px")
  })
  
  default_bands <- reactive({
    
    all_bands <- bands()
    
    if(is.element("Red",all_bands) == FALSE & is.element("Green", all_bands) == FALSE &
       is.element("Blue",all_bands)== FALSE){
      
      def_bands <- c(all_bands[pmatch("B04",all_bands)], all_bands[pmatch("B03",all_bands)],
                     all_bands[pmatch("B02",all_bands)])
      
    } else {
      
      def_bands <- c(all_bands[pmatch("Red",all_bands)], all_bands[pmatch("Green",all_bands)],
                     all_bands[pmatch("Blue",all_bands)]) 
      
    }
    
    return(def_bands)
    
  })
  
  default_nd <- reactive({
    
    all_bands <- bands()
    
    if(is.element("NIR", all_bands)){
      def_nd <- c("NIR", "Red")
    } else if(is.element("B8A", all_bands)){
      def_nd <- c("B8A","B04")
    } else {
      def_nd = NULL
    }
    
    return(def_nd)
    
  })
  
  output$bands_single <- renderUI({
    
    selectInput(inputId = "SingleChannel", label = NULL, choices = bands())
    
  })
  
  output$bands_multi_1 <- renderUI({
    
    div(style="display:inline-block", selectInput(inputId = "band1", label = "Band 1", choices = bands(),
                                                  selected = default_bands()[1], width = "100px"))
    
  })
  
  output$bands_multi_2 <- renderUI({
    
    div(style="display:inline-block", selectInput(inputId = "band2", label = "Band 2", choices = bands(),
                                                  selected = default_bands()[2], width = "100px"))
  })
  
  output$bands_multi_3 <- renderUI({
    
    div(style="display:inline-block", selectInput(inputId = "band3", label = "Band 3", choices = bands(),
                                                  selected = default_bands()[3], width = "100px"))
  })
  
  output$calc_1 <- renderUI({
    
    div(style="display:inline-block", selectInput(inputId = "calc1", label = "Band 1", choices = bands(),
                                                  selected = default_nd()[1], width = "100px"))
  })
  
  output$calc_2 <- renderUI({
    
    div(style="display:inline-block", selectInput(inputId = "calc2", label = "Band 2", choices = bands(),
                                                  selected = default_nd()[2], width = "100px"))
  })
  
  ###########################################################
  
  #---------------------------------------------------------#
  
  ##### PIXEL HISTORY #######################################

  output$band_multsel <- renderUI({
    
    selectInput(inputId = "band_multsel", label = NULL, choices = bands(), multiple = TRUE)
    
  })
  
  
  pixel_hist <- reactive({
    
    if(input$full == TRUE){
      dates = NULL
    } else {
      dates <- date_process()
    }
    
    crdnts <- c(input$coordX,input$coordY)
    
    band_multselected <- input$band_multsel
    
    hist <- pixel_history(input$coverage, coordinate_system(), band_multselected, crdnts, dates)
    
    return(hist)
    
  })
  
  observeEvent(input$load_history,{
    
    output$history <-renderPlot({
      
      isolate(pixel_hist())
      
    })
    
  })
  
  
  ###########################################################
  
  #---------------------------------------------------------#
  
  ##### ND-PIXEL HISTORY ####################################
  
  output$ndpixhist1 <- renderUI({
    
    div(style="display:inline-block", selectInput(inputId = "ndpixhist_band1", label = "Band 1", choices = bands(),
                                                  selected = default_nd()[1], width="150px"))
  })
  
  output$ndpixhist2 <- renderUI({
    
    div(style="display:inline-block", selectInput(inputId = "ndpixhist_band2", label = "Band 2", choices = bands(),
                                                  selected = default_nd()[2], width="150px"))
  })
  
  ND_re <- reactive({
    
    if(input$full == TRUE){
      dates = NULL
    } else {
      dates <- date_process()
    }
    
    coords = c(input$coordX, input$coordY)
    
    P_ndvi <- norm_diff_hist(input$coverage, coordinate_system(), coords, input$ndpixhist_band1,
                             input$ndpixhist_band2, dates)
    
    return(P_ndvi)
    
  })
  
  observeEvent(input$nd,{
    
    
    output$ND <-renderPlot({
      
      isolate(ND_re())
      
    })
    
  })
  
  ###########################################################
  
  #---------------------------------------------------------#
  
  ##### PIXELCHIP HISTORY ###################################
  
  # default buffer setting depending on coverage resolution
  default_buffer <-reactive({
    
    cov_reso <- coverage_get_resolution(desc_url, input$coverage)
    
    sel_buff <- seq(cov_reso, cov_reso*5, by=cov_reso) %>% as.character()
    
    return(sel_buff)
    
  })
  
  # band selection for pixelchip history
  output$bandpixelchip <- renderUI({
    
    div(style="display:inline-block", 
        selectInput(inputId = "bandpixelchip", label = NULL, choices = bands(), width = "150px"))
    
  })
  
  # buffer selection for pixelchip history
  output$buffer <- renderUI({
    
    div(style="display:inline-block",
        selectInput(inputId = "buffer", label = NULL, choices = default_buffer(),
                    selected = default_buffer()[1], width = "150px"))
    
  })
  
  # Computation of pixelchip history
  Pxlchip_re <- reactive({
    
    if(input$full == TRUE){
      dates = NULL
    } else {
      dates <- date_process()
    }
    
    coords = c(input$coordX, input$coordY)
    
    Pxlchip <- geocoded_pixel_buffer(input$coverage, coordinate_system(), coords, input$bandpixelchip, input$buffer, dates)
    
    return(Pxlchip)
    
  })
  
  # show pixelchip history when button with id = "pxlchp" is clicked
  observeEvent(input$pxlchp,{
    
    output$pixelchip <-renderPlot({
      
      isolate(Pxlchip_re())
      
    })
    
  })
  
  
  ###########################################################
  
  #---------------------------------------------------------#
  
  ##### TIMESTEP ############################################
  
  av_timestamps <- reactive({
    
    timestamps_img <- coverage_get_timestamps(desc_url, input$coverage)
    
    return(timestamps_img)
    
  })
  
  output$timestamps_av <- renderUI({
    
    as_dates <- as.Date(av_timestamps())
    
    selectInput(inputId = "timestamps", label = h3("Timestamp [yyyy-mm-dd]"), choices = as_dates)
    
  })
  
  timestep_img_s <- reactive({
    
    if(input$total == TRUE){
      
      slice_E = c(bounding_box()[1],bounding_box()[2])
      slice_N = c(bounding_box()[3],bounding_box()[4])
      
    } else {
     
      slice_E = c(slicing_process()[1],slicing_process()[2])
      slice_N = c(slicing_process()[3],slicing_process()[4])
      
    }
    
    tmp_date <- pmatch(input$timestamps, av_timestamps())
    
    eff_date <- av_timestamps()[tmp_date]
    
    ref_Id <- coordsys_reference(desc_url, coord_url, input$coverage)
    
    time_img <- image_from_coverage(input$coverage, coordinate_system(), slice_E, slice_N, eff_date,
                                    ref_Id, input$res, input$format, input$SingleChannel)
    
    return(time_img)
    
  })
  
  timestep_img_m <- reactive({
    
    if(input$total == TRUE){
      
      slice_E = c(bounding_box()[1],bounding_box()[2])
      slice_N = c(bounding_box()[3],bounding_box()[4])
      
    } else {
      
      slice_E = c(slicing_process()[1],slicing_process()[2])
      slice_N = c(slicing_process()[3],slicing_process()[4])
      
    }
    
    tmp_date <- pmatch(input$timestamps, av_timestamps())
    
    eff_date <- av_timestamps()[tmp_date]
    
    ref_Id <- coordsys_reference(desc_url, coord_url, input$coverage)
    
    m_channels <- c(input$band1, input$band2, input$band3)
    
    time_img <- image_from_coverage(input$coverage, coordinate_system(), slice_E, slice_N, eff_date,
                                    ref_Id, input$res, input$format, m_channels)
    
    return(time_img)
    
  })
  
  output$leaf <- renderLeaflet({
    
    leaflet() %>% addTiles() %>% setView(lng=11.5, lat=46.816667,zoom = 9)
    
  })
  
  observeEvent(input$Single_channel,{
    
    proxy <- leafletProxy("leaf")
    
    proxy %>% clearImages() 
    
    proxy %>% addRasterImage(timestep_img_s()[[1]],colors = gray.colors(255),opacity = 0.8, maxBytes = 1024*1024*50)
    
    
  })
  
  observeEvent(input$Multi_channel,{
    
    proxy <- leafletProxy("leaf")
    
    proxy %>% clearImages()
    
    proxy %>% addRasterImage(timestep_img_m()[[1]], color = red(255), opacity = 0.8) %>% 
      addRasterImage(timestep_img_m()[[2]], color = green(255), opacity = 0.45) %>% 
      addRasterImage(timestep_img_m()[[3]], color = blue(255), opacity = 0.3)
    
  })
  
  observeEvent(input$clear,{
    
    proxy <- leafletProxy("leaf")
    
    proxy %>% clearImages()
    
  })

  ###########################################################
  
  #---------------------------------------------------------#
  
  ##### ND image ##########################################
  
  ND_IMG <- reactive({
    
    if(input$total == TRUE){
      
      slice_E = c(bounding_box()[1],bounding_box()[2])
      slice_N = c(bounding_box()[3],bounding_box()[4])
      
    } else {
      
      slice_E = c(slicing_process()[1],slicing_process()[2])
      slice_N = c(slicing_process()[3],slicing_process()[4])
      
    }
    
    tmp_date <- pmatch(input$timestamps, av_timestamps())
    
    eff_date <- av_timestamps()[tmp_date]
    
    ref_Id <- coordsys_reference(desc_url, coord_url, input$coverage)
    
    nd_img <- norm_diff_raster(input$coverage, coordinate_system(), slice_E, slice_N, eff_date, ref_Id,
                                    input$res, input$format, input$calc1, input$calc2)
    
    return(nd_img)
    
  })
  
  observeEvent(input$Calc,{
    
    proxy <- leafletProxy("leaf")
    
    proxy %>% clearImages() %>% clearControls()
    
    if(input$calc1 == "B03" | input$calc1 == "Red"){
      proxy %>% addRasterImage(ND_IMG(),colors = NDSI_palette(255), opacity = 0.8, maxBytes = 1024*1024*100) %>% 
        addLegend("bottomright", colors = NDSI_palette(11), labels = seq(-1,1,by = 2/10))
    } else {
      proxy %>% addRasterImage(ND_IMG(),colors = NDVI_palette(255), opacity = 0.8, maxBytes = 1024*1024*100) %>% 
        addLegend("bottomright", colors = NDVI_palette(11), labels = seq(-1,1,by = 2/10))
    }
  })
  
  #############################################################
  
  #-----------------------------------------------------------#
  
  ##### HELP BUTTON ###########################################
  
  observeEvent(input$help,{
  showNotification(paste0('Calculates: ', '(', input$calc1, ' - ', input$calc2, ") / (",
                           input$calc1, ' - ', input$calc2, ')'), duration = 5, closeButton = TRUE, type = "message")
  })
  
})




