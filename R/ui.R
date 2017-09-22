ui <- navbarPage(title = "Rasdaman",
                 
                 tabPanel("Map",
                          leafletOutput(outputId = "leaf", width = '100%', height = "90vh"),
                          
                          absolutePanel(id = "Coverage", class = "well", fixed = TRUE,
                                        draggable = TRUE,top = 80, left = "auto", right = 20, bottom = "auto",
                                        width = 350, height = "auto",
                                        
                                        tags$head(tags$style(HTML('#Coverage {background-image:
                                                                  linear-gradient(green,yellow,red);}'))),
                                        
                                        selectInput(inputId = "coverage", label = h2("Data set selection"),
                                                    choices = coverages),
                                        selectInput(inputId = "format", label = h3("Format"),
                                                    choices = formats),
                                        
                                        ##### OPTIONAL METADATA #####
                                        
                                        # h4("The coverages coordinate system is:"),
                                        # verbatimTextOutput(outputId = "reference"),
                                        # h4("with coordinate format:"),
                                        # verbatimTextOutput(outputId = "system"),
                                        
                                        #############################
                                        
                                        h3("Spatial extent [m]"),
                                        
                                        uiOutput("default_slice_X_header"),
                                        
                                        div(style="display:inline-block", uiOutput("slice_fromX")),
                                        div(style="display:inline-block", uiOutput("slice_toX")),
                                        
                                        uiOutput("default_slice_Y_header"),
                                        
                                        div(style="display:inline-block", uiOutput("slice_fromY")),
                                        div(style="display:inline-block", uiOutput("slice_toY")),
                                        
                                        checkboxInput(inputId = "total", label = "Show whole image ?",
                                                      width = "150px"),
                                        
                                        uiOutput("timestamps_av"),
                                        h3("Available bands are:"),
                                        verbatimTextOutput(outputId="bands"),
                                        
                                        # change fontsize of verbatim text
                                        tags$head(tags$style(HTML("#bands {
                                                                  font-size: 18px;
                                                                  }
                                                                  "))),
                                        
                                        style = "opacity:0.8"
                          ),
                          
                          absolutePanel(id = "Visualization", class = "well", fixed = TRUE,
                                        draggable = TRUE, top = 80, left = 60, right = "auto", bottom = "auto",
                                        width = 400, height ="auto",
                                        
                                        tags$head(tags$style(HTML('#Visualization {background-image:
                                                                  linear-gradient(green,yellow,red);}'))),
                                        
                                        h2("Visualization"),
                                        
                                        h3("Single-channel"),
                                        uiOutput("bands_single"),
                                        
                                        h3("Multi-channel"),
                                        
                                        div(style="display:inline-block", uiOutput("bands_multi_1")),
                                        div(style="display:inline-block", uiOutput("bands_multi_2")),
                                        div(style="display:inline-block", uiOutput("bands_multi_3")),
                                        
                                        sliderInput(inputId = "res", label = "Scale resolution by factor:",
                                                    min=1, max=10, value=1, step=1),
                                        
                                        div(style="display:inline-block",
                                            actionButton(inputId = "Single_channel", label = "Single-channel")),
                                        div(style="display:inline-block",
                                            actionButton(inputId = "Multi_channel", label = "Multi-channel")),
                                        div(style="display:inline-block",
                                            actionButton(inputId = "clear", label = "Clear Map")),
                                        
                                        style = "opacity:0.8"
                          ), 
                          
                          absolutePanel(id = "Calculation", class = "well", fixed = TRUE,
                                        draggable = TRUE, top = 550, left = 60, right = "auto", bottom = "auto",
                                        width = 400, height ="auto",
                                        
                                        tags$head(tags$style(HTML('#Calculation {background-image:
                                                                  linear-gradient(green,yellow,red);}'))),
                                        
                                        h2("Calculation"),
                                        
                                        h3("Normalized difference between:"),
                                        div(style="display:inline-block", uiOutput("calc_1")),
                                        div(style="display:inline-block", uiOutput("calc_2")),
                                        
                                        div(style="display:block",
                                            actionButton(inputId = "Calc", label = "Calculate"),
                                            div(style = "display:inline-block",
                                                actionButton(inputId = "help", label = NULL, icon("info")))),
                                        
                                        style = "opacity:0.8"
                                        
                                        ##### OPTIONAL HELPBUTTON PARAMETERS #####
                                        
                                        # tags$head(tags$style(HTML(".shiny-notification {
                                        #            width: 400px;;
                                        #            font-size: large;;}")))
                                        
                                        ###############################
                          )
                          
                          
                          
                 ),
                 
                 
                 tabPanel("Pixel history",
                          
                          absolutePanel(id = "pixel_control", class = "well", fixed = TRUE,
                                        draggable = TRUE, top = 150, left = 20, right = "auto", bottom = "auto",
                                        width = 350, height ="auto",
                                        
                                        tags$head(tags$style(HTML('#pixel_control {background-image:
                                                 linear-gradient(green,yellow,red);}'))),
                                        
                                        h2("Pixel coordinates"),
                                        uiOutput("defcoords"),
                                        uiOutput("deftime"),
                                        
                                        h2("Plot bands"),
                                        uiOutput("band_multsel"),
                                        actionButton(inputId ="load_history", label = "Load history", icon("refresh")),
                                        
                                        
                                        h2("Normalized difference"),
                                        div(style="display:inline-block",uiOutput("ndpixhist1")),
                                        div(style="display:inline-block",uiOutput("ndpixhist2")),
                                        
                                        actionButton(inputId ="nd", label = "Calculate", icon("refresh")),
                                        
                                        h2("Pixelchip timeseries"),
                                        
                                        div(style="display:inline-block",uiOutput("bandpixelchip")),
                                        div(style="display:inline-block",uiOutput("buffer")),
                                        
                                        actionButton(inputId = "pxlchp", label = "Pixelchip", icon("refresh")),
                                        
                                        checkboxInput(inputId="full", label = "Load full extent?", value = FALSE),
                                        
                                        style = "opacity:0.8"
                                        
                          ),
                          
                          plotOutput(outputId = "history"),
                          plotOutput(outputId = "ND"),
                          plotOutput(outputId = "pixelchip")
                 ),
                 
                 includeCSS("Rastafari.css")
)


