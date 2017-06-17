Visualization_Panel <- absolutePanel(id = "graphs", class = "panel panel-default", fixed = TRUE
                              ,draggable = TRUE, bottom = "auto", left = "auto", right = 30, top = 100, width = 800, height = "auto"
                              ,h3("Demographics Detail")
                              ,selectInput("Visual_Geo", "Visualize by:", as.list(c("GeoDemographics Graphs","Services Summary")), multiple = FALSE)
                              ,conditionalPanel("input.Visual_Geo == 'Services Summary'"
                                                ,DT::dataTableOutput("Services_Panel"))
                              ,conditionalPanel("input.Visual_Geo == 'GeoDemographics Graphs'"
                                                ,plotOutput("demographics_Panel"))
                              )

District_Options <- fluidRow(column(6,uiOutput("District")),
                             column(6,conditionalPanel("input.Visualization != 'Overall District'",
                                                     selectInput("Sub_District", "SubDistrict", c("All SubDistricts"=""), multiple=FALSE))))
Type_Options<- fluidRow(column(6,uiOutput("Type")),
                        column(6,conditionalPanel("input.Type_I",
                                                selectInput("SubType_I", "SubType", c("All Subtypes"=""), multiple=FALSE))))
Address_Options<-fluidRow(column(12,selectInput("Address", "Select specific Address", c("All Addresses"=""), multiple=FALSE)))
Distance_Options<- fluidRow(column(6,numericInput('Minutes', 'Walking Minutes', 5,min = 1, max = 10)),
                            column(6,numericInput('Velocity', 'Velocity (Km/h)', 4,min = 2.5, max = 6, step = 0.1)))
GeoDemographics_Options<- fluidRow(sliderInput("age_OV", label = "Age Range", min = 0, 
                                      max = 100, value = c(0, 100)),
selectInput("Sex_OV", "Gender", as.list(c("All","Men","Women")), multiple = FALSE),
selectInput("Nationality_OV", "Nationality", as.list(c("AUT","SCG","TUR","DEU","POL","BIH","HRV","ROU","CZE","HUN","OTHER")), multiple = TRUE))


Options_Panel <- absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE, 
                               draggable = TRUE, top = 60, left = 70, right = "auto", bottom = "auto",
                               width = 330, height = "auto",
                               h3("Visualize by:")
                               ,selectInput("Visualization", "", as.list(c("Overall District","Populations View","Providers View")), multiple = FALSE)
                               ,h3("Filters")
                               ,District_Options
                               ,conditionalPanel("input.Visualization == 'Providers View'",Type_Options)
                               ,conditionalPanel("input.Visualization == 'Providers View'|| input.Visualization == 'Populations View'",
                                                Address_Options, 
                                                h3("Distance Filter Criterias"),
                                                Distance_Options
                                                ),
                               conditionalPanel("input.Visualization == 'Providers View'  || input.Visualization == 'Overall District'",
                                                h3("GeoDemographics Filters"), 
                                                GeoDemographics_Options 
                                                ),
                               checkboxInput("V_Geo_Demo", label = "Visualize Geodemographics over Map", value = FALSE)
)


navbarPage("Vienna GeoDemographics", id="nav"
           ,tabPanel("Map"
                     ,div(class="outer"
                          ,tags$head(
                            includeCSS("www/style.css")                          
                            )
                        ,leafletOutput("map", width="100%", height="100%")
                        ,conditionalPanel("input.V_Geo_Demo",Visualization_Panel)
                        ,Options_Panel)
           )
          ,tabPanel("Services Summary"
                    ,h3("Services Summary")
                    ,DT::dataTableOutput("services")  
                   )
          ,tabPanel("GeoDemographics Summary"
                    ,h3("GeoDemographics")
                    ,plotOutput("demographics")
                   )
          ,tabPanel("About Me"
                    ,h3("Prototype developed by Maria Ines Plaza Schwarck")
                    ,br()
                    ,br()
                    ,h4("These are my profiles in Social Professional Networks:")
                    ,br()
                    ,br()
                    ,fluidRow(
                            column(width = 1
                              ,tags$div(class="header", checked=NA
                              ,tags$a(href="https://github.com/MariPlaza"
                              ,tags$img(src="Github.png")))
                            )
                           ,column(width =3, 
                                  tags$div(class="header", checked=NA
                                  ,tags$a(href="https://www.linkedin.com/in/maria-ines-plaza-schwarck-9825962/"
                                  ,tags$img(src="Linkedin.png"))
                                   )
                            )
                      )
          )
          
        )        
