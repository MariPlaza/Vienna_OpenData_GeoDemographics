function(input, output, session) {
  Districts <- c(unique(Information$District))
  Type_Provider <- c(unique(Provider$Type))

  output$District <- renderUI({
    selectInput(
      "District_I", "District", as.list(Districts), multiple = FALSE
    )
  })
  
  observe({
    SubDS <- if (is.null(input$District_I)) character(0) else {
      filter(Information, District %in% input$District_I) %>%
        `$`('Sub_District') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$Sub_District[input$Sub_District %in% SubDS])
    updateSelectInput(session, "Sub_District", choices = SubDS,
                      selected = stillSelected)
  })

  output$Type <- renderUI({
    selectInput(
      "Type_I", "Type", as.list(Type_Provider), multiple = FALSE
    )
  })

  observe({
    SubType <- if (is.null(input$Type_I)) character(0) else {
      Provider %>% filter(Type %in% input$Type_I) %>%
        `$`('SubType') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$SubType_I[input$SubType_I %in% SubType])
    updateSelectInput(session, "SubType_I", choices = SubType,
                      selected = stillSelected)
  })
  
  observe({
    
    Address_Provider <- if (is.null(input$Type_I)) character(0) else {
      Provider %>% filter(District %in% input$District_I,
                          (input$Sub_District=='') | Sub_District %in% input$Sub_District) %>%
        filter(Type %in% input$Type_I,
               (input$SubType_I=='') | SubType %in% input$SubType_I) %>%
        `$`('Address') %>%
        unique() %>%
        sort()
    }
  
    Address_Population <- Population %>% filter(District %in% input$District_I,
                                              (input$Sub_District=='') | Sub_District %in% input$Sub_District) %>%
      `$`('Address') %>%
      unique() %>%
      sort()
    
    Address <- ifelse(input$Visualization=="Populations View",Address_Population, Address_Provider)
    
    stillSelected <- isolate(input$Address[input$Address %in% Address])
    updateSelectInput(session, "Address", choices = Address,
                      selected = stillSelected)
  })
  
  data_map <- reactive({
    
    
    #First Filter
    if (input$Visualization == 'Overall District'){
        data_map <- spdf[spdf$District==input$District_I,]
        center <- as.data.frame(coordinates(data_map))
        center <- center%>%summarise_each(funs(mean))
        
        xy_center <- center[,c("Longitud","Latitud")]
        Reference  <- SpatialPointsDataFrame(coords = xy_center, data = center,
                                              proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))  
    
    }else{
        if (input$Address ==''){
          data_map <- spdf[spdf$District==input$District_I,]
          center <- as.data.frame(coordinates(data_map))
          center <- center%>%summarise_each(funs(mean))
          
          xy_center <- center[,c("Longitud","Latitud")]
          Reference  <- SpatialPointsDataFrame(coords = xy_center, data = center,
                                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))  
        }
        else{
          data_map <- spdf
          Reference <- spdf[spdf$Address==input$Address,] 
        }
    }
    
    #Calculating Distance
    spdf.proj   <- spTransform(data_map,CRS(epsg.31253))
    reference.proj <- spTransform(Reference, CRS(epsg.31253))
    spdf.proj$Distance <- round(gDistance(reference.proj, spdf.proj, byid = TRUE)[,1])
    
    #Second Filter if neccesary
    
      if (input$Visualization != 'Overall District'){
        Minutes <- input$Minutes
        Velocity <- input$Velocity
        Distance_Filter <- Velocity * (Minutes/60) * 1000
        Target <- spdf.proj[spdf.proj$Distance<=Distance_Filter,]  
      }else{
        Target<-spdf.proj
      }  
    
    #Remove Projection for preparing the data for the map
    data_map <- spTransform(Target,CRS(proj4string(Limits)))
    
    #Applying GeoDemographics Formula
    data_map$Gender_Ratio <- 1
    
    if (input$Sex_OV=="Men") data_map[data_map$SEX==2,"Gender_Ratio"]<- 0
    if (input$Sex_OV=="Women") data_map[data_map$SEX==1,"Gender_Ratio"]<- 0
      
    data_map$Unique_ID <- paste0(data_map$ObjectID,'-',data_map$SEX)
    
    data_map_df <- data_map@data

    Total_Pop <- data_map_df %>%
                        group_by(ObjectID) %>%
                        summarise(Total_Population = sum(N_Population))
    

    vars_Age <- names(data_map_df)[grepl('AGE',names(data_map_df))]
    Interval_Param <- as.vector(input$age_OV)
    coeffs_Age <- Intervals_Proportions(Interval_Param, Age_Reference_Intervals, Age_Reference_Length)
    data_map_df$Age_Ratio <- as.matrix(data_map_df[,vars_Age],with=FALSE)%*%coeffs_Age
    
    vars_Nat <- names(data_map_df)[grepl('POP',names(data_map_df))]
    if(is.null(input$Nationality_OV)){
      coeffs_Nat <- rep(1, each=length(vars_Nat))
    }else{
      coeffs_Nat <- gsub('POP_','',vars_Nat) %in% input$Nationality_OV
    }
    data_map_df$Nat_Ratio <- as.matrix(data_map_df[,vars_Nat],with=FALSE)%*%coeffs_Nat
    
    data_map_df <- data_map_df %>% 
                   select(ObjectID, Unique_ID, Age_Ratio, Nat_Ratio)
    
    data_map_df$Age_Ratio <- as.numeric(data_map_df$Age_Ratio)
    data_map_df$Nat_Ratio <- as.numeric(data_map_df$Nat_Ratio)
    
    data_map_df <- data_map_df %>% 
                   left_join(Total_Pop, by ="ObjectID")
    
    data_map <- merge(data_map, data_map_df)
    
    data_map$N_Pop_TAge <- data_map$N_Population*data_map$Age_Ratio*data_map$Gender_Ratio
    data_map$N_Pop_TNat <- data_map$N_Population*data_map$Nat_Ratio*data_map$Gender_Ratio
    
    data_map$N_Pop_Target <- data_map$N_Population*data_map$Nat_Ratio*data_map$Age_Ratio*data_map$Gender_Ratio
    
    if(input$Sex_OV=='All'){
      data_map$Population_Target <- data_map$Total_Population  
    }else{
      data_map$Population_Target <-data_map$N_Population
    }
    
    data_map$Population_Target <- data_map$Population_Target*data_map$Nat_Ratio*data_map$Age_Ratio*data_map$Gender_Ratio
    
    
    data_map
  })  
  
  limits_map <- reactive({
    limits_map <-  Limits[Limits$BEZNR==input$District_I,]
    limits_map  
  }) 
  
  output$map <- renderLeaflet({
    data_base <- data_map()
    Limits_map <-limits_map()
    data_map_Customer <- data_base[data_base$Category==2,]
    
    center <- as.data.frame(coordinates(data_base))
    center <- center%>%summarise_each(funs(mean))
    
    xy_center <- center[,c("Longitud","Latitud")]
    if (input$Address !=''){
        data_map_reference <- SpatialPointsDataFrame(coords = xy_center, data = center,
                                            proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))  
      
    }else{
          data_map_reference <- data_base[data_base$Address==input$Address,]
    }
    
    
    data_map_Provider <- data_base[data_base$Category==3,]
    data_map_Transport <- data_map_Provider[data_map_Provider$Type %in% Trasnport,] 
    data_map_Education <- data_map_Provider[data_map_Provider$Type %in% Main_Education,]
    data_map_Business <-  data_map_Provider[data_map_Provider$Type %in% Business,] 
    data_map_Comp_Education <- data_map_Provider[data_map_Provider$Type %in% Complementary_Education,] 
    
    html_legend <- "<b>Markers Reference</b><br/>
                    <img src='Transport.png'>Transport<br/>
                    <img src='Education.png'>Main Education<br/>
                    <img src='Add_Education.png'>Complementary Education<br/>
                    <img src='Business.png'>Businesses<br/>
                    <img src='Reference.PNG'>References<br/>
                      "
    
    pal <- colorFactor(palette = c("#4B4545","#AA3939"), domain = c("Target","Total Population"))
    
    overall_map = leaflet(data_map_Customer) %>% 
      setView(lng = center$Longitud, lat = center$Latitud, zoom = 15) %>%
      addTiles(group = "Default") %>% 
      addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
      addCircleMarkers(
        radius = ~Total_Population/10,
        color = c("#AA3939"),
        stroke = FALSE, fillOpacity = 0.5, group = "Density"
      ) %>%
      addCircleMarkers(
        radius = ~Population_Target/10,
        color = c("#4B4545"),
        stroke = FALSE, fillOpacity = 0.5, group = "Density Target"
      ) %>%
      addAwesomeMarkers(data = data_map_reference,  group = "Reference", 
                        icon = awesomeIcons(markerColor = "black"), 
                        label=~as.character('Reference Point'), 
                        labelOptions = labelOptions(noHide = T, direction = "bottom") 
      ) %>%
      addAwesomeMarkers(data = data_map_Transport,  group = "Transport", 
                        icon = awesomeIcons(markerColor = "green", icon = 'heart'), 
                        label=~as.character(paste0(Type,' - ',SubType,' - ',Address)) 
      ) %>%
      addAwesomeMarkers(data = data_map_Education, group = "Main Education",
                        icon = awesomeIcons(markerColor = "orange", icon = 'certificate'), label=~as.character(paste0(Type,' - ',SubType,' - ',Address)) 
      ) %>%
      addAwesomeMarkers(data = data_map_Comp_Education, group = "Complementary Education",
                        icon = awesomeIcons(markerColor = "blue", icon='star'), label=~as.character(paste0(Type,' - ',SubType,' - ',Address)) 
      ) %>%
      addAwesomeMarkers(data = data_map_Business,  group = "Businesses",
                        icon = awesomeIcons(markerColor = "purple", icon='usd'), label=~as.character(paste0(Type,' - ',SubType,' - ',Address)) 
      ) %>%
      addLegend("bottomright", pal = pal, values = c("Total Population","Target"),
                title = "GeoPoint Category",
                opacity = 1
      ) %>%
      addPolygons(data = Limits_map,color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.05,
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = FALSE), group = "Borders")%>%
      # Layers control
      addLayersControl(
        baseGroups = c("Default", "Toner", "Toner Lite"),
        overlayGroups = c("Density","Density Target","Reference","Transport","Main Education","Complementary Education","Businesses","Borders"),
        options = layersControlOptions(collapsed = TRUE)
      )%>%
      addControl(html = html_legend, position = "bottomright")
    
    
    overall_map
    
      })
  
  Population_Info_DF<- reactive({
    data_base <- data_map()
    Population_Info <- data_base[data_base$Category==2,]
    Population_Info_DF <- Population_Info@data
    Population_Info_DF
  })
  
  Services_Info_DF <- reactive({
      data_base <- data_map()
      Services_Info <- data_base[data_base$Category==3,]
      Services_Info_DF <- Services_Info@data
      Services_Info_DF
  })
  
  output$Services_Panel <- output$services <-  DT::renderDataTable({
      Info_Base <- Services_Info_DF()
      Services_Summary <- Info_Base%>%
                          select(Type,SubType,Distance)%>%
                          group_by(Type,SubType)%>%
                          summarise_at(vars(Distance), funs(min,max, n()))
      
      DT::datatable(Services_Summary, 
                    escape = FALSE)
    })
  
  output$demographics_Panel <- output$demographics <-renderPlot({
      Info_Base <- Population_Info_DF()
      if (input$Visualization == 'Overall District'){
            SubTitle <- paste0('in the ', input$District_I,' District')
      }else{
            Distance <- round((input$Minutes/60) * input$Velocity * 1000)
            SubTitle <- paste0('in the ',Distance,' meters around the reference')
      }
      
      Ratios <- Info_Base%>%
                select(SEX, N_Population, N_Pop_TAge,N_Pop_TNat)%>%
                mutate(Gender = ifelse(SEX==1,"Men","Women"))%>% 
                group_by(Gender) %>% 
                summarise_each(funs(sum))%>%
                mutate(Age_Ratio = N_Pop_TAge/N_Population)%>%
                mutate(Nationality_Ratio = N_Pop_TNat/N_Population)
                       
      Population_Info_DF <-   Info_Base%>%
                              select(SEX, N_Population, 
                                      AGE_00_02, AGE_03_05, AGE_06_09, AGE_10_14, AGE_15_19, AGE_20_24, AGE_25_29, AGE_30_44, AGE_45_59, AGE_60_74, AGE_75., 
                                      POP_AUT, POP_SCG, POP_TUR, POP_DEU, POP_POL, POP_BIH, POP_HRV, POP_ROU, POP_CZE, POP_HUN, POP_OTHER)%>%
                              mutate(Gender = ifelse(SEX==1,"Men","Women"))%>% 
                              mutate_each(funs(.*N_Population), starts_with("AGE"))%>%
                              mutate_each(funs(.*N_Population), starts_with("POP"))%>%
                              group_by(Gender) %>% 
                              summarise_each(funs(sum))%>%
                              select(-SEX,-N_Population)%>%
                              gather(Demographics, Population, 2:23)%>%
                              separate(Demographics, c("Demographics_Category", "Demographics_Value"), extra = "merge")
      
      vars_Age <- unique(Population_Info_DF[Population_Info_DF$Demographics_Category=="AGE","Demographics_Value"])
      Interval_Param <- as.vector(input$age_OV)
      coeffs_Age <- as.data.frame(cbind(vars_Age,Intervals_Proportions(Interval_Param, Age_Reference_Intervals, Age_Reference_Length)),stringsAsFactors=FALSE)
      names(coeffs_Age) <- c("Demographics_Value","Ratio_Age")
      
        
      Age_Detail <-   Population_Info_DF%>%
                      filter(Demographics_Category=="AGE")%>%
                      rowwise() %>% 
                      mutate(Ratio_Gender = ifelse(input$Sex_OV=='All',1,ifelse(input$Sex_OV==Gender,1,0)))%>%
                      left_join(Ratios, by = c("Gender"))%>%
                      left_join(coeffs_Age, by = c("Demographics_Value"))%>%
                      mutate(Pop_Target = round(Population*Ratio_Gender*Nationality_Ratio*Ratio_Age))%>%
                      select(Gender, Demographics_Category, Demographics_Value, Population, Pop_Target)
      
                      
      Pop_Detail <-   Population_Info_DF%>%
                      filter(Demographics_Category=="POP")%>%
                      rowwise() %>% 
                      mutate(Ratio_Nationality = ifelse(is.null(input$Nationality_OV), 1, Demographics_Value %in% input$Nationality_OV))%>%
                      rowwise() %>% 
                      mutate(Ratio_Gender = ifelse(input$Sex_OV=='All',1,ifelse(input$Sex_OV==Gender,1,0)))%>%
                      left_join(Ratios, by = c("Gender"))%>%
                      mutate(Pop_Target = round(Population*Ratio_Gender*Age_Ratio*Ratio_Nationality))%>%
                      select(Gender, Demographics_Category, Demographics_Value, Population, Pop_Target)%>%
                      arrange(desc(Population)) 
      
      
      Age_Detail_Summary <- Age_Detail %>%
                            select(Demographics_Category, Population, Pop_Target) %>%
                            group_by(Demographics_Category) %>%
                            summarise_each(funs(sum)) 
      

      p1<-ggplot(data=Age_Detail) +
          geom_bar(aes(Demographics_Value,-Population,group=Gender), fill="#FB8792", colour="black", stat = "identity",subset(Age_Detail,Age_Detail$Gender=="Women")) + 
          geom_bar(aes(Demographics_Value,Population,group=Gender), fill="#8490DC", colour="black", stat = "identity",subset(Age_Detail,Age_Detail$Gender=="Men"))  +
          geom_bar(aes(Demographics_Value,-Pop_Target,group=Gender),fill="#FE0018", colour="black" , stat = "identity", width=.5, subset(Age_Detail,Age_Detail$Gender=="Women"))+
          geom_bar(aes(Demographics_Value,Pop_Target,group=Gender), fill="#03147D", colour="black", stat = "identity", width=.5, subset(Age_Detail,Age_Detail$Gender=="Men"))+ 
          coord_flip() + theme(panel.grid.major = element_line(colour = "white")) + theme_bw() + labs(y="Population",x="Age Ranges") +
          ggtitle(expression(atop("by Ages")))
          
      
      p2<-  ggplot(data=Pop_Detail) +
            geom_bar(aes(Demographics_Value,Population,group=Gender),fill="#FDC24C",colour="black", stat = "identity") +
            geom_bar(aes(Demographics_Value,Pop_Target,group=Gender),fill="#6B4700",colour="black", stat = "identity", width=.5) +
            theme(panel.grid.major = element_line(colour = "white")) + theme_bw() + labs(y="Population",x="Nationalities") +
            ggtitle(expression(atop("by Nationality")))
      
      p3 <-   ggplot(data = melt(Age_Detail_Summary, id.vars = "Demographics_Category"), aes(variable)) + 
              geom_bar(aes(y = round(value)), fill="#118000", stat="identity") +
              geom_text(aes(label = round(value), y = round(value)), stat = "identity", colour = "white", size = 5, position = position_stack(vjust = 0.5))+
              ggtitle(bquote(atop("Demographics Information",  atop(bold(.(SubTitle)), ""))))+
              labs(x="Type",y="Population") +
              theme(panel.grid.major = element_line(colour = "white")) + theme_bw() 
      
      multiplot(p1, p3, p2, cols=3)
      
  })
}




