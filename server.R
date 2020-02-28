server <- function(input, output, session) {
  output$mymap <- renderLeaflet({
    bounds = st_bbox(dep)%>%unname()
    
    m = leaflet()%>%
      addTiles()%>%
      fitBounds(lng1 = bounds[1],lat1 = bounds[2],
                lng2 = bounds[3],lat2 = bounds[4])%>%
      leafem::addMouseCoordinates()%>%
      addPolygons(data=dep,fillOpacity = 0,fill=T,layerId = ~CC_2,label = ~NAME_2)
    
    
    
  })
  
  my_group = reactiveVal()

  output$slider_lambda = renderUI({
    req(input$choix_dep)
    lambdas = sort(unique(diffs_per_dep[dep%in%input$choix_dep&diff>0]$lambda))
    shinyWidgets::sliderTextInput("num_vs_sante", choices = lambdas,
                                  width = '250px',label = div(style='width:210px;',
                                                            div(style='float:left;', 'numérique'),
                                                            div(style='float:right;', 'santé')),
                  animate = animationOptions(interval =1000, loop = FALSE),
                selected=sample(lambdas,1))
  })
  
  config_plot = reactiveVal(NULL)
  current_dep = reactiveVal("00")

  
  observeEvent(input$num_vs_sante,{
    req(input$num_vs_sante)
    proxy = leafletProxy("mymap")
    lambda = input$num_vs_sante
    communes[,combo_couvertures := (lambda*(apl_mg2018/3+apl_mk2016/50+apl_inf2016/90+apl_sf2016/9)/4+(1-lambda)*(tx_4G_max+tx_3G_max)/2)]
    communes[,couvertures_tranches := floor(10*rank(combo_couvertures)/(.N+1)),by=dep]
    sub = fonds_de_carte[substr(fonds_de_carte$code,1,2)%in%input$choix_dep,]
    sub = merge(sub,na.omit(communes[,c("code","couvertures_tranches",'combo_couvertures')]),by="code")
    # cols = colour_values_rgb(Hmisc::cut2(sub$combo_couvertures,g = 10)%>%as.numeric,
    #                          palette = "green2red",include_alpha = FALSE) / 255
    # cols = colour_values_rgb(sub$couvertures_tranches,
    #                          palette = "green2red",include_alpha = FALSE) / 255
    bounds = st_bbox(sub)%>%unname()
    key = keygen()
    
    if (!is.null(config_plot())){
      modified = merge(sub,config_plot(),by="code")
      modified = modified[modified$couvertures_tranches!=modified$last_tranches,]
      proxy %>%
        addPolygons(data = modified,color = "black",fillOpacity = 0,group=key)
    }
    
    proxy %>%
      addPolygons(data=sub, fillColor = ~pal(couvertures_tranches),label=~paste(nom,'- tranche :',couvertures_tranches+1),
                  color=~pal(couvertures_tranches),opacity = 0.1,fillOpacity = .5,
                  # popup = ~nom,
                  group=key)%>%
      leaflet::clearGroup(my_group())
    
    setnames(sub,'couvertures_tranches','last_tranches')
    config_plot(sub[,c("code","last_tranches")]%>%st_set_geometry(NULL))
    my_group(key)
    
    if (current_dep() != input$choix_dep){
      bounds = st_bbox(sub)%>%unname()
      proxy %>%
        fitBounds(lng1 = bounds[1],lat1 = bounds[2],
                  lng2 = bounds[3],lat2 = bounds[4])
      current_dep(input$choix_dep)
    }
    

  })
  
  observeEvent(input$mymap_click, { # update the location selectInput on map clicks
    p <- input$mymap_click
    print(p)
  })
  observeEvent(input$go,{
    print(names(input))
  })
  
  observeEvent(input$mymap_shape_click,{
    print(input$mymap_shape_click)
    updateSelectizeInput(session,"choix_dep",selected = input$mymap_shape_click$id)
  })
  
  
  
}
