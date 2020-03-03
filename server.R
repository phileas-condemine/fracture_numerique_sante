server <- function(input, output, session) {
  
  observeEvent(input$selected_tab,{
    print('tab selectionnée')
    print(input$selected_tab)
  })
  
  output$mymap <- renderLeaflet({
    
    m
    
    
  })
  
  my_group = reactiveVal()
  
  output$choix_inds = renderUI({
    req(input$choix_dep)
    if(input$selected_tab=="tradeoff"){
      tagList(
        selectizeInput("Indicateur d'offre numérique",inputId = "num_ind",
                       setNames(c('tx_4G_max','tx_3G_max'),c('Meilleure couverture 4G','Meilleure couverture 3G'))),
        selectizeInput("Indicateur d'offre de soins",inputId = "sant_ind",
                       setNames(c('apl_mg2018','apl_inf2016','apl_sf2016','apl_mk2016'),
                                c("APL Médecins Généralistes","APL Infirmiers","APL Sages-femmes","APL Masseurs Kinésithérapeutes"))))
    }else NULL
  })
  output$explications = renderUI({
    req(input$choix_dep)
    if(input$selected_tab=="carto"){
      box(background = "navy",width="12",explication_tradeoff,collapsible = T)
    } else NULL
  })
  output$slider_lambda = renderUI({
    req(input$choix_dep)
    if(input$selected_tab=="carto"){
      lambdas = sort(unique(diffs_per_dep[dep%in%input$choix_dep&diff>0]$lambda))
      shinyWidgets::sliderTextInput("num_vs_sante", choices = lambdas,
                                    width = '250px',label = div(style='width:210px;',
                                                                div(style='float:left;', 'numérique'),
                                                                div(style='float:right;', 'santé')),
                                    animate = animationOptions(interval =1000, loop = FALSE),
                                    selected=sample(lambdas,1))
    } else NULL
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
  
  
  
  output$distPlot <- renderPlotly({
    req(input$choix_dep)
    req(input$sant_ind)
    req(input$num_ind)
    print('make scatter plot')
    val = input$choix_dep
    # browser()
    data = communes[dep == val, c(input$num_ind, input$sant_ind,"lib_com"),with=F]
    setnames(data,c(input$num_ind, input$sant_ind),c("num_ind","sant_ind"))
    seuils = seuils_sant[[input$sant_ind]]
    seuil_num = seuils_num[input$num_ind]
    num_txt = c('tx_3G_max'="3G","tx_4G_max"="4G")
    sant_text = c('apl_mg2018'='les médecins généralistes en 2018',
                  'apl_inf2016'='les infirmiers en 2016',
                  'apl_sf2016'='les sages-femmes en 2016',
                  'apl_mk2016'='les masseurs kinésithérapeutes en 2016')
    top_value = max(data$sant_ind)
    ajustement_y = top_value/8
    print(ajustement_y)
    
    distplot <- ggplot(data = data,
                aes(x = num_ind,
                    y = sant_ind,label=lib_com)) + 
      geom_point(stat = "identity") + 
      geom_vline(xintercept = seuil_num) + 
      geom_hline(yintercept = seuils[1]) + 
      geom_hline(yintercept = seuils[2]) + 
      annotate(geom = "text", x = 0, y = 0, hjust = 0,
               label = "Couverture médicale faible",
               color = rgb(192/255, 0, 0)) + 
      annotate(geom = "text", x = 0, y = -ajustement_y, hjust = 0,
               label = "Couverture numérique faible",
               color = rgb(192/255, 0, 0)) + 
      annotate(geom = "text", x = 1.1, y = 0, hjust = 0,
               label = "Couverture médicale faible",
               color = rgb(192/255, 0, 0)) + 
      annotate(geom = "text", x = 1.1, y = -ajustement_y, hjust = 0,
               label = "Couverture numérique forte",
               color = rgb(112/255, 173/255, 71/255)) +
      annotate(geom = "text", x = 0, y = top_value, hjust = 0,
               label = "Couverture numérique faible",
               color = rgb(192/255, 0, 0)) + 
      annotate(geom = "text", x = 0, y = top_value + ajustement_y, hjust = 0,
               label = "Couverture médicale forte",
               color = rgb(112/255, 173/255, 71/255))+
      annotate(geom = "text", x = 1.1, y = top_value, hjust = 0,
               label = "Couverture numérique forte",
               color = rgb(112/255, 173/255, 71/255)) +
      annotate(geom = "text", x = 1.1, y = top_value + ajustement_y, hjust = 0,
               label = "Couverture médicale forte",
               color = rgb(112/255, 173/255, 71/255)) +
      xlim(-.1, 1.6) + 
      ylim(-2*ajustement_y,top_value+2*ajustement_y)+
      xlab(paste0("Couverture numérique : meilleure couverture de la commune en ",num_txt[input$num_ind])) +
      ylab(paste0("Couverture médicale :\nAPL pour ",sant_text[input$sant_ind])) +
      ggtitle(paste0("Répartition des communes du département ", val, "\nselon leurs couvertures numérique et médicale")) +
      theme_grey()+
      theme(plot.title = element_text(colour = rgb(0, 0, 128/255), 
                                      hjust = 0.5,
                                      face = "bold"))
    
    ggplotly(distplot)
    
  })
  
  output$plot2 <- renderPlotly({
    req(input$choix_dep)
    req(input$sant_ind)
    req(input$num_ind)
    # browser()
    print('make cols plot')
    val = input$choix_dep

    seuils = seuils_sant[[input$sant_ind]]
    seuil_num = seuils_num[input$num_ind]
    
    data = communes[dep==input$choix_dep,c(input$num_ind,'pop2016',input$sant_ind),with=F]
    setnames(data,c(input$num_ind,input$sant_ind),c("num_ind","sant_ind"))
    data[sant_ind<=seuils[2],med_mood:="Médicale (=)"]
    data[sant_ind<=seuils[1],med_mood:="Médicale (-)"]
    data[sant_ind>seuils[2],med_mood:="Médicale (+)"]
    data[,med_mood:=factor(med_mood,levels=c("Médicale (-)","Médicale (=)","Médicale (+)"))]
    data[num_ind<=seuil_num,num_mood:="Numérique (-)"]
    data[num_ind>seuil_num,num_mood:="Numérique (+)"]
    data[,num_mood:=factor(num_mood,levels=c("Numérique (-)","Numérique (+)"))]
    data = data[,.('Nombre de communes' = .N,"Nombre d'habitants (milliers)" = round(sum(pop2016)/1000)),by=c('med_mood','num_mood')]
    facetted_cols <- ggplot(data=melt(data,id.vars = c('med_mood','num_mood')))+
      geom_col(aes(x=med_mood,y=value,fill=variable),position = position_dodge())+
      facet_grid(.~num_mood)+
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            axis.title = element_blank(),
            plot.title = element_text(colour = rgb(0, 0, 128/255), 
                                      hjust = 0.5,
                                      face = "bold")) +
      scale_fill_manual(values = c(rgb(0, 0, 128/255), rgb(1, 102/255, 0))) + 
      ggtitle(paste0("Couvertures en numérique et santé du département ", input$choix_dep))
    
    ggplotly(facetted_cols)
    
    
    
  })
  
  
}
