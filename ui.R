dashboardPage(header = dashboardHeader(title = "Accès aux soins et au numérique"),
              sidebar = dashboardSidebar( width = '250px',
                                          selectizeInput("choix_dep",label = "Département",choices = deps,multiple=T, options = list(maxItems = 1)),
                                          uiOutput('slider_lambda'),
                                          uiOutput('choix_inds'),
                                          sidebarMenu(id="selected_tab",
                                                      menuItem("Cartographie",selected = T, tabName = "carto", icon = icon("map-pin")),
                                                      menuItem(text = "Compromis santé-numérique", tabName = "tradeoff", icon = icon("balance-scale")
                                                      ))
                                          ,uiOutput('explications')
                                          
                                          
              ),
              
              
              body = dashboardBody(
                tabItems(
                  tabItem(tabName = "carto",
                          shinycssloaders::withSpinner(
                            leafletOutput("mymap",height = "800px")
                            ,size = 2,type = 6)
                  )
                  ,
                  tabItem(tabName = "tradeoff",
                          plotlyOutput("distPlot"),
                          plotlyOutput("plot2")
                  )
                )
              )
              
)

