ui <- dashboardPage(header = dashboardHeader(title = "AccÃ¨s aux soins et au numÃ©rique"),
              sidebar = dashboardSidebar( width = '250px',
                selectizeInput("choix_dep",label = "DÃ©partement",choices = deps,multiple=T, options = list(maxItems = 1)),
                uiOutput('slider_lambda'),
                conditionalPanel('input.choix_dep !== null',box(background = "navy",width="12",
                    HTML("<p>Les communes sont classÃ©es grÃ¢ce Ã  un score entre 1 et 10",
                    "<b>relatif au dÃ©partement<b>. <ul> <li> 1 les plus dÃ©munies <li>",
                    "10 les mieux dotÃ©es</ul> Le menu glissant ci-dessus permet de modifier",
                    "l'importance relative de l'accÃ¨s au numÃ©rique contre l'accÃ¨s aux soins dans",
                    "la construction du score.<br>",
                    "Afin de facilier la lecture de la carte, lorsque le curseur est dÃ©placÃ©,",
                    "les communes dont le classement a Ã©tÃ© modifiÃ© sont dÃ©tourÃ©es.</p>")))

              ),
              body = dashboardBody(
                shinycssloaders::withSpinner(
                  leafletOutput("mymap",height = "800px"),size = 2,type = 6),
                plotOutput("distPlot"),
                plotOutput("plot2")
              )
)


# shinyApp(ui, server)
