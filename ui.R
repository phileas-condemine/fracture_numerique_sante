dashboardPage(header = dashboardHeader(title = "Accès aux soins et au numérique"),
              sidebar = dashboardSidebar( width = '250px',
                selectizeInput("choix_dep",label = "Département",choices = deps,multiple=T, options = list(maxItems = 1)),
                uiOutput('slider_lambda'),
                conditionalPanel('input.choix_dep !== null',box(background = "navy",width="12",
                    HTML("<p>Les communes sont classées grâce à un score entre 1 et 10",
                    "<b>relatif au département<b>. <ul> <li> 1 les plus démunies <li>",
                    "10 les mieux dotées</ul> Le menu glissant ci-dessus permet de modifier",
                    "l'importance relative de l'accès au numérique contre l'accès aux soins dans",
                    "la construction du score.<br>",
                    "Afin de facilier la lecture de la carte, lorsque le curseur est déplacé,",
                    "les communes dont le classement a été modifié sont détourées.</p>")))

              ),
              body = dashboardBody(
                shinycssloaders::withSpinner(
                  leafletOutput("mymap",height = "800px")
                  ,size = 2,type = 6)
              )
)
