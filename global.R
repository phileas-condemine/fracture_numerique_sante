library(shinydashboard)
library(shiny)
library(leaflet)
library(sf)
library(data.table)
library(colourvalues)
library(ggplot2)
library(plotly)

# source('ident_get_ggdrive_data.R')
source('ident_get_dropbox.R')


load("data/communes_scored.RData")
load("data/fond_de_carte.RData")
load("data/useful_thresholds.RData")
dep = readRDS('data/gadm36_FRA_2_sf.rds')
bounds = st_bbox(dep)%>%unname()
m = leaflet()%>%
  addTiles()%>%
  fitBounds(lng1 = bounds[1],lat1 = bounds[2],
            lng2 = bounds[3],lat2 = bounds[4])%>%
  leafem::addMouseCoordinates()%>%
  addPolygons(data=dep,fillOpacity = 0,fill=T,layerId = ~CC_2,label = ~NAME_2)



communes[,dep:=substr(code,1,2)]

deps = sort(unique(substr(fonds_de_carte$code,1,2)))

input = list('choix_dep'='05','num_vs_sante'=.5)

keygen <- function(n = 1) {
  a <- do.call(paste0, replicate(10, sample(LETTERS, n, TRUE), FALSE))
  a
}

pal <- colorNumeric(
  palette = "Blues",
  domain = 0:9)

explication_tradeoff = HTML("<p>Les communes sont classées grâce à un score entre 1 et 10",
                            "<b>relatif au département<b>. <ul> <li> 1 les plus démunies <li>",
                            "10 les mieux dotées</ul> Le menu glissant ci-dessus permet de modifier",
                            "l'importance relative de l'accès au numérique contre l'accès aux soins dans",
                            "la construction du score.<br>",
                            "Afin de facilier la lecture de la carte, lorsque le curseur est déplacé,",
                            "les communes dont le classement a été modifié sont détourées.</p>")


seuils_sant = list(
  "apl_mg2018" =  c(2.5,4),
  "apl_inf2016" = c(70,140),
  "apl_sf2016" = c(6,15),
  "apl_mk2016" = c(40,100)
)

seuils_num = c('tx_4G_max'=.8,'tx_3G_max'=.95)
