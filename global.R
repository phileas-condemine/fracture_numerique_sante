library(shinydashboard)
library(shiny)
library(leaflet)
library(sf)
library(data.table)
library(colourvalues)

source('ident_get_ggdrive_data.R')


load("data/communes_scored.RData")
load("data/fond_de_carte.RData")
load("data/useful_thresholds.RData")
dep = readRDS('data/gadm36_FRA_2_sf.rds')

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


# leaflet()%>%addTiles()%>%
#   addPolygons(data=dep,fillOpacity = 0,fill=T,layerId = ~CC_2,label = ~NAME_2)
