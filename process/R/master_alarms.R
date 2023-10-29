
# Packages


library(fastverse)
library(RCurl)
library(httr2)
library(jsonlite)
library(sf)
library(arcpullr)
library(stringdist)
library(ggplot2)
library(qs)
library(anytime)
library(units)
library(foreach)
library(ragg)
library(ggtext)
library(geomtextpath)
library(ggh4x)
library(polyglotr)
library(gganimate)
library(colorspace)
library(wesanderson)


options(scipen = 999)

## utils ------------
he_tran <- function(text) {
  he_text <-polyglotr::mymemory_translate(text = text,source_language = "he")
  rstudioapi::insertText(text = he_text)
}
#---------------



# ran source -----------------------------

source("process/R/create_alarms_geo.R")
source("process/R/create_alarms_map.R")

