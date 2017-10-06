rm(list = ls())

library('readr')
#library('mailR')
library('dplyr')
library('lubridate')   
library('scales')
#library('xlsx')
library('tidyr')
library('readtext')
#library('knitr')
#library('ggplot2')
library('purrr')

#grid
# source('segmentations.R')
# source('grid.R')
# source('dictionaries.R')
# source('writers.R')

#funnels
source('pre_funnel.R')
source('funnels.R')
source('postFunnel.R')

load("Z:/all_data.Rdata")
