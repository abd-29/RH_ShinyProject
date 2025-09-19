#######################################################
#################### DATA MANAGEMENT #################
######################################################

############# LIBRARY IMPORTATION #####################
library(shiny)
library(readr)
library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)
library(plotly)
library(shinyWidgets)
library(bslib)



############### DATA IMPORTATION #####################
salaires <- read_excel("data/RH_Salaries.xlsx")
contrats <- read_excel("data/RH_Contrats.xlsx")

############## DATA MANIPULATION ####################



############## FUNCTIONS ###########################