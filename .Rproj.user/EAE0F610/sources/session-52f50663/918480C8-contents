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
Salaires <- read_excel("data/RH_Salaries.xlsx")
Contrats <- read_excel("data/RH_Contrats.xlsx")

############## DATA MANIPULATION ####################

# renommer quelques colonnes des tables salaires et contrats 
# les noms de colonnes en minuscules

Salaires <- Salaires %>%
  rename(
    id_salarie = `id_salarié`,
    annee_naissance = `Année naissance`,
    etat_civil = `Etat Civil`,
    sexe = Sexe,
    enfants = Enfants,
    salaire = Salaire,
    age = Age
  )

Contrats <- Contrats %>%
  rename(
    id_salarie = `id_salarié`,
    duree_hebdo = `Durée hebdo`,
    contrat = Contrat
    
  )

# exclure les salaries qui n'ont pas de contrat
data <- Salaires %>%
  inner_join(Contrats, by = "id_salarie")



############## FUNCTIONS ###########################

