---------------------------------------------------------
##### Faire une v2 du tableau pour avoir les count de chaque type d'activité par commune
---------------------------------------------------------
  
  
# Packages ----
install.packages("matrixStats")
library(readxl)
library(dplyr)
library(tidyr)
library(writexl)
library(matrixStats)


#Ouvrir le fichier excel ----
isere_tourisme <- read_excel("00_data_init/isere_tourisme2.xlsx")
View(isere_tourisme)

#Count de chaque type d'activité par commune
communes_stats <- isere_tourisme %>%
  count(commune, code_postal, type_regroupe) %>%
  pivot_wider(
    names_from = type_regroupe,
    values_from = n,
    values_fill = 0
  )
View(communes_stats)

#nombre total 
communes_stats <- communes_stats %>%
  mutate(
    total_activites =
      `Randonnée pédestre et découverte` +
      `Cyclisme et VTT` +
      `Équitation` +
      `Orientation et découverte` +
      `Escalade, montagne et activités verticales` +
      `Activités nautiques et aquatiques` +
      `Pêche` +
      `Sports d'hiver` +
      `Sports et loisirs sportifs` +
      `Sports aériens` +
      `Bien-être, thermalisme et remise en forme` +
      `Loisirs, culture, tourisme et équipements`
  )

# Calculer le type dominant
colonnes_types <- c(
  "Randonnée pédestre et découverte",
  "Cyclisme et VTT",
  "Équitation",
  "Orientation et découverte",
  "Escalade, montagne et activités verticales",
  "Activités nautiques et aquatiques",
  "Pêche",
  "Sports d'hiver",
  "Sports et loisirs sportifs",
  "Sports aériens",
  "Bien-être, thermalisme et remise en forme",
  "Loisirs, culture, tourisme et équipements"
)

communes_stats$type_principal <-
  colonnes_types[
    max.col(communes_stats[, colonnes_types], ties.method = "first")
  ]

# Export ----
write_xlsx(communes_stats, "00_data_init/commune_stat.xlsx")

