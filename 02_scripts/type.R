---------------------------------------------------------
##### Faire une version simplifier de la colonne "type"
---------------------------------------------------------


# Packages ----
  #install.packages("writexl")
library(readxl)
library(dplyr)
library(writexl)

#Inséré le fichier excel ----
isere_tourisme <- read_excel("00_data_init/isere_tourisme_insee.xlsx")
View(isere_tourisme)


# Liste des valeurs uniques de la colonne type
types_list <- unique(isere_tourisme$type)
# Afficher la liste
print(types_list)

# Création de la nouvelle colonne (classe avec l'IA)
df <- isere_tourisme %>%
  mutate(
    type_regroupe = case_when(
      
      # 1. Randonnée pédestre et découverte
      type %in% c(
        "Itinéraire de randonnée pédestre",
        "Itinéraires trail",
        "Itinéraire de Trail",
        "Parcours / sentier thématique",
        "Randonnées accompagnées",
        "Sports pédestres",
        "Voies et sommets"
      ) ~ "Randonnée pédestre et découverte",
      
      # 2. Cyclisme et VTT
      type %in% c(
        "Itinéraire VTT",
        "Itinéraires VTT",
        "Itinéraire de VTT à Assistance Électrique",
        "Itinéraire de Vélo à Assistance Electrique",
        "Itinéraire gravel bike",
        "Itinéraire Fat Bike",
        "Itinéraire enduro",
        "Itinéraire cyclotourisme",
        "Itinéraires cyclotourisme",
        "Itinéraire cyclo",
        "Véloroute",
        "Voie verte",
        "Véloroute et voie verte",
        "Sports cyclistes"
      ) ~ "Cyclisme et VTT",
      
      # 3. Équitation
      type %in% c(
        "Itinéraire de randonnée équestre",
        "Itinéraires de randonnées équestres",
        "Centre équestre"
      ) ~ "Équitation",
      
      # 4. Orientation et découverte
      type %in% c(
        "Parcours d'orientation",
        "Parcours d'orientation en VTT",
        "Geocaching"
      ) ~ "Orientation et découverte",
      
      # 5. Escalade, montagne et activités verticales
      type %in% c(
        "Site de blocs",
        "Site d'escalade",
        "Voie d'escalade",
        "Mur d'escalade",
        "Via ferrata",
        "Via corda",
        "Tyrolienne",
        "Slackline",
        "Site de spéléologie"
      ) ~ "Escalade, montagne et activités verticales",
      
      # 6. Activités nautiques et aquatiques
      type %in% c(
        "Eau libre",
        "Halte nautique",
        "Itinéraire en canoë / en kayak",
        "Base de canoë kayak/Aviron",
        "Base nautique",
        "Rampe de mise à l'eau",
        "Téleski nautique",
        "Croisière",
        "Parcs aquatiques"
      ) ~ "Activités nautiques et aquatiques",
      
      # 7. Pêche
      type %in% c(
        "Site de pêche – 2ème catégorie",
        "Site de pêche - 1ère catégorie",
        "Pêche",
        "Plan d’eau ou rivière réciprocitaire",
        "Plan d’eau ou rivière non réciprocitaire",
        "Eau close"
      ) ~ "Pêche",
      
      # 8. Sports d'hiver
      type %in% c(
        "Itinéraire raquettes",
        "Espace raquette à neige",
        "Stade de biathlon",
        "Itinéraire de ski de randonnée alpin",
        "Itinéraire de ski de randonnée",
        "Ski",
        "Sports d'hiver",
        "Sports de glace",
        "Patinoire",
        "Parc DVA",
        "Piste de luge enfants / débutants",
        "Piste de luge enfants",
        "Piste de luge sportive / bobsleigh",
        "Tubing",
        "Snow tubing"
      ) ~ "Sports d'hiver",
      
      # 9. Sports et loisirs sportifs
      type %in% c(
        "Practice park",
        "Golf",
        "Golf 9 trous",
        "Golf 18 trous",
        "Disc Golf",
        "Paintball",
        "Trampoline / accro bungy",
        "Parcours santé/obstacles",
        "Parcours de santé",
        "Aire de musculation / fitness",
        "Salle de sport",
        "Gymnase",
        "Minigolf",
        "Bowling",
        "Sports de sable",
        "Sports divers"
      ) ~ "Sports et loisirs sportifs",
      
      # 10. Sports aériens
      type %in% c(
        "Site d'envol et d'atterrissage",
        "Sports aériens",
        "Aérodrome / altiport",
        "Survols en avion, hélico, montgolfière"
      ) ~ "Sports aériens",
      
      # 11. Bien-être, thermalisme et remise en forme
      type %in% c(
        "Espace Spa thermal",
        "Espace Spa",
        "Centre de balnéothérapie",
        "Centre de remise en forme",
        "Forme et bien-être",
        "Etablissement thermal",
        "Hammam"
      ) ~ "Bien-être, thermalisme et remise en forme",
      
      # 12. Loisirs, culture, tourisme et équipements
      type %in% c(
        "Galerie expo",
        "Jeu d'évasion/ Escape Game",
        "Parc à thèmes",
        "Jeux pour enfants",
        "Jeu de piste / Chasse au trésor",
        "Karaoké box",
        "Salle de spectacle",
        "Routes touristiques",
        "Trains touristiques",
        "Transports touristiques",
        "Zone ludique",
        "Base de loisirs",
        "Plages et bases de loisirs",
        "Espace public numérique",
        "Espace de réception",
        "Parc animalier / Zoo",
        "Grotte touristique / Autres sites naturels aménagés",
        "Salle de jeux autour des nouvelles technologies",
        "Bar et pub",
        "Bar à vin",
        "Salle de concert",
        "Casino",
        "Divertissement",
        "Loisirs",
        "Loisirs culturels",
        "Jeux de rôle / Enigmes",
        "Complexe de loisirs multiactivités",
        "Remontées mécaniques / téléphériques",
        "Domaine public",
        "Gestion privée",
        "Piste de descente",
        "Salle de billard", 
        "Aire de pique-nique"
      ) ~ "Loisirs, culture, tourisme et équipements",
      
      TRUE ~ "Non classé"
    )
  )
# Voir si il y a des type non classé
df %>%
  filter(type_regroupe == "Non classé") %>%
  count(type, sort = TRUE)


# Export ----
write_xlsx(df, "00_data_init/isere_tourisme_insee2.xlsx")