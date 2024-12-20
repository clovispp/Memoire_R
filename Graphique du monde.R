# Charger les bibliothèques nécessaires
library(tidyverse)
library(ggplot2)
library(rworldmap)

# Lire le fichier CSV
data <- read.csv("/Users/ouedraogoclovispp/Desktop/MEMOIRE/Données/Kelly.csv",
                 header = TRUE, row.names = NULL, check.names = FALSE)
# Sélectionner les colonnes d'intérêt
selected_data_crypto <- Crypto_Adoption[, c("Country", "Classement_du_volume_d_echange_pair_a_pair_P2P")]
selected_data_cbdc <- CBDC_Adoption[, c("Country", "score_du_projet_CBDC_de_detail")]

# Afficher les premières lignes pour vérifier la structure des données
head(selected_data)

# Renommer les colonnes pour simplifier
colnames(selected_data) <- c("Country", "score_du_projet_CBDC_de_detail", "Classement_du_volume_d_echange_pair_a_pair_P2P")

# Vérifier si les noms des pays correspondent à ceux de la bibliothèque rworldmap
unique(selected_data$Country)

# Joindre les données avec le jeu de données de carte du monde
world_map_cbdc <- joinCountryData2Map(selected_data_cbdc, joinCode = "NAME", nameJoinColumn = "Country")
world_map_crypto <- joinCountryData2Map(selected_data_crypto, joinCode = "NAME", nameJoinColumn = "Country")

# Créer le graphique
par(mfrow=c(1, 2))

# Carte pour score du projet CBDC de détail
mapCountryData(world_map_cbdc, nameColumnToPlot = "score_du_projet_CBDC_de_detail", 
               mapTitle = "Classement du volume d'échange P2P", 
               catMethod = "fixedWidth", 
               colourPalette = "heat",)

# Carte pour classement du volume d'échange P2P
mapCountryData(world_map_crypto, nameColumnToPlot = "Classement_du_volume_d_echange_pair_a_pair_P2P", 
               mapTitle =  "Classement du volume d'échange P2P", 
               catMethod = "fixedWidth", 
               colourPalette = "heat")
stargazer(summary(selected_data), type = "latex")
