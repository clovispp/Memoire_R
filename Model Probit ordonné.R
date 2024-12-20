# Charger les bibliothèques nécessaires
library(tidyverse)
library(ordinal)
library(broom)

# Lire le fichier CSV
# Lire le fichier CSV
data <- read.csv("/Users/ouedraogoclovispp/Desktop/MEMOIRE/Données/Document.csv",
                 header = TRUE, row.names = NULL, check.names = FALSE)


# Afficher les premières lignes pour vérifier la structure des données
head(data)

# Renommer les colonnes pour simplifier
colnames(data) <- c("Country", "CBDC_Score", "P2P_Volume_Ranking")

# Vérifier les niveaux de la variable dépendante
table(data$CBDC_Score)

# Convertir CBDC_Score en facteur ordonné si ce n'est pas déjà le cas
data$CBDC_Score <- factor(data$CBDC_Score, ordered = TRUE)

# Ajuster le modèle de régression probit ordonné
model <- clm(CBDC_Score ~ P2P_Volume_Ranking, data = data, link = "probit")

# Résumé du modèle
summary(model)

# Coefficients du modèle
tidy(model)

# Calcul des pseudo R² pour évaluer l'ajustement du modèle
pseudoR2 <- function(model) {
  null.deviance <- model$null.deviance
  residual.deviance <- model$deviance
  1 - (residual.deviance / null.deviance)
}

pseudoR2(model)

# Prédictions et analyse des résultats
data$predicted_score <- predict(model, newdata = data, type = "class")

# Comparaison des scores réels et prévus
table(data$CBDC_Score, data$predicted_score)
