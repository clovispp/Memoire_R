# Charger les bibliothèques nécessaires
library(tidyverse)
library(ordinal)
library(broom)


# Lire le fichier CSV
data <- read.csv("/Users/ouedraogoclovispp/Desktop/MEMOIRE/Données/Document.csv",
                 header = TRUE, row.names = NULL, check.names = FALSE)

# Lire le fichier CSV
data <- read.csv("/Users/ouedraogoclovispp/Downloads/Document_sorted.csv",
                 header = TRUE, row.names = NULL, check.names = FALSE)

# Afficher les premières lignes pour vérifier la structure des données
head(data)
str(data)

# Convertir score_du_projet_CBDC_de_detail en facteur ordonné
data$score_du_projet_CBDC_de_detail <- factor(data$score_du_projet_CBDC_de_detail, 
                                              levels = sort(unique(data$score_du_projet_CBDC_de_detail), decreasing = TRUE), 
                                              ordered = TRUE)
str(data)
sum(is.na(data$score_du_projet_CBDC_de_detail))



# Ajuster le modèle de régression probit ordonné multivariée
model <- clm(score_du_projet_CBDC_de_detail ~ `Global Innovation Index: Global Innovation Index` + 
               Informal_Economy_Indicator + `Mobile cellular subscriptions (per 100 people)` + 
               `Trade (% of GDP)`, data = data, link = "probit")

summary(model)




# Coefficients du modèle
tidy(model)

# Calcul des pseudo R² pour évaluer l'ajustement du modèle sans conversion
pseudoR2_without_order <- function(model) {
  null.deviance <- model$null.deviance
  residual.deviance <- model$deviance
  1 - (residual.deviance / null.deviance)
}

pseudoR2_without_order(model_without_order)

# Ajuster le modèle de régression probit ordonné en convertissant la variable en facteur ordonné
data$score_du_projet_CBDC_de_detail_1 <- factor(data$score_du_projet_CBDC_de_detail, ordered = TRUE)
model_with_order <- clm(score_du_projet_CBDC_de_detail_1 ~ `Global Innovation Index: Global Innovation Index` + 
                          Informal_Economy_Indicator + `Mobile cellular subscriptions (per 100 people)` + 
                          `Trade (% of GDP)`, data = data, link = "probit")

# Résumé du modèle avec conversion
summary(model_with_order)

# Coefficients du modèle avec conversion
tidy(model_with_order)

# Calcul des pseudo R² pour évaluer l'ajustement du modèle avec conversion
pseudoR2_with_order <- function(model) {
  null.deviance <- model$null.deviance
  residual.deviance <- model$deviance
  1 - (residual.deviance / null.deviance)
}

pseudoR2_with_order(model_with_order)
