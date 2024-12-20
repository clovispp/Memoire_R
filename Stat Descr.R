# Installer et charger le package psych
install.packages("psych")
install.packages(c("Hmisc", "pastecs"))
library(Hmisc)
library(pastecs)
library(psych)
library(ggplot2)
library(haven)
library(tidyverse)
library(tidyr)
library(dplyr)
library(factoextra)
library(fpc)
library(NbClust)
library(AER)
library(descr)
library(corrplot)
library(stargazer)
library(magrittr)
library(lmtest)
library(sandwich)
library(readr)
library(readxl)
library(dplyr)
library(zoo)
library(ggplot2)
# Installer les packages si nécessaire
install.packages("MASS")
install.packages("ggplot2")
install.packages("reshape2")

# Charger les packages
library(MASS)
library(ggplot2)
library(reshape2)

# Lire le fichier CSV
data <- read.csv("/Users/ouedraogoclovispp/Desktop/MEMOIRE/Données/Document.csv",
                 header = TRUE, row.names = NULL, check.names = FALSE)

# Statistiques descriptives générales
describe(data)

# Statistiques descriptives pour les variables d'intérêt
summary(data$score_du_projet_CBDC_de_detail)
# Charger le package stargazer
library(stargazer)

data_1<-

# Générer les statistiques descriptives en LaTeX
stargazer(selected_data, type = "latex", title = "Statistiques Descriptives", summary = TRUE)

describe(selected_data)
# Calculer des statistiques descriptives détaillées
detailed_stats <- describe(selected_data)

# Convertir les statistiques en data frame
stats_df <- as.data.frame(t(detailed_stats))

# Convertir les statistiques descriptives en LaTeX avec stargazer
stargazer(stats_df, type = "latex", title = "Statistiques Descriptives", summary = FALSE)


# Delete rows with missing values
data.na <- na.omit(data)
# Table de fréquences pour les variables ordinales/catégorielles
table(data$score_du_projet_CBDC_de_detail)

summary(data)

# Histogramme pour la variable Classement_du_volume_d_echange_pair_a_pair_P2P
hist(data$Classement_du_volume_d_echange_pair_a_pair_P2P, 
     main = "Distribution du Classement du volume d'échange pair à pair (P2P)",
     xlab = "Classement du volume d'échange pair à pair (P2P)",
     col = "blue", 
     border = "black")


# Boîte à moustaches pour la variable score_du_projet_CBDC_de_detail
boxplot(data$Classement_du_volume_d_echange_pair_a_pair_P2P ~ data$score_du_projet_CBDC_de_detail, 
        main = "Classement du volume d'échange P2P par score du projet CBDC de détail",
        xlab = "Score du projet CBDC de détail",
        ylab = "Classement du volume d'échange P2P",
        col = "orange")


# Calculer la moyenne et l'écart-type du Classement_du_volume_d_echange_pair_a_pair_P2P par groupe de score_du_projet_CBDC_de_detail
aggregate(Classement_du_volume_d_echange_pair_a_pair_P2P ~ score_du_projet_CBDC_de_detail, data, function(x) c(mean = mean(x), sd = sd(x)))


# Installer et charger les packages nécessaires pour les visualisations
install.packages("ggplot2")
library(ggplot2)

# Boxplot du Classement_du_volume_d_echange_pair_a_pair_P2P par score_du_projet_CBDC_de_detail
ggplot(data, aes(x = score_du_projet_CBDC_de_detail, y = Classement_du_volume_d_echange_pair_a_pair_P2P)) +
  geom_boxplot(fill = "orange") +
  labs(title = "Classement du volume d'échange P2P par score du projet CBDC de détail",
       x = "Score du projet CBDC de détail",
       y = "Classement du volume d'échange P2P")

# Scatter plot avec tendance linéaire
ggplot(data, aes(x = Classement_du_volume_d_echange_pair_a_pair_P2P, y = as.numeric(score_du_projet_CBDC_de_detail))) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relation entre le classement P2P et le score du projet CBDC de détail",
       x = "Classement du volume d'échange P2P",
       y = "Score du projet CBDC de détail (numérique)")

# Installer et charger le package ggbiplot pour visualiser les résultats de l'ACP
install.packages("ggbiplot")
library(ggbiplot)





# Sélectionner les variables numériques à inclure dans l'ACP
numerical_data <- selected_data[, sapply(selected_data, is.numeric)]

# Supprimer les lignes avec des valeurs manquantes
numerical_data <- na.omit(numerical_data)

# Normaliser les données (mettre à l'échelle les variables)
numerical_data_scaled <- scale(numerical_data)

# Réaliser l'ACP
pca_result <- prcomp(numerical_data_scaled, center = TRUE, scale. = TRUE)

# Résumé des résultats de l'ACP
summary(pca_result)
stargazer(pca)
# Supprimer les lignes avec des valeurs manquantes
selected_data_PCA <- na.omit(selected_data)
# Normaliser les données (mettre à l'échelle les variables)
selected_data_PCA <- scale(selected_data_PCA)
# Vérifier les dimensions des données nettoyées
print(dim(selected_data_PCA))

# Vérifier les dimensions des résultats PCA
print(dim(pca_result$x))  
  
  
  
  
  
# Graphique des composantes principales
ggbiplot(pca_result, obs.scale = 1, var.scale = 1, 
         groups = selected_data_PCA$cbd, ellipse = TRUE, 
         circle = TRUE) +
  scale_color_discrete(name = '') +
  theme_minimal() +
  labs(title = "Graphique des composantes principales",
       x = "Composante principale 1",
       y = "Composante principale 2")

# Sélectionner les variables numériques
numerical_data <- data[, sapply(data, is.numeric)]

# Supprimer les lignes avec des valeurs manquantes
numerical_data <- na.omit(numerical_data)

# Calculer la matrice de corrélation
cor_matrix <- cor(numerical_data)
table(cor_matrix)
# Afficher la matrice de corrélation
print(cor_matrix)

# Installer et charger le package corrplot pour visualiser la matrice de corrélation
install.packages("corrplot")
library(corrplot)

# Visualiser la matrice de corrélation
corrplot(cor_matrix, method = "circle", type = "upper", 
         tl.col = "black", tl.srt = 45, 
         addCoef.col = "black", number.cex = 0.7)

corrplot(cor_matrix, method = "circle")


######################################### VISUALISATION DE REGRESSION

# Créer une séquence de valeurs pour Classement_du_volume_d_echange_pair_a_pair_P2P
new_data <- data.frame(Classement_du_volume_d_echange_pair_a_pair_P2P = seq(min(data$Classement_du_volume_d_echange_pair_a_pair_P2P), 
                                                                            max(data$Classement_du_volume_d_echange_pair_a_pair_P2P), 
                                                                            length.out = 100))
new_data1 <- data.frame(Classement_du_volume_d_echange_pair_a_pair_P2P = seq(min(data$Classement_du_volume_d_echange_pair_a_pair_P2P), 
                                                                            max(data$Classement_du_volume_d_echange_pair_a_pair_P2P), 
                                                                            length.out = 100))

# Prédictions des probabilités pour chaque niveau de score_du_projet_CBDC_de_detail
pred_probs <- predict(model_probit_ord, newdata = new_data, type = "prob")

# Convertir en data frame pour ggplot2
pred_df <- cbind(new_data, pred_probs)
pred_df <- melt(pred_df, id.vars = "Classement_du_volume_d_echange_pair_a_pair_P2P", variable.name = "Level", value.name = "Probability")

# Créer la visualisation avec ggplot2
ggplot(pred_df, aes(x = Classement_du_volume_d_echange_pair_a_pair_P2P, y = Probability, color = Level)) +
  geom_line(linewidth = 1) +  # Utiliser linewidth au lieu de size
  labs(title = "Probabilités Prédites par le Modèle Probit Ordonné",
       x = "Classement du volume d'échange pair à pair (P2P)",
       y = "Probabilité Prédites") +
  theme_minimal()





selected_data$CBDCD<- as.numeric(selected_data$CBDCD)
describe(selected_data)
# Calculer des statistiques descriptives détaillées
detailed_stats <- describe(selected_data)

# Convertir les statistiques en data frame
stats_df <- as.data.frame(t(detailed_stats))

# Convertir les statistiques descriptives en LaTeX avec stargazer
stargazer(stats_df, type = "latex", title = "Statistiques Descriptives", summary = FALSE)
str(selected_data)
# Calculer des statistiques descriptives pour chaque variable numérique
mean_vals <- sapply(selected_data, mean)
sd_vals <- sapply(selected_data, sd)
median_vals <- sapply(selected_data, median)
min_vals <- sapply(selected_data, min)
max_vals <- sapply(selected_data, max)
quantile_vals <- sapply(selected_data, quantile, )

# Combiner les statistiques dans un data frame
stats_df <- data.frame(
  Mean = mean_vals,
  SD = sd_vals,
  Median = median_vals,
  Min = min_vals,
  Max = max_vals)

# Ajouter les noms des variables
stats_df$Variable <- row.names(stats_df)
stats_df <- stats_df[, c("Variable", "Mean", "SD", "Median", "Min", "Max")]
row.names(stats_df) <- NULL

# Convertir les statistiques descriptives en LaTeX avec stargazer
stargazer(stats_df, type = "latex", title = "Statistiques Descriptives", summary = FALSE, rownames = FALSE)
# Créer un tableau LaTeX avec stargazer
stargazer(selected_data, type = "latex", title = "Statistiques Descriptives", summary = TRUE )

# Calculer les statistiques descriptives détaillées
detailed_stats <- describe(selected_data)

# Convertir l'objet describe en data frame
detailed_stats_df <- as.data.frame(detailed_stats)

# Ajouter la médiane
medians <- apply(selected_data, 2, median, na.rm = TRUE)
detailed_stats <- as.data.frame(detailed_stats)
detailed_stats$median <- medians

# Préparer les données pour stargazer
stargazer_data <- selected_data %>%
  select(n, mean, sd, min, median, max) %>%
  rename(N = n, Mean = mean, `St. Dev.` = sd, Min = min, Median = median, Max = max)

# Créer un tableau LaTeX avec stargazer
stargazer(stargazer_data, type = "latex", title = "Statistiques Descriptives", summary = FALSE, rownames = TRUE)


log_P2P <- log(data$Classement_du_volume_d_echange_pair_a_pair_P2P)





#"Country"                                                                          
#[2] ""                                                       
#[3] "score_global_du_projet"                                                           
#[4] "score_du_projet_CBDC_de_detail"                                                   
#[5] "score_du_projet_CBDC_de_gros"                                                     
#[6] "interet_de_recherche_normalise"                                                   
#[7] "Indice_des_discours_des_banquiers_centraux_normalise"                             
#[8] "interet_de_recherche"                                                             
#[9] "indice_de_position_des_banquiers_centraux"                                        
#[10] "Classement_general_de_l_indice"                                                   
#[11] "Classement_de_la_valeur_des_services_centralises_recus"                           
#[12] "Classement_de_la_valeur_des_services_centralises_reçus_dans_le_commerce_de_detail"
#[13] "Classement_du_volume_d_echange_pair_a_pair_P2P"                                   
#[14] "Classement_de_la_valeur_recue_en_DeFi"                                            
#[15] "Classement_de_la_valeur_reçue_en_DeFi_de_detail"                                  
#[16] "Global Innovation Index: Global Innovation Index"                                 
#[17] "Mobile cellular subscriptions (per 100 people)"                                   
#[18] "Trade (% of GDP)"