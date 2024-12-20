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
##############################################################################################################

#Remitance_outflows
#Remitance_inflows
#Remitance_inflows_GD
#WIPO_GII_filtered_data
#WDI
#Mobile_Money_Adoption_Index_pivoted_data
#CBDC_Adoption
#Crypto_Adoption
#Crypto_Ban
#Financial_Development_Index_pivoted_data
#Informal_Economy
#WEO

###############################################################################################################

# Economie informelle

# Lire le fichier CSV sans utiliser de colonne pour les noms de ligne
Informal_Economy <- read.csv("/Users/ouedraogoclovispp/Desktop/MEMOIRE/Données/Economonie informelle MIMIC_p.csv",
                header = TRUE, row.names = NULL, sep = ";", check.names = FALSE)

Informal_Economy <- Informal_Economy[, !colnames(Informal_Economy) %in% c("2000","2001","2002", 
                                                                          "2003", "2004", "2005", "2006", "2007", "2008", "2009",
                                   "2010", "2011", "2012", "2013", "2014", "2015",
                                   "2016", "2017", "2018", "2019")]
str(Informal_Economy)

# Fonction pour remplacer les virgules par des points et convertir en numeric
replace_comma_and_convert <- function(column) {
  as.numeric(gsub(",", ".", column))
}
# Appliquer la fonction à la troisième colonne
Informal_Economy[, 3] <- replace_comma_and_convert(Informal_Economy[, 3])

df_Informal_Economy <- Informal_Economy %>%
  rename(
    Country = Counrty,
    Informal_Economy_Indicator = '2020')

df_Informal_Economy <- df_Informal_Economy[, !colnames(df_Informal_Economy) %in% c("Code")]



##############################################################################################################

#Chainalysis Crypto adoption Index

# Lire le fichier CSV sans utiliser de colonne pour les noms de ligne
Crypto_Adoption <- read.csv("/Users/ouedraogoclovispp/Desktop/MEMOIRE/Données/The 2023 Global Crypto Adoption Index: The Full List .csv",
                header = TRUE, row.names = NULL, sep = ";", check.names = FALSE)

colnames(Crypto_Adoption)
# Liste des colonnes de rang à transformer
rank_columns <- c("Classement_general_de_l_indice", "Classement_de_la_valeur_des_services_centralises_recus",
                  "Classement_de_la_valeur_des_services_centralises_reçus_dans_le_commerce_de_detail",
                  "Classement_du_volume_d_echange_pair_a_pair_P2P",
                  "Classement_de_la_valeur_recue_en_DeFi",
                  "Classement_de_la_valeur_reçue_en_DeFi_de_detail") # Remplacez les noms par ceux de vos colonnes

# Fonction pour transformer les rangs en rang centile inverse
transform_rank_to_percentile <- function(rank_column) {
  max_rank <- max(rank_column, na.rm = TRUE)
  min_rank <- min(rank_column, na.rm = TRUE)
  (max_rank - rank_column) / (max_rank - min_rank) * 100
}

# Application de la transformation aux colonnes de rang
Crypto_Adoption <- Crypto_Adoption %>%
  mutate(across(all_of(rank_columns), transform_rank_to_percentile))






##############################################################################################################

# CBDC project Index

# Lire le fichier CSV sans utiliser de colonne pour les noms de ligne
CBDC_Adoption <- read.csv("/Users/ouedraogoclovispp/Desktop/MEMOIRE/Données/score_cbdc_project.csv",
                header = TRUE, row.names = NULL, sep = ";", check.names = FALSE)

### Arrangement

# Appliquer sur plusieurs colonnes ou sur toute la dataframe si nécessaire
CBDC_Adoption[CBDC_Adoption == "n/a"] <- NA

# Fonction pour remplacer les virgules par des points et convertir en numeric
replace_comma_and_convert <- function(column) {
  as.numeric(gsub(",", ".", column))
}

# Appliquer la fonction aux colonnes 6 à 29
CBDC_Adoption[, 3:9] <- lapply(CBDC_Adoption[, 3:9], replace_comma_and_convert)
warnings(CBDC_Adoption[, 3:9] <- lapply(CBDC_Adoption[, 3:9], replace_comma_and_convert))

# Vérifier la conversion
str(CBDC_Adoption)
str(CBDC_Adoption)


#REnommer avec case_when

CBDC_Adoption <- CBDC_Adoption %>%
  mutate(Country = case_when(
    Country == "Bahamas (the)" ~ "Bahamas", 
    Country == "Bolivia (Plurinational State of)" ~ "Bolivia",
    Country == "Chinese Taipei" ~ "Taiwan",
    Country == "Korea (the Republic of)" ~ "South Korea", 
    Country == "Moldova (the Republic of)" ~ "Moldova",
    Country == "Netherlands (the)" ~ "Netherlands", 
    Country == "Philippines (the)" ~ "Philippines",
    Country == "Russian Federation (the)" ~ "Russia", Country == "Türkiye" ~ "Turkey", 
    Country == "United Arab Emirates (the)" ~ "UAE", 
    Country == "United States of America (the)" ~ "United States",
    Country == "United Kingdom of Great Britain and Northern Ireland (the)" ~ "United Kingdom", 
    Country == "Viet Nam" ~ "Vietnam", Country == "Syrian Arab Republic" ~ "Syria",
    Country == "Tanzania, United Republic of" ~ "Tanzania", 
    Country == "Venezuela (Bolivarian Republic of)" ~ "Venezuela", 
    Country == "Lao People's Democratic Republic (the)" ~ "Laos", 
    Country == "Islamic Republic of Iran" ~ "Iran", 
    Country == "Hong Kong SAR" ~ "Hong Kong",  Country == "Czechia" ~ "Czech Republic",
    Country == "Dominican Republic (the)" ~ "Dominican Republic", TRUE ~ Country))



##############################################################################################################

# World Development Indicators
str(WDI)
# Lire le fichier CSV sans utiliser de colonne pour les noms de ligne
WDI <- read.csv("/Users/ouedraogoclovispp/Desktop/MEMOIRE/Données/World Development Indicators.csv",
                header = TRUE, row.names = NULL, sep = ";", check.names = FALSE)

# Méthode : Supprimer les colonnes spécifiques avec les fonctions de base en R
WDI <- WDI[, !colnames(WDI) %in% c("2004", "2005", "2006", "2007", "2008", "2009",
                                                                        "2010", "2011", "2012", "2013", "2014", "2015",
                                                                        "2016", "2017", "2018", "2019", "2020", "2021", "2023")]

#Arrangement
str(WDI)

### Arrangement

# Appliquer sur plusieurs colonnes ou sur toute la dataframe si nécessaire
WDI[WDI == ".."] <- NA

# Appliquer la fonction aux colonnes, sauf les cinq premières colonnes
# Transformer la quatrième colonne en numérique
WDI[, 4] <- as.numeric(WDI[, 4])

# Retenir les lignes où le nom est "John" ou "Jane" en utilisant dplyr
WDI <- WDI %>%
  filter(`Series Name` %in% c("Mobile cellular subscriptions (per 100 people)", "Trade (% of GDP)"))
# Utiliser pivot_wider pour pivoter les lignes en colonnes
df_WDI <- WDI %>%
  pivot_wider(names_from = `Series Name`, values_from = `2022`)


str(WDI)
sum(is.na(WDI$'2022'))


##############################################################################################################

#WIPO-GII

# Lire le fichier CSV sans utiliser de colonne pour les noms de ligne
WIPO_GII <- read.csv("/Users/ouedraogoclovispp/Desktop/MEMOIRE/Données/WIPO_GII.csv",
                header = TRUE, row.names = NULL, sep = ";", check.names = FALSE)
sum(is.na(ABA_WIPO_GII_filtered$Year2022))
unique(ABA_WIPO_GII_filtered$`2022`)

unique(ABA_WIPO_GII_filtered$Year2022)

#Structure des données
str(ABA_WIPO_GII_filtered)

# Remplacer les valeurs non numériques par NA
clean_and_convert <- function(column) {
  as.numeric(as.character(column))
}

# Convertir les colonnes en numeric, sauf les cinq premières colonnes
ABA_WIPO_GII_filtered[, -c(1:5)] <- lapply(ABA_WIPO_GII_filtered[, -c(1:5)], clean_and_convert)

# Vérifier la conversion
str(ABA_WIPO_GII_filtered)

# Vérifier la conversion
str(ABA_WIPO_GII_filtered)


#"Score"
# Filtrer les lignes contenant "Global Innovation Index: Global Innovation Index"
WIPO_GII_filtered_data <- WIPO_GII %>% filter(Indicator == "Global Innovation Index: Global Innovation Index")
WIPO_GII_filtered_data <- WIPO_GII_filtered_data %>% filter(`Attribute 1` == "Score")

# Méthode 1 : Supprimer les colonnes spécifiques avec les fonctions de base en R
WIPO_GII_filtered_data <- WIPO_GII_filtered_data[, !colnames(WIPO_GII_filtered_data) %in% c("Attribute 2", "Attribute 3", "Partner")]

# Méthode : Supprimer les colonnes spécifiques avec les fonctions de base en R
WIPO <- ABA_WIPO_GII_filtered[, !colnames(ABA_WIPO_GII_filtered) %in% c("2019", "2020", "2021")]

library(dplyr)

# Suppression des colonnes 2 à 4
WIPO_GII_filtered_data <- WIPO_GII_filtered_data %>% select(-(6:11))
# Supposons que WIPO_GII est votre data frame
# Identifier les quatre dernières colonnes
last_four_cols <- tail(names(WIPO_GII_filtered_data), 4)

# Filtrer les lignes où les quatre dernières colonnes ne sont pas toutes vides
ABA_WIPO_GII_filtered <- WIPO_GII_filtered_data %>%
  filter(!if_all(all_of(last_four_cols), is.na) & 
           !if_all(all_of(last_four_cols), ~ . == ""))

######____________________________________________________________________-
# Fonction pour remplacer les virgules par des points et convertir en numeric
replace_comma_and_convert <- function(column) {
  as.numeric(gsub(",", ".", column))
}

# Appliquer la fonction aux colonnes, sauf les cinq premières colonnes
ABA_WIPO_GII_filtered[, -c(1:5)] <- lapply(ABA_WIPO_GII_filtered[, -c(1:5)], replace_comma_and_convert)

# Vérifier la conversion
str(ABA_WIPO_GII_filtered)
sum(is.na(ABA_WIPO_GII_filtered$'2022'))

#Imputation Merdeeeeee
# Imputation de la dernière observation transporté (LOCF)
# Identifier la dernière colonne
last_col <- ncol(ABA_WIPO_GII_filtered)

# Boucle pour parcourir les lignes et appliquer la logique personnalisée de LOCF
for (i in 1:nrow(ABA_WIPO_GII_filtered)) {
  # Si la dernière colonne a une valeur NA
  if (is.na(ABA_WIPO_GII_filtered[i, last_col])) {
    # Trouver la première valeur non-NA en remontant parmi les colonnes précédentes
    for (j in (last_col - 1):1) {
      if (!is.na(ABA_WIPO_GII_filtered[i, j])) {
        ABA_WIPO_GII_filtered[i, last_col] <- ABA_WIPO_GII_filtered[i, j]
        break  # Arrêter la boucle une fois une valeur non-NA trouvée
      }
    }
  }
}
sum(is.na(ABA_WIPO_GII_filtered$'2021'))
######____________________________________________________________________-






# Vérifier le résultat
print(head(data))
sum(is.na(data$X2022))
sum(is.na(ABA_WIPO_GII_filtered$Year2022))

sum(ABA_WIPO_GII_filtered$Year2022 == 0)
class()

ABA_WIPO_GII_filtered <- ABA_WIPO_GII_filtered %>%
  rename(Year2020 = '2020', Year2019 = '2019')


print("Nombre de lignes: ")
print(nrow(data))
print("Nombre de NA dans la dernière colonne avant modification: ")
print(sum(is.na(data[, last_col])))

# Exécutez votre script de modification ici

print("Nombre de NA dans la dernière colonne après modification: ")
print(sum(is.na(data[, last_col])))
print("Aperçu des premières lignes après modification:")
print(head(data))










# Vérification du résultat
print(ABA_WIPO_GII_filtered)

write.csv(WIPO, "WIPO.csv", row.names = FALSE)
write.csv(merged_inner, "score_grand.csv", row.names = FALSE)
write.csv(merged_outer, "score_grand_vrai.csv", row.names = FALSE)

##############################################################################################################

#Remittances

# Lire le fichier CSV sans utiliser de colonne pour les noms de ligne

# Inflow By GDP
Remitance_inflows_GD <- read.csv("/Users/ouedraogoclovispp/Desktop/MEMOIRE/Données/Remitance_inflows_GDP.csv",
                header = TRUE, row.names = NULL, sep = ";", check.names = FALSE)

# Inflow
Remitance_inflows <- read.csv("/Users/ouedraogoclovispp/Desktop/MEMOIRE/Données/Remittance_inflows .csv",
                header = TRUE, row.names = NULL, sep = ";", check.names = FALSE)

# Outflows
Remitance_outflows <- read.csv("/Users/ouedraogoclovispp/Desktop/MEMOIRE/Données/Remittance_outflows.csv",
                header = TRUE, row.names = NULL, sep = ";", check.names = FALSE)

##############################################################################################################

#Remitance_outflows
#Remitance_inflows
#Remitance_inflows_GD
#WIPO_GII_filtered_data
#WDI
#Mobile_Money_Adoption_Index_pivoted_data
#CBDC_Adoption
#Crypto_Adoption
#Crypto_Ban
#Financial_Development_Index_pivoted_data
#Informal_Economy
#WEO
# # Sauvegarde de la dataframe dans un fichier CSV (Save)
#write.csv(ABA_WIPO_GII_filtered, "/Users/ouedraogoclovispp/Desktop/MEMOIRE/Donnéesmy_dataframe.csv", row.names = FALSE)
str(Crypto_Adoption)
#
#Regression
model <- lm(merged_inner$score_du_projet_CBDC_de_detail ~ merged_inner$Classement_du_volume_d_echange_pair_a_pair_P2P, data = merged_inner)
# Visualiser les résultats avec ggplot2
ggplot(merged_inner, aes(x = merged_inner$Classement_general_de_l_indice, y = merged_inner$score_du_projet_CBDC_de_detail)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Régression Linéaire Simple",
       x = "Adoption des Cryptomonnaies",
       y = "Adoption des CBDCs") +
  theme_minimal()

# Résumé des résultats
summary(model)
colnames(CBDC_Adoption)

#################################################################

# Fusion interne (seulement les lignes avec des id correspondants dans les deux datasets)
merged_inner <- merge(CBDC_Adoption, Crypto_Adoption, by = "Country")
print("Inner Join:")
print(merged_inner)

merged_outer_2 <- merge(merged_outer, WIPO, by = "Country", all = TRUE)

df_merged_outer_2 <- merged_outer_2 %>%
  pivot_wider(names_from = `Indicator`, values_from = "2022")



######### Chercher les doublons
library(dplyr)

duplicates <- merged_outer_2 %>%
  dplyr::group_by(Country, ISO2, score_global_du_projet, score_du_projet_CBDC_de_detail, score_du_projet_CBDC_de_gros,
                  interet_de_recherche_normalise, Indice_des_discours_des_banquiers_centraux_normalise, interet_de_recherche,
                  indice_de_position_des_banquiers_centraux, Region, Classement_general_de_l_indice,
                  Classement_de_la_valeur_des_services_centralises_recus,
                  Classement_de_la_valeur_des_services_centralises_reçus_dans_le_commerce_de_detail,
                  Classement_du_volume_d_echange_pair_a_pair_P2P, Classement_de_la_valeur_recue_en_DeFi,
                  Classement_de_la_valeur_reçue_en_DeFi_de_detail, Code, `Indicator ID`, `Attribute 1`, Indicator) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L)

# Afficher les doublons
print(duplicates)

# Supprimer les doublons
merged_outer_2_unique <- merged_outer_2 %>%
  distinct(Country, ISO2, score_global_du_projet, score_du_projet_CBDC_de_detail, score_du_projet_CBDC_de_gros,
           interet_de_recherche_normalise, Indice_des_discours_des_banquiers_centraux_normalise, interet_de_recherche,
           indice_de_position_des_banquiers_centraux, Region, Classement_general_de_l_indice,
           Classement_de_la_valeur_des_services_centralises_recus,
           Classement_de_la_valeur_des_services_centralises_reçus_dans_le_commerce_de_detail,
           Classement_du_volume_d_echange_pair_a_pair_P2P, Classement_de_la_valeur_recue_en_DeFi,
           Classement_de_la_valeur_reçue_en_DeFi_de_detail, Code, `Indicator ID`, `Attribute 1`, Indicator, `2022`, .keep_all = TRUE)

# Pivot les données après suppression des doublons
df_merged_outer_2 <- merged_outer_2_unique %>%
  pivot_wider(names_from = `Indicator`, values_from = "2022")

# Méthode : Supprimer les colonnes spécifiques avec les fonctions de base en R
df_merged_outer_2 <- df_merged_outer_2[, !colnames(df_merged_outer_2) %in% c("Code", "Attribute 1", "NA", "Code")]

# Afficher le data frame pivoté
print("Data Frame Pivoté après Suppression des Doublons:")
print(df_merged_outer_2)

# Fusion externe complète (toutes les lignes des deux datasets)
merged_outer_3 <- merge(df_merged_outer_2, df_WDI, by = "Country", all = TRUE)
# Méthode : Supprimer les colonnes spécifiques avec les fonctions de base en R
merged_outer_3 <- merged_outer_3[, !colnames(merged_outer_3) %in% c("ISO2", "Code", "Indicator ID")]

colnames(merged_outer_3$'Country')
unique(merged_outer_3$Country)

###################################################################################################################################################################################################

# Fusion externe complète (toutes les lignes des deux datasets) FINALLLLLLL
DATA <- merge(df_Informal_Economy, merged_outer_3, by = "Country", all = TRUE)
unique(DATA$Country)

#REnommer avec case_when

a_DATA <- DATA %>%
  mutate(Country = case_when(
    Country == "Bahamas, The" ~ "Bahamas",
    Country == "Comoros (the)" ~ "Comoros",
    Country == "Korea, Rep." ~ "South Korea", 
    Country == "Islamic Republic of Pakistan" ~ "Pakistan",
    Country == "Democratic Republic of Congo" ~ "Congo, Dem. Rep.", 
    Country == "Egypt, Arab Rep." ~ "Egypt",
    Country == "Russian Federation (the)" ~ "Russia", Country == "Türkiye" ~ "Turkey",
    Country == "Turkiye" ~ "Turkey",
    Country == "UAE" ~ "United Arab Emirates", 
    Country == "Gambia, The" ~ "Gambia", Country == "Gambia (the)" ~ "Gambia",
    Country == "Viet Nam" ~ "Vietnam", Country == "Syrian Arab Republic" ~ "Syria",
    Country == "Russian Federation" ~ "Russia", 
    Country == "Venezuela, RB" ~ "Venezuela", 
    Country == "Lao PDR" ~ "Laos", 
    Country == "Iran, Islamic Rep." ~ "Iran", 
    Country == "Hong Kong SAR, China" ~ "Hong Kong",  Country == "Czechia" ~ "Czech Republic",
    Country == "Macao SAR, China" ~ "Macao", Country == "Macao SAR" ~ "Macao", Country == "Sudan (the)" ~ "Sudan",
    Country == "Kyrgyz Republic" ~ "Kyrgyzstan", TRUE ~ Country))

# Méthode : Supprimer les colonnes spécifiques avec les fonctions de base en R
a_DATA <- a_DATA[, !colnames(a_DATA) %in% c("Region")]
head(a_DATA)
colnames(a_DATA)

# Fusionner les lignes avec des colonnes similaires et remplacer les valeurs manquantes
df_DATA <- a_DATA %>%
  group_by(Country) %>%
  summarise(
    Informal_Economy_Indicator = coalesce(Informal_Economy_Indicator[!is.na(Informal_Economy_Indicator)][1], NA),
    score_global_du_projet = coalesce(score_global_du_projet[!is.na(score_global_du_projet)][1], NA),
    score_du_projet_CBDC_de_detail = coalesce(score_du_projet_CBDC_de_detail[!is.na(score_du_projet_CBDC_de_detail)][1], NA),
    score_du_projet_CBDC_de_gros = coalesce(score_du_projet_CBDC_de_gros[!is.na(score_du_projet_CBDC_de_gros)][1], NA),
    interet_de_recherche_normalise = coalesce(interet_de_recherche_normalise[!is.na(interet_de_recherche_normalise)][1], NA),
    Indice_des_discours_des_banquiers_centraux_normalise = coalesce(Indice_des_discours_des_banquiers_centraux_normalise[!is.na(Indice_des_discours_des_banquiers_centraux_normalise)][1], NA),
    interet_de_recherche = coalesce(interet_de_recherche[!is.na(interet_de_recherche)][1], NA),
    indice_de_position_des_banquiers_centraux = coalesce(indice_de_position_des_banquiers_centraux[!is.na(indice_de_position_des_banquiers_centraux)][1], NA),
    Classement_general_de_l_indice = coalesce(Classement_general_de_l_indice[!is.na(Classement_general_de_l_indice)][1], NA),
    Classement_de_la_valeur_des_services_centralises_recus = coalesce(Classement_de_la_valeur_des_services_centralises_recus[!is.na(Classement_de_la_valeur_des_services_centralises_recus)][1], NA),
    Classement_de_la_valeur_des_services_centralises_reçus_dans_le_commerce_de_detail = coalesce(Classement_de_la_valeur_des_services_centralises_reçus_dans_le_commerce_de_detail[!is.na(Classement_de_la_valeur_des_services_centralises_reçus_dans_le_commerce_de_detail)][1], NA),
    Classement_du_volume_d_echange_pair_a_pair_P2P = coalesce(Classement_du_volume_d_echange_pair_a_pair_P2P[!is.na(Classement_du_volume_d_echange_pair_a_pair_P2P)][1], NA),
    Classement_de_la_valeur_recue_en_DeFi = coalesce(Classement_de_la_valeur_recue_en_DeFi[!is.na(Classement_de_la_valeur_recue_en_DeFi)][1], NA),
    Classement_de_la_valeur_reçue_en_DeFi_de_detail = coalesce(Classement_de_la_valeur_reçue_en_DeFi_de_detail[!is.na(Classement_de_la_valeur_reçue_en_DeFi_de_detail)][1], NA),
    Classement_de_la_valeur_reçue_en_DeFi_de_detail = coalesce(Classement_de_la_valeur_reçue_en_DeFi_de_detail[!is.na(Classement_de_la_valeur_reçue_en_DeFi_de_detail)][1], NA),
    `Global Innovation Index: Global Innovation Index` = coalesce(`Global Innovation Index: Global Innovation Index`[!is.na(`Global Innovation Index: Global Innovation Index`)][1], NA),
    `Mobile cellular subscriptions (per 100 people)` = coalesce(`Mobile cellular subscriptions (per 100 people)`[!is.na(`Mobile cellular subscriptions (per 100 people)`)][1], NA),
    `Trade (% of GDP)` = coalesce(`Trade (% of GDP)`[!is.na(`Trade (% of GDP)`)][1], NA),
    .groups = 'drop'
  )

str(a_DATA)

# Utiliser dplyr::filter pour supprimer les lignes avec des valeurs NA dans la colonne "Score"
df_filtered_df_DATA <- df_DATA %>%
  filter(!is.na(Classement_general_de_l_indice))

df_filtered_df_DATA <- df_filtered_df_DATA %>%
  filter(!is.na(score_du_projet_CBDC_de_detail))

# Chemin du fichier de sortie
output_file <- "/Users/ouedraogoclovispp/Desktop/MEMOIRE/Données/Document.csv"

# Sauvegarder la data frame en laissant les NA vides
write.table(df_filtered_df_DATA, file = output_file, sep = ",", row.names = FALSE, col.names = TRUE, na = "")

###################################################################################################################################################################################################
#Conclusion 
df_DATA # Base de donnees definitive
df_filtered_df_DATA #Base de donnees filtrée définitive



###################################################################################################################################################################################################
df_Informal_Economy

# Fusion externe complète (toutes les lignes des deux datasets)
merged_outer <- merge(CBDC_Adoption, Crypto_Adoption, by = "Country", all = TRUE)
print("Full Outer Join:")
print(merged_outer)

unique(merged_outer_2$Country)
library(dplyr)

# Afficher le data frame modifié
print("Data Frame Modifié avec dplyr:")
print(df)

# Afficher le data frame modifié
print("Data Frame Modifié:")
print(df)
######################################################

# Lire le fichier CSV
Informal_Economy_DGE <- read.csv("/Users/ouedraogoclovispp/Desktop/DGE_p-Tableau 1.csv",
                 header = TRUE, row.names = NULL,sep = ";", check.names = FALSE)
str(Informal_Economy_DGE)

# Fonction pour remplacer les virgules par des points et convertir en numeric
replace_comma_and_convert <- function(column) {
  as.numeric(gsub(",", ".", column))
}
# Appliquer la fonction à la troisième colonne
Informal_Economy_DGE[, 2] <- replace_comma_and_convert(Informal_Economy_DGE[, 2])



Informal_Economy_DGE <- Informal_Economy_DGE %>%
  mutate(Country = case_when(
    Country == "Bahamas, The" ~ "Bahamas",
    Country == "Comoros (the)" ~ "Comoros",
    Country == "Korea, Rep." ~ "South Korea", 
    Country == "Islamic Republic of Pakistan" ~ "Pakistan",
    Country == "Democratic Republic of Congo" ~ "Congo, Dem. Rep.", 
    Country == "Egypt, Arab Rep." ~ "Egypt",
    Country == "Russian Federation (the)" ~ "Russia", Country == "Türkiye" ~ "Turkey",
    Country == "Turkiye" ~ "Turkey",
    Country == "UAE" ~ "United Arab Emirates", 
    Country == "Gambia, The" ~ "Gambia", Country == "Gambia (the)" ~ "Gambia",
    Country == "Viet Nam" ~ "Vietnam", Country == "Syrian Arab Republic" ~ "Syria",
    Country == "Russian Federation" ~ "Russia", 
    Country == "Venezuela, RB" ~ "Venezuela", 
    Country == "Lao PDR" ~ "Laos", 
    Country == "Iran, Islamic Rep." ~ "Iran", 
    Country == "Hong Kong SAR, China" ~ "Hong Kong",  Country == "Czechia" ~ "Czech Republic",
    Country == "Macao SAR, China" ~ "Macao", Country == "Macao SAR" ~ "Macao", Country == "Sudan (the)" ~ "Sudan",
    Country == "Kyrgyz Republic" ~ "Kyrgyzstan", Country == "Yemen, Rep." ~ "Yemen", TRUE ~ Country))

# Renommer des colonnes spécifiques
colnames(selected_data)[colnames(selected_data) == "score_du_projet_CBDC_de_detail"] <- "CBDCD"
colnames(selected_data)[colnames(selected_data) == "Classement_de_la_valeur_des_services_centralises_recus"] <- "SC"
colnames(selected_data)[colnames(selected_data) == "Classement_du_volume_d_echange_pair_a_pair_P2P"] <- "P2P"
colnames(data)[colnames(data) == "Classement_de_la_valeur_des_services_centralises_reçus_dans_le_commerce_de_detail"] <- "SCD"
colnames(selected_data)[colnames(selected_data) == "Classement_de_la_valeur_reçue_en_DeFi_de_detail"] <- "DeFiD"
colnames(data)[colnames(data) == "Classement_de_la_valeur_recue_en_DeFi"] <- "DeFi"
colnames(data)[colnames(data) == "Classement_general_de_l_indice"] <- "General"
colnames(selected_data)[colnames(selected_data) == "Mobile cellular subscriptions (per 100 people)"] <- "Mobile"
colnames(selected_data)[colnames(selected_data) == "Informal_Economy_Indicator"] <- "Informal"
colnames(selected_data)[colnames(selected_data) == "Trade (% of GDP)"] <- "Trade"
colnames(selected_data)[colnames(selected_data) == "Global Innovation Index: Global Innovation Index"] <- "Innovation"
# Effectuer un left join pour conserver toutes les lignes de df1
KELLY <- data %>%
  left_join(Informal_Economy_DGE, by = "Country")
print("Left Join Result:")
str(KELLY)
unique(KELLY$Country)

sum(is.na(KELLY$I_E_DGE))
unique(KELLY$Country)
unique(data$Country)
unique(Informal_Economy_DGE$Country)
write.csv(KELLY, "Kelly.csv", row.names = FALSE)


# Identifiez les différences
differences <- data$Country !=  Informal_Economy_DGE$Country
comparaison <- data$Country ==  Informal_Economy_DGE$Country
print(differences)
# Trouver les pays présents dans df1 mais pas dans df2
diff1 <- setdiff(data$Country, Informal_Economy_DGE$Country)
print(diff1)

# Résultat: FALSE TRUE FALSE TRUE

# Afficher les lignes où les colonnes diffèrent
df_differences <- df[differences, ]

print(df_differences)
# Résultat:
#   col1 col2
# 2    b    x
# 4    d    y


