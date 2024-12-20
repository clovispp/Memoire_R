# Installer et charger les packages nécessaires
install.packages("ordinal")
install.packages("xtable")
library(ordinal)
library(xtable)
library(caret)
library(effects)
library(sjPlot)
library(ordinal)
library(sjmisc)
library(sjlabelled)
library(arsenal)
library(table)
library(knitr)
library(gtsummary)
library(rcompanion)
library(summarytools)
install.packages("ordinal")
install.packages("sjPlot")
install.packages("sjmisc")
install.packages("sjlabelled")

# Lire le fichier CSV
data <- read.csv("/Users/ouedraogoclovispp/Desktop/MEMOIRE/Données/Kelly.csv",
                 header = TRUE, row.names = NULL, check.names = FALSE)
str(data)
colnames(data)
# Sélectionner les colonnes 4 à 9
selected_data <- data[, 4:9]

# Sélectionner les colonnes d'intérêt
selected_data <- data[, c("score_du_projet_CBDC_de_detail", "Global Innovation Index: Global Innovation Index",
                          "Informal_Economy_Indicator", "Mobile cellular subscriptions (per 100 people)",
                          "Classement_du_volume_d_echange_pair_a_pair_P2P", 
                          "Trade (% of GDP)")]
table(data$score_du_projet_CBDC_de_detail, )
table(CBDC_Adoption$score_du_projet_CBDC_de_detail)
table(selected_data$CBDCD)
# Renommer des colonnes spécifiques
colnames(selected_data)[colnames(selected_data) == "score_du_projet_CBDC_de_detail"] <- "CBDCD"
colnames(selected_data)[colnames(selected_data) == "Mobile cellular subscriptions (per 100 people)"] <- "Mobile"
colnames(selected_data)[colnames(selected_data) == "Classement_du_volume_d_echange_pair_a_pair_P2P"] <- "P2P"
colnames(selected_data)[colnames(selected_data) == "Informal_Economy_Indicator"] <- "Informal"
colnames(selected_data)[colnames(selected_data) == "Trade (% of GDP)"] <- "Trade"
colnames(selected_data)[colnames(selected_data) == "Global Innovation Index: Global Innovation Index"] <- "Innovation"
selected_data$CBDCD <- as.factor(selected_data$CBDCD)
library(dummies)

# Calculate the correlation matrix
cor_matrix <- cor(selected_data, , use = "complete.obs")
# Display a correlation graph
corrplot(cor_matrix, method = "number")

# Filter observations according to specified criteria
Growthtest <- GrowthDJ[GrowthDJ$oil == "no" & GrowthDJ$inter == "yes", ]
crosstab(Growthtest$oil, Growthtest$oecd, prop.r = TRUE, prop.c = TRUE, prop.t =
           TRUE)
head(Growthtest)


# Créer un tableau descriptif comparant les groupes
table_desc <- tableby(score_du_projet_CBDC_de_detail ~ Classement_du_volume_d_echange_pair_a_pair_P2P + 
                        `Mobile cellular subscriptions (per 100 people)` + 
                        `Global Innovation Index: Global Innovation Index`+
                        Informal_Economy_Indicator, data = data)

# Afficher le tableau descriptif
summary(table_desc)
# Extraire les résultats du modèle de régression probit ordonné
model_summary <- summary(model)
coef_table <- coef(model_summary)
coef_table <- as.data.frame(coef_table)
coef_table$Variable <- rownames(coef_table)
rownames(coef_table) <- NULL

# Créer un tableau combiné
tbl1 <- as.data.frame(summary(table_desc))
tbl2 <- coef_table

# Ajouter un identifiant pour fusionner les deux tableaux
tbl1$id <- 1:nrow(tbl1)
tbl2$id <- 1:nrow(tbl2)

# Fusionner les deux tableaux
tbl_merged <- merge(tbl1, tbl2, by = "id", all = TRUE)

# Conversion du tableau combiné en LaTeX avec xtable
latex_code_x <- xtable(tbl_merged)

# Afficher le code LaTeX
print(latex_code_x, type = "latex", include.rownames = FALSE)
 str(data)
cor
# Convertir la variable dépendante en facteur
data$score_du_projet_CBDC_de_detail <- as.factor(data$score_du_projet_CBDC_de_detail)

str(selected_data)
# Supprimer les lignes avec des valeurs manquantes
df_clean <- na.omit(selected_data)

# Ajuster le modèle de régression probit ordinale
model_Crypto <- clm(score_du_projet_CBDC_de_detail ~ Classement_du_volume_d_echange_pair_a_pair_P2P,
                             na.action = na.omit, data = data, link = "probit")
colnames(selected_data)
model0<- clm(CBDCD ~ 1,
             data = selected_data, na.action = na.omit , link = "probit")
model0_na<- clm(CBDCD ~ 1,
             data = df_clean, na.action = na.omit , link = "probit")


model1<- clm(CBDCD ~ P2P,
                       data = selected_data, na.action = na.omit , link = "probit")
model2<- clm(CBDCD ~ P2P + Mobile + Informal+Innovation,
             data = selected_data, na.action = na.omit , link = "probit")
model3<- clm(CBDCD ~ P2P + Mobile+Innovation,
             data = selected_data, na.action = na.omit , link = "probit")
model4<- clm(CBDCD ~ P2P + Mobile+Informal,
             data = selected_data, na.action = na.omit , link = "probit")
model5<- clm(CBDCD ~ Mobile + Informal+Innovation,
             data = selected_data, na.action = na.omit , link = "probit")
model6<- clm(CBDCD ~ Mobile,
             data = selected_data, na.action = na.omit , link = "probit")
model7<- clm(CBDCD ~ Informal,
             data = selected_data, na.action = na.omit , link = "probit")
model8<- clm(CBDCD ~ Innovation,
             data = selected_data, na.action = na.omit , link = "probit")
model10<- clm(CBDCD ~ P2P + Mobile + Informal+Innovation,
             data = df_clean, na.action = na.omit , link = "probit")
model11<- clm(CBDCD ~ P2P + Mobile + Informal+Innovation+Trade,
              data = df_clean, na.action = na.omit , link = "probit")

model2_polr<- polr(CBDCD ~ P2P + Mobile + Informal+Innovation,
             data = selected_data, na.action = na.omit , method = "probit", Hess = TRUE)

model10_polr<- polr(CBDCD ~ P2P + Mobile + Informal+Innovation,
              data = df_clean, na.action = na.omit , method = "probit", Hess = TRUE)
summary(model2_polr)

pseudoR2_2(model10_polr)
library(brant)
library(performance)
library(foreign)
library(car)
compare_performance(model2, model3, model4)
brant(model)
brant(model2_polr)
vif(model6)
# Extraire les résidus de Pearson
residuals_pearson <- residuals(model10, type = "pearson")

library(AER)
stargazer(coeftest(model2_polr), , title = "Coefficients",
          type = "latex", summary = FALSE)

# Ajuster un modèle linéaire pour vérifier la multicolinéarité
model2_lm1 <- lm(Innovation ~ Mobile + Informal + P2P, na.action = na.omit,data = selected_data)

# Calculer le VIF
vif(model2_lm1)

# Résidus de Pearson
residuals_pearson <- residuals(model2_polr, type = "pearson")

# Résidus de Deviance
residuals_deviance <- residuals(model2_polr, type = "deviance")

# Afficher les résidus
print(residuals_pearson)
print(residuals_deviance)

predict(model2_polr, newdata = selected_data, type = "prob")

# Tracer les résidus de Pearson
plot(residuals_pearson, type = "o", main = "Tracé des résidus de Pearson", ylab = "Résidus de Pearson", xlab = "Index")
abline(h = 0, col = "red")

# Autocorrélogramme des résidus de Pearson
acf(residuals_pearson, main = "Autocorrélogramme des résidus de Pearson")


summary(model11)
anova(model0_na, model2)
AIC(model3, model4)
nagelkerke(fit = model10, null = model0_na)

confint(model2)
exp(coef(model2))
stargazer(model1, model6, model7, model8, model2, model3, model4, model5, title = "Ordered Probit Regressions",
          type = "latex", summary = FALSE)

stargazer(model1, model6, model7, model8, title = "Ordered Probit Regressions",
          type = "latex", summary = FALSE)

stargazer(model2_polr, final_model, title = "Validation croisée entre modeles Probit et Log-log",
          type = "latex", summary = FALSE)

summary(model2_polr)

# Appliquer la régression probit ordinale
model_P2P_Mobile<- clm(score_du_projet_CBDC_de_detail ~ Classement_du_volume_d_echange_pair_a_pair_P2P + 
                                    `Mobile cellular subscriptions (per 100 people)`+Informal_Economy_Indicator+
                         `Global Innovation Index: Global Innovation Index`,
                        data = data, na.action = na.omit , link = "probit")
P_model_probit_ord_seul <- polr(score_du_projet_CBDC_de_detail ~ Classement_du_volume_d_echange_pair_a_pair_P2P, data = data,
                                na.action = na.omit, method = "probit")
summary(model_P2P_Mobile)
summary(model)
stargazer(model, model_sans_Informal, model_sans_Innovation, title = "Ordered Probit Regressions",
          type = "latex", summary = FALSE)

stargazer(final_model, title = "Loglog Regressions", na.action=na.omit, 
          type = "latex", summary = FALSE)

class(final_model)

# Charger les bibliothèques nécessaires
install.packages("texreg")
library(texreg)

# Créer une table LaTeX du modèle final avec texreg
texreg::texreg(
  final_model,
  caption = "Log-log Regressions",
  label = "tab:loglog_regressions",
  custom.coef.names = c("P2P", "Mobile", "Informal", "Innovation", "Intercept1", "Intercept2", "Intercept3"),
  custom.model.names = "Log-log Model",
  use.packages = FALSE
)

# Créer une table LaTeX combinée pour les modèles log-log et probit
texreg::texreg(
  list(final_model, model2_polr),
  caption = "Comparison of Log-log and Probit Regressions",
  label = "tab:comparison_regressions",
  custom.coef.names = c("P2P", "Mobile", "Informal", "Innovation", "Intercept1", "Intercept2", "Intercept3"),
  custom.model.names = c("Log-log Model", "Probit Model"),
  use.packages = FALSE
)

summary(model2_polr)
library(texreg)
stargazer(model1, model2, model3,
          ,
          out = "table.tex")
library(marginaleffects)
effetmarg <- marginaleffects::marginaleffects(model2_polr)
effetmarg2 <- marginaleffects::marginaleffects(final_model)
summary(effetmarg)

stargazer(effetmarg, type = "latex", title = "Effets marginaux de la regression",
          header = FALSE)
##################################################################################################################
#Validation du model

# Définir la fonction de prédiction pour polr
predict_polr <- function(model2_polr, newdata) {
  predict(model2_polr, newdata, type = "class")
}

# Définir les contrôles de la validation croisée
train_control <- trainControl(method = "cv", number = 10, savePredictions = TRUE)

# Ajuster le modèle avec la validation croisée
cv_model <- train(
  CBDCD ~ P2P + Mobile + Informal+Innovation,
  data = selected_data,
  method = "polr",
  trControl = train_control,
  tuneLength = 1,
  preProcess = NULL,
  metric = "Accuracy", na.action = na.omit
)

# Résultats de la validation croisée
print(cv_model)

# Extraire et afficher les coefficients du modèle final
final_model <- cv_model$finalModel
summary(final_model)


#####################################################################################################################


# Résumé du modèle
summary(model2)
# Résumé du modèle
summary(model3)
# Résumé du modèle
summary(model4)


# Coefficients du modèle
tidy(model)
# Coefficients du modèle
tidy(model_probit_ord_seul)

# Calcul des pseudo R² pour évaluer l'ajustement du modèle
pseudoR2_2 <- function(model10_polr) {
  null.deviance <- model10_polr$null.deviance
  residual.deviance <- model10_polr$deviance
  1 - (residual.deviance / null.deviance)
}
# Calcul des pseudo R² pour évaluer l'ajustement du modèle
pseudoR2 <- function(model_probit_ord_seul) {
  null.deviance <- model_probit_ord_seul$null.deviance
  residual.deviance <- model_probit_ord_seul$deviance
  1 - (residual.deviance / null.deviance)
}


anova(model2,model3)

pseudoR2_2(model10_polr)
pseudoR2(model_probit_ord_seul)


# Prédictions et analyse des résultats
selected_data$predicted_score <- predict(model2_polr, newdata = selected_data, type = "class")
# Prédictions et analyse des résultats
data$predicted_score_seul <- predict(model_probit_ord_seul, newdata = data, type = "class")

# Comparaison des scores réels et prévus
WAW <- table(selected_data$P2P, selected_data$predicted_score)
# Comparaison des scores réels et prévus
table(data$score_du_projet_CBDC_de_detail, data$predicted_score_seul)
WAW


# Créer une table des résultats
results <- summary(model2_polr)$coefficients
results <- as.data.frame(results)
colnames(results) <- c("Coef", "Std Err", "z value", "Pr(>|z|)")

# Générer le code LaTeX avec xtable
latex_table <- xtable(results, caption = "Résultats de la régression probit ordinale", label = "tab:probit_results")

# Afficher le code LaTeX
print(latex_table, type = "latex", include.rownames = TRUE)


##########################################################################################################################################################################################################################################
#Visualisation du modèle

# Visualiser les coefficients du modèle
plot_model(model_probit_ord, type = "est", show.values = TRUE, value.offset = .3, title = "Coefficients du modèle probit ordinal")

# Installer et charger le package margins si nécessaire

library(marginaleffects)

# Calculer les effets marginaux
margins_probit <- marginaleffects::slopes(model_probit_ord)
summary(margins_probit)
# Effets marginaux des variables explicatives
plot_slopes(margins_probit, by = )




# Faire des prédictions
predictions <- predict(model_probit_ord, newdata = data, type = "class")
data$predictions <- predictions

# Histogramme des prédictions
# Créer un graphique à barres pour les prédictions
ggplot(data, aes(x = predictions)) +
  geom_bar(fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogramme des prédictions", x = "Prédictions", y = "Fréquence")
# Scatter plot des prédictions vs. valeurs réelles
ggplot(data, aes(x = predictions, y = score_du_projet_CBDC_de_detail)) +
  geom_jitter(color = "blue", alpha = 0.5) +
  labs(title = "Prédictions vs. Valeurs Réelles", x = "Prédictions", y = "Score du projet CBDC de détail")

# Faire des prédictions de probabilités
probs <- predict(model_probit_ord, newdata = data, type = "prob")
head(probs)

table(selected_data$CBDCD, selected_data$P2P)
crosstab(selected_data$CBDCD, selected_data$P2P, missing.include = TRUE, prop.t = TRUE)
crosstab(selected_data$CBDCD, selected_data$P2P)
summarytools::freq(selected_data$P2P)


# Création d'une table de contingence
table55<-table(selected_data$CBDCD, selected_data$P2P)

# Test de chi-2
chi2_test <- chisq.test(table55)

# Affichage des résultats
print(chi2_test)
# T-test pour deux échantillons indépendants
t_test <- t.test(selected_data_clean$CBDCD, selected_data_clean$P2P)

# Affichage des résultats
print(t_test)
str(selected_data)
anova_result <- aov(P2P ~ CBDCD, data = selected_data_clean)
summary(anova_result)
aov(selected_data$norm_P2P~selected_data$CBDCD)
sum(is.na(selected_data$norm_P2P))
TukeyHSD(anova_result)

# Test de Kruskal-Wallis
kruskal_result <- kruskal.test(log(P2P) ~ CBDCD, data = selected_data)
print(kruskal_result)

# Test de normalité
shapiro_result <- shapiro.test(selected_data_clean$Innovation)
print(shapiro_result)

# Appliquer la transformation normale inverse pour obtenir les rangs normalisés
selected_data$norm_P2P <- qnorm(selected_data$P2P/100)
sum(is.na(selected_data$P2P))

# Afficher les lignes avec des valeurs problématiques
selected_data[!is.finite(selected_data$norm_P2P), ]

# Supprimer les lignes avec des valeurs NA ou infinies
selected_data <- selected_data[!is.na(selected_data$norm_P2P) & is.finite(selected_data$norm_P2P), ]

selected_data_clean <- selected_data %>%
  filter(CBDCD != "0")
Crypto_Adoption_filtered <- Crypto_Adoption %>%
  filter(CBDC != 0)
str(data$score_du_projet_CBDC_de_detail)
Boxplot(data$score_du_projet_CBDC_de_detail)
#####################################################################################################################