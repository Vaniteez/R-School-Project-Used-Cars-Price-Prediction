# Charger les librairies nécessaires
library(tidyverse)
library(dplyr)
library(tidyr)

# Charger les données
used_cars <- read.csv("used_cars.csv")

# Aperçu des données
head(used_cars)
str(used_cars)
summary(used_cars)

# ELT
# Nettoyage de la colonne price
used_cars <- used_cars %>%
  mutate(price_clean = as.numeric(gsub("[\\$,]", "", price)))

# Nettoyage de la colonne milage
used_cars <- used_cars %>%
  mutate(milage_clean = as.numeric(gsub("[^0-9]", "", milage)))

# Création d'une colonne pour l'âge du véhicule
used_cars <- used_cars %>%
  mutate(vehicle_age = 2024 - model_year)

# Suppression des anciennes colonnes price et milage
used_cars <- used_cars %>%
  select(-price, -milage)

# Remplacement des espaces par des "No" pour la colonne clean_title
used_cars <- used_cars %>%
  mutate(clean_title = ifelse(trimws(clean_title) == "" | is.na(clean_title), "No", clean_title))

# Encodage en 0 / 1 pour clean_title
used_cars <- used_cars %>%
  mutate(clean_title_encoded = ifelse(clean_title == "Yes", 1, 0))


# Remplacement des espaces par des "Electric" pour fuel_type
used_cars <- used_cars %>%
  mutate(fuel_type = ifelse(trimws(fuel_type) == "" | is.na(fuel_type), "Electric", fuel_type))

# Suppression des vides dans accident car ils ne correspondent à rien cette fois ci
used_cars <- used_cars %>%
  filter(trimws(accident) != "")

# Encodage en 0 / 1 pour la variable accident
used_cars <- used_cars %>%
  mutate(accident_binary = ifelse(trimws(accident) == "At least 1 accident or damage reported", 1, 0))

# Suppression de l'ancienne colonne accident
used_cars <- used_cars %>%
  select(-accident)

# Encodage des variables catégoriques en facteurs
used_cars <- used_cars %>%
  mutate(across(c(fuel_type, transmission, brand, model), as.factor))

# Encodage de fuel_type en chiffre de la même manière que accident
# Suppression des vides (voir excel)
used_cars <- used_cars %>%
  filter(!grepl("–", fuel_type))

# Créer un encodage numérique

# Dictionnaire pour l'encodage, trié par ordre de pollution
fuel_dict <- c("Diesel" = 1 , "Gasoline" = 2, "E85 Flex Fuel" = 3, "Hybrid" = 4, "Plug-In Hybrid" = 5, "Electric" = 6, "not supported" = 7)

used_cars <- used_cars %>%
  mutate(fuel_type_clean = as.numeric(factor(fuel_type, levels = names(fuel_dict))))

# Extraction de la puissance en HP pour la colonne engine 
# Extraire les chevaux (HP) de la colonne engine
used_cars$hp <- as.numeric(gsub("([0-9.]+)HP.*", "\\1", used_cars$engine))

# Supprimer les lignes avec des NA dans la colonne hp
used_cars <- na.omit(used_cars)

# Vérifier la distribution des couleurs dans ext_col
table(used_cars$ext_col)

# Encodage de la variable ext col en chiffre pour pouvoir l'utiliser lors de la regression, la couleur étant un élement important lors de l'achat
color_dict <- c("Black" = 1 , "White" = 2, "Gray" = 3, "Silver" = 4, "Blue" = 5, "Red" = 6, "Green" = 7, "Brown" = 8, "Gold" = 9 , "Beige"= 10 , "Orange" = 11 , "Yellow" = 12, "Purple" = 13, "Pink" = 14)

used_cars <- used_cars %>%
  mutate(color = as.numeric(factor(ext_col, levels = names(color_dict))))


# Model Planing
# Analyse de corrélatoin et sélection des variables

# Vérification de la corrélation entre les variables et le prix
library(dplyr)
correlation_matrix <- cor(used_cars %>% 
                            select(price_clean, vehicle_age, hp, milage_clean, fuel_type_clean, accident_binary, color), 
                          use = "complete.obs")
print(correlation_matrix)

# Sélection manuelle des variables importantes basées sur la corrélation
selected_features <- c("hp", "milage_clean", "fuel_type_clean", 
                        "accident_binary", "color")


# Visualisation des valeurs aberrantes pour la variable cible (price_clean)
boxplot(used_cars$price_clean, main = "Boxplot prices of the cars", ylab = "Price ($)")

# Retirer les valeurs aberrantes
# Calculer Q1, Q3 et l'IQR pour price_clean
q1 <- quantile(used_cars$price_clean, 0.25, na.rm = TRUE) 
q3 <- quantile(used_cars$price_clean, 0.75, na.rm = TRUE) 
iqr <- q3 - q1 


borne_inferieure <- q1 - 1.5 * iqr
borne_superieure <- q3 + 1.5 * iqr


used_cars <- used_cars %>%
  filter(price_clean >= borne_inferieure & price_clean <= borne_superieure)

boxplot(used_cars$price_clean,
        main = "Boxplot prices after cleaning",
        ylab = "Price ($)")

#Séparation du dataset
install.packages("ggplot2")
install.packages("lattice")
install.packages("caret", dependencies = TRUE)
library(caret)

set.seed(123)  
train_index <- createDataPartition(used_cars$price_clean, p = 0.7, list = FALSE)  

train_data <- used_cars[train_index, ]
test_data <- used_cars[-train_index, ]


# Model Builing et vérification
model <- lm(price_clean ~   hp   + vehicle_age + fuel_type_clean + color 
                       milage_clean + accident_binary, 
                   data = train_data)

summary(model)


model2 <- lm(price_clean ~   hp   + vehicle_age + fuel_type_clean + 
            milage_clean + accident_binary, 
            data = train_data)

summary(model2)

# Analyse des residue
mean_residuals <- mean(model2$residuals)
mean_residuals

# Q-Q Plot
qqnorm(rstandard(model2), 
       main = "Q-Q Plot des résidus standardisés")
qqline(rstandard(model2), col = "red", lty = 2)

# Test de Shapiro-Wilk
shapiro_test <- shapiro.test(model2$residuals)
shapiro_test

# Graphique résidus vs valeurs ajustées
plot(model2$fitted.values, model2$residuals, 
     xlab = "Valeurs ajustées", ylab = "Résidus",
     main = "Résidus vs valeurs ajustées")
abline(h = 0, col = "red", lty = 2)

# Test de Breusch-Pagan
if (!require("lmtest")) install.packages("lmtest")
library(lmtest)
bptest(model2)
# Si la p-valeur est > 0.05, l'hypothèse d'homoscédasticité est vérifiée.

# Calcul des résidus du modèle
residuals <- resid(model2)

# Nuage de points pour vérifier l'autocorrelation
plot(residuals ~ seq_along(residuals), 
     main = "Nuage de points des résidus vs indices", 
     xlab = "Index", 
     ylab = "Résidus", 
     pch = 19, col = "black")
abline(h = 0, col = "red", lty = 2)

# Transformation log
train_data$log_price <- log(train_data$price_clean)
model_log <- lm(log_price ~ hp + vehicle_age + fuel_type_clean + 
                  milage_clean + accident_binary, data = train_data)

summary(model_log)

shapiro_test2 <- shapiro.test(model_log$residuals)
shapiro_test

bptest(model_log)

# Prediction sur le set de test

price_prediction <- predict(model2, newdata = test_data)

# Affichage des premières prédictions comparées aux vraies valeurs
predicted_vs_actual <- data.frame(Real = test_data$price_clean, Predicted = price_prediction)
head(predicted_vs_actual)

# Visualiser les valeurs réelles vs les valeurs prédites
plot(test_data$price_clean, price_prediction, 
     xlab = "Valeurs réelles", ylab = "Prédictions", 
     main = "Prédictions vs Réelles")
abline(a = 0, b = 1, col = "red")  

# Calcul du RMSE
rmse <- sqrt(mean((price_prediction - test_data$price_clean)^2))
print(paste("RMSE:", rmse))

# Calcul du R²
r_squared <- 1 - sum((price_prediction - test_data$price_clean)^2) / sum((mean(test_data$price_clean) - test_data$price_clean)^2)
print(paste("R²:", r_squared))

# Calcul de la MAE
mae <- mean(abs(price_prediction - test_data$price_clean))
print(paste("MAE:", mae))

