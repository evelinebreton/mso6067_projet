#__________________________________PROJET FINAL MSO6067________________________

#_________________________________________________Preparation of the environment
#cleanup the environment 
rm( list = objects() )

gc()

if( !require( 'pacman' ) ) { install.packages( 'pacman' ) }

pacman::p_load(
  openxlsx,
  lubridate,
  tidyverse,
  readxl,
  ggplot2,
  dplyr,
  gtsummary,
  tableone,
  tidyr,
  arsenal,
  emmeans
)

#________________________________________________________________Importing data
# original dataset file name, importing the data
#to be able to run program whether in Daniela or Eveline's computer

#ordinateur Eveline
health_lifestyle_ds_EB_FN <- 
  "~/Eveline/UdeM/mso6067/projet/synthetic_health_data.xlsx"

#ordinateur Daniela
#Daniela#######entrer ton chemin

#commande: projet_heart_disease_DT_FN <- 
"~/Eveline/UdeM/mso6067/projet/heart_disease_ds/synthetic_health_data.xlsx"


health_lifestyle_ds_FN <- health_lifestyle_ds_EB_FN

#if( ! file.exists( health_lifestyle_ds_FN ) )
#{
 # health_lifestyle_ds_FN <- health_lifestyle_ds_DT_FN
  #
#  if( ! file.exists( health_lifestyle_ds_FN ) )
#  {
#    stop( "health_lifestyle input dataset not found!" )
#  }
#}


# dataset first version before any manipulation of data
health_lifestyle_ds01 <- read_excel( health_lifestyle_ds_FN )

view(health_lifestyle_ds01)

#Renaming the dataset before cleaning
health_lifestyle_ds02 <- health_lifestyle_ds01 


#-------------------------------------------------Nettoyage des données

#checking data type of columns
sapply(health_lifestyle_ds02, class)   

#checking missing values
colSums(is.na(health_lifestyle_ds02))

view(health_lifestyle_ds02) 
summary(health_lifestyle_ds02)


#----------------------Type de variables-----------------
#recoder les variables catégorielles en factor
health_lifestyle_ds02 <- health_lifestyle_ds02 |>
  mutate(across(where(is.numeric), ~ as.factor(.)))

#recoder les variables continues en numeric
health_lifestyle_ds02 <- health_lifestyle_ds02 |>
  mutate(across(where(is.character), ~ as.numeric(.)))

summary(health_lifestyle_ds02)



#---------------------statut tabagique--------------------

#Renaming the dataset before adding new column
health_lifestyle_ds03 <- health_lifestyle_ds02 

#créer nouvelle colonne pour smoking oui/non
#remplacer o/1 par NS/S pour non smoker/smoker
health_lifestyle_ds03$Smoking_Status <- 
  factor(health_lifestyle_ds03$Smoking_Status, 
         levels = c(0, 1), labels = c("NS", "S"))


view(health_lifestyle_ds03) 
summary(health_lifestyle_ds03)


#-----------------------variables aberrantes------------------
#visualiser les variables pour identifier les variables aberrantes

vars <- c("Age", "BMI", "Diet_Quality", "Sleep_Hours",
          "Alcohol_Consumption", "Health_Score")

par(mfrow = c(2, 3))  

for (v in vars) {
  boxplot(health_lifestyle_ds03[[v]], main = v)
}



#distribution des variables quantitatives (pertinent???)
health_lifestyle_ds03 %>%
  select(where(is.numeric)) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  facet_wrap(~ name, scales = "free") +
  theme_minimal()



#Diet_Quality >100
#Combien d'occurence?
sum(health_lifestyle_ds03$Diet_Quality > 100)
sum(health_lifestyle_ds03$Diet_Quality == 100, na.rm = TRUE)
#résultat: 18>100, 0=100


#Alcohol_Consumption <0
sum(health_lifestyle_ds03$Alcohol_Consumption <0)
sum(health_lifestyle_ds03$Alcohol_Consumption == 0, na.rm = TRUE)
#résultat: 70<0, 0=0, même pour les enfants?? autres variables aberrantes à considérer


#Habitudes de vie des enfants de moins de 12 ans
sum(health_lifestyle_ds03$Age<12, na.rm = TRUE)
health_lifestyle_ds03[health_lifestyle_ds03$Age < 12, ]
#ça n'a pas de sens, les 6 enfants devront être enlevés
#enlever tous les moins de 18 ans pour considérer seulement population adulte

sum(health_lifestyle_ds03$Age<18, na.rm = TRUE)
health_lifestyle_ds03[health_lifestyle_ds03$Age < 18, ]



#enlever de la base de données ces valeurs aberrantes
#Renaming the dataset before deleting rows
health_lifestyle_ds04 <- health_lifestyle_ds03


#formule identiée avec Copilot
health_lifestyle_ds04 <- health_lifestyle_ds04 %>%
  filter(!(Diet_Quality > 100 | Alcohol_Consumption < 0| Age<18))

sum(health_lifestyle_ds04$Diet_Quality > 100
    | health_lifestyle_ds04$Alcohol_Consumption < 0
    | health_lifestyle_ds04$Alcohol_Consumption <18, na.rm = TRUE)


#combien de lignes au jeu de données maintenant?
nrow(health_lifestyle_ds04)

summary(health_lifestyle_ds04)

#Revisualiser les données
vars <- c("Age", "BMI", "Diet_Quality", "Sleep_Hours",
          "Alcohol_Consumption", "Health_Score")

par(mfrow = c(2, 3))  

for (v in vars) {
  boxplot(health_lifestyle_ds04[[v]], main = v)
}



health_lifestyle_ds04 %>%
  select(where(is.numeric)) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  facet_wrap(~ name, scales = "free") +
  theme_minimal()



#---------------------------------------------------------------------Tableau 1
#Version 1 trouvées dans Copilot

library(gtsummary)

Tableau1_v1 <- health_lifestyle_ds04 %>%
  tbl_summary(
      missing = "no"
  )

Tableau1_v1


#------------version 2
str(health_lifestyle_ds04)
cat("c(", paste0('"', names(health_lifestyle_ds04), '"', collapse = ", "), ")")


vars <- c(c( "Age", "BMI", "Exercise_Frequency", "Diet_Quality", "Sleep_Hours", 
             "Smoking_Status", "Alcohol_Consumption", "Health_Score" ))
factorVars <- c("Smoking_Status")

Tableau1_v2 <- CreateTableOne(
  vars = vars,
   data = health_lifestyle_ds04,
  factorVars = factorVars
)

print(Tableau1_v2, showAllLevels = TRUE)

#--------------version 3 avec tableby, de la librairie arsenal

Tableau1_v3 <- tableby(
  ~ .,                      
  data = health_lifestyle_ds04,     
  test = FALSE,             
  numeric.stats = c("N", "meansd", "medianq1q3", "range", "Nmiss2"),     
  cat.stats = c("countpct", "Nmiss2"),    
  stats.labels = list(
    N           = "Nombre",
    countpct    = "Nombre (%)",
    meansd      = "Moyenne (É-T)",          
    medianq1q3  = "Médiane (Q1–Q3)",
    range       = "Min – Max",
    Nmiss2      = "Manquants"
  )
)


#Exporter vers word
Tableau1_v3 <- summary(
  Tableau1_v3,
  title = "Tableau 1",
  digits = 2                
)

# Construire automatique le nom du fichier, incluant la date du jour
dossier<-"~/Eveline/UdeM/mso6067/projet/"
Tableau1 <- paste0(
  dossier,
  "Tableau1_",
  format(Sys.Date(), "%d %B %Y"),
  ".docx"
)

# Export vers Word (.docx)
arsenal::write2word(
  Tableau1_v3,
  Tableau1,
  title = "Tableau 1: données de santé...",
  quiet = TRUE
)

#-------------------version 3b plus simple, à retravailler#########
Tableau1_v3b <- tableby(
  ~ .,                      
  data = health_lifestyle_ds04,     
  test = FALSE,             
  numeric.stats = c("meansd", "medianq1q3", "range"),     
  cat.stats = c("countpct"),    
  stats.labels = list(
   
    countpct    = "Nombre (%)",
    meansd      = "Moyenne (É-T)",          
    medianq1q3  = "Médiane (Q1–Q3)",
    range       = "Min – Max"
   
  )
)


#Exporter vers word
Tableau1_v3b <- summary(
  Tableau1_v3b,
  title = "Tableau 1",
  digits = 2                
)

# Construire automatique le nom du fichier, incluant la date du jour
dossier<-"~/Eveline/UdeM/mso6067/projet/"
Tableau1 <- paste0(
  dossier,
  "Tableau1_",
  format(Sys.Date(), "%d %B %Y"),
  ".docx"
)

# Export vers Word (.docx)
arsenal::write2word(
  Tableau1_v3b,
  Tableau1,
  title = "Tableau 1: données de santé...",
  quiet = TRUE
)



#------------------------------------------------------------------------------
#--------------------------------------------------------------ANALYSE
#-----------------------------------------------------------------------------

#------------------------------MODELE------------------------------------------
#########tester linéarité du modèle (cours 8)
#Faire un graphique de nuage de points entre x et y
# Nuage de points
par(mfrow = c(1, 1))

plot(health_lifestyle_ds04$Sleep_Hours,health_lifestyle_ds04$Health_Score,
     xlab = "Heures de sommeil",
     ylab = "Indice de santé",
     main= "Linéarité",
     pch = 16, col = "grey40")


#Faire un graphique de r´esidus partiels ;
#Tester si l’inclusion d’un terme au carr´e pour x est statistiquement
#significatif dans le mod`ele.
#Ceci correspond `a mod´eliser une parabole au lieu d’une simple ligne
#entre x et y.

#---------------------------------Régression
#Question de recherche: Quelle est l’association entre l’indice santé et les heures de sommeil?
#Modèle de régression simple
mod1_simple <- lm(Health_Score ~ Sleep_Hours, data = health_lifestyle_ds04)
abline(mod1_simple,col = "red", lwd = 2)
summary(mod1_simple)
confint(mod1_simple)


################NON
#Modèle de régression linéaire multiple avec âge seulement
mod2_mult<- lm(Health_Score ~ Sleep_Hours+Age, data = health_lifestyle_ds04)
summary(mod2_mult)
confint(mod2_mult)


#modèle de régression multiple avec toutes les variables de confusion
mod3_mult<- lm(Health_Score ~ Sleep_Hours+Age
               +Exercise_Frequency
               +Diet_Quality
               +Smoking_Status
               +Alcohol_Consumption
               , data = health_lifestyle_ds04)
summary(mod3_mult)
confint(mod3_mult)


#Modèle avec âge come modificateur d'effet
mod4_mult<- lm(Health_Score ~ Sleep_Hours*Age
               +Exercise_Frequency
               +Diet_Quality
               +Smoking_Status
               +Alcohol_Consumption
               , data = health_lifestyle_ds04)
summary(mod4_mult)
confint(mod4_mult)

emtrends(mod4_mult,~Age, var="Sleep_Hours")
#emtrends à faire par groupe d'âge??? malgré que modidification d'effet non-significatif?
#18-40, 40-60, 60-80, 80+
#emtrends(mod3_mult, ~ Age, var = "Sleep_Hours", at = list(stress = Age_groups))

#----------------------------DIAGNOSTICS--------------------------------------

#----------Mesures d'influence
#????Est-ce qu'on fait les analyses de résidus sur le modèle simple ou complet?


#Distance de cook
cook1 <- cooks.distance(mod1_simple)
which(cook1 > 1)
which(cook1 > 0.5)
cook1

cook3 <- cooks.distance(mod3_mult)
which(cook3 > 1)
which(cook3 > 0.5)
cook3

#DFBETAS
dfb1 <- dfbetas(mod1_simple) 
drapeau1 <- apply(abs(dfb1) > 1, 1, any)
which(drapeau1)

dfb1[drapeau1, ]
coef(mod1_simple)

dfb3 <- dfbetas(mod3_mult) 
drapeau3 <- apply(abs(dfb3) > 1, 1, any)
which(drapeau3)

dfb3[drapeau3, ]
coef(mod3_mult)

#DFFITS
dff1 <- dffits(mod1_simple)
n<-887
pp1<-2
seuil_dff1 <- 3 * sqrt(pp1 / (n - pp1))
which(abs(dff1) > seuil_dff1)

dff3 <- dffits(mod3_mult)
n<-887
pp3<-12
seuil_dff3 <- 3 * sqrt(pp3 / (n - pp3))
which(abs(dff3) > seuil_dff3)


#----------------------------Homoscédasticité
#modèle complet
valeurs_aj3=fitted(mod3_mult)
residus3 <- rstandard(mod3_mult)
plot(valeurs_aj3,residus3)


#étant donné ligne de points, graphique avec chacune des variables pour identifier
#variables fautive
#modèle 1 (simple)
valeurs_aj1=fitted(mod1_simple)
residus1 <- rstandard(mod1_simple)
plot(valeurs_aj1,residus1)


plot(mod1_simple)

#modèle avec âge seulement
mod_age<- lm(Health_Score ~ Age , data = health_lifestyle_ds04)
valeurs_aj_age=fitted(mod_age)
residus_age <- rstandard(mod_age)
plot(valeurs_aj_age,residus_age)

#la ligne est probablement dûe à la variable dépendante indice de santé?



#avec données avant nettoyage?
mod3_mult_ds2<- lm(Health_Score ~ Sleep_Hours+Age
               +Exercise_Frequency
               +Diet_Quality
               +Smoking_Status
               +Alcohol_Consumption
               , data = health_lifestyle_ds02)

valeurs_aj3_ds2=fitted(mod3_mult_ds2)
residus3_ds2 <- rstandard(mod3_mult_ds2)
plot(valeurs_aj3_ds2,residus3_ds2)
#résultat: même chose



#modèle complet

valeurs_aj3=fitted(mod3_mult)
residus3 <- rstandard(mod3_mult)
plot(valeurs_aj3,residus3)

#graphique bizarre, sandwich?
plot(mod3_mult)


library(sandwich); library(lmtest); library(car)
coeftest(mod3_mult,vcov=vcovHC(mod3_mult,type = "HC3"))                                         
Confint(mod3_mult,vcov.=vcovHC(mod3_mult,type="HC3"))


#analyse de résidus, diagnostique, analyse de sensibilité si variable influente


#---------------------------Linéarité

plot(health_lifestyle_ds04$Sleep_Hours,health_lifestyle_ds04$Health_Score,
     xlab = "Heures de sommeil",
     ylab = "Indice de santé",
     main= "Linéarité",
     pch = 16, col = "grey40")


mod.lin <- lm(Health_Score ~ Sleep_Hours, data = health_lifestyle_ds04)

mod.quad <- lm(Health_Score ~ Sleep_Hours + I(Sleep_Hours^2), data = health_lifestyle_ds04)

library(splines)
mod.ns <- lm(Health_Score ~ ns(Sleep_Hours, df = 4), data = health_lifestyle_ds04)



## 3) Grille de x pour tracer les relations estimées
xg <- seq(min(health_lifestyle_ds04$Sleep_Hours), max(health_lifestyle_ds04$Sleep_Hours), length.out = 300)
newdat <- data.frame( Sleep_Hours= xg)

pred.lin  <- predict(mod.lin,  newdata = newdat)
pred.quad <- predict(mod.quad, newdata = newdat)
pred.ns   <- predict(mod.ns,   newdata = newdat)

## 4) Graphique : données + 3 courbes estimées
plot(health_lifestyle_ds04$Sleep_Hours,health_lifestyle_ds04$Health_Score, pch = 16, cex = 0.7,
     xlab = "Heures de sommeil", ylab = "Indice de santé",
     main = "Relations estimées")

lines(xg, pred.lin,  lwd = 2, lty = 1)
lines(xg, pred.quad, lwd = 2, lty = 2, col = 'blue')
lines(xg, pred.ns,   lwd = 2, lty = 3, col = 'red')

legend("bottomright",
       legend = c("Modèle linéaire",
                  "Modèle quadratique",
                  "Spline naturelle (df = 4)"),
       lty = c(1, 2, 3),
       lwd = 2,
       bty = "n")

#----------------------------------------Multicolinéarité
library(car)
vif(mod3_mult)

###############################################################################
#----------------------------------------------------------------------------
#Health score: variable dépendante distribution atypique avec énorme bande à 100%
#------------------------------------------------------------------------------
#combien d'individus ont 100?
sum(health_lifestyle_ds03$Health_Score == 100, na.rm = TRUE)
#résultat: 242
#si on enlève ces 242 de l'échantillon et qu'on fait tous les tests et analyses?
#-------------------------------------------------------------------------------

#enlever de la base de données ces valeurs aberrantes
#Renaming the dataset before deleting rows
health_lifestyle_ds05 <- health_lifestyle_ds03


#formule identiée avec Copilot
health_lifestyle_ds05 <- health_lifestyle_ds05 %>%
  filter(!(Diet_Quality > 100 | Alcohol_Consumption < 0| Age<18 | Health_Score ==100))

sum(health_lifestyle_ds05$Diet_Quality > 100
    | health_lifestyle_ds05$Alcohol_Consumption < 0
    | health_lifestyle_ds05$Health_Score ==100
    | health_lifestyle_ds05$Alcohol_Consumption <18, na.rm = TRUE)


#combien de lignes au jeu de données maintenant?
nrow(health_lifestyle_ds05)

summary(health_lifestyle_ds05)

#Revisualiser les données
vars <- c("Age", "BMI", "Diet_Quality", "Sleep_Hours",
          "Alcohol_Consumption", "Health_Score")

par(mfrow = c(2, 3))  

for (v in vars) {
  boxplot(health_lifestyle_ds05[[v]], main = v)
}



health_lifestyle_ds05 %>%
  select(where(is.numeric)) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  facet_wrap(~ name, scales = "free") +
  theme_minimal()



#---------------------------------------------------------------------Tableau 1
#à faire selon version adoptée plus  haut

#------------------------------------------------------------------------------
#--------------------------------------------------------------ANALYSE
#-----------------------------------------------------------------------------

#------------------------------MODELE------------------------------------------
#########tester linéarité du modèle (cours 8)
#Faire un graphique de nuage de points entre x et y
# Nuage de points
par(mfrow = c(1, 1))

plot(health_lifestyle_ds05$Sleep_Hours,health_lifestyle_ds05$Health_Score,
     xlab = "Heures de sommeil",
     ylab = "Indice de santé",
     main= "Linéarité",
     pch = 16, col = "grey40")



#---------------------------------Régression
#Question de recherche: Quelle est l’association entre l’indice santé et les heures de sommeil?
#Modèle de régression simple
mod1b_simple <- lm(Health_Score ~ Sleep_Hours, data = health_lifestyle_ds05)
abline(mod1_simple,col = "red", lwd = 2)
summary(mod1_simple)
confint(mod1_simple)



#modèle de régression multiple avec toutes les variables de confusion
mod3b_mult<- lm(Health_Score ~ Sleep_Hours+Age
               +Exercise_Frequency
               +Diet_Quality
               +Smoking_Status
               +Alcohol_Consumption
               , data = health_lifestyle_ds05)
summary(mod3b_mult)
confint(mod3b_mult)


#Modèle avec âge come modificateur d'effet
mod4b_mult<- lm(Health_Score ~ Sleep_Hours*Age
               +Exercise_Frequency
               +Diet_Quality
               +Smoking_Status
               +Alcohol_Consumption
               , data = health_lifestyle_ds05)
summary(mod4b_mult)
confint(mod4b_mult)

#----------------------------DIAGNOSTICS--------------------------------------

#----------Mesures d'influence
#????Est-ce qu'on fait les analyses de résidus sur le modèle simple ou complet?


#Distance de cook
cook1b <- cooks.distance(mod1b_simple)
which(cook1 > 1)
which(cook1 > 0.5)
cook1b

cook3b <- cooks.distance(mod3b_mult)
which(cook3b > 1)
which(cook3b > 0.5)
cook3b

#DFBETAS
dfb1b <- dfbetas(mod1b_simple) 
drapeau1b <- apply(abs(dfb1b) > 1, 1, any)
which(drapeau1b)

dfb1b[drapeau1b, ]
coef(mod1b_simple)

dfb3b <- dfbetas(mod3b_mult) 
drapeau3b <- apply(abs(dfb3b) > 1, 1, any)
which(drapeau3b)

dfb3b[drapeau3b, ]
coef(mod3b_mult)

#DFFITS
dff1 <- dffits(mod1b_simple)
nb<-692
pp1b<-2
seuil_dff1b <- 3 * sqrt(pp1b / (nb - pp1b))
which(abs(dff1) > seuil_dff1)

dff3b <- dffits(mod3b_mult)
nb<-692
pp3b<-12
seuil_dff3b <- 3 * sqrt(pp3b / (nb - pp3b))
which(abs(dff3b) > seuil_dff3b)


#----------------------------Homoscédasticité
#modèle complet
valeurs_aj3b=fitted(mod3b_mult)
residus3b <- rstandard(mod3b_mult)
plot(valeurs_aj3b,residus3b)



#graphique bizarre, sandwich?
plot(mod3b_mult)


library(sandwich); library(lmtest); library(car)
coeftest(mod3b_mult,vcov=vcovHC(mod3b_mult,type = "HC3"))                                         
Confint(mod3b_mult,vcov.=vcovHC(mod3b_mult,type="HC3"))


#analyse de résidus, diagnostique, analyse de sensibilité si variable influente


#---------------------------Linéarité

plot(health_lifestyle_ds05$Sleep_Hours,health_lifestyle_ds05$Health_Score,
     xlab = "Heures de sommeil",
     ylab = "Indice de santé",
     main= "Linéarité",
     pch = 16, col = "grey40")


mod.linb <- lm(Health_Score ~ Sleep_Hours, data = health_lifestyle_ds05)

mod.quadb <- lm(Health_Score ~ Sleep_Hours + I(Sleep_Hours^2), data = health_lifestyle_ds05)

library(splines)
mod.nsb <- lm(Health_Score ~ ns(Sleep_Hours, df = 4), data = health_lifestyle_ds05)



## 3) Grille de x pour tracer les relations estimées
xgb <- seq(min(health_lifestyle_ds05$Sleep_Hours), max(health_lifestyle_ds05$Sleep_Hours), length.out = 300)
newdatb <- data.frame( Sleep_Hours= xgb)

pred.linb  <- predict(mod.linb,  newdata = newdatb)
pred.quadb <- predict(mod.quadb, newdata = newdatb)
pred.nsb   <- predict(mod.nsb,   newdata = newdatb)

## 4) Graphique : données + 3 courbes estimées
plot(health_lifestyle_ds05$Sleep_Hours,health_lifestyle_ds05$Health_Score, pch = 16, cex = 0.7,
     xlab = "Heures de sommeil", ylab = "Indice de santé",
     main = "Relations estimées")

lines(xgb, pred.linb,  lwd = 2, lty = 1)
lines(xgb, pred.quadb, lwd = 2, lty = 2, col = 'blue')
lines(xgb, pred.nsb,   lwd = 2, lty = 3, col = 'red')

legend("bottomright",
       legend = c("Modèle linéaire",
                  "Modèle quadratique",
                  "Spline naturelle (df = 4)"),
       lty = c(1, 2, 3),
       lwd = 2,
       bty = "n")

#----------------------------------------Multicolinéarité
library(car)
vif(mod3b_mult)


