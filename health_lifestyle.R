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

if( ! file.exists( health_lifestyle_ds_FN ) )
{
  health_lifestyle_ds_FN <- health_lifestyle_ds_DT_FN
  
  if( ! file.exists( health_lifestyle_ds_FN ) )
  {
    stop( "health_lifestyle input dataset not found!" )
  }
}


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


#########tester linéarité du modèle (cours 8)
#Faire un graphique de nuage de points entre x et y
# Nuage de points
plot(health_lifestyle_ds04$Sleep_Hours,health_lifestyle_ds04$Health_Score,
     xlab = "Heures de sommeil",
     ylab = "Indice de santé",
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
abline(mod1_simple,col = "red", lwd = 2)

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


#Interaction ou modificateur d'effet? Âge
mod3_mult<- lm(Health_Score ~ Sleep_Hours*Age
               +Exercise_Frequency
               +Diet_Quality
               +Smoking_Status
               +Alcohol_Consumption
               , data = health_lifestyle_ds04)
summary(mod3_mult)
confint(mod3_mult)

emtrends(mod3_mult,~Age, var="Sleep_Hours")
#emtrends à faire par groupe d'âge
#18-40, 40-60, 60-80, 80+
#emtrends(mod3_mult, ~ Age, var = "Sleep_Hours", at = list(stress = Age_groups))

#analyse de sensibilité pour BMI?
#comment choisir interaction?


#analyse de résidus, diagnostique, analyse de sensibilité si variable influente


#test 1
