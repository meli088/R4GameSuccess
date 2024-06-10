#PROBLEMATIQUE: Dans l'univers compétitif des jeux vidéo, quels sont les 
#éléments clés qui définissent le succès ?

install.packages("dplyr")
install.packages("readr")
install.packages("ggplot2")
library(dplyr)
library(readr)
library(ggplot2)


data <- read.csv("video_games_2.csv", na.strings = c(" ", ""))
View(data)
summary(data)

# Elimination données avec valeurs manquantes
data_clean <- na.omit(data)

# Renommage des colonnes
nouveaux_noms <- c("Titre", "Caractéristiques.Portable", "Caractéristiques.Max.Joueurs", 
                   "Caractéristiques.Multiplateforme", "Caractéristiques.En.Ligne", 
                   "Métadonnées.Genres", "Métadonnées.Licencié", "Métadonnées.Éditeurs", 
                   "Métadonnées.Suite", "Métriques.Score.Critique", "Métriques.Ventes", 
                   "Métriques.Prix.Occasion", "Console.Sortie", "Classification.Sortie", 
                   "Réédition.Sortie", "Année.Sortie", "Durée.Tous.Styles.Moyenne", 
                   "Durée.Tous.Styles.Loisisr", "Durée.Tous.Styles.Médiane", 
                   "Durée.Tous.Styles.Sondés", "Durée.Tous.Styles.Précipité", 
                   "Durée.Complétionnistes.Moyenne", "Durée.Complétionnistes.Loisisr", 
                   "Durée.Complétionnistes.Médiane", "Durée.Complétionnistes.Sondés", 
                   "Durée.Complétionnistes.Précipité", "Durée.Principal+Extras.Moyenne", 
                   "Durée.Principal+Extras.Loisisr", "Durée.Principal+Extras.Médiane", 
                   "Durée.Principal+Extras.Sondés", "Durée.Principal+Extras.Précipité", 
                   "Durée.Histoire.Principale.Moyenne", "Durée.Histoire.Principale.Loisisr", 
                   "Durée.Histoire.Principale.Médiane", "Durée.Histoire.Principale.Sondés", 
                   "Durée.Histoire.Principale.Précipité")


colnames(data_clean) <- nouveaux_noms

# Vérification des changements
head(data_clean)

# Faire un tableau qui select que les trucs utiles le reste on s'en balek, osef
#aussi des va qui se répètent de la même manière pour tous les individus (ex : 
#TRUE multiplateforme pour tous les jeux...)
video_games_clean <- data_clean %>%
  select(Titre,
         Métriques.Score.Critique, 
         Métriques.Ventes, 
         Métadonnées.Genres, 
         Métadonnées.Éditeurs, 
         Console.Sortie, 
         Classification.Sortie, 
         Année.Sortie)




##### SOUS- PROB1: Existe-t-il un seuil de score critique au-delà duquel les 
#ventes de jeux vidéo augmentent de manière significative ?

# Étape 1: Segmentation des Scores
breaks <- c(-Inf, 40, 70, 100)
labels <- c("Faible", "Moyen", "Élevé")
video_games_clean$ScoreCatégorie <- cut(video_games_clean$Métriques.Score.Critique, 
                                        breaks = breaks, 
                                        labels = labels, include.lowest = TRUE)

# Étape 2: Analyse Descriptive
ventes_par_score <- video_games_clean %>%
  group_by(ScoreCatégorie) %>%
  summarise(MoyenneVentes = mean(Métriques.Ventes),
            MédianeVentes = median(Métriques.Ventes),
            ÉcartTypeVentes = sd(Métriques.Ventes))
print(ventes_par_score)

# Étape 3: Analyse de Seuil
modele_regress <- lm(Métriques.Ventes ~ Métriques.Score.Critique, 
                     data = video_games_clean)
summary(modele_regress)

# Le coefficient pour 'Métriques.Score.Critique' est de 0.027085. 
# Cela indique qu'une augmentation d'un point dans le score de critique 
# est associée à une augmentation moyenne des ventes de 0.027085 unités.
# Puisqu'il n'y a pas de jeux avec un score de 0 dans l'ensemble de données,
# l'interception (Intercept) a moins de pertinence pratique.

# La valeur p (Pr(>|t|)) pour 'Métriques.Score.Critique' est < 2e-16,
# ce qui est extrêmement faible. Cela indique que le score de la critique 
# a un effet statistiquement significatif sur les ventes des jeux vidéo.

# La valeur t pour 'Métriques.Score.Critique' est 9.460, 
# montrant que le coefficient est statistiquement significatif.

# Le R-carré du modèle est de 0.08643, ce qui signifie que 
# le modèle explique environ 8.64% de la variabilité dans les ventes.

# L'erreur standard résiduelle est de 1.122, donnant une idée de la dispersion 
# des résidus (erreurs) du modèle par rapport aux valeurs observées.

# le score de la critique semble avoir un impact significatif sur les ventes mais
# d'autres facteurs influencent également les ventes.

# Étape 4: Visualisation
ggplot(video_games_clean, aes(x = Métriques.Score.Critique, y = Métriques.Ventes, color = ScoreCatégorie)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  scale_color_manual(values = c("Faible" = "lightcoral", "Moyen" = "red2", "Élevé" = "red4")) +
  labs(title = "Relation entre Score Critique et Ventes",
       x = "Score Critique",
       y = "Ventes") 


# Commentaire pour le graphique "Relation entre Score Critique et Ventes"
# Ce graphique montre une tendance positive entre les scores critiques et les ventes de jeux vidéo, 
# suggérant que les jeux bien notés ont souvent de meilleures ventes. Cependant, la relation n'est pas stricte ;
# de nombreux jeux avec des scores élevés ne se traduisent pas toujours par des ventes élevées, et vice versa.
# Les outliers, ou jeux avec des ventes bien au-delà de la moyenne, mettent en évidence que certains titres 
# parviennent à capturer l'attention du marché malgré des scores critiques moins favorables. 
# La dispersion des données autour de la ligne de tendance bleue indique que d'autres facteurs, 
# au-delà des scores critiques, influencent les ventes. En somme, le score critique est un indicateur parmi d'autres 
# et ne détermine pas à lui seul le succès commercial d'un jeu.

# SOUS-PROBLÉMATIQUE 2: Analyse des éditeurs transformant des jeux à faible ou moyen score critique en succès commerciaux.

### Introduction: Analyse des Ventes Globales
# Histogramme pour les ventes : petite figure qui sert d'intro pour la suite
ggplot(video_games_clean, aes(x = Métriques.Ventes)) +
  geom_histogram(binwidth = 1, fill = "orangered4", color = "black") +
  scale_y_continuous(trans = "sqrt") +
  labs(x = "Ventes", y = "Fréquence (racine carrée)",
       title = "Distribution des Ventes de Jeux Vidéo")

# Commentaire pour le graphique "Distribution des Ventes de Jeux Vidéo"
# Ce graphique met en évidence la distribution des ventes de jeux vidéo. Il montre clairement que la majorité des jeux 
# vendent une quantité modeste, comme le suggère la première barre élevée. Alors que les ventes augmentent, le nombre 
# de jeux qui atteignent ces chiffres plus élevés diminue rapidement, ce qui est illustré par la décroissance des barres 
# sur le graphique. 
# L'utilisation de l'échelle racine carrée sur l'axe des y aide à mettre en évidence les données pour les jeux 
# avec des ventes plus faibles, qui autrement pourraient être éclipsées par quelques jeux à ventes très élevées. 
# Les jeux ayant des ventes au-delà de 5 millions d'unités sont relativement rares, indiquant que seuls quelques 
# titres parviennent à un tel niveau de succès commercial. 
# Cette distribution des ventes peut être indicative des défis du marché des jeux vidéo, où quelques hits dominent 
# tandis que la plupart des jeux réalisent des performances de vente plus discrètes.

### Étape 1: Identification des Jeux à Ventes Exceptionnelles
# Calcul de la moyenne des ventes et définition du seuil pour les ventes exceptionnelles
moyenne_ventes <- mean(video_games_clean$Métriques.Ventes)
seuil_ventes_exceptionnelles <- 2 * moyenne_ventes


# Sélection des jeux dont les ventes dépassent ce seuil
jeux_ventes_exceptionnelles <- video_games_clean[video_games_clean$Métriques.Ventes > seuil_ventes_exceptionnelles, ]

### Étape 2: Analyse Spécifique des Éditeurs
# Filtrage des jeux publiés par des éditeurs majeurs
éditeurs_inclus <- c("Nintendo", "Rockstar", "Ubisoft", "Sony", "Sega", "Microsoft", "Konami", "EA", "Activision", "2K")
jeux_éditeurs_inclus <- subset(video_games_clean, Métadonnées.Éditeurs %in% éditeurs_inclus)

# Calcul et visualisation des pourcentages de ventes par éditeur
pourcentages_ventes <- prop.table(table(jeux_éditeurs_inclus$Métadonnées.Éditeurs)) * 100
df_pourcentages <- data.frame(Éditeurs = names(pourcentages_ventes), Pourcentage = as.numeric(pourcentages_ventes))


nouvelles_couleurs <- c("2K" = "#390603",      
                        "Activision" = "#890d06",  
                        "EA" = "#a10e05",          
                        "Konami" = "#c70e03",      
                        "Microsoft" = "#e7170a",   
                        "Nintendo" = "#ff1204",    
                        "Rockstar" = "#cf2e2e",    
                        "Sega" = "#ff5c53",        
                        "Sony" = "#fd7d76",        
                        "Ubisoft" = "#fcfaea"      
)

ggplot(df_pourcentages, aes(x = "", y = Pourcentage, fill = Éditeurs)) +
  geom_bar(stat = "identity", width=1, color="black") +
  coord_polar(theta = "y") +
  labs(title = "Part des Ventes par Éditeur", fill = "Éditeurs") +
  theme_void() +
  scale_fill_manual(values = nouvelles_couleurs) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal")


# Commentaire pour le graphique "Part des Ventes par Éditeur"
# Ce diagramme en camembert coloré nous présente un aperçu de la répartition des ventes de jeux vidéo parmi une sélection d'éditeurs majeurs du secteur. 
# Chaque segment du camembert correspond à la part des ventes totales attribuable à chaque éditeur, ce qui offre une perspective instantanée de leur 
# poids sur le marché. Des éditeurs comme Nintendo, Sony et Ubisoft se distinguent par des parts significatives, indiquant leur succès commercial 
# et leur influence sur l'industrie. 
# D'autres, bien que représentant une plus petite portion du camembert, jouent néanmoins un rôle important dans le paysage du jeu vidéo. 
# Cette représentation nous aide à comprendre quelles maisons d'édition dominent le marché et la diversité concurrentielle présente. 

### Étape 3: Analyse des Jeux à Faible ou Moyen Score Critique mais à Ventes Élevées
# Définition d'un nouveau seuil pour les ventes élevées (moyenne + écart type)
seuil_ventes_élevées <- moyenne_ventes + sd(video_games_clean$Métriques.Ventes)

# Sélection et visualisation des jeux à faible ou moyen score critique mais à ventes élevées
anomalies <- video_games_clean %>%
  filter(ScoreCatégorie %in% c("Faible", "Moyen"), Métriques.Ventes >= seuil_ventes_élevées)

ggplot(anomalies, aes(x = Métriques.Score.Critique, y = Métriques.Ventes, color = Métadonnées.Éditeurs)) +
  geom_point() +
  labs(title = "Jeux à Faible ou Moyen Score Critique mais à Ventes Élevées",
       x = "Score Critique",
       y = "Ventes") +
  scale_color_manual(values = c("Nintendo" = "orangered4", "Sega" = "red2", "Nintendo,Sega" = "lightcoral"))

# Commentaire pour le graphique "Jeux à Faible ou Moyen Score Critique mais à Ventes Élevées"
# Ce graphique de dispersion met en lumière une situation intéressante sur le marché des jeux vidéo : certains jeux réussissent à réaliser des ventes importantes 
# même si leur score critique n'est que faible ou moyen. Les points représentent des jeux individuels, avec leur positionnement horizontal indiquant le score critique 
# et le vertical pour les ventes. On note que malgré des scores critiques qui ne sont pas exceptionnels, certains de ces jeux ont connu un succès commercial notable, 
# dépassant même les 10 millions d'unités vendues.
# Les couleurs différencient les jeux par éditeurs, avec des noms bien connus comme Nintendo et Sega. Cela suggère que la réputation de l'éditeur ou d'autres 
# stratégies de marketing peuvent avoir contribué à ces succès de vente. 
# C'est une indication que le score critique, bien qu'important, n'est pas l'unique déterminant du succès commercial d'un jeu, et que les éditeurs ayant 
# une forte notoriété ou des franchises établies peuvent influencer positivement les ventes.


### Étape 4: Comparaison Statistique des Ventes
# Test t de Student pour comparaison des moyennes de ventes
video_games_clean$ScoreGroupé <- ifelse(video_games_clean$ScoreCatégorie %in% c("Faible", "Moyen"), "Faible/Moyen", "Élevé")
test_t <- t.test(Métriques.Ventes ~ ScoreGroupé, data = video_games_clean)
print(test_t)

### Conclusion:
# Bien que les scores critiques soient un indicateur important, ils ne sont pas
# le seul facteur déterminant le succès commercial d'un jeu. Les éditeurs jouent
# un rôle crucial dans la réussite commerciale des jeux, et certains sont capables
# de transformer des jeux moins bien notés en succès commerciaux. 


# SOUS-PROBLÉMATIQUE 3: Y a-t-il des tendances temporelles dans le choix des consoles pour le lancement des jeux qui influencent leur succès ?

# Étape 1: Analyse Temporelle des Consoles
# Agrégation du nombre de jeux par console et par année

jeux_par_console_annee <- video_games_clean %>%
  group_by(Console.Sortie, Année.Sortie) %>%
  summarise(NombreJeux = n())

# Visualisation avec un graphique en ligne
ggplot(jeux_par_console_annee, aes(x = Année.Sortie, y = NombreJeux, color = Console.Sortie)) +
  geom_line(size = 1.5) +
  scale_color_brewer(palette = "OrRd") +
  labs(title = "Nombre de Jeux Lancés par Console au Fil du Temps",
       x = "Année de Sortie",
       y = "Nombre de Jeux") 

# Commentaire pour le graphique "Nombre de Jeux Lancés par Console au Fil du Temps"
# Cette visualisation trace l'évolution du nombre de jeux publiés pour différentes consoles de jeux vidéo sur une période de cinq ans. 
# Chaque ligne colorée représente une console spécifique, ce qui nous permet de suivre les tendances et les changements dans le temps.
# Par exemple, on observe que la Nintendo DS et la Wii ont connu une augmentation significative du nombre de jeux disponibles, 
# avec un pic notable pour la Wii en 2007. En revanche, la PlayStation 3 montre une montée en puissance progressive depuis son lancement.
# La Sony PSP, après une forte présence initiale, voit son nombre de jeux diminuer vers la fin de la période, tandis que la X360 maintient 
# une certaine constance tout au long des années observées.

# Etape 2 : Catégoriser l'année de sortie 
# Indépendance entre l'année de sortie et les niveaux de ventes

# Catégorisation annuelle
video_games_clean$Catégorie_Année <- as.factor(video_games_clean$Année.Sortie)

# Étape 3 : Catégoriser les Ventes
# Exemple de catégorisation des ventes
seuils_ventes <- quantile(video_games_clean$Métriques.Ventes, probs = c(0, 0.33, 0.66, 1))
video_games_clean$Catégorie_Ventes <- cut(video_games_clean$Métriques.Ventes, 
                                          breaks = seuils_ventes, 
                                          labels = c("Bas", "Moyen", "Élevé"),
                                          include.lowest = TRUE)

# Création du tableau de contingence
tableau_contingence <- table(video_games_clean$Catégorie_Année, video_games_clean$Catégorie_Ventes)

# Test du Chi-carré
test_chi2 <- chisq.test(tableau_contingence)
print(test_chi2)

# Résultats du Test du Chi-carré de Pearson pour l'indépendance

# Le test a été réalisé pour examiner l'indépendance entre l'année de sortie des jeux vidéo (catégorisée)
# et leurs niveaux de ventes (également catégorisés).

# Statistique du Chi-carré (X-squared) : 8.9239
# Cette valeur mesure l'écart entre les fréquences observées dans notre tableau de contingence
# et les fréquences attendues si les deux variables étaient indépendantes.

# Degrés de liberté (df) : 8
# Les degrés de liberté sont déterminés par le nombre de catégories dans chaque variable,
# avec la formule (nombre de catégories - 1) pour chaque variable.

# Valeur p (p-value) : 0.3488
# La valeur p est utilisée pour déterminer la signification statistique. Une valeur p inférieure à 0.05
# est généralement considérée comme indiquant une association statistiquement significative.
# Ici, avec une valeur p de 0.3488, nous ne pouvons pas rejeter l'hypothèse nulle d'indépendance.

# Interprétation :
# Les résultats suggèrent qu'il n'y a pas de preuve statistique significative d'une association entre
# l'année de sortie des jeux vidéo et leurs niveaux de ventes. En d'autres termes, l'analyse n'indique pas
# de tendance claire ou de relation statistiquement significative entre l'année de sortie des jeux vidéo
# et leurs performances en termes de ventes.

# Conclusion :
# Sur la base de ces résultats, il ne semble pas que l'année de sortie des jeux vidéo soit statistiquement liée
# à la performance de leurs ventes dans cet ensemble de données.


# SOUS-PROBLÉMATIQUE 4: Les jeux destinés à des publics plus matures génèrent-ils des scores de critiques et des 
# ventes plus élevés en raison de leur contenu ?


# Étape 1: Comparaison de Contenu
# Résumé du nombre de jeux par classification
table_classification <- table(video_games_clean$Classification.Sortie)
barplot(table_classification, main = "Nombre de Jeux par Classification", xlab = "Classification", ylab = "Nombre de Jeux")

# Étape 2: Relation avec les Scores et Ventes
# Histogramme pour les scores par classification (E + EVERYONE, M = MATURE, T = TEEN)

ggplot(video_games_clean, aes(x = Métriques.Score.Critique, fill = Classification.Sortie)) +
  geom_histogram(position = "dodge", binwidth = 5) +
  scale_fill_manual(values = c("E" = "firebrick3", "M" = "darkred", "T" = "#fd7d76")) +
  labs(title = "Distribution des Scores par Classification",
       x = "Score Critique",
       y = "Nombre de Jeux")

# Analyse figure "" :
# Jeux pour Tous (E) : Les jeux classés E semblent avoir une large distribution, 
# avec un certain nombre de jeux obtenant de très bons scores et d'autres ayant 
# des scores plus bas.
# Jeux pour Adolescents (T) : Les jeux classés T montrent également une distribution
# large mais avec une concentration moindre dans les scores les plus élevés par 
# rapport aux jeux classés E.
# Jeux pour Adultes (M) : Les jeux classés M semblent avoir un pic notable pour 
# les scores plus élevés, suggérant que les jeux destinés à un public mature sont
# généralement bien reçus par les critiques ou que les critiques tendent à noter
# plus favorablement des jeux avec du contenu mature.
# Ces données pourraient suggérer que les jeux destinés à un public mature (M) ont
# tendance à obtenir de meilleurs scores critiques (peut être grace à complexité
# des thèmes, la qualité de la production, ou les attentes des joueurs.

# Etape 3 : Test
# Test ANOVA pour les scores
anova_scores <- aov(Métriques.Score.Critique ~ Classification.Sortie, data = video_games_clean)
summary(anova_scores)

# Test ANOVA pour les ventes
anova_ventes <- aov(Métriques.Ventes ~ Classification.Sortie, data = video_games_clean)
summary(anova_ventes)

# Interprétation des Résultats des Tests ANOVA

# ANOVA pour les Scores Critiques
# Les résultats de l'ANOVA pour les scores critiques montrent une valeur F de 1.709 et une valeur p de 0.182.
# - Df : 2 pour la classification, 945 pour les résidus.
# - La valeur p supérieure à 0.05 indique qu'il n'y a pas de différence statistiquement significative 
#   dans les scores critiques entre les différentes classifications des jeux.

# ANOVA pour les Ventes
# L'ANOVA pour les ventes indique une valeur F de 3.034 et une valeur p de 0.0486.
# - Df : 2 pour la classification, 945 pour les résidus.
# - La valeur p juste en dessous de 0.05 suggère une différence statistiquement significative 
#   dans les ventes entre les différentes classifications des jeux.
# - Cependant, comme la valeur p est très proche de 0.05, cette conclusion doit être interprétée avec prudence.
# - Un test post-hoc, tel que le test de Tukey, pourrait être utilisé pour explorer plus en détail 
#   les différences spécifiques entre les classifications.

# Conclusion
# Selon l'ANOVA, la classification des jeux n'a pas d'impact significatif sur les scores critiques, 
# mais semble avoir un effet sur les ventes. Cela pourrait indiquer que les jeux destinés à des publics 

# plus matures génèrent des ventes plus élevées.

# Conclusion à la problématique initiale : 

# En conclusion, bien que la qualité perçue (mesurée
# par le score critique) ait une influence positive sur les ventes de jeux vidéo, 
# d'autres facteurs comme la classification et l'éditeur jouent également un rôle 
# important. La classification des jeux semble avoir un effet sur les ventes, 
# suggérant que le public cible et le contenu du jeu sont des facteurs cruciaux 
# dans le succès commercial. Cependant, la qualité perçue n'est pas l'unique 
# déterminant des ventes, comme le montrent les anomalies de jeux à scores faibles
# ou moyens mais à ventes élevées. Ces résultats indiquent la complexité du marché
# des jeux vidéo et la nécessité de prendre en compte plusieurs dimensions pour 
# comprendre pleinement le succès commercial d'un jeu.

