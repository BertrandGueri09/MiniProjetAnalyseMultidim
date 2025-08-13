library(knitr) 
Dictionnaire=tibble::tribble(
  ~VARIABLE, ~NATURE, ~DESCRIPTION, ~MODALITES,
  "Pays", "Qualitative", "Nom du pays", "Chaine de caractère",
  "Enfant_mort", "Quantitative", "Décès d'enfants de moins de 5 ans pour 1000 naissances vivantes", "Numerique decimal",
  "exportations", "Quantitative", "Exportations de biens et services par habitant.Donné en pourcentage du PIB par habitant", "Numerique decimal",
  "dep_sante", "Quantitative", "Dépenses totales de santé par habitant.Données en pourcentage du PIB par habitant", "Numerique decimal",
  "importations", "Quantitative", "Importations de biens et services par habitant. Donné en pourcentage du PIB par habitant", "Numerique decimal",
  "revenu", "Quantitative", "Revenu net par personne", "Numerique entier",
  "taux_croissance", "Quantitative", "La mesure du taux d’inflation annuel du PIB total", "Numerique decimal",
  "esperance_vie", "Quantitative", "Le nombre moyen d'années qu'un nouveau-né vivrait si les tendances de mortalité actuelles devaient rester les mêmes", "Numerique decimal",
  "total_fertilite", "Quantitative", "Le nombre d'enfants qui naîtraient à chaque femme si les taux de fécondité par âge actuels devaient rester les mêmes.", "Numerique decimal",
  "pib_par_hab", "Quantitative", "Le PIB par habitant. Calculé comme le PIB total divisé par la population totale.", "Numerique entier"
  
)
knitr::kable(Dictionnaire) 
#importation du Dataset
Help_I=read.csv("C:/Users/HP/Desktop/INSSEDS/Cours R/MiniProjetAnalyseMultidim/help_international.csv",sep=";",dec=".",row.names=1)
# Afficher les premières lignes pour vérifier
knitr::kable(head(Help_I))
##Structure du jeu de données
knitr::kable(str(Help_I))
knitr::kable(summary(Help_I))
##Visualisation et traitement des valeurs manquantes
# Fonction pour calculer la proportion de valeurs manquantes par variable
proportion_valeurs_manquantes <- function(data) {
  # Calcul du nombre de valeurs manquantes par colonne
  nb_valeurs_manquantes <- sapply(data, function(x) sum(is.na(x)))
  # Calcul de la proportion de valeurs manquantes
  proportion_manquantes <- nb_valeurs_manquantes / nrow(data)
  # Création d'un dataframe pour le résultat
  resultat <- data.frame(Nombre = nb_valeurs_manquantes, Proportion = proportion_manquantes)
  return(resultat)
}
# Utilisation de la fonction avec votre base de données
resultat <- proportion_valeurs_manquantes(Help_I)
# Affichage du résultat
knitr::kable(resultat)
##visualisation 
# Charger le package VIM
if (!require(dplyr)) install.packages("VIM")
library(VIM)
# Utilisation de la fonction aggr() pour visualiser les valeurs manquantes
aggr(Help_I, col=c('navyblue','yellow'), numbers=TRUE, sortVars=TRUE, 
     labels=names(Help_I), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
library("DataExplorer")
introduce(Help_I)
plot_intro(Help_I)
plot_str(Help_I)
plot_missing(Help_I)
###Traitement
library(DMwR2)
Help_I= knnImputation(Help_I, k = 10, scale = TRUE, meth = "median")
knitr::kable(head(Help_I))
##Visualisation après traitement
aggr(Help_I, col=c('navyblue','yellow'), numbers=TRUE, sortVars=TRUE, 
     labels=names(Help_I), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
##Resume sans les valeurs manquantes
knitr::kable(summary(Help_I))
##Visualisation et traitement des valeurs extrême 
##Visualisation 
library(rpart)
par(mfrow=c(3,3), mar=c(3,3,3,3))
boxplot(Help_I$enfant_mort, col = "green",main="enfant_mort")
boxplot(Help_I$exportations, col = "yellow",main="exportations")
boxplot(Help_I$dep_sante, col = "red",main="dep_sante")
boxplot(Help_I$importations, col =  "orange",main="importations")
boxplot(Help_I$revenu,main="revenu")
boxplot(Help_I$taux_croissance, col = "#E7B800",main="taux_croissance")
boxplot(Help_I$life_expec, col = "orangered",main="life_expec")
boxplot(Help_I$total_fertilite, col = "skyblue",main="total_fertilite")
boxplot(Help_I$pib_par_hab, col = "lightcoral",main="pib_par_hab")
par(mfrow=c(1,1), mar=c(3,3,3,3))
#library(DescTools)
##Traitement des valeurs extrêmes et aberrantes
Help_I$enfant_mort=Winsorize(Help_I$enfant_mort)
Help_I$exportations=Winsorize(Help_I$exportations)
Help_I$dep_sante=Winsorize(Help_I$dep_sante)
Help_I$importations=Winsorize(Help_I$importations)
Help_I$revenu=Winsorize(Help_I$revenu)
Help_I$taux_croissance=Winsorize(Help_I$taux_croissance)
Help_I$life_expec=Winsorize(Help_I$life_expec)
Help_I$total_fertilite=Winsorize(Help_I$total_fertilite)
Help_I$pib_par_hab=Winsorize(Help_I$pib_par_hab)
###Visualisation après traitement
par(mfrow=c(3,3), mar=c(3,3,3,3))
boxplot(Help_I$enfant_mort, col = "green",main="enfant_mort")
boxplot(Help_I$exportations, col = "yellow",main="exportations")
boxplot(Help_I$dep_sante, col = "red",main="dep_sante")
boxplot(Help_I$importations, col =  "orange",main="importations")
boxplot(Help_I$revenu,main="revenu")
boxplot(Help_I$taux_croissance,col = "#E7B800",main="taux_croissance")
boxplot(Help_I$life_expec, col = "orangered",main="life_expec")
boxplot(Help_I$total_fertilite, col = "skyblue",main="total_fertilite")
boxplot(Help_I$pib_par_hab, col = "lightcoral",main="pib_par_hab")
par(mfrow=c(1,1), mar=c(3,3,3,3))
plot_histogram(Help_I)
plot_qq(Help_I)
library("DataExplorer")
plot_correlation(Help_I, cor_args = list("use" = "pairwise.complete.obs"))
library(corrplot)
cor=cor(Help_I)
corrplot(cor)
library(GGally)
ggpairs(Help_I)
##ACP
library(FactoMineR)
res.pca=PCA(Help_I)
knitr::kable(summary(res.pca))
plot_prcomp((Help_I), nrow = 2L, ncol = 2L)
library(factoextra)
library(corrplot)
fviz_eig(res.pca, addlabels = TRUE)
res=get_pca_var(res.pca)
corrplot(res$cos2)
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols=c("#00AFBB","#E7B800", "#FC4E07"),
             repel=TRUE
)
corrplot(res$contrib, is.corr = FALSE)
fviz_pca_ind(res.pca,col.ind = "cos2",
             gradient.cols=c("#00AFBB","#E7B800", "#FC4E07"),
             repel=TRUE)
#Classification des donnees
Classe=sample(1:dim(Help_I)[1],50)
Help_Isample=Help_I[Classe,]
#Calcul de la statistique Hopkins
get_clust_tendency(Help_I,graph = FALSE, n=50, seed = 123)

src=source(url("https://raw.githubusercontent.com/larmarange/JLutils/master/R/clustering.R"))
best=best.cutree(hc)
best
library(dendextend)
plot(hc, main= "Partition en 3 classe", xlab="", sub="", axes=FALSE, hang= - 1)
rect.hclust(hc, k=best, border = "blue")
groups= cutree(hc,k=best)
knitr::kable(groups)
set.seed(123)
#Executation de l'algorithme k-means avec k=3
kmeans_out=kmeans(Help_I, centers = best, nstart = 50)
kmeans_out
# Visualisation des résultats
fviz_cluster(kmeans_out, data = Help_I)
library(factoextra)
fviz_dend(hc, k=best, show_labels = FALSE, rect = TRUE)
#Calcul de l'indice Silhouette
library("cluster")
si<-silhouette(kmeans_out$cluster, dist(Help_I, "euclidean"))
summary(si)
#Centre de gravité de chaque cluster
centroide <- kmeans_out$centers
centroide
#Indice moyen observé chez les patients pour chaque groupe
for (i in 1:3) {
  print(sum(centroide[i, ]))
}
library(FactoMineR)
res<-PCA(Help_I)
res.hcpc<-HCPC(res,nb.clust=best,graph=TRUE)
names(res.hcpc)
# AFFECTATION DE CHAQUE INDIVIDU A UNE CLASSE (CLUSTER)
cluster = res.hcpc$data.clust 
knitr::kable(head(cluster))
#extraction des individus par groupe
cluster = res.hcpc$data.clust
groupe_1 = subset(cluster, clust ==1)
groupe_2 = subset(cluster, clust ==2)
groupe_3 = subset(cluster, clust ==3)
# CARACTERISATION DES CLUSTERS PAR LES VARIABLES
res.hcpc$desc.var
# CARACTERISATION DES CLUSTERS PAR LES DIMENSIONS (axe)
res.hcpc$desc.axes
# IDENTIFICATION DES PARAGONS (individu le plus proche du centre des classes
res.hcpc$desc.ind

