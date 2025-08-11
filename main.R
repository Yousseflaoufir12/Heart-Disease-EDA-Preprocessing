#==========================================================================================================================
# Script de préparation des données : Analyse des maladies cardiaques 
#  Dataset : heart-disease (https://archive.ics.uci.edu/dataset/45/heart+disease)
#  Objectif : Nettoyage, Transformation et etiquettage des variables 

#==========================================================================================================================

# Chargement des données
data=read.csv("data.csv")
# Renommage des colonnes pour plus de clarté
colnames(data)=c("age","sex","cp","trestbps","chol","fbs","resecg","thalach","exang","oldpeak","slope","ca","thal","target")
#  Correction des erreurs de saisies dans la variable cible
# Les valeurs 2,3 et 4 sont mappés à 1 -> malade
  data$target[data$target==2]=1
  data$target[data$target==3]=1
  data$target[data$target==4]=1
# Suppression des lignes contenant des valeurs manquantes encodés par '?'
  valeurs_manquantes_ca=which(data$ca%in%"?") # Indexes valeurs manquantes ca
  valeurs_manquantes_thal=which(data$thal%in%"?") # Indexes valeurs manquantes thal 
  valeurs_manquantes=c(valeurs_manquantes_ca,valeurs_manquantes_thal)
  data=data[-valeurs_manquantes,]
  # Vérification des types de variables , souvent mal détécté lors du chargement des données
str(data)
# Conversion des variables qualitatives en facteurs
data$sex=as.factor(data$sex)
data$cp=as.factor(data$cp)
data$fbs=as.factor(data$fbs)
data$resecg=as.factor(data$resecg)
data$exang=as.factor(data$exang)
data$slope=as.factor(data$slope)
data$ca=as.factor(data$ca)
data$thal=as.factor(data$thal)
data$target=as.factor(data$target)
# Conversion des variables quantitatives en entiers
data$age=as.integer(data$age)
data$trestbps=as.integer(data$trestbps)
data$chol=as.integer(data$chol)
data$thalach=as.integer(data$thalach)
# Verification finale de la structure des données
str(data)
# Attribution d'etiquettes plus explicites aux variables qualitatives
levels(data$sex)=c("Femme","Homme")
levels(data$cp)=c("Angine stable","Angine instable","Autre douleur","Asymptomatique")
levels(data$fbs)=c("Non","Oui")
levels(data$resecg)=c("Normal","Anomalie","Hypertrophie")
levels(data$exang)=c("Non","Oui")
levels(data$slope)=c("En hausse","Stable","En baisse")
levels(data$ca)=c("Absence d'anomalie","Faible","Moyen","Elevé")
levels(data$thal)=c("Non","Thalassémie sous controle","Thalassémie instable")
levels(data$target)=c("Non","Oui")

#========================================================================================================
# Analyse descriptive : Variables qualitatives - Effectifs & Frequence
#========================================================================================================
analyser_variable_qualitative=function(variable,nom_variable){
  effectif=table(variable)
  frequence=round(prop.table(effectif),4)*100
  
  cat("Variable :  ",nom_variable,"\n")
  print(effectif)
  print(frequence)
  cat("--------------------------------------------------- \n")
}
analyser_variable_qualitative(data$sex,"Sexe")
analyser_variable_qualitative(data$cp,"Douleur thoracique (cp)") #
analyser_variable_qualitative(data$fbs,"Glycémie > 120 mg /dl (fbs)")
analyser_variable_qualitative(data$resecg,"Resultat electrocardiogramme (reseg)")
analyser_variable_qualitative(data$exang,"Angine induite a l'effort (exang)")
analyser_variable_qualitative(data$slope,"Pente du segment ST (slope)")
analyser_variable_qualitative(data$ca,"Nombre de vaisseaux coloré (ca)")
analyser_variable_qualitative(data$thal,"Thaléssémie (thal)")
analyser_variable_qualitative(data$target,"Présence de maladie cardiaque (target)")

#========================================================================================================
# Analyse descriptive : Variables quantitative - Moyennes,Médiane,quartiles,min, max,var,sd
#========================================================================================================

analyser_variable_quantitative=function(variable,nom_variable){
  cat("Variable : ",nom_variable,"\n")
  stats=summary(variable)
  print(stats)
  print("Indicateurs de dispersion")
  cat("Variance :",var(variable),"\n")
  cat("Ecart-type : ",sd(variable),"\n")
  # Interpretation auto de la dispersion : 
  if (sd(variable) < 5){
    interpretation="Les valeurs sont faiblement dispersés autour de la moyenne."
  }
  else if (sd(variable)>=5 && sd(variable)<10){
    interpretation="Les valeurs sont modérément dispérsés autour de la moyenne"
  }
  else{
    interpretation="Les valeurs sont fortement dispérsés autour de la moyenne"
  }
  cat("Interprétation : ",interpretation,"\n")
  cat("--------------------------------------")
}
analyser_variable_quantitative(data$age,"Age")
analyser_variable_quantitative(data$trestbps,"Pression arterielle au repos (prestbps)")
analyser_variable_quantitative(data$chol,"Cholésterol (chol)")
analyser_variable_quantitative(data$thalach,"Fréquence cardiaque maximale atteinte (thalach)")
analyser_variable_quantitative(data$oldpeak,"Dépression ST induite par l'exercice (oldpeak)")

#============================================================================================
# Data-visualisation : Variable qualitative (Diagramme à bar et Diagramme circulaire (camembert))
#===========================================================================================
## variable Sex: 
bar_plot_sex=plot(data$sex,xlab="Sexe",ylab="Effectif",main="Répartition des patients selon Sexe",
     las=1,
     #horiz=T,
     sub="Données : Heart Disease Dataset (UCL ML)",
     #names.arg=c("H","F"),
     space=0.4,
     col=c("#e63946","#a8dadc"),
     border='gray',
     #density=20
     cex.main=1.5,
     cex.axis=1.1,
     cex.lab=1.2,
     ylim=c(0,250)
)
text(x=bar_plot_sex,y=table(data$sex)+15,label=as.character(table(data$sex)),cex=1.2,font = 3)

## variable CP : 
bar_plot_cp=plot(data$cp,
     xlab="Douleurs Thoraciques",
     ylab="Effectif",
     main="Répartition des patients selon les Douleurs Thoraciques ",
    space=0.3,
    col=c("#08C5D1","#6A645A","#226D68","#D6955B"),
    ylim=c(0,150),
    cex.main=1.5,
    cex.lab=1.2,
)
text(x=bar_plot_cp,y=table(data$cp)+5,label=as.character(table(data$cp)),cex=1.2,font = 3)

## variable fbs : 
bar_plot_fbs=plot(data$fbs,
                  xlab="Glycémie à jeun",
                  ylab="Effectif",
                  main="Répartition des patients selon la glycémie à jeun (>120mg/L)",
                  space=0.1,
                  col=c("#08C5D1","#6A645A"),
                  ylim=c(0,300),
                  cex.main=1.2,
                  cex.lab=1.2)
text(x=bar_plot_fbs,y=table(data$fbs)+17,labels = as.character(table(data$fbs),cex=1.2,font=3))

## variable resecg : 
bar_plot_resecg=plot(data$resecg,
     xlab="Electrocardiogramme au repos",
     ylab="Effectif",
     main="Répartition des patients selon l'electrocardiogramme au repos",
     space=0.1,
     ylim=c(0,200),
     col=c("#226D68","#D6955B","#08C5D1"),
     cex.main=1.2,
     cex.lab=1.2
     )
text(x=bar_plot_resecg,y=table(data$resecg)+15,labels=as.character(table(data$resecg)),cex=1.2,font=3)
# Variable exang : 
bar_plot_exang=plot(data$exang,
                    xlab="Angine induite par exercice",
                    ylab="Effectif",
                    main="Répartition des patients selon l'angine induite par exercice",
                    space=0.2,
                    ylim=c(0,250),
                    col=c("#08C5D1","#6A645A"),
                    cex.main=1.2,
                    cex.lab=1.2
)

text(bar_plot_exang,y = table(data$exang)+20,labels = as.character(table(data$exang)),cex = 1.2,font=3)
##  variable slope : 
bar_plot_slope=plot(data$slope,
     xlab="Pente du segment ST à l'effort",
     ylab="Effectif",
     main="Répartition des patients selon pente ST`",
     ylim=c(0,180),
     space=0.1,
     col=c("#226D68","#D6955B","#08C5D1"),
     cex.main=1.2,
     cex.lab=1.2
     )
text(bar_plot_slope,y=table(data$slope)+13,labels=as.character(table(data$slope)),cex = 1.2,font=3)

##  Variable ca 
bar_plot_ca=plot(data$ca,
     xlab="Nombre de vaisseaux principaux colorés par fluoroscopie (0 à 3)",
     ylab="Effectif",
     main="Répartition des patients selon le Nombre de vaisseaux principaux colorés par fluoroscopie",
     ylim=c(0,200),
     space=0.1,
     col=c("#08C5D1","#6A645A","#226D68","#D6955B"),
     cex.main=1.2,
     cex.lab=1.2)
text(bar_plot_ca,y=table(data$ca)+10,labels=as.character(table(data$ca)),cex=1.2,font=3)
## variable thal 
bar_plot_thal=plot(data$thal,
                   xlab="Anomalie Thaléssemique",
                   ylab="Effectif",
                   space=0.2,
                   ylim=c(0,200),
                   col=c("#226D68","#D6955B","#08C5D1"),
                   cex.main=1.2,
                   cex.lab=1.2)
text(bar_plot_thal,y=table(data$thal)+10,labels=as.character(table(data$thal)),cex=1.2,font = 3)

## variable target 
labels=c("Absence de la maladie (0) ","Présence de maladie (1)")
values=table(data$target)
pourcentages=round(100*values/sum(values),1)
etiquettes=paste0(labels,":",pourcentages,"%")
pie(values,
    labels = etiquettes,
    main="Répartition des patients selon la présence de maladie cardiaque",
    clockwise = TRUE,
    border = "white",
    col=c("#08C5D1","#6A645A"),
    cex=1.1
    
    )
#============================================================================================
# Data-visualisation : Variable quantitative (Diagramme en boite et Histogramme)
#===========================================================================================

## variable age : 
boxplot(x = data$age,ylab="age",main="Boite à moustache des patients selon l'age",
        las=1,
        col='red',
        cex.main=1.3,
        cex.lab=1.1,
        sub="Données : Heart Disease Dataset (UCI ML)",
        notch = TRUE,
        ylim=c(20,80))

hist_age=hist(data$age,xlab="Age",ylab = "Effectif",
     main = "Répartition des patients selon l'age",
     cex.main=1,2,
     las=1,
     col='lightblue',
     ylim=c(0,250),
     cex.lab=1.3,
     cex.main=1.2
     )
text(hist_age$mids,hist_age$counts,labels = hist_age$counts,adj = c(-0.5,1.2))

## variable trestbps

boxplot(x = data$trestbps,
        ylab="Tension arterielle au repos",
        main="Boite à moustache des patients selon la Tension arterielle au repos",
        las=1,
        col="Blue",
        ylim=c(80,200),
        cex.lab=1.3,
        cex.main=1.2,
        sub="Données : Heart Disease Dataset (UCI ML)",
        notch = TRUE)

hist_trestbps=hist(data$trestbps,ylab = "Effectif",
     xlab = "Tension arterielle au repos",
     main="Répartition des patients selon la Tension arterielle au repos",
     las=1,
     col='red3',
     ylim = c(0,100),
     xlim=c(80,200),
     cex.lab=1.2,
     cex.main=1.2
     )
text(hist_trestbps$mids,y = hist_trestbps$counts,labels = hist_trestbps$counts,adj = c(0,0))

## variable chol : 
boxplot(x = data$chol,
        ylab="Cholestérol",
        main="Boite à moustache des patients selon le Cholésterol",
        las=1,
        col="Blue",
        cex.lab=1.3,
        cex.main=1.2,
        sub="Données : Heart Disease Dataset (UCI ML)",
        notch = TRUE)

hist_chol=hist(data$chol,ylab = "Effectif",
                   xlab = "Cholestérol",
                   main="Répartition des patients selon le Cholestérol",
                   las=1,
                   col='red2',
               ylim = c(0,150),
               xlim = c(80,600),
                   cex.lab=1.2,
                   cex.main=1.2
)
text(hist_chol$mids,y = hist_chol$counts,labels = hist_chol$counts,adj = c(0.5,-1))
## variable thalac :

boxplot(x = data$thalach,
        ylab="Fréquence cardiaque maximale atteinte",
        main="Boite à moustache des patients selon la Fréquence cardiaque maximale atteinte",
        las=1,
        col="Blue",
        cex.lab=1,
        cex.main=1.1,
        sub="Données : Heart Disease Dataset (UCI ML)",
        notch = TRUE)

hist_thalac=hist(data$thalach,ylab = "Effectif",
               xlab = "Fréquence cardiaque maximale atteinte",
               main="Répartition des patients selon la Fréquence cardiaque maximale atteinte",
               las=1,
               col='red3',
               xlim = c(65,200),
               ylim = c(0,80),
               cex.lab=1.2,
               cex.main=1.2
)
text(hist_thalac$mids,y = hist_thalac$counts,labels = hist_thalac$counts,adj = c(0.5,-1))

#============================================================================================
# Analyse bi-variée : Influence des variables explicatives sur la présence de maladies cardiaques
#===========================================================================================
# Variables qualitatives (Diagramme à barre croisée)
## variable sex et target 
bar_plot_sex_and_target=barplot(table(data$target,data$sex),beside = TRUE,
        col=c("#08C5D1","red3"),
        xlab = "Sexe",
        ylab = "Patients",
        las=1,
        main = "Répartition des patients selon la présence d'une maladie cardio-vasculaire \n et le sexe",
        ylim = c(0,150),
        cex.main=1.1,
        cex.lab=1.2
)
legend("top",legend = levels(data$target),fill =c("#08C5D1","red3"),title = "Maladie cardiovasculaire",horiz = TRUE)
text(x =bar_plot_sex_and_target,y = table(data$target,data$sex)+10,labels = as.character(table(data$target,data$sex)),cex = 1.1,font=3)
## Variable cp et target 
bar_plot_cp_and_target=barplot(table(data$target,data$cp),
                               beside = TRUE,
                               las=1,
                               ylim=c(0,120),
                               col=c("#08C5D1","red3","#6A645A","#226D68"),
                               main="Repartition des patients selon la présence de maladies cardiovasculaires \n et le type de douleurs thoraciques",
                               xlab="Type de douleurs thoraciques",
                               ylab="Patients",
                               cex.main=1.2,
                               cex.lab=1.2
                               
                               )
legend("top",legend = levels(data$target),fill = c("#08C5D1","red3"),title = "Maladie cardiovasculaire",horiz=TRUE)
text(x=bar_plot_cp_and_target,y=table(data$target,data$cp)+10,labels=as.character(table(data$target,data$cp)),cex = 1.1,font=3)

##variable fbs et target 
bar_plot_fbs_and_target=barplot(table(data$target,data$fbs),
                                beside = TRUE,
                                las=1,
                                ylim=c(0,180),
                                col = c("#08C5D1","red3"),
                                main = "Répartition des patients selon la présence de maladies cardiovasculaires \n et la glycémie à jeun (>120mg/Dl)",
                                cex.main=1.2,
                                cex.lab=1.2
)
legend("top",legend = levels(data$target),fill = c("#08C5D1","red3"),title = "Maladie cardiovasculaire",horiz = TRUE)
text(x = bar_plot_fbs_and_target,y=table(data$target,data$fbs)+10,labels = as.character(table(data$target,data$fbs)))

## variable resecg et target
bar_plot_resecg_and_target=barplot(table(data$target,data$resecg),
        beside = TRUE,
        col=c("#08C5D1","red3"),
        xlab = "Résultat de l'electrocardiogamme",
        ylab ="Patients",
        main = "Répartition des patients selon la présence d'une maladie cardio-vasculaire \n et le résultat de l'électrocardiogramme",
        cex.main=1.2,
        cex.lab=1.1,
        ylim = c(0,100),
        las=1)
legend("top",legend = levels(data$target),fill = c("#08C5D1","red3"),title = "Maladie cardiovasculaire",horiz = TRUE)
text(x = bar_plot_resecg_and_target,y = table(data$target,data$resecg)+2,labels = as.character(table(data$target,data$resecg)))
## variable exang et target 
barplot_exang_and_target=barplot(table(data$target,data$exang),
        beside = TRUE,
        col = c("#08C5D1","red3"),
        main = "Répartition des patients selon la présence d'une maladie cardio-vasculaire \n et l'angine induite à l'effort",
        cex.main=1.2,
        las=1,
        ylim=c(0,150)
        )
legend("top",legend = levels(data$target),fill = c("#08C5D1","red3"),title = "Maladie cardiovasculaire",horiz = TRUE )
text(x = barplot_exang_and_target,y = table(data$target,data$exang)+3,labels = as.character(table(data$target,data$exang)))
## variable slope et target 
barplot_slope_and_target=barplot(table(data$target,data$slope),
        beside = TRUE,
        col=c("#08C5D1","red3"),
        las=1,
        main = "Répartition des patients selon la présence d'une maladie cardio-vasculaire \n et la pente du segment ST ",
        cex.main=1.2,
        cex.lab=1.2,
        ylim=c(0,120))
legend("top",legend = levels(data$target),fill = c("#08C5D1","red3"),title = "Maladie cardiovasculaire",horiz = TRUE)
text(x = barplot_slope_and_target,y = table(data$target,data$slope)+5,labels = as.character(table(data$target,data$slope)))
## variable ca et target
bar_plot_ca_and_target=barplot(table(data$target,data$ca),
                               beside = TRUE,
                                col=c("#08C5D1","red3"),
                               xlab = "Nombre de vaisseaux principaux colorés par fluoroscopie (0 à 3)",
                               ylab = "Patients",
                               ylim = c(0,150),
                               las=1,
                               main = "Répartition des patients selon la présence d'une maladie cardio-vasculaire \n et le Nombre de vaisseaux principaux colorés par fluoroscopie",
                               cex.main=1.2,
                               cex.lab=1.2

                               )
legend("top",legend = levels(data$target),fill = c("#08C5D1","red3"),title="Maladie cardiovasculaire",horiz = TRUE)
text(x = bar_plot_ca_and_target,y = table(data$target,data$ca)+10,labels = as.character(table(data$target,data$ca),cex=1.1,font=3))

## variable thal et target 
barplot_thal_and_target=barplot(table(data$target,data$thal),
        beside = TRUE,
        las=1,
        col=c("#08C5D1","red3"),
        ylim=c(0,150),
        main = "Répartition des patients selon la présence d'une maladie cardio-vasculaire \n et Thaléssemie",
        cex.main=1.2,
        cex.lab=1.1)
legend("top",legend = levels(data$target),fill = c("#08C5D1","red3"),title = "Maladie cardiovasculaire",horiz = T)
text(x = barplot_thal_and_target,y = table(data$target,data$thal)+4,labels = as.character(table(data$target,data$thal)))

# Variables quantitatives (Boite à moustache)
boiteMoustacheCroisee=function (nom_variable,variable,lim_y){
  boxplot(variable~data$target,
          col="Orange",
          xlab = "Présence d'une maladie cardiovasculaire",
          ylab = nom_variable,
          main=paste("Boite à moustache des patients selon",nom_variable,"et la présence d'une maladie cardio-vasculaire"),
          cex.main=1.1,
          cex.lab=1.1,
          las=1,
          ylim=lim_y)
  }

boiteMoustacheCroisee("age",data$age,c(20,80))
boiteMoustacheCroisee("Tension arterielle au repos",data$trestbps,c(80,200))
boiteMoustacheCroisee("cholesterol",data$chol,c(80,500))

#===============================================================
# Tests statistiques : variables qualitatives
#===============================================================
# calcul des pourcentages 
round(prop.table(table(data$sex,data$target),margin = 1),4)*100 # profil ligne 
round(prop.table(table(data$cp,data$target),margin = 1),4)*100 
round(prop.table(table(data$fbs,data$target),margin = 1),4)*100 
round(prop.table(table(data$resecg,data$target),margin = 1),4)*100 
round(prop.table(table(data$exang,data$target),margin = 1),4)*100 
round(prop.table(table(data$slope,data$target),margin = 1),4)*100 
round(prop.table(table(data$ca,data$target),margin = 1),4)*100 
round(prop.table(table(data$thal,data$target),margin = 1),4)*100 
# Test du chi-2 
## H0 : Les deux variables sont indépendantes (p_value > 0.05)
## H1 : Les deux variables sont dépendantes  (p_value <= 0.05)
chisq.test(data$sex,data$target) # on rejette H0 et accepte H1
chisq.test(data$cp,data$target) # On rejette H0 et on accepte H1
chisq.test(data$fbs,data$target) # On rejette H1 et on accepte H0
chisq.test(data$resecg,data$target) # On rejette H0 et on accepte H1
chisq.test(data$exang,data$target) # On rejette H0 et on accepte H1
chisq.test(data$slope,data$target) # On rejette H0 et on accepte H1
chisq.test(data$ca,data$target) # On rejette H0 et on accepte H1
chisq.test(data$thal,data$target) # On rejette H0 et on accepte H1
help("read.csv")

#===============================================================
# Tests statistiques : variables quantitatives
#===============================================================
# On va chercher a savoir si la variable a un impact sur la maladie
# calcul des moyennes 
tapply(data$age,data$target,mean)
tapply(data$trestbps,data$target,mean)
tapply(data$chol,data$target,mean)
tapply(data$thalach,data$target,mean)
tapply(data$oldpeak,data$target,mean)

# Test de Shapiro-Wilk : (on va voir si la variable quantitative suit une distribution normale)
## H0 : L'echantillon suit une distribution normale (p_value >0.05)
## H1 :  L'echantillon  ne suit pas  une distribution normale (p_value <=0.05)

library(dplyr)
# install.packages("dplyr")
shapiro.test(filter(data,target=="Oui")$age) ## (p_value <=0.05 donc ne suit pas une distribution normale) ->H1
shapiro.test(filter(data,target=="Oui")$trestbps) # H1
shapiro.test(filter(data,target=="Oui")$chol) # H0
shapiro.test(filter(data,target=="Oui")$thalach) # H0
shapiro.test(filter(data,target=="Oui")$oldpeak) # H1

#-------------------------------------------------------------------------------
# Note : 
# H0 : On effectue le test de Student
# H1 : On effectue le test de Mann-Whitney
#-------------------------------------------------------------------------------

# Test de Mann-Whitney
#------------------------------------------------------------------------
# H0 : Pas de différence significative entre la moyenne des deux variables ((p_value > 0.05))
# H1 : Il ya une différence significative entre la moyenne des deux variables  (p_value <= 0.05)
#--------------------------------------------------------------------------
wilcox.test(data$age~data$target)
wilcox.test(data$trestbps~data$target)
wilcox.test(data$oldpeak~data$target) 
# Note : Ici les p_value <0.05 -> les differences entre les moyennes ne sont pas dues à un problème d'echantillonage

# Test de Student
#-------------------------------------------------------------------------
# H0 : Il n'ya pas de différence significative entre les moyennes des deux variables (p_value > 0.05)
# H1 : Il ya une diff significative entre les moyennes des deux variables (p_value <=0.05)
#--------------------------------------------------------------------------
t.test(data$chol~data$target) # H0
t.test(data$thalach~data$target) # H1
#View(data)

#=========================================================================
# Partie : Machine Learning
# Objectif : Nous allons chercher créer un modèle de regression logistique pour faire des prédictions 
#==========================================================================

# Division des données en un ensemble d'entrainement et de test 
set.seed(99)
# install.packages("caTools")
library(caTools)
split=sample.split(data$target,SplitRatio = 0.8)
train_set=subset(data,split==TRUE)
test_set=subset(data,split==FALSE)

# Model 
log_model=glm(target~.,data =train_set,family="binomial")
print(paste0("Summary Before Optimisation"))
summary(log_model)
## Optimisation du model (on conserve seulement les variables les plus significatives en supprimant celles qui ont un p_value >=0.05)
log_model=update(log_model,.~.-restecg)
log_model=update(log_model,.~.-slope)
log_model=update(log_model,.~.-thal)
log_model=update(log_model,.~.-age)
log_model=update(log_model,.~.-fbs)
log_model=update(log_model,.~.-chol)
log_model=update(log_model,.~.-thalach)
#log_model=update(log_model,.~.-cp) # On appliquera le critère AIC pour voir si on conserve ou on garde la variable 
log_model=update(log_model,.~.-trestbps)
print("----------------------------")
print(paste0("Summary after Optimisation"))

summary(log_model)
# Note : D'apres le critere AIC , on decide de garder la variable ca vu que en la gardant on a un AIC inferieur qu'en la supprimant

# Prédiction 
prediction=predict(log_model,test_set,type = "response")
prediction_df=as.data.frame(prediction)
prediction_df$classe_predite=ifelse(prediction_df$prediction > 0.5,1,0)

# Performances du model 
## Confusion matrix 
levels(test_set$target)=c(0,1)
#install.packages("caret")
library(caret)
confusionMatrix(as.factor(test_set$target),as.factor(prediction_df$classe_predite))

comp=as.data.frame(cbind(test_set,prediction_df$classe_predite))
str(comp)
comp$`prediction_df$classe_predite`=as.factor(comp$`prediction_df$classe_predite`)
# Test de Hosmer et Lemeshow
##------------------------------------------
## H0 : Ajustement du model aux données est bien (p_value >0.05)
## H1 :  Ajustement du model aux données est mauvais (p_value <=0.05)
##-------------------------------
##install.packages("performance")
library(performance)
performance_hosmer(log_model) 
## Note : Dans notre cas , le model s'ajuste bien aux données

# Courbe roc 
#install.packages("pROC")
library(pROC)
roc(train_set$target,log_model$fitted.values,plot=TRUE,
    main="Courbe ROC du modèle de régression logistique",
    lwd=2,
    xlab="FP",
    ylab="TP",
    legacy.axes=T)

