rm(list = ls(all= T))

library(ggplot2)

#setwd("C:/Users/Emmanuelle CAM/SUIVI/STAGES/Coline-stage/Coline-test")
setwd("C:/Users/colin/OneDrive/Bureau/2021_2022/Stage/asex")

#options(htmltools.dir.version = FALSE)
#knitr::opts_chunk$set(comment = "")

####################

#multinomial Generalized linear model
#https://datasciencebeginners.com/2018/12/20/multinomial-logistic-regression-using-r/
# http://www.sthda.com/english/articles/36-classification-methods-essentials/147-multinomial-logistic-regression-essentials-in-r/
#https://bookdown.org/chua/ber642_advanced_regression/multinomial-logistic-regression.html
##############################################

#sexe inconnu = 1
#m�le probable = 2
#femelle probable = 3
#m�le =4
#femelle =5 # (?)

#statuts
# N pr�reproducteur
# Q premi�re reproduction �ge inconnu
# P premi�re reproduction �ge connu
# R = reproducteur exp�riment�
#S sabbatique
# I inconnu

# age = 99 si inconnu

#######################################

setwd("C:/Users/colin/OneDrive/Bureau/2021_2022/Stage/asex")

## Data ----
### read the data ----
colinec <- read.table("colinec.dat")
head(colinec)
dim(colinec)
class(colinec)




# functions 

no <- function(vector, x){ # retourne le nombre d'occurence de x dans le vecteur vector
  compteur <- 0
  for (i in (1: length(vector))){
    
    if (vector[i] == x){
      compteur <- compteur + 1
    }
  }
  return(compteur)
}

#--------------------------------------

noNA <- function(vector, x){# no augment�e (prends en compte les NA)
  compteur <- 0
  if (length(vector) == 0){ compteur <- 0}
  else
  {
    for (i in (1: length(vector))){
      
      if (vector[i] == x){
        compteur <- compteur + 1
      }
    }
  }
  return(compteur)
}







#P

colinecP <- colinec[colinec$V4 == "P", ]# s�lectionne les recrutants
colinecP <- colinecP[colinecP$V2 != 99, ]#dont on connait l'�ge

max(colinecP$V2)#�ge max de recrutement



## ggplot P ----

colinecP$V3= as.factor(colinecP$V3)#sexe en temps que facteur

colinecPs<- colinecP
colinecPs$V3[colinecPs$V3==2]<- 4 #ne prends pas en compte les suppos�es,
colinecPs$V3[colinecPs$V3==3]<- 5 #uniquement les av�r�s (i.e. transforme les suppos�s en av�r�s)
colinecPs<- colinecPs[colinecPs$V3!=1,]#exclu les sexes inconnus
colinecPs$V3<- as.factor(colinecPs$V3)#sexe en temps que facteur

m4 <- mean (colinecPs$V2[colinecPs$V3==4])#moyenne d'�ge des m�les recrutant
m5 <- mean (colinecPs$V2[colinecPs$V3==5])#moyenne d'�ge des femelles recrutant

(ggplot(data = colinecPs, mapping = aes( x = V2 , color = V3, group = V3))
  + geom_bar()
  + facet_grid(~colinecPs$V3)
  + geom_vline(mapping = aes (xintercept = m4, color = "m4"))
  + geom_vline(mapping = aes (xintercept = m5, color = "m5"))
)#fr�quence des differentes classes d'�ges , m�les et femelles s�par�s


sxr<- colinecPs


nM<- numeric(length = max(sxr$V2))# pr�paration du vecteur
nF<- numeric(length = max(sxr$V2))#
sxratio<- matrix( nrow= (max(sxr$V2)- min(sxr$V2)), ncol = 3)# pr�paration de la matrice
sxratio<- as.data.frame(sxratio)

for ( i in (min(sxr$V2):max(sxr$V2))){ # sur les valeurs de classe d'�ge concern�es
  
  vr<- sxr$V3[sxr$V2==i] # contient le sexe de tous les individus de l'�ge i
  
  nM[i]<- no(vr,4) #nombre d'occurence de m�le dans vr
  nF[i]<- no(vr,5) #m�me chose pour les femelles
  
  sxratio[i,1]<- nM[i]/nF[i]# calcul du sexe ratio
  sxratio[i,2]<- i
  sxratio[i,3]<- nM[i]+nF[i]
  
  
  
}

sxratio$V3<- (sxratio$V3/sum(sxratio$V3, na.rm = T))*10#bidouillage math�matique pour avoir l'abondance relative sur le m�m graphe
sxratio


(ggplot(data = sxratio, mapping = aes( x = V2, y = V1))
  + geom_point(color = "purple", size = 2, mapping = aes( y = V1))
  +geom_line(mapping = aes(y = V3), color = "dark green", size = 1)
  #  + scale_y_log10()
  +labs(y = "sexratio", x = "age")
  + geom_hline(yintercept = 1)
  
)# abondance relative et sexe ratio en fonction de la classe d'�ge 

(ggplot(data = sxratio, mapping = aes( x = V2, y = V1))
  + geom_point(color = "purple", size = 2, mapping = aes( y = V1))
  +labs(y = "sexratio", x = "age")
  + geom_hline(yintercept = 1)
  
)#sans l'abondance relative




(ggplot(data = colinecPs, mapping = aes( x = V2 , color = V3, group = V3))
  + geom_bar(position = "fill")
  + geom_hline(mapping = aes (yintercept = 0.5))
)#fr�quence des differentes classes d'�ges , m�les et femelles s�par�s



















# (R ou S) et s------------


#colinecRSs

colinecRSs <- colinec[colinec$V4 == "R"|colinec$V4=="S", ]#selectionne le segment reproducteur
colinecRSs <- colinecRSs[colinecRSs$V2 != 99, ]#dont on connait l'�ge

max(colinecRSs$V2)# �ge maximum


colinecRSs$V3[colinecRSs$V3==2]<- 4
colinecRSs$V3[colinecRSs$V3==3]<- 5
colinecRSs<- colinecRSs[colinecRSs$V3!=1,]
colinecRSs$V3 <- as.factor(colinecRSs$V3)


(ggplot(data = colinecRSs, mapping = aes( x = V2 , color = V3, group = V3))
  + geom_bar()
  + facet_grid(cols = vars(colinecRSs$V3), rows = vars(colinecRSs$V4))
)#fr�quence d'abondance en fonction de la classe d'�ge s�par�s par statut et sexe

(ggplot(data = colinecRSs, mapping = aes( x = V2 , color = V3, group = V3))
  + geom_bar(position = "fill")

)#fr�quence d'abondance en fonction de la classe d'�ge

#sex-ratio ---------------


sxr<- colinecRSs


nM<- numeric(length = max(sxr$V2))# pr�paration du vecteur
nF<- numeric(length = max(sxr$V2))#
sxratio<- matrix( nrow= (max(sxr$V2)- min(sxr$V2)), ncol = 3)# pr�paration de la matrice
sxratio<- as.data.frame(sxratio)

for ( i in (min(sxr$V2):max(sxr$V2))){ # sur les valeurs de classe d'�ge concern�es
  
  vr<- sxr$V3[sxr$V2==i] # contient le sexe de tous les individus de l'�ge i
  
  nM[i]<- no(vr,4) #nombre d'occurence de m�le dans vr
  nF[i]<- no(vr,5) #m�me chose pour les femelles
  
  sxratio[i,1]<- nM[i]/nF[i]# calcul du sexe ratio
  sxratio[i,2]<- i
  sxratio[i,3]<- nM[i]+nF[i]
  
  
  
}

sxratio$V3<- (sxratio$V3/sum(sxratio$V3, na.rm = T))*10#bidouillage math�matique pour avoir l'abondance relative sur le m�m graphe
sxratio


(ggplot(data = sxratio, mapping = aes( x = V2, y = V1))
  + geom_point(color = "purple", size = 2, mapping = aes( y = V1))
  +geom_line(mapping = aes(y = V3), color = "dark green", size = 1)
#  + scale_y_log10()
  +labs(y = "sexratio", x = "age")
  + geom_hline(yintercept = 1)
  
)# abondance relative et sexe ratio en fonction de la classe d'�ge 

(ggplot(data = sxratio, mapping = aes( x = V2, y = V1))
  + geom_point(color = "purple", size = 2, mapping = aes( y = V1))
  +labs(y = "sexratio", x = "age")
  + geom_hline(yintercept = 1)
  
)#sans l'abondance relative







# R et S et s ----


sxrRS<- (colinecRSs)



nMR<- numeric(length = max(sxrRS$V2))
nFR<- numeric(length = max(sxrRS$V2))
nMS<- numeric(length = max(sxrRS$V2))
nFS<- numeric(length = max(sxrRS$V2))
sxratioR<- matrix( nrow= (max(sxrRS$V2)- min(sxrRS$V2)), ncol = 4)
sxratioR<- as.data.frame(sxratio)
sxratioS<- matrix( nrow= (max(sxrRS$V2)- min(sxrRS$V2)), ncol = 4)
sxratioS<- as.data.frame(sxratio)

caRmin <- min(sxrRS$V2[sxrRS$V4 == "R"]) # classe d'age minimum contenant un individu de statut R
caSmin <- min(sxrRS$V2[sxrRS$V4 == "S"]) # 
caRmax <- max(sxrRS$V2[sxrRS$V4 == "R"])
caSmax <- max(sxrRS$V2[sxrRS$V4 == "S"])



for ( i in ((max(c(caRmin, caSmin)):(min(c(caRmax, caSmax)))))){#pour le plus petit grand intervalle (si si c'est logique)
  
  vrR<- sxrRS$V3[(sxrRS$V2 == i) &( sxrRS$V4== "R")] # contient le sexe de tous les individus de classe d'�ge i et de statut R
  vrS<- sxrRS$V3[(sxrRS$V2 == i) & (sxrRS$V4 == "S")] # m�me chose pour le statut S
  
  nMR[i]<- noNA(vrR,4)# nombre d'occurence de male R
  nFR[i]<- noNA(vrR,5)#nombre d'ocurrence de femelle R
  
  sxratioR[i,1]<- nMR[i]/nFR[i]#sexe ratio
  sxratioR[i,2]<- i
  sxratioR[i,3]<- nMR[i]+nFR[i]#abondance
  sxratioR[i,4]<- "R"
  
  
  #M�me chose pour le statut S
  
  nMS[i]<- noNA(vrS,4)
  nFS[i]<- noNA(vrS,5)
  
  sxratioS[i,1]<- nMS[i]/nFS[i]
  sxratioS[i,2]<- i
  sxratioS[i,3]<- nMS[i]+nFS[i]
  sxratioS[i,4]<- "S"
  
}

sxratioR$V3<- (sxratioR$V3/sum(sxratioR$V3, na.rm = T))*10#m�me bidouillage math�matique 
sxratioS$V3<- (sxratioS$V3/sum(sxratioS$V3, na.rm = T))*10# que plus haut 
sx <- rbind (sxratioS,sxratioR)#concat�nage de la matrice
sx

(ggplot(data = sx, mapping = aes( x = V2, y = V1, color = V4, group = V4))
  + geom_point( mapping = aes( y = V1))
  +geom_line(mapping = aes(y = V3))
  #  + scale_y_log10()
  +labs(y = "sexratio", x = "age")
  + geom_hline(yintercept = 1)
  
)#  abondance relative et sexe ratio en fonction de la classe d'�ge s�par� en fonction du statut






#---------------------------------------------------------------------------------------------------------------
# Avec les sexes inconnus 
#---------------------------------------------------------------------------------------------------------------




#P

colinecP <- colinec[colinec$V4 == "P", ]# s�lectionne les recrutants
colinecP <- colinecP[colinecP$V2 != 99, ]#dont on connait l'�ge

max(colinecP$V2)#�ge max de recrutement



## ggplot P ----

colinecP$V3= as.factor(colinecP$V3)#sexe en temps que facteur

colinecPs<- colinecP
colinecPs$V3[colinecPs$V3==2]<- 4 #ne prends pas en compte les suppos�es,
colinecPs$V3[colinecPs$V3==3]<- 5 #uniquement les av�r�s (i.e. transforme les suppos�s en av�r�s)

colinecPs$V3<- as.factor(colinecPs$V3)#sexe en temps que facteur

m4 <- mean (colinecPs$V2[colinecPs$V3==4])#moyenne d'�ge des m�les recrutant
m5 <- mean (colinecPs$V2[colinecPs$V3==5])#moyenne d'�ge des femelles recrutant

(ggplot(data = colinecPs, mapping = aes( x = V2 , color = V3, group = V3))
  + geom_bar()
  + facet_grid(~colinecPs$V3)
  + geom_vline(mapping = aes (xintercept = m4, color = "m4"))
  + geom_vline(mapping = aes (xintercept = m5, color = "m5"))
)#fr�quence des differentes classes d'�ges ,inconnus, m�les et femelles s�par�s


sxr<- colinecPs


nM<- numeric(length = max(sxr$V2))# pr�paration du vecteur
nF<- numeric(length = max(sxr$V2))#
nI<- numeric(length = max(sxr$V2))#
sxratio<- matrix( nrow= (max(sxr$V2)- min(sxr$V2)), ncol = 4)# pr�paration de la matrice
sxratio<- as.data.frame(sxratio)

for ( i in (min(sxr$V2):max(sxr$V2))){ # sur les valeurs de classe d'�ge concern�es
  
  vr<- sxr$V3[sxr$V2==i] # contient le sexe de tous les individus de l'�ge i
  
  nM[i]<- no(vr,4) #nombre d'occurence de m�le dans vr
  nF[i]<- no(vr,5) #m�me chose pour les femelles
  nI[i]<- no(vr,1)
  
  sxratio[i,1]<- nM[i]/nF[i]# calcul du sexe ratio
  sxratio[i,2]<- i
  sxratio[i,3]<- nM[i]+nF[i]+nI[i]
  sxratio[i,4]<- nI[i]
  
  
  
}

(ggplot(data = sxratio, mapping = aes( x = V2, y = V1))
  +geom_point(mapping = aes(y = V3), color = "dark green", size = 2)
  +geom_point(mapping = aes(y = V4), color = " green", size = 2)
  #  + scale_y_log10()
  +labs(y = "sexratio", x = "age")
  + geom_hline(yintercept = 1)
  
)# abondance relative et sexe ratio en fonction de la classe d'�ge 


sxratio$V3<- (sxratio$V3/sum(sxratio$V3, na.rm = T))*10#bidouillage math�matique pour avoir l'abondance relative sur le m�m graphe
sxratio$V4<- (sxratio$V4/sum(sxratio$V4, na.rm = T))*10
sxratio


(ggplot(data = sxratio, mapping = aes( x = V2, y = V1))
  + geom_point(color = "purple", size = 2, mapping = aes( y = V1))
  +geom_line(mapping = aes(y = V3), color = "dark green", size = 1)
  +geom_line(mapping = aes(y = V4), color = " green", size = 1)
  #  + scale_y_log10()
  +labs(y = "sexratio", x = "age")
  + geom_hline(yintercept = 1)
  
)# abondance relative et sexe ratio en fonction de la classe d'�ge 

(ggplot(data = sxratio, mapping = aes( x = V2, y = V1))
  + geom_point(color = "purple", size = 2, mapping = aes( y = V1))
  +labs(y = "sexratio", x = "age")
  + geom_hline(yintercept = 1)
  
)#sans l'abondance relative





















# (R ou S) et s------------


#colinecRSs

colinecRSs <- colinec[colinec$V4 == "R"|colinec$V4=="S", ]#selectionne le segment reproducteur
colinecRSs <- colinecRSs[colinecRSs$V2 != 99, ]#dont on connait l'�ge

max(colinecRSs$V2)# �ge maximum


colinecRSs$V3[colinecRSs$V3==2]<- 4
colinecRSs$V3[colinecRSs$V3==3]<- 5
colinecRSs<- colinecRSs[colinecRSs$V3!=1,]
colinecRSs$V3 <- as.factor(colinecRSs$V3)


(ggplot(data = colinecRSs, mapping = aes( x = V2 , color = V3, group = V3))
  + geom_bar()
  + facet_grid(cols = vars(colinecRSs$V3), rows = vars(colinecRSs$V4))
)#fr�quence d'abondance en fonction de la classe d'�ge s�par�s par statut et sexe

(ggplot(data = colinecRSs, mapping = aes( x = V2 , color = V3, group = V3))
  + geom_bar()
  
)#fr�quence d'abondance en fonction de la classe d'�ge

(ggplot(data = colinecRSs, mapping = aes( x = V2 , color = V3, group = V3))
  + geom_bar(position = "fill")
  + geom_hline(mapping = aes (yintercept = 0.5))
  
)#fr�quence d'abondance en fonction de la classe d'�ge

#sex-ratio ---------------


sxr<- colinecRSs


nM<- numeric(length = max(sxr$V2))# pr�paration du vecteur
nF<- numeric(length = max(sxr$V2))#
sxratio<- matrix( nrow= (max(sxr$V2)- min(sxr$V2)), ncol = 3)# pr�paration de la matrice
sxratio<- as.data.frame(sxratio)

for ( i in (min(sxr$V2):max(sxr$V2))){ # sur les valeurs de classe d'�ge concern�es
  
  vr<- sxr$V3[sxr$V2==i] # contient le sexe de tous les individus de l'�ge i
  
  nM[i]<- no(vr,4) #nombre d'occurence de m�le dans vr
  nF[i]<- no(vr,5) #m�me chose pour les femelles
  
  sxratio[i,1]<- nM[i]/nF[i]# calcul du sexe ratio
  sxratio[i,2]<- i
  sxratio[i,3]<- nM[i]+nF[i]
  
  
  
}

sxratio$V3<- (sxratio$V3/sum(sxratio$V3, na.rm = T))*10#bidouillage math�matique pour avoir l'abondance relative sur le m�m graphe
sxratio


(ggplot(data = sxratio, mapping = aes( x = V2, y = V1))
  + geom_point(color = "purple", size = 2, mapping = aes( y = V1))
  +geom_line(mapping = aes(y = V3), color = "dark green", size = 1)
  #  + scale_y_log10()
  +labs(y = "sexratio", x = "age")
  + geom_hline(yintercept = 1)
  
)# abondance relative et sexe ratio en fonction de la classe d'�ge 

(ggplot(data = sxratio, mapping = aes( x = V2, y = V1))
  + geom_point(color = "purple", size = 2, mapping = aes( y = V1))
  +labs(y = "sexratio", x = "age")
  + geom_hline(yintercept = 1)
  
)#sans l'abondance relative







# R et S et s ----


sxrRS<- (colinecRSs)



nMR<- numeric(length = max(sxrRS$V2))
nFR<- numeric(length = max(sxrRS$V2))
nMS<- numeric(length = max(sxrRS$V2))
nFS<- numeric(length = max(sxrRS$V2))
sxratioR<- matrix( nrow= (max(sxrRS$V2)- min(sxrRS$V2)), ncol = 4)
sxratioR<- as.data.frame(sxratio)
sxratioS<- matrix( nrow= (max(sxrRS$V2)- min(sxrRS$V2)), ncol = 4)
sxratioS<- as.data.frame(sxratio)

caRmin <- min(sxrRS$V2[sxrRS$V4 == "R"]) # classe d'age minimum contenant un individu de statut R
caSmin <- min(sxrRS$V2[sxrRS$V4 == "S"]) # 
caRmax <- max(sxrRS$V2[sxrRS$V4 == "R"])
caSmax <- max(sxrRS$V2[sxrRS$V4 == "S"])



for ( i in ((max(c(caRmin, caSmin)):(min(c(caRmax, caSmax)))))){#pour le plus petit grand intervalle (si si c'est logique)
  
  vrR<- sxrRS$V3[(sxrRS$V2 == i) &( sxrRS$V4== "R")] # contient le sexe de tous les individus de classe d'�ge i et de statut R
  vrS<- sxrRS$V3[(sxrRS$V2 == i) & (sxrRS$V4 == "S")] # m�me chose pour le statut S
  
  nMR[i]<- noNA(vrR,4)# nombre d'occurence de male R
  nFR[i]<- noNA(vrR,5)#nombre d'ocurrence de femelle R
  
  sxratioR[i,1]<- nMR[i]/nFR[i]#sexe ratio
  sxratioR[i,2]<- i
  sxratioR[i,3]<- nMR[i]+nFR[i]#abondance
  sxratioR[i,4]<- "R"
  
  
  #M�me chose pour le statut S
  
  nMS[i]<- noNA(vrS,4)
  nFS[i]<- noNA(vrS,5)
  
  sxratioS[i,1]<- nMS[i]/nFS[i]
  sxratioS[i,2]<- i
  sxratioS[i,3]<- nMS[i]+nFS[i]
  sxratioS[i,4]<- "S"
  
}

sxratioR$V3<- (sxratioR$V3/sum(sxratioR$V3, na.rm = T))*10#m�me bidouillage math�matique 
sxratioS$V3<- (sxratioS$V3/sum(sxratioS$V3, na.rm = T))*10# que plus haut 
sx <- rbind (sxratioS,sxratioR)#concat�nage de la matrice
sx

(ggplot(data = sx, mapping = aes( x = V2, y = V1, color = V4, group = V4))
  + geom_point( mapping = aes( y = V1))
  +geom_line(mapping = aes(y = V3))
  #  + scale_y_log10()
  +labs(y = "sexratio", x = "age")
  + geom_hline(yintercept = 1)
  
)#  abondance relative et sexe ratio en fonction de la classe d'�ge s�par� en fonction du statut





#-----------------------------------------------------------------------
# Sex ratio ----






#P

colinecP <- colinec[colinec$V4 == "P", ]# s�lectionne les recrutants
colinecP <- colinecP[colinecP$V2 != 99, ]#dont on connait l'�ge

max(colinecP$V2)#�ge max de recrutement



## ggplot P ----



sxr<- colinecPs


nM<- numeric(length = max(sxr$V2))# pr�paration du vecteur
nF<- numeric(length = max(sxr$V2))#
nI<- numeric(length = max(sxr$V2))#
sxratio<- matrix( nrow= (max(sxr$V2)- min(sxr$V2)), ncol = 4)# pr�paration de la matrice
sxratio<- as.data.frame(sxratio)

for ( i in (min(sxr$V2):max(sxr$V2))){ # sur les valeurs de classe d'�ge concern�es
  
  vr<- sxr$V3[sxr$V2==i] # contient le sexe de tous les individus de l'�ge i
  
  nM[i]<- no(vr,4) #nombre d'occurence de m�le dans vr
  nF[i]<- no(vr,5) #m�me chose pour les femelles
  nI[i]<- no(vr,1)
  
  sxratio[i,1]<- nM[i]/(nF[i]+nM[i]+nI[i])# calcul du sexe ratio
  sxratio[i,2]<- i
  sxratio[i,3]<- nF[i]/(nM[i]+nF[i]+nI[i])
  sxratio[i,4]<- nI[i]/(nM[i]+nF[i]+nI[i])
  
  
  
}

(ggplot(data = sxratio, mapping = aes( x = V2, y = V1))
  +geom_point(mapping = aes(y = V1), color = "dark blue", size = 3)
  +geom_point(mapping = aes(y = V3), color = " light blue", size = 3)
  +geom_point(mapping = aes(y = V4), color = " orange", size = 2)
  #  + scale_y_log10()
  +labs(y = "proportion du sexe", x = "age", title = "Proportion de chaque sexe en fonction de la classe d'�ge chez les pr�-reproducteurs")
  + geom_hline(yintercept = 0.5)

  
)# proportion de chaque sexe en fonction de la classe d'�ge 

head(colinecPs)

prop<- colinecPs
prop$V3<- as.numeric(prop$V3)
prop$V3[prop$V3 == 1] <- 6 
prop$V3<- as.factor(prop$V3)


(ggplot(data = prop, mapping = aes( x = V2, fill = V3, groupe = V3))
  
  +geom_bar(position = "fill")
  +geom_hline(yintercept = 0.5, color = "white")
  +scale_fill_manual(name ="Sex", labels = c( "Male", "Female","Unknown"), values =c( "dark blue", "light blue","orange"))
  + labs (y = "proportion du sexe", x = "age",title = "PB sex proportion by age class")
)

 



## ggplot R&S ----



sxr<- colinecRSs


nM<- numeric(length = max(sxr$V2))# pr�paration du vecteur
nF<- numeric(length = max(sxr$V2))#
nI<- numeric(length = max(sxr$V2))#
sxratio<- matrix( nrow= (max(sxr$V2)- min(sxr$V2)), ncol = 4)# pr�paration de la matrice
sxratio<- as.data.frame(sxratio)

for ( i in (min(sxr$V2):max(sxr$V2))){ # sur les valeurs de classe d'�ge concern�es
  
  vr<- sxr$V3[sxr$V2==i] # contient le sexe de tous les individus de l'�ge i
  
  nM[i]<- no(vr,4) #nombre d'occurence de m�le dans vr
  nF[i]<- no(vr,5) #m�me chose pour les femelles
  nI[i]<- no(vr,1)
  
  sxratio[i,1]<- nM[i]/(nF[i]+nM[i]+nI[i])# calcul du sexe ratio
  sxratio[i,2]<- i
  sxratio[i,3]<- nF[i]/(nM[i]+nF[i]+nI[i])
  sxratio[i,4]<- nI[i]/(nM[i]+nF[i]+nI[i])
  
  
  
}

(ggplot(data = sxratio, mapping = aes( x = V2, y = V1))
  +geom_point(mapping = aes(y = V1), color = "dark blue", size = 3)
  +geom_point(mapping = aes(y = V3), color = " light blue", size = 3)
  +geom_point(mapping = aes(y = V4), color = " orange", size = 2)
  #  + scale_y_log10()
  +labs(y = "proportion du sexe", x = "age", title = "Proportion de chaque sexe en fonction de la classe d'�ge chez les reproducteurs et sabbatiques ")
  + geom_hline(yintercept = 0.5)
  
  
)# proportion de chaque sexe en fonction de la classe d'�ge 


head(colinecRSs)

prop<- colinecRSs

(ggplot(data = prop, mapping = aes( x = V2, fill = V3, groupe = V3))
  
  +geom_bar(position = "fill")
  +geom_hline(yintercept = 0.5, color = "white")
  +scale_fill_manual(name ="Sex", labels = c( "Male", "Female", "Inconnu"), values =c( "dark blue", "light blue", "orange"))
  + labs (y = "proportion du sexe", x = "age",title = "Adult sex proportion by age class")
)








## ggplot P&R&S ----

colinecPRSs <- colinec[colinec$V4 == "R"|colinec$V4=="S"|colinec$V4=="P", ]#selectionne le segment reproducteur
colinecPRSs <- colinecPRSs[colinecPRSs$V2 != 99, ]#dont on connait l'�ge


colinecPRSs$V3[colinecPRSs$V3==2]<- 4
colinecPRSs$V3[colinecPRSs$V3==3]<- 5
colinecPRSs$V3 <- as.factor(colinecPRSs$V3)


sxr<- colinecPRSs


nM<- numeric(length = max(sxr$V2))# pr�paration du vecteur
nF<- numeric(length = max(sxr$V2))#
nI<- numeric(length = max(sxr$V2))#
sxratio<- matrix( nrow= (max(sxr$V2)- min(sxr$V2)), ncol = 4)# pr�paration de la matrice
sxratio<- as.data.frame(sxratio)

for ( i in (min(sxr$V2):max(sxr$V2))){ # sur les valeurs de classe d'�ge concern�es
  
  vr<- sxr$V3[sxr$V2==i] # contient le sexe de tous les individus de l'�ge i
  
  nM[i]<- no(vr,4) #nombre d'occurence de m�le dans vr
  nF[i]<- no(vr,5) #m�me chose pour les femelles
  nI[i]<- no(vr,1)
  
  sxratio[i,1]<- nM[i]/(nF[i]+nM[i]+nI[i])# calcul du sexe ratio
  sxratio[i,2]<- i
  sxratio[i,3]<- nF[i]/(nM[i]+nF[i]+nI[i])
  sxratio[i,4]<- nI[i]/(nM[i]+nF[i]+nI[i])
  
  
  
}

(ggplot(data = sxratio, mapping = aes( x = V2, y = V1))
  +geom_point(mapping = aes(y = V1), color = "dark blue", size = 3)
  +geom_point(mapping = aes(y = V3), color = " light blue", size = 3)
  +geom_point(mapping = aes(y = V4), color = " orange", size = 2)
  #  + scale_y_log10()
  +labs(y = "proportion du sexe", x = "age", title = "Proportion de chaque sexe en fonction de la classe d'�ge chez tous les individus")
  + geom_hline(yintercept = 0.5)
  
  
)# proportion de chaque sexe en fonction de la classe d'�ge 


head(colinecPRSs)

prop<- colinecPRSs
prop$V3<- as.numeric(prop$V3)
prop$V3[prop$V3 == 1] <- 6 
prop$V3<- as.factor(prop$V3)


(ggplot(data = prop, mapping = aes( x = V2, fill = V3, groupe = V3))
  
  +geom_bar(position = "fill")
  +geom_hline(yintercept = 0.5, color = "white")
  +scale_fill_manual(name ="Sex", labels = c( "Male", "Female","Unknown"), values =c( "dark blue", "light blue","orange"))
  + labs ( y = "proportion du sexe", x = "age",title = "Sex proportion by age class _ All individuals")
  )


#######################################################################


















#----

dat<- colinecRSs
class(dat)
#----


an<-dat$V1
anf<-as.factor(an)
str(anf)
age<-dat$V2
str(age)
sexe<-dat$V3
str(sexe)
statut<-dat$V4
str(statut)






#GLM ----

require(nnet)
multinom.fit <- multinom(sexe ~ anf, data = dat)

# Checking the model
summary(multinom.fit)

#The output coefficients are represented in the log of odds.
#get odds
exp(coef(multinom.fit))

#get estimated probability of sx as a function of year
probability.table <- fitted(multinom.fit)

resultat<-cbind(probability.table,anf)
head(resultat)


