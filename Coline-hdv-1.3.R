rm(list = ls(all= T))

library(ggplot2)

#setwd("C:/Users/Emmanuelle CAM/SUIVI/STAGES/Coline-stage/Coline-test")
setwd("C:/Users/colin/OneDrive/Bureau/2021_2022/Stage/asex")


##############################################

#sexe inconnu = 1
#mâle probable = 2
#femelle probable = 3
#mâle =4
#femelle =5 # (?)

#statuts
# N préreproducteur
# Q première reproduction âge inconnu
# P première reproduction âge connu
# R = reproducteur expérimenté
#S sabbatique
# I inconnu

# age = 99 si inconnu

#######################################

## Data ----
### read the data ----
colinec <- read.table("colined.dat")
head(colinec)
dim(colinec)
class(colinec)

colinec$V3<- as.factor(colinec$V3)



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

noNA <- function(vector, x){# no augmentée (prends en compte les NA)
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


colined<- colinec [colinec$V2 != 99,]

#first thing first, isoler 1 individu
# pour le bien de la demonstration, prenons l'individu 2
# Et appellons le Simba, car nous allons modéliser son Histoire de vie


Simba <- colined[colined$V1 == 2,]

Simba

(ggplot(data = Simba, mapping = aes (x = V2, y = V1, color = V3, groupe = V3))
  
  +geom_point()
)

#prenons maintenant un peu plus d'individus

Mouettes <- colined[colined$V4 <= 50,]

Mouettes

(ggplot(data = Mouettes, mapping = aes (x = V2, y = V1, color = V3, groupe = V3))
  
  +geom_point()
)



#

#essayons de separer mâles et femelles

N <- max(colinec$V1)
N

V5<- numeric(length = nrow(colined))

colinee<- cbind(colined, V5)

head(colinee)


for (i in 1:N)
{
  
  Ind <- colinee[colinee$V1 == i,]
  
  n5<- noNA(Ind$V3, 5)
  n4<- noNA(Ind$V3, 4)
  n3<- noNA(Ind$V3, 3)
  n2<- noNA(Ind$V3, 2)
  
  
  if (n5 >= 1|n3 >= 1)
  {
    colinee[colinee$V1 == i,5]<- "Fe"
  }
  if (n4 >= 1|n2 >= 1)
  {
    colinee[colinee$V1 == i,5] <- "M"
  }  
  
  
}

head(colinee)


Fem <- colinee[colinee$V5 == "Fe",]
Male <- colinee[colinee$V5 == "M",]

Fem

(ggplot(data = colinee, mapping = aes (x = V2, y = V1, color = V3, groupe = V3))
  
  +geom_point()
  +facet_grid(~V5)
)


# sex-age determination

colinef<- colinee[colinee$V5!=0,]#Unknown sex exclusion 
AS<- matrix(nrow = N, ncol = 3)
AS<- as.data.frame(AS)

IS<- unique(colinef$V1)# vector that only take into account once each sexed animal

for (i in 1:N)
{
  ind <- colinee[colinee$V1 == i,]#subset the data set, take only into account the individual's row 
  compteur<- min(which (ind$V3 != 1))
  AS[i,1]<- i# individual number
  AS[i,2]<- ind$V2[compteur]# its sex-age
  AS[i,3]<- ind$V5[1]#its sex
}

ASsNA<- AS[is.na(AS$V2)==FALSE,]# remove NAs
ASsNA


mean(ASsNA$V2[ASsNA$V3=="Fe"])

mean(ASsNA$V2[ASsNA$V3=="M"])

t.test(ASsNA$V2[ASsNA$V3=="Fe"],ASsNA$V2[ASsNA$V3=="M"])

(ggplot(data = ASsNA, mapping = aes(x = V2, group = V3, color = V3))
  
  + geom_bar()
  + facet_grid(~V3)
  
  )

(ggplot(data = ASsNA, mapping = aes(x = V2, group = V3, color = V3))
  
  + geom_bar()
  + facet_grid(row = vars(V3))
  
)



head(ASsNA)


CAS<- matrix(nrow = 2*max(ASsNA$V2), ncol = 3)
CAS<- as.data.frame(CAS)
CASFe<- matrix(nrow = max(ASsNA$V2), ncol = 3)
CASFe<- as.data.frame(CASFe)
CASM<- matrix(nrow = max(ASsNA$V2), ncol = 3)
CASM<- as.data.frame(CASM)


for (i in (1:max(ASsNA$V2)))
{
  CASFe[i,1]<- noNA (ASsNA$V2[ASsNA$V3=="Fe"], i)
  CASFe[i,2]<- i
  CASFe[i,3]<- "Fe"
  
  CASM[i,1]<- noNA (ASsNA$V2[ASsNA$V3=="M"], i)
  CASM[i,2]<- i
  CASM[i,3]<- "M"
}

CAS<- rbind(CASFe, CASM)

(ggplot(data = CAS , mapping = aes(x = V2,y = V1, group = V3, color = V3), size = 2)
  
  + geom_point()
  
)






