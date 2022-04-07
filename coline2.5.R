
#' title: "estimation survie-recrutement, années 2000-2020, cohortes 2000-2019"

#test 16 janvier 2022: 10 dernières années
#5 classes d'âge seulement
#les classes 5 à 8 sont considérées identiques pour la survie et le recrutement
#les détectabilités des 1 an et 2 ans sont fixées égales
# âge sur survie, transition, et detectabilité des PB prereproducteurs

#Intro ----

#setwd("C:/Users/colin/OneDrive/Bureau/2021_2022/Stage/analyses")
setwd("D:/Coline-stage-2022")
#setwd("C:/Users/Emmanuelle CAM/SUIVI/STAGES/Coline-stage/Coline-test")
library(nimble)
library (MCMCvis)
options(htmltools.dir.version = FALSE)
knitr::opts_chunk$set(comment = "")

library(here)

## Data ----
### read the data ----
tridac <- read.table("sursxb02.dat")
head(tridac)
dim(tridac)
class(tridac)

#colnames(tridac)<- c("Anb", "Anr", "2000", "2001", "2002",  "2003", "2004", "2005", "2006", "2007","2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019" )
#attach(tridac)

#tridac2<-tridac[,][tridac[,1]>2009]


### format the data ----
hcr<-as.matrix(tridac)
head(hcr)
nr<-nrow(hcr)
nc<-ncol(hcr)
class(hcr)

#test<-hcr[,1]
#hcr2<-hcr[test>2009,]
#dim(hcr2)

# 13 janvier, sélection des 10 dernières années
test<-hcr[,1]
hcr2<-hcr[test>2009,]
dim(hcr2)

y <- hcr2[,13:22]
head(y)

#13 janvier, le test se fera sur 10 ans seulement
#19 janvier 20 ans
#y2<- y+1
#y <- hcr[,3:22]
#head(y)

y2<- y+1

head(y)
head(y2) #données


#calcul de FIRST sur le jeu de données réduit 
# 13 janvier 2022

First <- apply(y2, 1, function(x) min(which(x !=1)))
length(First)
First
#First[8514]
#y[8514,]


#Years <- 20
Years<-ncol(y2)
Years

N = nrow(y2)
N
###Age----
#13/01 Ajout de la matrice Age mais pour le moment bloqué à Age =8
y2[1,]
y2[1,6]

#ceci marche avec 8 classes d'âge
Age<- array(data=0, dim=c(nrow(y2),ncol(y2),8))
Age
Age[1,,1]
Age[1,1,1]

i<-1

#ceci marche en comptant 8 classes d'âge
for (i in (1:N))
{
 for (j in (First[i] : Years))
 {
   colo<-j-First[i]+1
   if(colo<=8){
     Age[i,j,colo]<-1
   } 
   else{
     Age[i,j,8]<-1
   }
  } 
 
}
#Age[1,6,]
#Age[1,7,]
#Age[1,8,]
#Age[1,9,]
#Age[1,10,]

#Age[1,6,1]
#Age[1,6,2]
#Age[1,6,3]
#Age[1,6,4]
#Age[1,6,5]
#Age[1,6,6]
#Age[1,6,7]
#Age[1,6,8]

#Age[1,7,1]
#Age[1,7,2]
#Age[1,7,3]
#Age[1,7,4]
#Age[1,7,5]
#Age[1,7,6]
#Age[1,7,7]
#Age[1,7,8]

#Age[1,1,1]
#Age[1,1,2]
#Age[1,1,3]
#Age[1,1,4]
#Age[1,1,5]
#Age[1,1,6]
#Age[1,1,7]
#Age[1,1,8]

#Age[,,1]
#Age[,1,]

Age
class(Age)

#modifie 12 janvier 2022: data=y2 car data est une reponse dans dcat(po....); il faut le nom de la chose que l'on nomme "data[i,j]" dans le code du modele
my.data <- list(data=y2,Age=Age)

#Model ----

model <- nimbleCode(
  {
    
    ##STATES-------
    # 1 alive and prebreeder
    # 2 alive breeder
    # 3 alive nonbreeder
    # 4 dead 
    
     ##boucle sur les individus ----
    
    ### etat initial px0 ----
    
#    px0<- vector(length = 4)
    
    px0[1] <- 1 # prebreeder   #au début, tout le monde est PB
    px0[2] <- 0 # breeder
    px0[3] <- 0 # nonbreeder # /!\ correction o -> 0
    px0[4] <- 0 # dead
    
    for (i in 1:N)
    {
      
      ### etat initial px0 ----
    alive[i,First[i]] ~ dcat(px0[1:4]) # d'où vient "First" ?(vector ?)Anb/ an naissance à coder dans le script.
    
    ##boucle sur le temps pour s (survival probability) et psi (transition probability) ----
    
      for (j in (First[i]:Years-1) )
      {

        ### Matrix px ----
        # define probabilities of Z(t) given Z(t-1)
        # etat reel px (etat veritable de l'individu)
        
        px[1,i,j,1] <- phiPB[i,j] * (1-psiPBB[i,j])
        px[1,i,j,2] <- phiPB[i,j] * psiPBB[i,j]
        px[1,i,j,3] <- 0
        px[1,i,j,4] <- 1-phiPB[i,j]
        px[2,i,j,1] <- 0
        px[2,i,j,2] <- phiB * (1-psiBNB)
        px[2,i,j,3] <- phiB * psiBNB
        px[2,i,j,4] <- 1-phiB
        px[3,i,j,1] <- 0
        px[3,i,j,2] <- phiNB * psiNBB
        px[3,i,j,3] <- phiNB * (1-psiNBB)
        px[3,i,j,4] <-  1-phiNB
        px[4,i,j,1] <- 0
        px[4,i,j,2] <- 0
        px[4,i,j,3] <- 0
        px[4,i,j,4] <- 1
     
        logit(psiPBB[i,j])<-ag_psiPBB2*Age[i,j,2]+ag_psiPBB2*Age[i,j,3]+ag_psiPBB3*Age[i,j,4]+ag_psiPBB4*Age[i,j,5]+ag_psiPBB5*Age[i,j,6]+ag_psiPBB5*Age[i,j,7]+ag_psiPBB5*Age[i,j, 8]
        logit(phiPB[i,j])<-ag_phiPB1*Age[i,j,1]+ag_phiPB2*Age[i,j,2]+ag_phiPB3*Age[i,j,3]+ag_phiPB4*Age[i,j,4]+ag_phiPB5*Age[i,j,5]+ag_phiPB5*Age[i,j,6]+ag_phiPB5*Age[i,j,7]+ag_phiPB5*Age[i,j, 8]
        
      }
      #------- STATES ----
      # 1 alive and prebreeder
      # 2 alive breeder
      # 3 alive nonbreeder
      # 4 dead 
      #------- OBSERVATIONS ---- 
      # 4 = seen nonbreeder
      # 3 = seen breeder
      # 2 = seen prebreeder
      # 1 = missed
      #----------------------

    for (j in (First[i]+1):Years)
      {
        #observations po et probabilite de detection ----
        
        # define probabilities of O(t) given Z(t)
        po[1,i,j,1] <- 1-pPB[i,j]       
        po[1,i,j,2] <- pPB[i,j]
        po[1,i,j,3] <- 0
        po[1,i,j,4] <- 0
        po[2,i,j,1] <- 1-pB
        po[2,i,j,2] <- 0
        po[2,i,j,3] <- pB
        po[2,i,j,4] <- 0
        po[3,i,j,1] <- 1-pNB
        po[3,i,j,2] <- 0
        po[3,i,j,3] <- 0
        po[3,i,j,4] <- pNB
        po[4,i,j,1] <- 1
        po[4,i,j,2] <- 0
        po[4,i,j,3] <- 0
        po[4,i,j,4] <- 0     
        
        pPB[i,j]<-ag_pPB1*Age[i,j,2]+ag_pPB3*Age[i,j,3]+ag_pPB4*Age[i,j,4]+ag_pPB5*Age[i,j,5]+ag_pPB5*Age[i,j,6]+ag_pPB5*Age[i,j,7]+ag_pPB5*Age[i,j, 8]

          ##boucle temps commençant a la seconde occasion ----
         
          alive[i,j] ~ dcat(px[alive[i,j-1],i,j-1,1:4])
          ##observations dans les données----
          data[i,j] ~ dcat(po[alive[i,j], i, j,1:4]) # cette ligne là qui doit poser pb T.T
            
        
          }#fin boucle temps p, état, observations
     
    }#fin boucle individu

     #priors ----

#    pPB ~ dunif(0,1)
    pNB ~ dunif(0, 1) # non-breeder detectability 
    pB ~ dunif(0, 1) # breeder detectability
#    phiPB ~ dunif(0,1)
    phiB ~ dunif(0,1)
    phiNB ~ dunif(0,1)
#    psiPBB[i,j] ~ dunif(0,1)
    psiBNB ~ dunif(0,1)
    psiNBB ~ dunif(0,1)

 #   ag_pPB1~dunif(-10,10)
    ag_pPB2~dunif(-10,10)
    ag_pPB3~dunif(-10,10)
    ag_pPB4~dunif(-10,10)
    ag_pPB5~dunif(-10,10)
   
    ag_psiPBB1~dunif(-10,10)
    ag_psiPBB2~dunif(-10,10)
    ag_psiPBB3~dunif(-10,10)
    ag_psiPBB4~dunif(-10,10)
    ag_psiPBB5~dunif(-10,10)

    ag_phiPB1~dunif(-10,10)
    ag_phiPB2~dunif(-10,10)
    ag_phiPB3~dunif(-10,10)
    ag_phiPB4~dunif(-10,10)
    ag_phiPB5~dunif(-10,10)
      
    #transformation reciproque pour obtenir les paramètres sur échelle naturelle
    dPB2 <- exp( ag_pPB2) / (1 +  exp(ag_pPB2))
    dPB3 <- exp( ag_pPB3) / (1 +  exp(ag_pPB3))
    dPB4 <- exp( ag_pPB4) / (1 +  exp(ag_pPB4))
    dPB5 <- exp( ag_pPB5) / (1 +  exp(ag_pPB5))

    transPBB1 <- exp( ag_psiPBB1) / (1 +  exp(ag_psiPBB1))
    transPBB2 <- exp( ag_psiPBB2) / (1 +  exp(ag_psiPBB2))
    transPBB3 <- exp( ag_psiPBB3) / (1 +  exp(ag_psiPBB3))
    transPBB4 <- exp( ag_psiPBB4) / (1 +  exp(ag_psiPBB4))
    transPBB5 <- exp( ag_psiPBB5) / (1 +  exp(ag_psiPBB5))

    surPB1 <- exp( ag_phiPB1) / (1 +  exp(ag_phiPB1))
    surPB2 <- exp( ag_phiPB2) / (1 +  exp(ag_phiPB2))
    surPB3 <- exp( ag_phiPB3) / (1 +  exp(ag_phiPB3))
    surPB4 <- exp( ag_phiPB4) / (1 +  exp(ag_phiPB4))
    surPB5 <- exp( ag_phiPB5) / (1 +  exp(ag_phiPB5))

    #fin boucle modèle
  })

## constants ----

my.constants <- list(N = nrow(y2), 
                     Years = ncol(y2), 
                     First = First)
## Zinits ----
#------- STATES ----
# 1 alive and prebreeder
# 2 alive breeder
# 3 alive nonbreeder
# 4 dead 
#------- OBSERVATIONS ---- 
# 4 = seen nonbreeder
# 3 = seen breeder
# 2 = seen prebreeder
# 1 = missed
#----------------------
y3 <- ifelse(y2 == 1, NA, y2)
y3 <- ifelse(y2 == 2, 1, y3)
y3 <- ifelse(y2 == 3, 2, y3)
y3 <- ifelse(y2 == 4, 3, y3)

for (i in (1:N))
{
  for (k in (First[i]: Years))
  {
    if(is.na(y3[i, k])) {
      y3[i, k] <- y3[i, k-1]
    }
  }
}

#tenter une initialisation un peu moins homogène (17 janvier 2022)

initial.values <- function(){list( ag_pPB2=0.2,ag_pPB3=0.2,ag_pPB4=0.2,ag_pPB5=0.2,
                                 pB = runif(1, 0, 1), 
                                 pNB=runif(1,0,1),
                                 ag_phiPB1=0.2, ag_phiPB2=0.2, ag_phiPB3=0.2, ag_phiPB4=0.2, ag_phiPB5=0.2,
                                 phiNB =runif(1, 0, 1), 
                                 phiB = runif(1, 0, 1), 
                                 ag_psiPBB1=0.2, ag_psiPBB2=0.2, ag_psiPBB3=0.2, ag_psiPBB4=0.2, ag_psiPBB5=0.2, 
                                 psiBNB =runif(1, 0, 1), 
                                 psiNBB = runif(1, 0, 1), 
                                 alive = y3
                                 )
  }

#pB
initial.values()

# Parameters ----

parameters.to.save <- c( "dPB2",
                         "dPB3",
                         "dPB4",
                         "dPB5",
                          "pB",
                         "pNB",
                         "surPB1",
                         "surPB2",
                         "surPB3",
                         "surPB4",
                         "surPB5",
                         "phiB",
                         "phiNB",
                         "transPBB1",
                         "transPBB2",
                         "transPBB3",
                         "transPBB4",
                         "transPBB5",
                          "psiBNB",
                         "psiNBB"
                        )

### 12 janvier 2022 18h45: A modifier pour relancer

#MCMC details ----
#n.iter<- 10000
#n.burnin <- 5000
#n.chains <- 1

#n.iter<- 500
#n.burnin <- 100
#n.chains <- 1

n.iter<- 40000
n.burnin <- 10000
n.chains <- 2

# RUN, NIMBLE, RUN ! ----
start<-as.POSIXlt(Sys.time())

mcmc.test <- nimbleMCMC(code = model, 
                        constants = my.constants,
                        data = my.data,              
                        inits = initial.values,
                        monitors = parameters.to.save,
                        niter = n.iter,
                        nburnin = n.burnin, 
                        nchains = n.chains
)

end <-as.POSIXlt(Sys.time())
duration = end-start
duration
     
#save(mcmc.test,duration,file='test.Rdata')                                          
#save(mcmc.test,file='test.Rdata')
#load("test.Rdata")

MCMCtrace(mcmc.test, pdf=FALSE)
MCMCplot(mcmc.test)
MCMCsummary(mcmc.test, round = 6)
########################################
######################################



#summary(mcmc.test)


Modeltest <- nimbleModel(code = model, 
                        constants = my.constants,
                        data = my.data,              
                        inits = list(pPB = matrix( runif(Years*N, 0, 1), N, Years), #/a!\ corection pB -> pPB
                                     pNB =runif(1, 0, 1), 
                                     pB = runif(1, 0, 1), 
                                     phiPB= runif(1, 0, 1), 
                                     phiNB =runif(1, 0, 1), 
                                     phiB = runif(1, 0, 1), 
                                     psiPBB = matrix(( runif(N*Years, 0, 1)), nrow = N, ncol = Years), 
                                     psiBNB =runif(1, 0, 1), 
                                     psiNBB = runif(1, 0, 1), 
                                     alive = y3),
                        )
#psiPBB
#inits$psiPBB

#--------------------------------------------------------------------------------------------------------------------------------------


Modeltest$check()


Modeltest$setData(data)


Modeltest$initializeInfo()

class(Modeltest$data)

Modeltest$data

Modeltest$alive

is.na(Modeltest$data)

Modeltest$plot()


help(modelInitialization)


pPB = runif(1, 0, 1)
 pNB =runif(1, 0, 1) 
 pB = runif(1, 0, 1) 
 phiPB= runif(1, 0, 1) 
 phiNB =runif(1, 0, 1) 
 phiB = runif(1, 0, 1) 
# psiPBB= runif(1, 0, 1) 
 psiBNB =runif(1, 0, 1) 
 psiNBB = runif(1, 0, 1) 

px<- matrix(nrow = 4, ncol = 4)
po<- matrix(nrow = 4, ncol = 4)

myModel<- compileNimble(model)




