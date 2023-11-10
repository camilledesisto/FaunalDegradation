
###IMPUTATION DATA MANAGEMENT###
  
#load packages
library(dplyr);library(ggplot2);library(abind)

#read in data 
tdat_frug2_df <- read.csv(file=paste0(data_path,"tdat_frug2_imputed_75.csv"))#read in the interaction data, either expert or 1/75 scenario
tdat_total_df <- read.csv(file=paste0(data_path,"agb-trees-complete.csv")) # read in the tree data
plot_subset <- read.csv(file=paste0(data_path,"model-data.csv"))# read in the plot data
plot_agb <- read.csv(file=paste0(data_path,"plot_agb.csv"))
plot_subset2 <- plot_subset %>% select(DispoCode)

#list of the 260 unique plots
plot <- unique(plot_subset2$DispoCode)

#subset data to only include the relevant 195 plots
tdat_total <- tdat_total_df %>% filter(DispoCode %in% plot) 
tdat_frug2 <- tdat_frug2_df %>% filter(DispoCode %in% plot) 

tdat_frug2$Loxodonta_cyclotis <- rbinom(nrow(tdat_frug2), 1 , tdat_frug2$Loxodonta_cyclotis)
tdat_frug2$Cephalophus_callipygus <- rbinom(nrow(tdat_frug2), 1 , tdat_frug2$Cephalophus_callipygus)
tdat_frug2$Cephalophus_dorsalis <- rbinom(nrow(tdat_frug2), 1 , tdat_frug2$Cephalophus_dorsalis)
tdat_frug2$Cephalophus_silvicultor <- rbinom(nrow(tdat_frug2), 1 , tdat_frug2$Cephalophus_silvicultor)
tdat_frug2$Cercopithecus_cephus <- rbinom(nrow(tdat_frug2), 1 , tdat_frug2$Cercopithecus_cephus)
tdat_frug2$Cercopithecus_nictitans <- rbinom(nrow(tdat_frug2), 1 , tdat_frug2$Cercopithecus_nictitans)
tdat_frug2$Cercopithecus_pogonias <- rbinom(nrow(tdat_frug2), 1 , tdat_frug2$Cercopithecus_pogonias)
tdat_frug2$Colobus_guereza <- rbinom(nrow(tdat_frug2), 1 , tdat_frug2$Colobus_guereza)
tdat_frug2$Colobus_satanas <- rbinom(nrow(tdat_frug2), 1 , tdat_frug2$Colobus_satanas)
tdat_frug2$Gorilla_gorilla <- rbinom(nrow(tdat_frug2), 1 , tdat_frug2$Gorilla_gorilla)
tdat_frug2$Heliosciurus_rufobrachium <- rbinom(nrow(tdat_frug2), 1 , tdat_frug2$Heliosciurus_rufobrachium)
tdat_frug2$Lophocebus_albigena <- rbinom(nrow(tdat_frug2), 1 , tdat_frug2$Lophocebus_albigena)
tdat_frug2$Mandrillus_sphinx <- rbinom(nrow(tdat_frug2), 1 , tdat_frug2$Mandrillus_sphinx)
tdat_frug2$Pan_troglodytes <- rbinom(nrow(tdat_frug2), 1 , tdat_frug2$Pan_troglodytes)
tdat_frug2$Philantomba_monticola <- rbinom(nrow(tdat_frug2), 1 , tdat_frug2$Philantomba_monticola)

tdat_frug2<-  tdat_frug2 %>%
  mutate(NDispersed = select(., Cephalophus_callipygus:Philantomba_monticola) %>% rowSums(na.rm = TRUE))

#Organize data to account for dietary redundancy
tdat_frug2$DispersedSolo <- tdat_frug2$NDispersed
tdat_frug2$DispersedSolo[tdat_frug2$DispersedSolo!=1] <- 0

tdat_frug2$DispersedBinary <- tdat_frug2$NDispersed
tdat_frug2$DispersedBinary[tdat_frug2$DispersedBinary!=0] <- 1

#Organize data for the taxa-level simulations
tdat_frug2 <- tdat_frug2 %>%
  mutate(ele = select(., Loxodonta_cyclotis) %>% rowSums(na.rm = TRUE))
tdat_frug2 <- tdat_frug2 %>%
  mutate(ape = select(., Gorilla_gorilla, Pan_troglodytes ) %>% rowSums(na.rm = TRUE))
tdat_frug2 <- tdat_frug2 %>%
  mutate(ceph = select(.,  Cephalophus_callipygus:Cephalophus_silvicultor) %>% rowSums(na.rm = TRUE))
tdat_frug2 <- tdat_frug2 %>%
  mutate(smam = select(., Heliosciurus_rufobrachium) %>% rowSums(na.rm = TRUE))
tdat_frug2 <- tdat_frug2 %>%
  mutate(monkey = select(., Mandrillus_sphinx, Lophocebus_albigena,Cercopithecus_cephus:Colobus_satanas ) %>% rowSums(na.rm = TRUE))

tdat_frug2$ele[tdat_frug2$ele!=0] <- 1
tdat_frug2$ape[tdat_frug2$ape!=0] <- 1
tdat_frug2$ceph[tdat_frug2$ceph!=0] <- 1
tdat_frug2$smam[tdat_frug2$smam!=0] <- 1
tdat_frug2$monkey[tdat_frug2$monkey!=0] <- 1


tdat_frug2$NDispersed_Taxa <- tdat_frug2$ele +tdat_frug2$ape +tdat_frug2$ceph +tdat_frug2$smam +tdat_frug2$monkey

tdat_frug2$ele2 <- ifelse((tdat_frug2$NDispersed_Taxa==1 & tdat_frug2$ele==1) >0, 1, 0)
tdat_frug2$ape2 <- ifelse((tdat_frug2$NDispersed_Taxa==1 & tdat_frug2$ape==1) >0, 1, 0)
tdat_frug2$ceph2 <- ifelse((tdat_frug2$NDispersed_Taxa==1 & tdat_frug2$ceph==1) >0, 1, 0)
tdat_frug2$smam2 <- ifelse((tdat_frug2$NDispersed_Taxa==1 & tdat_frug2$smam==1) >0, 1, 0)
tdat_frug2$monkey2 <- ifelse((tdat_frug2$NDispersed_Taxa==1 & tdat_frug2$monkey==1) >0, 1, 0)

write.csv(tdat_frug2,file=paste0(data_path,"/tdat_frug2_imputed.csv"))

