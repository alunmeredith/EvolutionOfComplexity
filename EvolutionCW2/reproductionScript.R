#source('reproductionHeader.r')

# Initialise variables
N <- 4000
groupSizeSmall <- 4L
groupSizeLarge <- 40L
#Rsmall = 4
#Rlarge = 40
#t <- 3
Gc <- 0.018
Gs <- 0.02
Cc <- 0.1
Cs <- 0.2
Tgen <- 50
R = 1
K = 0.1


#---------------------------------------
#-- STEP 1. INITIALISE N INDIVIDUALS ---
#---------------------------------------

# Because we are using R we describe individuals in a vectorised mannor. Each element of the vector is an allele, with a 3rd variable stating which group the group and 4th the size of the genotype. 

dispersedPop <- data.table(group = c("small", "large"), 
                           greedy = c(N/4, N/4),
                           coop = c(N/4, N/4))

# Initialise population table with maximum number of group sizes
maxGroups <- floor(N/groupSizeSmall)
population <- data.table(id = 1:maxGroups, group = NA_character_, greedy = NA_real_, coop = NA_real_)

genotypes <- data.table(greedySmall = rep(NA_real_, Tgen+1) ,greedyLarge = NA_real_,coopSmall = NA_real_, coopLarge = NA_real_)
genotypes[1, greedySmall := N/4]
genotypes[1, greedyLarge := N/4]
genotypes[1, coopSmall := N/4]
genotypes[1, coopLarge := N/4]



for (j in 1:Tgen){ # iterate for T generations
  groupFormation()
  for (i in 1:t) reproduction() # wait time t before mixing the pools and rebuilding groups
  dispersedPop <- buildMigrantPool()
  print(dispersedPop)
  
  # Save population values after each generation
  x <- dispersedPop[group == "small", greedy]
  if (length(x) == 0) x = 0
  genotypes[j+1, greedySmall := x]
  
  x <- dispersedPop[group == "large", greedy]
  if (length(x) == 0) x = 0
  genotypes[j+1, greedyLarge := x]
  
  x <- dispersedPop[group == "small", coop]
  if (length(x) == 0) x = 0
  genotypes[j+1, coopSmall := x]
  
  x <- dispersedPop[group == "large", coop]
  if (length(x) == 0) x = 0
  genotypes[j+1, coopLarge := x]
  
  # Print current state to console
  print(paste("Generation:", j))
  print(genotypes[j])
}


#---------------------------------------
#--------------- RESULTS ---------------
#---------------------------------------
name <- paste("Gens", t, "SizeBonus", Rlarge - Rsmall*10, sep="")

png(file = paste(name, ".png", sep=""), width = 840)

genotypes$index <- 0:Tgen
graph <- ggplot(genotypes, aes(x=index, y=value)) +
  geom_point(aes(y=greedySmall/N, col="Greedy/Small")) +
  geom_point(aes(y=greedyLarge/N, col="Greedy/Large")) + 
  geom_point(aes(y=coopSmall/N, col="Cooperative/Small")) +
  geom_point(aes(y=coopLarge/N,col="Cooperative/Large")) +
  geom_line(aes(y=greedySmall/N, col="Greedy/Small")) +
  geom_line(aes(y=greedyLarge/N, col="Greedy/Large")) + 
  geom_line(aes(y=coopSmall/N, col="Cooperative/Small")) +
  geom_line(aes(y=coopLarge/N,col="Cooperative/Large")) +
  xlim(0,50) +
  xlab("Generation") +
  ylab("Proportion of population") +
  ggtitle(paste("Generations before mixing", t, "- Large bonus", Rlarge - Rsmall*10))
print(graph)

dev.off()

save(genotypes, file = paste(name, ".Rda", sep=""))