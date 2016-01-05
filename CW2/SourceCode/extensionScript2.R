
# Initialise variables
N <- 4000
groupSizeSmall <- 4L
groupSizeLarge <- 40L
R = 1
Rsmall = 4
Rlarge = 50
t <- 4
levels = 11
G <- seq(0.018, 0.02, length.out = levels)
C <- seq(0.1, 0.2, length.out = levels)
Tgen <- 400
K = 0.1
source('extensionHeader.r')


#---------------------------------------
#-- STEP 1. INITIALISE N INDIVIDUALS ---
#---------------------------------------

# Because we are using R we describe individuals in a vectorised mannor.
# Each element of the vector is an allele, with a 3rd variable stating
# which group the group and 4th the size of the genotype. 

initial <- N/(levels*2)
dispersedPop <- data.table(group = c("small", "large"))
invisible(sapply(1:levels, function (x) {
  dispersedPop[, paste0("level",x) :=  rep(0, 2)]
}))
dispersedPop$level6 <- rep(N/2,2)

# Initialise population table with maximum number of group sizes
maxGroups <- floor(N/groupSizeSmall)
population <- data.table(id = 1:maxGroups, group = NA_character_)
invisible(sapply(1:levels, function(x) {
  population[, paste0("level", x) := NA_real_]
}))

genotypes <- data.table(level1small = c(initial, rep(NA_real_, Tgen)))
invisible(sapply(1:levels, function(x) {
  genotypes[, paste0("level", x, "small") := 
              c(initial, rep(NA_real_, Tgen))]
}))
invisible(sapply(1:levels, function(x) {
  genotypes[, paste0("level", x, "large") :=
              c(initial, rep(NA_real_, Tgen))]
}))

for (j in 1:Tgen) { # iterate for T generations
  groupFormation()
  for (i in 1:t) {
    reproduction() # wait time t before mixing 
    mutate(0.1) 
  }
  buildMigrantPool()
  print(dispersedPop)
  
  # Empty the groups 
  invisible(sapply(1:levels, function(x) {
    population[, paste0("level", x) := NA_real_]
  }))
  
  # Save population values after each generation
  x <- dispersedPop[1, setdiff(colnames(dispersedPop)
                               ,c("group","Sum")), with=FALSE]
  y <- dispersedPop[2, setdiff(colnames(dispersedPop),
                               c("group","Sum")), with=FALSE]
  row <- cbind(x,y)
  row[is.na(row)]<-0
  set(genotypes, as.integer(j + 1), 
      names(genotypes), as.list(round(row)))
  
  
  # Print current state to console
  print(paste("Generation:", j))
  print(genotypes[j])
  save(genotypes, file = "genotypes2.Rda")
}

