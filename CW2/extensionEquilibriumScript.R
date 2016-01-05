source('extensionHeader.r')

# Initialise variables
N <- 500
groupSizeSmall <- 4L
groupSizeLarge <- 40L
Rsmall = 4
Rlarge = 40
t <- 1
G <- seq(0.018, 0.02, length.out = levels)
C <- seq(0.1, 0.2, length.out = levels)
Tgen <- 100
R = 1
K = 0.1
Time <- matrix(nrow = 3, ncol = 8)
levels <- 11

results <- data.frame()
resultsList <- list()
for (i in 1:5) {
  for (groupSizeSmall in 1:8) {
    
    # Iniitalise empty dispered population table
    initial <- N/(levels*2)
    dispersedPop <- data.table(group = c("small", "large"))
    invisible(sapply(1:levels, function(x) {
      dispersedPop[, paste0("level",x) :=  c(initial, 0)]
    }))
    
    # Initialise population table with maximum number of group sizes
    maxGroups <- floor(N/groupSizeSmall)
    population <- data.table(id = 1:maxGroups, group = NA_character_)
    invisible(sapply(1:levels, function(x) {
      population[, paste0("level", x) := NA_real_]
    }))
    
    # Initialise empty genotypes table
    genotypes <- data.table(level1small = c(initial, rep(NA_real_, Tgen)))
    invisible(sapply(1:levels, function(x) {
      genotypes[, paste0("level", x, "small") := c(initial, rep(NA_real_, Tgen))]
    }))
    invisible(sapply(1:levels, function(x) {
      genotypes[, paste0("level", x, "large") := c(initial, rep(NA_real_, Tgen))]
    }))
    
    # Grow without mixing for T generations
    groupFormation()
    curGenotype <- data.frame()
    for (j in 1:Tgen)
    { 
      reproduction()
      temp <- buildMigrantPool()
      curGenotype <<- temp[1,]
      print(curGenotype)
      print(paste("Generation:", j, "complete"))    
      results <<- rbind(results, curGenotype)
    }
    # Save genotypes data frame
    resultsList[[groupSizeSmall]] <- results
    print(paste("GroupSize:", groupSizeSmall, "complete"))
  }
  print(paste("Repitition:", i, "complete"))
}

