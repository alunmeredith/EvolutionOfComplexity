# packages
if (!require(data.table)){
  install.packages("data.table")
  library(data.table)
}
if (!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
}


#------- STEP 2. GROUP FORMATION -------
groupFormation <- function () {
  
  # Total individuals with each group allele. 
  dispersedPop[, Sum := rowSums(.SD), .SDcols = 2:3]
  smallGroups <- floor(dispersedPop[group == "small", Sum] / groupSizeSmall)
  largeGroups <- floor(dispersedPop[group == "large", Sum] / groupSizeLarge)
  if (length(largeGroups) == 0) largeGroups = 0L
  if (length(smallGroups) == 0) smallGroups = 0L
  
  
  # Add each small individual to a pool and create a sample queue
  poolSmall <- c(rep(1L, dispersedPop[group == "small", greedy]),
                 rep(0L, dispersedPop[group == "small", coop]))
  poolSmall <- sample(poolSmall, dispersedPop[group == "small", Sum], replace = F)
  
  # Populate groups from the sample
  for (i in 0:(smallGroups-1)){
    numGreedy <- (sum(poolSmall[(i*groupSizeSmall):((i+1)*groupSizeSmall-1)]))
    population[i+1, 2:4 := .("small", numGreedy, groupSizeSmall - numGreedy)]
  }
  
  if (length(largeGroups) == 0) largeGroups = 0L
  if (largeGroups > 0) {
    # Repeat for large groups
    poolLarge <- c(rep(1L, dispersedPop[group == "large", greedy]),
                   rep(0L, dispersedPop[group == "large", coop]))
    poolLarge <- sample(poolLarge, dispersedPop[group == "large", Sum], replace = F)
    
    # Populate groups from the sample
    for (i in 0:(largeGroups-1)){
    numGreedy <- (sum(poolLarge[(i*groupSizeLarge):((i+1)*groupSizeLarge-1)]))
    population[i+smallGroups+1, 2:4 := .("large", numGreedy, groupSizeLarge - numGreedy)]
    }
  }
}


#-------- STEP 3 Repdroduction ---------


reproduction <- function() {

invisible(sapply(1:maxGroups, function(x) {
  
  greedy <- population[x, greedy]
  cooperative <- population[x, coop]
  
  # R is based on group size
  ifelse(population[x, (group == "large") & !is.na(group)],
         R <- Rlarge,
         R <- Rsmall
         ) 
    #R <- R * 1.05^groupSizeLarge * groupSizeLarge
    #R <- R * 1.05^groupSizeSmall * groupSizeSmall
  
  # Calculate share of the resource. 
  s <- greedy * Gs * Cs
  c <- cooperative * Gc * Cc
  rs <- R * s/(s+c) 
  rc <- R * c/(s+c) 
  
  # Calculate new population size
  newGreedy <- greedy + rs/Cs - K*greedy
  newCooperative <- cooperative + rc/Cc - K*cooperative
  
  # Set new population sizes
  population[x, "greedy" := newGreedy]
  population[x, "coop" := newCooperative]
  }))
}

buildMigrantPool <- function() {
  #---- STEP 4 MIGRANT POOL FORMATION ----
  
  dispersedPop <- (population[, .(greedy = sum(greedy, na.rm=T), coop = sum(coop, na.rm=T)), by = .(group)])
  
  # STEP 5 MAINTAINING GLOBAL CARRYING CAPACITY 
  dispersedPop[, Sum := rowSums(.SD), .SDcols = 2:3]
  scalingFactor <- sum(dispersedPop$Sum) / N
  dispersedPop[, greedy := round(greedy / scalingFactor)]
  dispersedPop[, coop := round(coop / scalingFactor)]
  dispersedPop[, Sum := rowSums(.SD), .SDcols = 2:3]
  
  return (dispersedPop)
  }

