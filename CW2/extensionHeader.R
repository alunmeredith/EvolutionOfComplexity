# packages
if (!require(data.table)){
  install.packages("data.table")
  library(data.table)
}
if (!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
}

test1 <- 0
test2 <- 0
test3 <- 0
#------- STEP 2 GROUP FORMATION -------
groupFormation <- function() {
  
  # Total individuals with each group allele. 
  dispersedPop[, Sum := rowSums(floor(.SD)), .SDcols = 2:(levels+1)]
  smallGroups <- floor(dispersedPop[group == "small", Sum] 
                       / groupSizeSmall)
  largeGroups <- floor(dispersedPop[group == "large", Sum] 
                       / groupSizeLarge)
  if (length(largeGroups) == 0) largeGroups = 0L
  if (length(smallGroups) == 0) smallGroups = 0L
  
  
  # Add each small individual to a pool and create a sample queue
  poolSmall <- vector()
  invisible(sapply(1:levels, function(x) {
    block <- rep(as.integer(x), 
                 dispersedPop[1, get(paste0("level", x))])
    poolSmall <<- c(poolSmall, block)
  }))
  poolSmall <- sample(poolSmall, length(poolSmall), replace = F)
  
  # Populate groups from the sample
  for (i in 1:(smallGroups - 1)) {
    startIndex <- groupSizeSmall * i
    endIndex <- groupSizeSmall * (i+1) - 1
    popRow <- as.numeric(sapply(1:levels, function(x) {
      sum(poolSmall[startIndex:endIndex] == x)
    }))
    population[i, group := .("small")]
    invisible(sapply(1:levels, function(x) {
      population[i, x + 2 := popRow[x]]
    }))
  }
  
  if (length(largeGroups) == 0) largeGroups = 0L
  if (largeGroups > 0) {
    # Repeat for large groups
    poolLarge <- vector()
    invisible(sapply(1:levels, function(x) {
      block <- rep(as.integer(x),
                   dispersedPop[1, get(paste0("level", x))])
      poolLarge <<- c(poolLarge, block)
    }))
    poolLarge <- sample(poolLarge, length(poolLarge), replace = F)
    
    # Populate groups from the sample
    for (i in 1:(largeGroups - 1)) {
      startIndex <- groupSizeLarge * i
      endIndex <- groupSizeLarge * (i+1) - 1
      popRow <- as.numeric(sapply(1:levels, function(x) {
        sum(poolLarge[startIndex:endIndex] == x)
      }))
      population[i + smallGroups + 1, group := .("large")]
      invisible(sapply(1:levels, function(x) {
        population[i + smallGroups + 1, x + 2 := popRow[x]]
      }))
    }
  }
}

#------- STEP 3 Repdroduction ---------


reproduction <- function() {
  
  invisible(sapply(1:maxGroups, function(x) {
    
    n <- population[x, .SD, .SDcols = 3:(levels+2)]
    
    # R is based on group size
    ifelse(population[x, (group == "large") & !is.na(group)],
           R <- Rlarge,
           R <- Rsmall
    ) 
    
    # Calculate share of the resource. 
    ngc <- n * G * C
    NGC <- sum(ngc)
    r <- R * ngc / NGC 
    
    # Calculate new population sizes
    nNext <- n + r/C - K*n
    
    # Set new population sizes
    invisible(sapply(1:levels, function (y) {
      population[x, y + 2 :=  as.numeric(nNext[[y]])]
    }))
  }))
}

buildMigrantPool <- function() {
#------- STEP 4 MIGRANT POOL FORMATION ---------
  unformatted <- population[, colSums(.SD, na.rm = T), 
                            .SD = 1:levels+2, by = .(group)]
  vars <- names(dispersedPop)[2:(levels+1)]
  set(dispersedPop, 1L, vars, 
      as.list(unformatted[ group == "small"]$V1))
  if(nrow(unformatted[group=="large"])>0 ) {
    set(dispersedPop, 2L, vars, 
        as.list(unformatted[ group == "large"]$V1))
  }
  
  # STEP 5 MAINTAINING GLOBAL CARRYING CAPACITY 
  dispersedPop[, Sum := rowSums(.SD), .SDcols = 2:(levels+1)]
  scalingFactor <- sum(dispersedPop$Sum) / N
  
  set(dispersedPop, 1L, vars,
      (dispersedPop[1L, .SD, .SDcols = (vars)] / scalingFactor))
  set(dispersedPop, 2L, vars,
      (dispersedPop[ 2L, .SD, .SDcols = (vars)] / scalingFactor))  
  
  dispersedPop[, Sum := rowSums(.SD), .SDcols = 2:(levels+1)]
  
  return(dispersedPop)
}

######## Mutation #############
mutate <- function(rate) {
  
  numberOfGroups <- floor(dispersedPop[group == "small", Sum] 
                          / groupSizeSmall) +
    floor(dispersedPop[group == "large", Sum] / groupSizeLarge)
  
  invisible(sapply(1:(numberOfGroups-1), function(i) {
    row <- population[i, .SD, .SDcols = 3:(levels+2)]
    if (sum(is.na(row) > 0)) return(row)
    # Calculate mutations going up
    end <- TRUE
    up <- sapply(row, function(x){
      if(x < 0) return(0)
      rand <- sample(ceiling(2/rate), floor(x), replace = TRUE)
      ifelse(end == TRUE, {
             mutateUp <- sum(rand == 1 | rand == 2)
             end <<- FALSE },
             mutateUp <- sum(rand == 1))
      return(mutateUp)
    })
    up[levels] <- 0
    
    # Calculate mutations going up
    end <- TRUE
    down <- rev(sapply(rev(row), function(x){
      if (x < 0) { return(0)}
      rand <- sample(ceiling(2/rate), floor(x), replace = TRUE)
      ifelse(end == TRUE,
             mutateDown <- sum(rand == 1 | rand == 2),
             mutateDown <- sum(rand == 1))
      return(mutateDown)
    }))
    down[1] <- 0 

    movement <- c(0,up[1:(levels-1)]) + 
      c(down[2:(levels)],0) - up - down 
    newRow <- movement + row
    
    vars <- names(population)[3:(levels+2)]
    set(population, as.integer(i), vars, newRow)
  }))
}