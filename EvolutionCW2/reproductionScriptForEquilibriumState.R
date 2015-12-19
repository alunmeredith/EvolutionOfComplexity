source('reproductionHeader.r')

# Initialise variables
N <- 1000
groupSizeSmall <- 4L
groupSizeLarge <- 40L
Rsmall = 4
Rlarge = 40
t <- 3
Gc <- 0.018
Gs <- 0.02
Cc <- 0.1
Cs <- 0.2
Tgen <- 100
R = 1
K = 0.1
Time <- matrix(nrow = 3, ncol = 8)

for (i in 1:3) {
  for (groupSizeSmall in 1:8) {
    dispersedPop <- data.table(group = c("small", "large"), 
                               greedy = c(N/2, 0),
                               coop = c(N/2, 0))
    
    # Initialise population table with maximum number of group sizes
    maxGroups <- floor(N/groupSizeSmall)
    population <- data.table(id = 1:maxGroups, group = NA_character_, greedy = NA_real_, coop = NA_real_)
    
    genotypes <- data.table(greedySmall = rep(NA_real_, Tgen) ,greedyLarge = NA_real_,
                            coopSmall = NA_real_, coopLarge = NA_real_)
    groupFormation()
    for (j in 1:Tgen)# iterate for T generations
    { 
      reproduction() 
      dominant <- sum(population$greedy) - sum(population$coop)
      if (dominant < 0) print(paste("coop dominant at time", j))
      else {
        print(paste("greedy dominant at time", j))
        break
      }      
      Time[i,groupSizeSmall] <- j
    }
  }
}

## Draw Graph
png(file = "Equilibrium State", width = 840)
x <- apply(Time, 2, mean)
plot <- ggplot(data.frame(x),aes(seq_along(x),x))+geom_bar(stat="identity")
dev.off()