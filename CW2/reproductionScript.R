
# Initialise variables
N <- 4000
groupSizeSmall <- 4L
groupSizeLarge <- 40L
R = 1
Rsmall = 4
Rlarge = 50
# Rlarge = 1.05^groupSizeLarge * groupSizeLarge
# Rsmall = 1.05^groupSizeSmall * groupSizeSmall
t <- 4
Gc <- 0.018
Gs <- 0.02
Cc <- 0.1
Cs <- 0.2
Tgen <- 120
K = 0.1
source('reproductionHeader.r')


#---------------------------------------
#-- STEP 1. INITIALISE N INDIVIDUALS ---
#---------------------------------------

# Because we are using R we describe individuals in a vectorised mannor.
# Each element of the vector is an allele, with a 3rd variable stating
# which group the group and 4th the size of the genotype. 

dispersedPop <- data.table(group = c("small", "large"), 
                           greedy = c(N/4, N/4),
                           coop = c(N/4, N/4))

# Initialise population table with maximum number of group sizes
maxGroups <- floor(N/groupSizeSmall)
population <- data.table(id = 1:maxGroups, group = NA_character_, 
                         greedy = NA_real_, coop = NA_real_)

genotypes <- data.table(greedySmall = rep(NA_real_, Tgen + 1) ,
                        greedyLarge = NA_real_,coopSmall = NA_real_,
                        coopLarge = NA_real_)
genotypes[1, greedySmall := N/4]
genotypes[1, greedyLarge := N/4]
genotypes[1, coopSmall := N/4]
genotypes[1, coopLarge := N/4]



for (j in 1:Tgen) { # iterate for T generations
  groupFormation()
  for (i in 1:t) reproduction() # wait time t
  dispersedPop <- buildMigrantPool()
  print(dispersedPop)
  population <- data.table(id = 1:maxGroups, group = NA_character_,
                           greedy = NA_real_, coop = NA_real_)
  
  # Save population values after each generation
  x <- dispersedPop[group == "small", greedy]
  if (length(x) == 0) x = 0
  genotypes[j + 1, greedySmall := x]
  
  x <- dispersedPop[group == "large", greedy]
  if (length(x) == 0) x = 0
  genotypes[j + 1, greedyLarge := x]
  
  x <- dispersedPop[group == "small", coop]
  if (length(x) == 0) x = 0
  genotypes[j + 1, coopSmall := x]
  
  x <- dispersedPop[group == "large", coop]
  if (length(x) == 0) x = 0
  genotypes[j + 1, coopLarge := x]
  
  # Print current state to console
  print(paste("Generation:", j))
  print(genotypes[j])
}


#---------------------------------------
#--------------- Draw plots ---------------
#---------------------------------------
png(filename = paste("./bin/", "Figure2", ".png", sep = ""),
    width = 900, height = 360)
library(gridExtra)
genotypes$index <- 0:Tgen


science_theme = theme(panel.grid.major 
                      = element_line(size = 0.5, color = "grey"), 
                      panel.grid.minor.y = element_blank(),
                      axis.line = element_line(size = 0.7, 
                                               color = "black"), 
                      legend.position = c(0.7,0.5), 
                      text = element_text(size = 26))

graph1 <- ggplot(genotypes, aes(x = index, y = value)) +
  geom_line(aes(y = greedySmall/N, col = "Selfish + Small", 
                linetype = "Selfish + Small", 
                size = "Selfish + Small")) +
  geom_line(aes(y = greedyLarge/N, col = "Selfish + Large", 
                linetype = "Selfish + Large", 
                size = "Selfish + Large")) + 
  geom_line(aes(y = coopSmall/N, col = "Cooperative + Small", 
                linetype = "Cooperative + Small", 
                size = "Cooperative + Small")) +
  geom_line(aes(y = coopLarge/N,col = "Cooperative + Large", 
                linetype = "Cooperative + Large", 
                size = "Cooperative + Large")) +
  xlab("Generation") +
  ylab("Global genotype frequencey") +
  theme_bw() +
  science_theme +
  scale_x_continuous( breaks = c(0,20,40,60,80,100,120),
                      limits = c(0,120)) +
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0),
                     limits = c(0,1)) +
  scale_colour_manual("", 
                      breaks = c("Selfish + Small", "Selfish + Large", 
                                 "Cooperative + Small", 
                                 "Cooperative + Large"),
                      values = c("Dark grey", "Dark grey", 
                                 "Black", "Black")) +
  scale_linetype_manual("",
                        breaks = c("Selfish + Small", 
                                   "Selfish + Large", 
                                   "Cooperative + Small", 
                                   "Cooperative + Large"),
                        values = c(3, 1, 3, 1)) +
  scale_size_manual("", 
                    breaks = c("Selfish + Small", "Selfish + Large", 
                               "Cooperative + Small", 
                               "Cooperative + Large"),
                    values = rep(1.4,4))
## plot2
Large <- apply(genotypes, 1, function(x) {
  sum((x["greedyLarge"] + x["coopLarge"]) / sum(x))
})
Greedy <- apply(genotypes, 1, function(x) {
  sum((x["greedyLarge"] + x["greedySmall"]) / sum(x))
})
Secondplot <- data.frame(Large = Large, Greedy = Greedy)

graph2 <- ggplot(data = Secondplot, 
                 aes(y = seq(0,1,0.2), x = seq(1,120,20))) +
  geom_line(aes(y = Large, x = 1:length(Large), 
                linetype = "Large group size", 
                size = "Large group size",
                col = "Large group size")) +
  geom_line(aes(y = Greedy, x = 1:length(Greedy), 
                linetype = "Selfish resource usage", 
                size = "Selfish resource usage",
                col = "Selfish resource usage")) + 
  theme_bw() +  
  science_theme +
  scale_linetype_manual("", 
                        breaks = c("Large group size", 
                                   "Selfish resource usage"),
                        values = c(3,1)) +
  scale_size_manual("", 
                    breaks = c("Large group size", 
                               "Selfish resource usage"),
                    values = c(1.2, 1.2)) +
  scale_color_manual("",
                     breaks = c("Large group size", 
                                "Selfish resource usage"),
                     values = c("Black", "Dark grey")) +
  xlab("Generation") +
  ylab("Global frequency") +
  scale_x_continuous(breaks = seq(0,120,20), 
                     limits = c(0,120)) + 
  scale_y_continuous(breaks = seq(0,1,0.2),
                        limits = c(0,0.8))
grid.arrange(graph2, graph1, nrow=1, ncol=2)
dev.off()

save(genotypes, file = paste0("./bin/" ,name, ".Rda"))