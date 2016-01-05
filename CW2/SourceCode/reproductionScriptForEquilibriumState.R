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

for (i in 1:5) {
  for (groupSizeSmall in 1:8) {
    dispersedPop <- data.table(group = c("small", "large"), 
                               greedy = c(N/2, 0),
                               coop = c(N/2, 0))
    
    # Initialise population table with maximum number of group sizes
    maxGroups <- floor(N/groupSizeSmall)
    population <- data.table(id = 1:maxGroups, group = NA_character_,
                             greedy = NA_real_, coop = NA_real_)
    
    genotypes <- data.table(greedySmall = rep(NA_real_, Tgen) ,
                            greedyLarge = NA_real_,
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
library("extrafont")
font_import()
png(filename = "./bin/EquilibriumState.png", width = 480, height = 360)

Time[is.na(Time)] <- 0
x <- apply(Time, 2, mean)

science_theme = theme(panel.grid.major = 
                        element_line(size = 0.5, color = "grey"), 
                      panel.grid.minor.y = element_blank(),
                      axis.line = element_line(size = 0.7,
                                               color = "black"), 
                      legend.position = c(0.85,0.7), 
                      text = element_text(size = 14))
plot <- ggplot(data.frame(x),aes(seq_along(x),x)) +
  geom_bar(stat = "identity", width = 1) +
  xlab("Initial Group Size") +
  ylab("Time spent in groups before mixing") +
  scale_x_discrete( limits = c(1,2,3,4,5,6,7,8), 
                    labels = c(1,2,3,4,5,6,7,"8+"),
                    breaks = c(1,2,3,4,5,6,7,8)) +
  scale_y_continuous( labels = c(0,20,40,60,80,100), 
                      breaks = c(0,20,40,60,80,100),
                      limits = c(0,100),
                      minor_breaks = NULL) +
  science_theme +
  theme_bw(base_size = 20) +
  annotate("text", x = 6, y = 100, 
           label = "\u25a0 Cooperative trait selected for", 
           color = "grey20")
plot
dev.off()

save(genotypes, file = "./bin/equilibriumStats.Rda")