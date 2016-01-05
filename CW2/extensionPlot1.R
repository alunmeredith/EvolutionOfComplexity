library(data.table)
library(ggplot2)
library(gridExtra)
load("~/Southampton/EvolutionOfComplexity/CW2/genotypes.Rda")
genotypes[, smallPop := rowSums(.SD, na.rm = T), .SDcols = 1:11]
genotypes[, largePop := rowSums(.SD, na.rm = T), .SDcols = 12:22]

colNamesSmall <- grep("level.*small",names(genotypes), value = T)
colNamesLarge <- grep("level.*large",names(genotypes), value = T)


newPlot <- genotypes[, smallCoop := 
            level1small + 
            2*level2small +
            3*level3small + 
            4*level4small +
            5*level5small +
            6*level6small +
            7*level7small +
            8*level8small + 
            9*level9small +
            10*level10small +
            11*level11small
          ]



science_theme = theme(panel.grid.major = 
                        element_line(size = 0.5, color = "grey"), 
                      panel.grid.minor.y = element_blank(),
                      axis.line = 
                        element_line(size = 0.7, color = "black"), 
                      legend.position = c(0.8,0.5), 
                      text = element_text(size = 26))

graph2 <- ggplot(data = genotypes, aes(x = 1:401, y = largePop)) +
  xlim(0,301) +
  ylim(1800,2200) +
  theme_bw() +
  science_theme +
  geom_line(aes(y = largePop, col = "largePop", 
                linetype = "largePop", size = "largePop")) +
  geom_line(aes(y = smallPop, col = "smallPop", 
                linetype = "smallPop", size = "smallPop")) + 
  xlab("Generation") +
  ylab("Global genotype frequencey") +
  scale_colour_manual("", 
                      breaks = c("largePop", "smallPop"),
                      values = c("Dark grey", "Black")) +
  scale_linetype_manual("",
                        breaks = c("largePop", "smallPop"),
                        values = c(1, 1)) +
  scale_size_manual("", 
                    breaks = c("largePop", "smallPop"),
                    values = rep(1.4,2))

graph1 <- ggplot(data = genotypes, aes(x = 1:401, y = largePop)) +
  xlim(0,301) +
  theme_bw() +
  science_theme +
  geom_line(aes(y = largeCoop, col = "largePop", 
                linetype = "largePop", size = "largePop")) +
  geom_line(aes(y = smallCoop, col = "smallPop", 
                linetype = "smallPop", size = "smallPop")) + 
  xlab("Generation") +
  ylab("Average greed level") +
  scale_colour_manual("", 
                      breaks = c("largePop", "smallPop"),
                      values = c("Dark grey", "Black")) +
  scale_linetype_manual("",
                        breaks = c("largePop", "smallPop"),
                        values = c(1, 1)) +
  scale_size_manual("", 
                    breaks = c("largePop", "smallPop"),
                    values = rep(1.4,2))

grid.arrange(graph2, graph1, nrow=1, ncol=2)

cols <- grey.colors(11)
ggplot(newPlot, aes(x = 1:401)) +
  geom_line(aes(y = level1large, col = "level1")) +
  geom_line(aes(y = level2large, col = "level2")) +
  geom_line(aes(y = level3large, col = "level3")) +
  geom_line(aes(y = level4large, col = "level4")) +
  geom_line(aes(y = level5large, col = "level5")) +
  geom_line(aes(y = level6large, col = "level6")) +
  geom_line(aes(y = level7large, col = "level7")) +
  geom_line(aes(y = level8large, col = "level8")) +
  geom_line(aes(y = level9large, col = "level9")) +
  geom_line(aes(y = level10large, col = "level10")) +
  geom_line(aes(y = level11large, col = "level11")) +
  xlim(0,60) +
  theme_bw() +
  science_theme +
  xlab("Generation") +
  ylab("Genotype Frequency") +
theme(legend.title=element_blank())  