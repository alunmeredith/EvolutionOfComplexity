#including libraries
library(dplyr)
library(ggplot2)
source("Evolutionary Algorithm Scripting.R")

# preprocessing
cols.num <- c("popSize", "decayRate", "iterations", "fitness", "loopNum")
cols.fac <- c("method", "target", "solution")
results[cols.num] <- lapply(results[cols.num], as.numeric)
results[cols.fac] <- lapply(results[cols.fac], as.factor)

# QUESTION 1 ----------------------------------------------

# The benefit of crossover is to propagate the benefitial mutations from multiple individuals. 
# Consider that in the no crossover case some of the individuals in the population may  
# positively mutate one allele but another mutates positively 3 alleles, without crossover the 
# benefits of the individual who doesn't win the tournament in the next generation are lost, 
# but with crossover some of these benefits can be carried onto the next generation. 
pdf("crossover.pdf")
ggplot(results[1:10, c("method","iterations")], aes(x=method,y=iterations)) +
  geom_boxplot() +
  geom_point(size=4, shape = 16) +
  labs(title="Comparing crossover to mutation")
dev.off()
# As shown the crossover results show signifficant increased speed than non-crossover with no 
# overlap between the two. This is as we expect considering the explanation above 

# QUESTION 2 -----------------------------------------------

# If the problem was more difficult you still expect crossover to be benefitial. The condition for crossover to 
# be benefitial is the probability that multiple individuals in the population have different positives mutations.
# In the case where there is local optima we expect crossover to perform a little worse by these optima in the 
# same way described below, the chance of a mutation jumping peaks out of a local optima << 1 so the benefit
# of crossover isn't present in these situations but there is a chance that the positive mutation be wiped
# out by the crossover (more than 50% in the case where more than one allele was needed to change which is 
# certainly the case for local optima).
pdf("complex.pdf")
ggplot(results[11:34, c("target","iterations", "method")], 
       aes(x=target,y=iterations, group=method, colour=method)) +
  geom_point(size=4, shape = 16) +
  labs(title="Comparing target complexity")
dev.off()
# QUESTION 3 ------------------------------------------------------------

# High mutation rates make it quick to get close to the solution but slow to reach the solution from that point. 

# if you imagine the extreme case where mutation rates are << 1 per individual then at any one time there is likely a maximum of 1 individual with higher fitness than the others, in this case crossover will be meaningless and possibly harmful as it has a 50% chance to remove the benefitial change of that one individual. In this case mutations happen one at a time then propagate slowly through the population. 

# in the opposite extreme where the individual changes more than 1 allele per iteration it should be exponentially harder to achieve (have to get 2 mutations right at the same time). I am unsure if crossover has any specific benefit here. 
pdf("decayRate.pdf")
ggplot(results[35:52, c("decayRate","iterations", "method")], aes(x=decayRate,y=iterations, group=method, colour=method)) +
  geom_point(size=4, shape = 16) +
  labs(title="Comparing decay rate")
dev.off()
# Comparing crossover, noCrossover and hill climbing, hillclimbing at first appears to be better than the noCrossover model, however in the hillClimber only one allele is mutated per iteration (because the population size is 1) and the genetic algorithms 500 mutations happen per iteration. 

pdf("hillClimber.pdf")
ggplot(results[c(1:10,53:62), c("method","iterations")], 
       aes(x=method,y=iterations, fill=method)) +
  geom_boxplot() +
  geom_point(size=4, shape = 16) +
  labs(title="Comparing hillclimber")
dev.off()
