---
title: "README"
author: "Alun Meredith"
date: "20 December 2015"
output: html_document
---

## Scripts 

####reproductionHeader.R 
  - This script contains all the major functions for the running of the evolutionary algorithm. Group formation, reproduction etc. The other scripts source this file (hence header) and call upon its functions to test various properties of the system for different parameters. 
  
####reproductionScript.R
  - This is the script to produce the main reproduction. I.e. Figure 2. from the paper showing the different relative frequencies of the 4 genotypes for a fixed set of parameters over 100 generations. 
  - It outputs both the data.frames of the genotypes changing and graphs with the name schema "GensXSizeBonusY" to describe the two main variables being tested. 

#### varyingGensSizeBonusScript.R
  - This is a simple set of loops producing the graphs and files from reproductionScript for a variety of different intermixing periods and different size bonuses. 
  
#### reproductionScriptForEquilibriumState.R
  - As its name suggests this is a script which reproduces the equilibrium state figure (figure 1.) from the paper. 
  - The script is a altered version of the reproductionScript and uses the same reproductionHeader functions but using only one groupo size (small) so that the other values are NA. 


## Notes

At the purpose of the scripts is to try and find a set of parameters that recreates figure 2. This is necessary because when using the given parameters for figure 2. our script doesn't produce the same results. 
Although the reproduction of figure 1 is very accurate, this leads me to believe it is either a problem with the Rsmall / Rlarge values in the formula or a small pertubation in the parameters is neccessary due to small differences in the code e.g. allowing decimal growth to carry (when to round, if round or floor, etc.)