---
title: "README"
author: "Alun Meredith"
date: "20 December 2015"
output: html_document
---

### Scripts 

* reproductionHeader.R 
  - This script contains all the major functions for the running of the evolutionary algorithm. Group formation, reproduction etc. The other scripts source this file (hence header) and call upon its functions to test various properties of the system for different parameters. 
  
* reproductionScript.R
  - This is the script to produce the main reproduction. I.e. Figure 2. from the paper showing the different relative frequencies of the 4 genotypes for a fixed set of parameters over 100 generations. 
  - It outputs both the data.frames of the genotypes changing and graphs with the name schema "GensXSizeBonusY" to describe the two main variables being tested. 