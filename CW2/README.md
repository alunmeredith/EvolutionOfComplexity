## Description
Coursework to reporduce results of an achedemic paper in the area and expanding on that paper. For this I reproduced the "Individual Selection for Cooperative Group Formation" paper by Simon T. Powers, Alexandra S. Penn and Richard A. Watson. (included in the ./Papers). The expansion was considering a closer to continuous model of greed / cooperative behaviour as the paper uses a binary model.

### Paper
*Abstract:*

In this paper we test the reproducibility of Powers’ work on individual selection for cooperative group formation. In which Powers et. al. build a model to show that selection can not only operate in favour of cooperative individuals but selection can act on the environmental conditions which enable evolution to show the conditions which favour cooperation are evolvable despite being initially selected against. We then expand on this model exploring a critique that the binary model may be the source of many of the features observed and hypothesising a coevolutionary algorithm style arms race in a more continuous system. However we observe consistent decrease in selﬁshness despite the environmental conditions selecting for it, supporting Powers’ arguement that ”any component of selection on structuremodifying traits that is due to social behaviour must be in the direction of increased cooperation”.

The report can be found at *./Report/paper.pdf*

## Scripts (./SourceCode)

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

 ### reproductionScriptForEquilibriumState.R
 	- Reproduces the equilibrium state plot from paper. 

###extensionScript.R / extensionHeader.R
	- These scripts have the same functions as the reproduction versions modified to accomodate a variety levels of greedy/cooperative behaviour. 

### extensionEquilibriupScript.R / extensionEquilibriumPlot.R
	- Reproduces the equilibrium plot from paper for the extended model. 

