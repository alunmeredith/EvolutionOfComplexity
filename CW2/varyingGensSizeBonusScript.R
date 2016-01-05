### This iscript runs through the reproduction script for a combination of t sizes and using the bonus resource for R or otherwise. Make sure to comment out the t and R manually being set in the reproductionscript. 

source('reproductionHeader.r')

for (t in 2:5)
{
  Rsmall = 4
  Rlarge = 40
  source('reproductionScript.R')
}

for (t in 2:5)
{
Rsmall = 4
Rlarge = 50
source('reproductionScript.R')
}

for (t in 2:5)
{
  Rsmall = 4
  Rlarge = 45
  source('reproductionScript.R')
}