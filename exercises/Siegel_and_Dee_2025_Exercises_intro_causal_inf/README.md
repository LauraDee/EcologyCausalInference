## Demonstration files to accompany Siegel & Dee


### Methods that condition on observable confounders: Matching and inverse probability of treatment weighting 

#### Code
_Matching_Weighting.Rmd_: Rmarkdown file with code demonstrating how to implement matching using the MatchIt package (Ho et al. 2011) and weighting using the ipw package (van der Wal & Geskus 2011).  

#### Data
_matching_ipw_data.csv_: Data for use in Matching_Weighting.Rmd. Data are adapted from Siegel et al. (2022). Please refer to the original paper for data sources and pre-processing methods.

### Methods that condition on observable and unobservable confounders: Panel models and fixed effects  

#### Code:
_Fixed_Effects.Rmd_: Rmarkdown file with code demonstrating how to implement panel methods and fixed effects models using the fixest package (Berge 2018). This tutorial is adapted from a tutorial developed by Laura Dee and Chris Severen, published to accompany Dee et al. (2023).    

#### Data:
_fixed_effects_data.csv_: Data for use in Fixed_Effects.Rmd. Processed and cleaned data are from Dee et al. (2023), with raw data from the Nutrient Network. Please refer to Dee et al. (2023) for additional information on data sources and pre-processing methods.

### References
Berge L (2018). Efficient estimation of maximum likelihood models with multiple fixed-effects: the R package FENmlm. _CREA Discussion Papers_.

Dee LE, Ferraro PJ, Severen CN, et al. (2023) Clarifying the effect of biodiversity on productivity in natural ecosystems with longitudinal data and methods for causal inference. _Nature Communications_, *14*, 2607. [doi:10.1038/s41467-023-37194-5](https://doi.org/10.1038/s41467-023-37194-5).

Ho D, Imai K, King G, Stuart E (2011). MatchIt: Nonparametric Preprocessing for Parametric Causal Inference. _Journal of Statistical Software_, *42*(8), 1-28. [doi:10.18637/jss.v042.i08](https://doi.org/10.18637/jss.v042.i08).

Siegel KJ, Larsen L, Stephens C, Stewart W, Butsic V (2022). Quantifying drivers of change in social-ecological systems: land management impacts wildfire probability in forests of the western US. _Regional Environmental Change_, *22*. [doi:10.1007/s10113-022-01950-y](https://doi.org/10.1007/s10113-022-01950-y).

van der Wal WM, Geskus RB (2011). ipw: An R Package for Inverse Probability Weighting. _Journal of Statistical Software_, *43*(13), 1-23. [doi:10.18637/jss.v043.i13](https://doi.org/10.18637/jss.v043.i13). 
