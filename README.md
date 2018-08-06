# Demographic drivers of collapse of an island Tree Swallow population

Repository for Data/Model/Analysis for manuscript\
In review\
**Authors**: L.U. Taylor, B.K. Woodworth, B.K. Sandercock, and N.T. Wheelwright

**Abstract**:
Diagnosing causes of population declines requires an understanding of the contributions of demographic vital rates to inter-annual variability and long-term changes in population size. At Kent Island, New Brunswick, Canada, an isolated population of Tree Swallows (Tachycineta bicolor) collapsed between 1987 and 2010, providing a unique opportunity to reconstruct how demographic rates drive population dynamics. We fit an integrated population model to 24 years of population count, reproductive success, and capture-recapture data to generate annual estimates of productivity, juvenile and adult survival, immigration, and the finite rate of population change (λ). The Kent Island population declined from 202 to 12 breeding adults over 24 years, with a geometric mean decline of 11.6% per year. Annual apparent survival of adults averaged 56% across sexes, whereas annual survival and recruitment of juveniles never exceeded 6%. Transient life table response experiments revealed that variation in male and female immigration rates were the major contributors to both overall and inter-annual variation in λ, followed by female and male adult survival. Local recruitment and reproductive rates had little effect on variation in λ. Given broad-scale regional declines in Tree Swallows, our study shows how declines of isolated populations can be driven by reductions in immigration, especially when coupled with variation in adult survival and low local recruitment.

## Required Software and Packages
[R](https://www.r-project.org/)\
[JAGS](http://mcmc-jags.sourceforge.net/)\
R Packages (model/analysis/making a beep noise):\
* [jagsUI](https://cran.r-project.org/web/packages/jagsUI/index.html)
* [plyr](https://cran.r-project.org/web/packages/plyr/index.html)
* [tidyverse](https://cran.r-project.org/web/packages/tidyverse/index.html)
* [gtools](https://cran.r-project.org/web/packages/gtools/index.html)
* [cowplot](https://cran.r-project.org/web/packages/cowplot/index.html)
* [matrixStats](https://cran.r-project.org/web/packages/matrixStats/index.html)
* [beepr](https://cran.r-project.org/web/packages/beepr/index.html)

Thanks to F. Abadi, M. Kéry, D. Koons, and M. Schaub for robust code examples in their winBUGS/IPM/LTRE work. See works cited in text.

## Files
Scripts/ contains R script files.\
Data/ contains R object data files\
Output/ contains visualization output

### **Run_TRES.R**:
Runs full process, including Init, JAGS Model, LTRE, and Vis
**Output**:
* **ipm** 	 (full jagsUI output)
* **out** 	 (ipm summary output)
* **gof** 	 (PPP goodness-of-fit results)
* **cont_summary** (vital rate contributions to overall variance in lambda, LTRE part 1)
* **yearly_cont_summary** (vital rate contributions to intervals *t* to *t*+1, LTRE part 2)
* **LTREcors_max**  (vital rate correl matrix (Maximum density r))
* **LTREcors_low**  (vital rate correl matrix (2.5% posterior quantile))
* **LTREcors_high** (vital rate correl matrix (97.5% posterior quantile))
* **LTREcors_P**    (vital rate correl matrix (P(r>0))) 
* Figures **1-7** in the text (saved to Output/)

### **Init_TRES.R**:
Initialize the data for TRES IPM, pulling from R objects stored in Data/. Also includes code used to draw from original datasets

### **ipm.TRES.bug**:
Contains JAGS model code used in Run_TRES.R jags() call

### **LTRE_Tres.R**:
Run life table response experiments (LTREs) to decompose contributions of vital rates to overall variance or (t to t+1) interval differences in lambda
**Output**:
* **cont_summary** (vital rate contributions to overall variance in lambda, LTRE part 1)
* **yearly_cont_summary** (vital rate contributions to intervals *t* to *t*+1, LTRE part 2)
* **LTREcors_max**  (vital rate correl matrix (Maximum density r))
* **LTREcors_low**  (vital rate correl matrix (2.5% posterior quantile))
* **LTREcors_high** (vital rate correl matrix (97.5% posterior quantile))
* **LTREcors_P**    (vital rate correl matrix (P(r>0))) 

### **Vis_TRES.R**:
Calculates simple vitalrate~lambda correlations and goodness-of-fit results. Draws and saves Figures 1-8 from the text
**Output**:
* **gof** 	 (PPP goodness-of-fit results)
* Figures **1-7** in the text (saved to Output/)