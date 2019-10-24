# BiasCorrector (!!!under development!!!)

BiasCorrector is the user friendly implementation of the algorithms described by Moskalev et. al in their research article *'Correction of PCR-bias in quantitative DNA methylation studies by means of cubic polynomial regression'*, published 2011 in *Nucleic acids research, Oxford University Press* (DOI: [https://doi.org/10.1093/nar/gkr213](https://doi.org/10.1093/nar/gkr213)).  

## Citation:  
```
@article{10.1093/nar/gkr213,
    author = {Moskalev, Evgeny A. and Zavgorodnij, Mikhail G. and Majorova, Svetlana P. and Vorobjev, Ivan A. and Jandaghi, Pouria and Bure, Irina V. and Hoheisel, Jörg D.},
    title = "{Correction of PCR-bias in quantitative DNA methylation studies by means of cubic polynomial regression}",
    journal = {Nucleic Acids Research},
    volume = {39},
    number = {11},
    pages = {e77-e77},
    year = {2011},
    month = {04},
    abstract = "{DNA methylation profiling has become an important aspect of biomedical molecular analysis. Polymerase chain reaction (PCR) amplification of bisulphite-treated DNA is a processing step that is common to many currently used methods of quantitative methylation analysis. Preferential amplification of unmethylated alleles—known as PCR-bias—may significantly affect the accuracy of quantification. To date, no universal experimental approach has been reported to overcome the problem. This study presents an effective method of correcting biased methylation data. The procedure includes a calibration performed in parallel to the analysis of the samples under investigation. DNA samples with defined degrees of methylation are analysed. The observed deviation of the experimental results from the expected values is used for calculating a regression curve. The equation of the best-fitting curve is then used for correction of the data obtained from the samples of interest. The process can be applied irrespective of the locus interrogated and the number of sites analysed, avoiding an optimization of the amplification conditions for each individual locus.}",
    issn = {0305-1048},
    doi = {10.1093/nar/gkr213},
    url = {https://dx.doi.org/10.1093/nar/gkr213},
    eprint = {http://oup.prod.sis.lan/nar/article-pdf/39/11/e77/16775711/gkr213.pdf},
}
```

## Installation  

### Using RStudio  

- Make sure, you have R and RStudio installed on your system:  
    
    + https://cran.r-project.org/  
    + https://www.rstudio.com/products/rstudio/download/#download  

- Then open R and install this R package:

```
library(devtools)
devtools::install_github("kapsner/BiasCorrector")
```

- To start BiasCorrector, just run the following command in R. A browser tab should open displaying BiasCorrector. Alternatively you can type the URL "localhost:3838/" in your browser.

```
library(BiasCorrector)
launch_app()
```

# More Infos:

- Original work by Moskalev et al.: https://doi.org/10.1093/nar/gkr213
- about RStudio: https://www.rstudio.com/products/rstudio/
- about Shiny: https://www.rstudio.com/products/shiny/
- about Shiny Server: https://www.rstudio.com/products/shiny/shiny-server/
- RStudio and Shiny are trademarks of RStudio, Inc.
- about Docker: https://www.docker.com/

