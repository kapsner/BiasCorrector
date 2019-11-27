# BiasCorrector (!!!under development!!!)

<!-- badges: start -->
[![pipeline status](https://gitlab.com/kapsner/BiasCorrector/badges/master/pipeline.svg)](https://gitlab.com/kapsner/BiasCorrector/commits/master)
[![coverage report](https://gitlab.com/kapsner/BiasCorrector/badges/master/coverage.svg)](https://gitlab.com/kapsner/BiasCorrector/commits/master)
<!-- badges: end -->

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
    issn = {0305-1048},
    doi = {10.1093/nar/gkr213},
    url = {https://dx.doi.org/10.1093/nar/gkr213},
    eprint = {http://oup.prod.sis.lan/nar/article-pdf/39/11/e77/16775711/gkr213.pdf},
}
```

## rBiasCorrection

`BiasCorrector` depends on the `rBiasCorrection` R-package, which is the implementation of the core functionality to correct measurment biases in DNA methylation analyses. `BiasCorrector` brings this functionality to a user-friendly shiny web application.

# Installation  

## Using R 

- Make sure, you have R installed on your system:  
    
    + https://cran.r-project.org/  

- Then open your environment and install this R package:

```
install.packages("devtools")
devtools::install_github("kapsner/BiasCorrector")
```

- To start BiasCorrector, just run the following command in R. A browser tab should open displaying BiasCorrector. Alternatively you can type the URL "localhost:3838/" in your browser.

```
library(BiasCorrector)
launch_app()
```

## Using Docker

To simplify installation an deployment of `BiasCorrector` you can clone this repository and build your own docker image. Make sure, you have Docker and docker-compose installed on your system.

### Build Docker Image Manually

```
# clone the repository
git clone https://github.com/kapsner/BiasCorrector

# go to the docker subfolder
cd BiasCorrector/docker/

# run the build script
./build_image.sh

# when the building is finished, just start the container by running
docker-compose -f docker-compose.local.yml up -d
```

### Using a Remote Docker Image

```
# clone the repository
git clone https://github.com/kapsner/BiasCorrector

# go to the docker subfolder
cd BiasCorrector/docker/

# start the Docker container
docker-compose -f docker-compose.remote.yml up -d
```

Type the URL "localhost:3838/" in your browser and start working with `BiasCorrector`.


# More Infos:

- Original work by Moskalev et al.: https://doi.org/10.1093/nar/gkr213
- about RStudio: https://www.rstudio.com/products/rstudio/
- about Shiny: https://www.rstudio.com/products/shiny/
- about Shiny Server: https://www.rstudio.com/products/shiny/shiny-server/
- RStudio and Shiny are trademarks of RStudio, Inc.
- about Docker: https://www.docker.com/

