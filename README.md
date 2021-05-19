# BiasCorrector 

<!-- badges: start -->
[![R CMD Check via {tic}](https://github.com/kapsner/BiasCorrector/workflows/R%20CMD%20Check%20via%20{tic}/badge.svg?branch=master)](https://github.com/kapsner/BiasCorrector/actions)
[![linting](https://github.com/kapsner/BiasCorrector/workflows/lint/badge.svg?branch=master)](https://github.com/kapsner/BiasCorrector/actions)
[![test-coverage](https://github.com/kapsner/BiasCorrector/workflows/test-coverage/badge.svg?branch=master)](https://github.com/kapsner/BiasCorrector/actions)
[![codecov](https://codecov.io/gh/kapsner/BiasCorrector/branch/master/graph/badge.svg)](https://codecov.io/gh/kapsner/BiasCorrector)
[![pipeline status](https://gitlab.com/kapsner/BiasCorrector/badges/master/pipeline.svg)](https://gitlab.com/kapsner/BiasCorrector/commits/master)
[![coverage report](https://gitlab.com/kapsner/BiasCorrector/badges/master/coverage.svg)](https://gitlab.com/kapsner/BiasCorrector/commits/master)
[![CRAN Status Badge](https://www.r-pkg.org/badges/version-ago/BiasCorrector)](https://cran.r-project.org/package=BiasCorrector)
[![CRAN Checks](https://cranchecks.info/badges/worst/BiasCorrector)](https://cran.r-project.org/web/checks/check_results_BiasCorrector.html)
<!-- badges: end -->

`BiasCorrector` is published in *'BiasCorrector: fast and accurate correction of all types of experimental biases in quantitative DNA methylation data derived by different technologies' (2021)* in the *International Journal of Cancer* (DOI: [https://onlinelibrary.wiley.com/doi/10.1002/ijc.33681](https://doi.org/10.1002/ijc.33681)).

`BiasCorrector` is the user friendly implementation of the algorithms described by Moskalev et. al in their research article *'Correction of PCR-bias in quantitative DNA methylation studies by means of cubic polynomial regression'*, published 2011 in *Nucleic acids research, Oxford University Press* (DOI: [https://doi.org/10.1093/nar/gkr213](https://doi.org/10.1093/nar/gkr213)).  

# Installation  

## Using R 

- Make sure, you have R installed on your system:  
    
    + https://cran.r-project.org/  

- Then open your development environment and install this R package:

### CRAN version

You can install `BiasCorrector` simply with via R's `install.packages` interface:

```r
install.packages("BiasCorrector")
```

### development version

If you want to use the latest development version, you can install the github version of `BiasCorrector` with:

```r
install.packages("devtools")
devtools::install_github("kapsner/BiasCorrector")
```

- To start BiasCorrector, just run the following command in R. A browser tab should open displaying BiasCorrector. Alternatively you can type the URL "localhost:3838/" in your browser.

```r
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

## rBiasCorrection

`BiasCorrector` depends on the `rBiasCorrection` R-package, which is the implementation of the core functionality to correct measurement biases in DNA methylation analyses. `BiasCorrector` brings this functionality to a user-friendly shiny web application.  
`rBiasCorrection` is available at [https://github.com/kapsner/rBiasCorrection](https://github.com/kapsner/rBiasCorrection). 

# Video Tutorial 

A video tutorial describing the workflow of how to use `BiasCorrector` in order to correct measurement bias in DNA methylation data is available [on youtube](https://youtu.be/xOf8uDbUrms). 

# Demo Version

A demo version of `BiasCorrector` is available [here](https://biascorrector.diz.uk-erlangen.de/).

# Frequently Asked Questions 

FAQs can be found [here](https://github.com/kapsner/rBiasCorrection/blob/master/FAQ.md).

# Citation of Kapsner et al. (2021)   

```
L.A. Kapsner, M.G. Zavgorodnij, S.P. Majorova, A. Hotz‐Wagenblatt, O.V. Kolychev, I.N. Lebedev, J.D. Hoheisel, A. Hartmann, A. Bauer, S. Mate, H. Prokosch, F. Haller, and E.A. Moskalev, BiasCorrector: fast and accurate correction of all types of experimental biases in quantitative DNA methylation data derived by different technologies, Int. J. Cancer. (2021) ijc.33681. doi:10.1002/ijc.33681.
```

```
@article{kapsner2021,
  title = {{{BiasCorrector}}: Fast and Accurate Correction of All Types of Experimental Biases in Quantitative {{DNA}} Methylation Data Derived by Different Technologies},
  author = {Kapsner, Lorenz A. and Zavgorodnij, Mikhail G. and Majorova, Svetlana P. and Hotz-Wagenblatt, Agnes and Kolychev, Oleg V. and Lebedev, Igor N. and Hoheisel, J{\"o}rg D. and Hartmann, Arndt and Bauer, Andrea and Mate, Sebastian and Prokosch, Hans-Ulrich and Haller, Florian and Moskalev, Evgeny A.},
  year = {2021},
  month = may,
  pages = {ijc.33681},
  issn = {0020-7136, 1097-0215},
  doi = {10.1002/ijc.33681},
  journal = {International Journal of Cancer},
  language = {en}
}
```

# More Infos

- Original work by Moskalev et al.: https://doi.org/10.1093/nar/gkr213
- about Shiny: https://www.rstudio.com/products/shiny/
- RStudio and Shiny are trademarks of RStudio, Inc.
- about Docker: https://www.docker.com/
