# shinymalia
- Analysis and visualization GUI for malia package.

## Introduction
- This shiny app provides visual and numerical summaries for malia package. This app was built by laboratory of fish population analysis at TUMSAT.

## Prerequisite
- R version(>= 3.5.2)
- 'malia' package should be installed on your laptop

## Installation
```
#install.packages("devtools")  #if you haven't installed `devtools` package
#devtools::install_github("y-yasutomo/malia")
#devtools::install_github("y-yasutomo/shinymalia")
library(malia)
library(shinymalia)
shinymalia() #run app
```
## Preparation
- Already created results of malia package are needed. See SDAM() function of the [manual](https://y-yasutomo.github.io/malia/docs/functions.html).
- The results object is required to be put in the working directory.
- In order to handle the results, it is needed to put sighing and effort data of the survey corresponding with the results in the `data` folder at current directory.

## Functions
- Draw and download plots.
- Creat and doenload tables.
- Synthesize the results of multiple surveys.
