# rfm

#### Description

This is a package to create a customers segmentation model type RFV model 

## Installation


To install this package you need to have the `devtools` package installed. To install devtools type in your console: `install.packages('devtools')`.

Then to install rfm run the following on your console:

```R
devtools::install_github('CJB2014/rfm')
```

## Usage

By typing on your console:

```R
library(rfm)
rfm_data <- rfm_load('data.csv')
model <- rfm_model(rfm_data)
```
