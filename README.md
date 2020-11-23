# cjpowR 
### A Priori Power Analyses for Conjoint Experiments

<img src="cjpowR_hex.png" alt="drawing" width="200"/>

Based on [Schuessler/Freitag (2020)](), this R package provides simple
functions to calculate power, minimum required sample size, Type S and the expected Type M error for forced-choice conjoint experiments.

#### Cite as:

Freitag, M. and J. Schuessler(2020): “cjpowR – A Priori Power Analyses for Conjoint Experiments,” R-Package. 

## Installation

To install, enter the following lines of code in R:

```{r}
if(!require(devtools)) install.packages("devtools")
library(devtools)
devtools::install_github("m-freitag/cjpowR")
```

## Usage

In its current development version, the package consists of three functions. `cjpowr_amce()` and `cjpowr_amcie()` return a `data.frame`-object holding, depending on the user inputs, the calculated minimum required sample size or power along with the Type S and the expected Type M Error. 
For convenience, if a sample size is provided, power is calculated, whereas if power is provided, the minimum required sample size is put out. 
Further, the effect size, the number of levels, the alpha-level and, especially in the case of differences in conditional AMCEs, the treatment probabilities have to be provided. 

For instance, 

```{r}
library(cjpowR)
cjpowr_amce(amce = 0.05, power = 0.8, levels = 5, alpha=0.05)
```
gives the minimum required effective sample size as well as the Type S Error and the exaggeration ratio given specified parameters. The functions can be manipulated to calculate the desired outputs for ranges of parameters (e.g. ranges of effect sizes). 

For example, an [interactive exaggeration curve](https://rawgit.com/m-freitag/cjpowR/master/Type\%20M.html) for a 3x4 level AMCIE can be easily generated using:

```{r}
# Due to the iterative calculation, vectorizing the functions is 
# recommended when calculating curves only for the exaggeration ratio
# given other parameters. The number of iteration steps can be
# decreased using the "sims" parameter.

cjpowr_amcie_vec <- Vectorize(cjpowr_amcie)

d <- expand.grid(delta3 = c(0.01, 0.02, 0.03, 0.05), n = seq(from = 100, to = 50000, length.out = 1000))

df <- t(cjpowr_amcie_vec(delta3 = d$delta3, n = d$n, sims = 10000, levels1 = 3, levels2=4, alpha = 0.05, delta0 = 0.5))

df <- data.frame(df)

df[] <- lapply(df, unlist)

# Interactive plot
library(plotly)

plot_ly(df, x = ~n, y = ~exp_typeM, type = 'scatter', mode = 'lines', linetype = ~delta3) %>%
 layout(
   xaxis = list(title = "Effective Sample Size",
                zeroline = F,
                hoverformat = '.0f'),
   yaxis = list(title = "Exaggeration Ratio",
                range = c(0,10),
                zeroline = F,
                hoverformat = '.2f'),
   legend=list(title=list(text='<b> AMCIE </b>')),
   hovermode = "x unified"
 )

```

 To facilitate plotting and inspection, `cjpowr_plotly` provides a convenience function to create interactive power curves. For more information, readers are referred to the package documentation.
