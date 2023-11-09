# scitb


Provides Some Useful Functions for Making Statistical Tables

## Installation

What is' scitb '?

The scitb package is currently under development. It aims to make generating statistical tables simpler for users.

The scitb package follows the GPL-3 protocol. You can freely use and modify it.

But if you use it for commercial purposes, you need the developer's consent and authorization.


``` r
install.packages("scitb")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(scitb)
## basic example code
## Import data
bc<-prematurity
## Hierarchical variables converted to factors.
bc$race<-as.factor(bc$race)
###Define all variables, categorical and stratified.
allVars <-c("age", "lwt",  "smoke", "ptl", "ht", "ui", "ftv", "bwt")
fvars<-c("smoke","ht","ui")
strata<-"race"
out<-scitb1(vars=allVars,fvars=fvars,strata=strata,data=bc)
###Stratified variables are continuous variables.
allVars <-c("race", "lwt",  "smoke", "ptl", "ht", "ui", "ftv", "bwt")
fvars<-c("smoke","ht","ui","race")
strata<-"age"
out<-scitb1(vars=allVars,fvars=fvars,strata=strata,data=bc)
```

