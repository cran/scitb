#'@title  scitb1
#'@name  scitb1
#'@description  You can use it to draw a baseline table of data.
#'
#'@details  Table 1 represents the relationship between the baseline values of the data.
#'          This function can be easily done.Creates 'Table 1', i.e., description of baseline patient
#'          characteristics, which is essential in every medical research.
#'          Supports both continuous and categorical variables, as well as
#'          p-values and standardized mean differences.
#'
#'
#'@param vars The full range of variables you don't want to compare.
#'@param fvars The categorical variables in the data are filled in here.
#'@param strata Enter the variables to be layered. If you fill in consecutive variables, by default they will be split into 3 layers.
#'@param data Enter your data.
#'@param fvars Define the categorical variables in your data.
#'@param dec The precision of the data, which defaults to 2 decimal places.
#'@param num When continuous variables are layered, use it to control the number of layers, which defaults to 3.
#'@param nonnormal When the data belongs to a non-normal distribution, this parameter is needed to indicate which is variable is non-normally distributed.
#'
#'@return A data frame.
#'
#'
#'@format NULL
#'@usage NULL
#'@export
#'@examples
#'## Import data
#'bc<-prematurity
#'## Hierarchical variables converted to factors.
#'bc$race<-as.factor(bc$race)
#'###Define all variables, categorical and stratified.
#'allVars <-c("age", "lwt",  "smoke", "ptl", "ht", "ui", "ftv", "bwt")
#'fvars<-c("smoke","ht","ui")
#'strata<-"race"
#'out<-scitb1(vars=allVars,fvars=fvars,strata=strata,data=bc)
#'print(out)
#'
#'###Stratified variables are continuous variables.
#'allVars <-c("race", "lwt",  "smoke", "ptl", "ht", "ui", "ftv", "bwt")
#'fvars<-c("smoke","ht","ui","race")
#'strata<-"age"
#'out<-scitb1(vars=allVars,fvars=fvars,strata=strata,data=bc)
#'print(out)




scitb1<-function(vars,fvars=NULL,strata,data,dec,num,nonnormal=NULL) {
  if (missing(vars)) {stop("Missing vars.")}
  if (missing(strata)) {stop("Missing strata.")}
  if (missing(data)) {stop("Missing data.")}
  vars<-vars;fvars<-fvars;data<-data;nonnormal<-nonnormal;
  if (missing(dec)) {dec<-2} else {dec<-dec}
  if (missing(num)) {num<-3} else {num<-num}
  strata<-strata
  if (!is.factor(data[,strata]) | length(levels(factor(data[,strata]))) >5 ) {
    G<-rankvar(data[,strata],num=num)
    data$G<-G
    strata<-"G"
  }
  mvars<-setdiff(vars, fvars)
  if (!is.null(fvars)) {
    if (identical(vars,fvars)) {
      fout<-sci1freq(mvars=fvars,x=strata,data=data,nonnormal=nonnormal,dec=dec)
      dat<-fout
    } else {
      fout<-sci1freq(mvars=fvars,x=strata,data=data,nonnormal=nonnormal,dec=dec)
      mout<-sci1mean(mvars=mvars,x=strata,data=data,nonnormal=nonnormal,dec=dec)
      dat<-rbind(mout,fout)
    }
  } else {
    mout<-sci1mean(mvars=mvars,x=strata,data=data,nonnormal=nonnormal,dec=dec)
    dat<-mout
  }
  dat
}