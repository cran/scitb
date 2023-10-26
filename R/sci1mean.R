#'@title  sci1mean
#'@name  sci1mean
#'@description  You can use it to draw a baseline table of data.Creates 'Table 1', i.e., description of baseline patient
#'              characteristics, which is essential in every medical research.
#'              Supports both continuous and categorical variables, as well as
#'              p-values and standardized mean differences.
#'
#'@details  Table 1 represents the relationship between the baseline values of the data.
#'          This function can be easily done.
#'
#'
#'@param mvars The full range of variables you don't want to compare.
#'@param x Enter the variables to be layered. If you fill in consecutive variables, by default they will be split into 3 layers.
#'@param data Enter your data.
#'@param dec The precision of the data, which defaults to 2 decimal places.
#'@param nonnormal When the data belongs to a non-normal distribution, this parameter is needed to indicate which is variable is non-normally distributed.
#'
#'@return A data frame.

utils::globalVariables(c('aov',
                         'kruskal.test'
))




sci1mean<- function(mvars,x,data,dec,nonnormal=NULL) {
  mvars<-mvars;x<-x;data<-data;nonnormal<-nonnormal
  if (missing(dec)) {dec<-2} else {dec<-dec}
  xvt<-data[,x];nc<-length(mvars);varsdt<-data[,mvars];
  if (nc==1) varsdt<-as.matrix(varsdt,ncol=nc)
  n.x<-length(levels(factor(xvt)));
  queshiliebiao<-is.na(cbind(xvt,varsdt))
  pp<-NULL; st.diff<-NULL;d0<-NULL
  a<-as.raw (as.hexmode ("a1"))
  b<-as.raw (as.hexmode ("c0"))
  xxx<-c(a,b)
  for (i in (1:nc)) {
    scimean<-tapply(varsdt[,i],factor(xvt),average)
    scistd<-tapply(varsdt[,i],factor(xvt),stdev)
    scinn<-table(xvt[!is.na(varsdt[,i])])
    scimedian<-numfmt(tapply(varsdt[,i],factor(xvt),mxmedian),dec)
    scimin<-numfmt(tapply(varsdt[,i],factor(xvt),mxmin),dec)
    scimax<-numfmt(tapply(varsdt[,i],factor(xvt),mxmax),dec)
    sciq1<-numfmt(tapply(varsdt[,i],factor(xvt),mxq1),dec)
    sciq3<-numfmt(tapply(varsdt[,i],factor(xvt),mxq3),dec)
    scitmp<-xvt[apply(queshiliebiao[,c(1,i+1)],1,sum)==0]
    if (length(levels(factor(scitmp)))>1) {
      pvalue<-summary(aov(varsdt[,i]~factor(xvt)))[[1]]$"Pr(>F)"[1]
      pp1<-ifelse(pvalue<0.001, "<0.001",pvformat(pvalue,3))
    }
    if (length(levels(factor(scitmp)))>1) {
      pvalue.npr<-kruskal.test(varsdt[,i]~factor(xvt))$p.value
      pp1.npr<-ifelse(pvalue.npr<0.001, "<0.001",pvformat(pvalue.npr,3))}
    if (!mvars[i] %in% nonnormal) {
      d1<-paste(numfmt(scimean,dec),rawToChar(xxx),numfmt(scistd,dec),sep="")
      p<-pp1
    }
    if (mvars[i] %in% nonnormal) {
      d1<-paste(scimedian," (",sciq1,"-",sciq3,")",sep="")
      p<-pp1.npr
    }
    d0<-rbind(d0,d1);pp<-rbind(pp,p)
  }
  dd<-rbind(scinn,d0)
  tmp.cname<-paste(x,".",levels(factor(xvt)),sep="")
  dd1<-cbind(dd,c("",pp))
  dd2<-cbind(c("N",mvars),dd1)
  colnames(dd2)<-c("Characteristic",tmp.cname,"p value")
  dd2
}
