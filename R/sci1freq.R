#'@title  sci1freq
#'@name  sci1freq
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
#'@param statistic Statistical effect values. Usually, it is the default F, and selecting T will return a statistical effect value.
#'
#'@return A data frame.

utils::globalVariables(c('chisq.test'
))


sci1freq<- function(mvars,x,data,dec,nonnormal=NULL,statistic=NULL) {
  options(warn=-1)
  mvars<-mvars;x<-x;data<-data;nonnormal<-nonnormal
  if (missing(dec)) {dec<-2} else {dec<-dec}
  xvt<-data[,x];nc<-length(mvars);varsdt<-data[,mvars];
  if (nc==1) varsdt<-as.matrix(varsdt,ncol=nc)
  n.x<-length(levels(factor(xvt)));
  queshiliebiao<-is.na(cbind(xvt,varsdt))
  pp<-NULL; st.diff<-NULL;d<-NULL;sv<-NULL
  for (i in (1:nc)) {
    t1<-table(varsdt[,i],factor(xvt),useNA="no")
    pvalue<-chisq.test(t1,correct=FALSE)$p.value;svv1<-chisq.test(t1,correct=FALSE)[["statistic"]]
    pp1<-ifelse(pvalue<0.001, "<0.001", pvformat(pvalue,3));sv1<-ifelse(svv1<0.001, "<0.001",pvformat(svv1,3))
    ooi<-cbind(matrix(rep(" ",times=ncol(t1)),nrow=1), pp1)
    p1<-prop.table(t1,2)
    tb1<-matrix(paste(format(t1)," (", numfmt(p1*100,dec), "%)", sep=""),nrow=nrow(t1))
    tmp.names<-c(mvars[i],rownames(t1))
    if (statistic==T) {
      d1<-cbind(rownames(t1),tb1,rep(" ",times=length(rownames(t1))),rep(" ",times=length(rownames(t1))))
    } else {d1<-cbind(rownames(t1),tb1,rep(" ",times=length(rownames(t1))))}
    if (statistic==T){
      d2<-c(mvars[i],rep(" ",times=n.x),sv1,pp1)
    } else {d2<-c(mvars[i],rep(" ",times=n.x),pp1)}
    d0<-rbind(d2,d1)
    d<-rbind(d,d0)
  }
  tmp.cname1<-paste(x,".",levels(factor(xvt)),sep="")
  if (statistic==T) {
    colnames(d)<-c("Characteristic",tmp.cname1,"statistic","p value")
  } else {colnames(d)<-c("Characteristic",tmp.cname1,"p value")}
  d
}
