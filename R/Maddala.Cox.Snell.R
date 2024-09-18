#'@title  Maddala.Cox.Snell
#'@name  Maddala.Cox.Snell
#'@description  Maddala Cox Snell in the computational model.
#'
#'
#'@param fit Your model. Support logistic regression and Cox regression.
#'
#'
#'@return Maddala Cox Snell.
#'
#'
#'
#'#'@details The outcome variables in the model must be represented using 0 and 1. Among them, 
#'           1 represents the occurrence of the event.
#'       
#'@export       
#'
#'@references Riley RD, Ensor J, Snell KIE, Harrell FE, Martin GP, Reitsma JB, et al. Calculating the sample size required for 
#'            developing a clinical prediction model. BMJ (Clinical research ed). 2020
#'
#'@examples
#'data<-data.frame(x=c(rep(0, 50), rep(1, 50)),y=x)
#'fit<-glm(y~x,family = binomial(link = "logit"),data=data)
#'Maddala.Cox.Snell(fit)
#'       