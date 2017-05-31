\name{VEplot}
\alias{VEplot}
\alias{VEplot.cox.zph}
\alias{myplot.cox.zph}
\title{
Vaccine Efficacy Plots}
\description{
Vaccine efficacy plots. 
}
\usage{

VEplot (object, ...) 
 
\method{VEplot}{cox.zph} (object, resid = TRUE, se = TRUE, df = 4, nsmo = 40, 
    var, ylab="VE", xlab="Time", xaxt="s", cex.axis=1, ...) 

myplot.cox.zph (x, resid = TRUE, se = TRUE, df = 4, nsmo = 40, var, 
    coef.transform=NULL, 
    ylab=NULL, 
    xlab="Time", xaxt="s", cex.axis=1, 
    ...) 

}

\arguments{

  \item{object}{An object}
  \item{x}{An object of type cox.zph}
  \item{resid}{Boolean, whether to plot residuals}
  \item{se}{Boolean, whether to plot confidence band}
  \item{df}{degrees of freedom}
  \item{nsmo}{number of points used to plot the fitted spline}
  \item{var}{estimated variance matrix from the Cox model fit}
  \item{xlab}{x label}
  \item{xaxt}{x axis}
  \item{cex.axis}{cex for axis}
  \item{ylab}{y label}
  \item{coef.transform}{a function to transform Cox hazard ratio estimate}
  \item{...}{additional parameters}
  
}

\details{
VEplot and myplot.cox.zph are extensions of survival::plot.cox.zph to plot VE curve and other transformations.

myplot.cox.zph adds the following parameters to the original list of parameters in plot.cox.zph:
    coef.transform: a function to transform the coefficients
    ylab: y axis label 
    xlab: x axis label
}
\author{
Youyi Fong, Dennis Chao
}
\references{
Durham, L. Kathryn, et al. "Estimation of vaccine efficacy in the presence of waning: application to cholera vaccines." American Journal of Epidemiology 147.10 (1998): 948-959.
}
\examples{

library(survival)
vfit <- coxph(Surv(time,status) ~ trt + factor(celltype) + 
              karno + age, data=veteran, x=TRUE) 
temp <- cox.zph(vfit) 

par(mfrow=c(2,2))
for (v in c("trt","age")) {
    VEplot(temp, var=v, resid=FALSE, main=v, ylab="VE", cex.axis=1.5)
    plot(temp, var=v, resid=FALSE, main=v)
}

}
