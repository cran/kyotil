\name{marginal.risk}
\alias{marginal.risk}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute maringalized risk
}
\description{
Computes risk of disease as a function of marker s by marginalizing over a covariate vector Z.
}
\usage{
marginal.risk(fit.risk, fit.s, data, marker.name, weights = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fit.risk}{
A regression object, e.g. with formula y~Z+marker. Need to support predict(fit.risk)
}
  \item{fit.s}{
A linear regression object, e.g. with formula marker~Z. Need to support predict(fit.risk) and summary(fit.s)$sigma. Make sure fit.risk and fit.s are fit using the same data (e.g. by taking care of missing data)
}
  \item{data}{
A data frame
}
  \item{marker.name}{
A character string
}
  \item{weights}{
Inverse prob sampling weight, optional
}
}
\value{
A matrix of two columns named marker and prob.
}
\details{
See the vignette file for more details.
}
\references{
Gilbert, P. B., Gabriel, E. E., Miao, X., Li, X., Su, S. C., Parrino, J., Chan, I. S. (2014). Fold rise in Antibody Titers by gpELISA is an Excellent Correlate of Protection for a Herpes Zoster Vaccine, Demonstrated via the Vaccine Efficacy Curve. J. Infect. Dis, 210, 1573-1581.
}
\examples{


\dontrun{

markers="EIA.log10d14overd0"
data=subset(dat.wide, trt==1)
formula=y2~riskScore.mat.endpoint1+vacc2birth

for (marker.name in markers) {
    dat.tmp=data[!is.na(data[[marker.name]]), ] # make sure there is no missing data
    fit.risk=glm(update(formula, as.formula(paste0("~.+",marker.name))), dat.tmp, family=binomial)
    fit.s=lm(update(formula, as.formula(paste0(marker.name,"~."))), dat.tmp) 
    res=marginal.risk(fit.risk, fit.s, dat.tmp, marker.name)
    
    plot(prob~marker, res, type="l", xlab=marker.name, ylab="probability of endpoint 2")
    abline(h=mean(data$y2), col="gray", lty=2)
}

}

}
