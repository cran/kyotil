marginal.risk=function(fit.risk, fit.s, data, marker.name, weights=NULL) {
    ss=quantile(data[[marker.name]], seq(.05,.95,by=0.01))
    risks=sapply(ss, function(s) {
        f.s.zi = dnorm(s, mean=predict(fit.s), sd=summary(fit.s)$sigma)
        dat.tmp=data; dat.tmp[[marker.name]]=s    
        if (is.null(weights)) weights=rep(1, nrow(data))
        sum(weights*predict(fit.risk, newdata=dat.tmp, type="response") * f.s.zi) / sum(weights*f.s.zi)    
    })
    cbind(marker=ss, prob=risks)
}
