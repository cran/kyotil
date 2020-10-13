# only pass ph2 data to this function
marginal.risk=function(fit.risk, fit.s, data, ss=NULL, weights=rep(1, nrow(data)), t=NULL) {
    marker.name= as.character(fit.s$terms[[2]])
    ss.is.null=is.null(ss) 
    if (ss.is.null) ss=quantile(data[[marker.name]], seq(.05,.95,by=0.01))
        
    dat.tmp=data
    if (!is.null(t)) {
        time.var=fit.risk$terms[[2]][[2]]
        dat.tmp[[time.var]]=t
    }
    
    risks=sapply(ss, function(s) {
        f.s.zi = dnorm(s, mean=predict(fit.s), sd=summary(fit.s)$sigma)
        dat.tmp[[marker.name]]=s    
        risks = if(is.null(t)) {
            # glm
            predict(fit.risk, newdata=dat.tmp, type="response")
        } else {
            # coxph survival prob
            1 - exp(-predict(fit.risk, newdata=dat.tmp, type="expected"))
        }
        sum(weights * risks  * f.s.zi) / sum(weights*f.s.zi)    
    })
    
    if (ss.is.null) cbind(marker=ss, prob=risks) else risks
}



# bootstrap 
marginal.risk.boot=function(formula, marker.name, data, B, ci.type="quantile", numCores=1) {  
    
    # store the current rng state 
    save.seed <- try(get(".Random.seed", .GlobalEnv), silent=TRUE) 
    if (class(save.seed)=="try-error") {set.seed(1); save.seed <- get(".Random.seed", .GlobalEnv) }      
    
    ss=quantile(data[[marker.name]], seq(.05,.95,by=0.01))
    n=row(data)
    
    dat.tmp=data
    fit.risk=glm(update(formula, as.formula(paste0("~.+",marker.name))), dat.tmp, family=binomial)
    fit.s=lm(update(formula, as.formula(paste0(marker.name,"~."))), dat.tmp) 
    prob=marginal.risk(fit.risk, fit.s, dat.tmp, ss=ss)
    
    out=mclapply(1:B, mc.cores = numCores, FUN=function(seed) {   
        set.seed(seed)    
        dat.tmp=data[sample(n, replace=TRUE),]
        fit.risk=glm(update(formula, as.formula(paste0("~.+",marker.name))), dat.tmp, family=binomial)
        fit.s=lm(update(formula, as.formula(paste0(marker.name,"~."))), dat.tmp) 
        marginal.risk(fit.risk, fit.s, dat.tmp, ss=ss)
    })
    res=do.call(cbind, out)
    
    # restore rng state 
    assign(".Random.seed", save.seed, .GlobalEnv)    
    
    if (ci.type=="quantile") {
        ci.band=t(apply(res, 1, function(x) quantile(x, c(.025,.975))))
    } else {
        stop("only quantile bootstrap CI supported for now")
    }
    
    list(marker=ss, prob=prob, boot=res, lb=ci.band[,1], ub=ci.band[,2])     
}    


#marginal.risk=function(fit.risk, fit.s, data, t=NULL, ss=NULL, weights=NULL) {
#    marker.name= as.character(fit.s$terms[[2]])
#    ss.is.null=is.null(ss) 
#    if (ss.is.null) ss=quantile(data[[marker.name]], seq(.05,.95,by=0.01), na.rm=T)
#    risks=sapply(ss, function(s) {
#        f.s.zi = dnorm(s, mean=predict(fit.s), sd=summary(fit.s)$sigma)
#        dat.tmp=data
#        dat.tmp[[marker.name]]=s    
#        dat.tmp[["EventTime"]]=t
#        if (is.null(weights)) weights=rep(1, nrow(data))
#        sum(weights* (1-exp(-predict(fit.risk, newdata=dat.tmp, type="expected"))) * f.s.zi) / sum(weights*f.s.zi)    
#    })
#    if (ss.is.null) cbind(marker=ss, prob=risks) else risks
#}
#
#
