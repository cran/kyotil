getK=function(X,kernel,para=NULL,X2=NULL,C = NULL){    
    
    kernel <- substr(tolower(kernel[1]),1,1)
    
    if (is.null(C)) {
        # by default, we set C=TRUE for ibs and hamming kernel b/c the R implementation uses nested loop and can be slow
        if (kernel %in% c("i","h")) C=TRUE else C=FALSE
    }    
    
    if(C) return(getKernel(x=X,kernel=kernel,para = para,x2=X2))
    
    if (kernel=="r" | kernel=="e") {
        if (!is.null(X2)) {
            aux = X[rep(1:nrow(X),nrow(X2)),,drop=FALSE] - X2[rep(1:nrow(X2),each=nrow(X)),,drop=FALSE]
            dist.mat = matrix(rowSums(aux^2), nrow=nrow(X))
#            aux=X2[rep(1:nrow(X2),nrow(X)),] - X[rep(1:nrow(X),each=nrow(X2)),]
#            dist.mat = matrix(rowSums(aux^2), nrow=nrow(X2))
        } else {
            dist.mat = as.matrix(dist(X))^2
        }
    }
    
    if (is.null(X2)) X2=X
    switch(kernel, 
        p=(tcrossprod(X,X2)+1)^para, # polynomial
        r=exp(-para*dist.mat), # rbf
        e=dist.mat, # Euclidean distance
        l=tcrossprod(X,X2), # linear
        i = ibs(X,X2,para,C = FALSE), # IBS 
        h = hamming.sim(X,X2,para,C = FALSE),
        stop(kernel %+% " kernel not supported")
    )
    
}



# Author: Krisztian Sebestyen <ksebestyen@gmail.com>


getKernel <- function(x,kernel = c("linear","euclidean","polynomial","rbf","ibs","hamming"),para = NULL,x2 = NULL){
    
    # note: the order of the kernels to be matched against must be exactly as below to match C
    kernel <- substr(tolower(kernel[1]),1,1)
    if(!any(kernel == c("l","e","i","h")) && is.null(para)) stop("kernel parameter is not set")
    
    y <- x2
    if(is.null(y)) y <- x
    if(!is.matrix(x)) x <- as.matrix(x)
    if(!is.matrix(y)) y <- as.matrix(y)
    dx <- dim(x)
    dy <- dim(y)    
    x <- as.double(x)
    y <- as.double(y)
    dim(x) <- c(dx[1] , dx[2])
    dim(y) <- c(dy[1] , dy[2])
    
    # para = weights for 'hamming' and 'ibs'
    if(is.null(para)){  
        if(any(kernel == c("i","h"))){
            para <- as.double(-1.0)
            # para <- rep(1.0,min(c(dx[2],dy[2])))  
        }else{
            para <- double(1)
        }
    }else{
        if(any(kernel == c("i","h")))
            para <- as.double(rep(para,length.out = min(c(dx[2],dy[2]))))
    }
    
    kernel <- match(kernel,c("l","e","p","r","i","h"),nomatch = 0)-1
    if(kernel == -1) stop("Invalid kernel selected.")
    
#    K <- .C("getKernel",dx[1],dx[2],x,dy[1],dy[2],y,kernel = as.integer(kernel),para = para,K = double(dx[1] * dy[1]),DUP = FALSE)$K
#    dim(K) <- c(dx[1] , dy[1])
#    K

    .Call("getKernel2", x, y, kernel = as.integer(kernel), para = para)
}

R_ibsX <- function(x,para = NULL,order = 2){
    n <- nrow(x)
    K <- matrix(NA,n,n)
    S <- ncol(x)
    
    if(is.null(para)){
        for(i in 1:n)
            for(j in i:n)
                    K[i,j] <- sum(order - abs(x[i,]-x[j,])) / (2.0 * S)
    }else{
        w <- para
        sum.w <- sum(w)
        for(i in 1:n)
            for(j in i:n)
                    K[i,j] <- sum(w * (order - abs(x[i,]-x[j,]))) / sum.w       
    }
    
    for(i in 1:(n-1))
        for(j in (i+1):n)
                K[j,i] <- K[i,j]
    K   
}

R_ibsXY <- function(x1,x2,para = NULL,order = 2){
    n1 <- nrow(x1)
    n2 <- nrow(x2)
    K <- matrix(NA,n1,n2)
    S <- ncol(x1)
    if(is.null(para)){
        for(i in 1:n1)
            for(j in 1:n2)
                K[i,j] <- sum(order - abs(x1[i,]-x2[j,]))/(2.0 * S)
    }else{
        w <- para
        sum.w <- sum(w)
        for(i in 1:n1)
            for(j in 1:n2)
                    K[i,j] <- sum(w * (order - abs(x1[i,]-x2[j,]))) / sum.w     
    }
    K   
}

R_ibs <- function(x,x2 = NULL,para = NULL,order = 2){
    if(!is.null(x2)) return(R_ibsXY(x,para = para,x2 = x2,order = order))
    R_ibsX(x,para = para,order = order)
}

# if 'x' and 'x2' have differing column dimensions, the firts 1..min(ncol(x),ncol(x2)) are used
ibs <- function(x,x2 = NULL,para = NULL,C = TRUE,order = 2){
    if(!C) return(R_ibs(x,para=para,x2=x2,order = order)) else stop("try calling getKernel for using C implementation")
    
#    if(!is.matrix(x))x<-as.matrix(x)
#    dx <- dim(x)
#    x <- as.double(x)
#    if(is.null(x2)){
#        # para = weights for 'hamming' and 'ibs'
#        if(is.null(para)){  
#            para <- as.double(-1.0) 
#        }else{
#            para <- as.double(rep(para,length.out = dx[2],dx2[2]))
#        }
#        return(.C("ibsXY_kernel",dx[1],dx[2],x,dx[1],dx[2],x2,para,order = as.integer(order),K = matrix(0.0,dx[1],dx2[1]),DUP = FALSE)$K)
#    }
#        
#    if(!is.matrix(x2))x2<-as.matrix(x2)
#    dx2 <- dim(x2)
#    x2 <- as.double(x2)
#
#    # para = weights for 'hamming' and 'ibs'
#    if(is.null(para)){  
#        para <- as.double(-1.0) 
#        # para <- rep(1.0,min(c(dx[2],dx2[2]))) 
#    }else{
#        para <- as.double(rep(para,length.out = min(c(dx[2],dx2[2]))))
#    }       
#    return(.C("ibsXY_kernel",dx[1],dx[2],x,dx[1],dx[2],x2,para,order = as.integer(order),K = matrix(0.0,dx[1],dx2[1]),DUP = FALSE)$K)
}

R_hamming.dist <- function(x1,x2 = NULL,para = NULL){
    if(is.null(x2))x2 <- x1
    n1 <- nrow(x1)
    n2 <- nrow(x2)
    K <- matrix(NA,n1,n2)
    
    if(is.null(para)){
        for(i in 1:n1)
            for(j in 1:n2)
                K[i,j] <- sum(x1[i,] == x2[j,])
    }else{
        w <- para
        sum.w <- sum(w)
        for(i in 1:n1)
            for(j in 1:n2)
                    K[i,j] <- sum(w * (x1[i,] == x2[j,])) / sum.w       
    }
    K   
}

# hamming simmilarity (as opposed to dissimilarity) - binary data
hamming.sim <- function(x,x2 = NULL,para = NULL,C = TRUE){
    if(!C)return(R_hamming.dist(x,para=para,x2=x2)) else stop("try calling getKernel for using C implementation")
    
#    if(!is.matrix(x))x<-as.matrix(x)
#    dx <- dim(x)
#    x <- as.double(x)   
#    if(is.null(x2)){
#        # para = weights for 'hamming' and 'ibs'
#        if(is.null(para)){  
#            para <- as.double(-1.0)
#        }else{
#            para <- as.double(rep(para,length.out = dx[2]))
#        }       
#        return(.C("ibsX_kernel",dx[1],dx[2],x,para,order=as.integer(1),K = matrix(0.0,dx[1],dx2[1]),DUP = FALSE)$K)
#    }
#        
#    if(!is.matrix(x2))x2<-as.matrix(x2)
#    dx2 <- dim(x2)
#    x2 <- as.double(x2) 
#    
#    # para = weights for 'hamming' and 'ibs'
#    if(is.null(para)){  
#        para <- as.double(-1.0)
#        # para <- rep(1.0,min(c(dx[2],dx2[2]))) 
#    }else{
#        para <- as.double(rep(para,length.out = min(c(dx[2],dx2[2]))))
#    }   
#    return(.C("ibsXY_kernel",dx[1],dx[2],x,dx2[1],dx2[2],x2,para,order=as.integer(1),K = matrix(0.0,dx[1],dx2[1]),DUP = FALSE)$K)
}




# R's 'euclidean' dist()^2
edist2 <- function(x,y = NULL){
    if(is.null(y)) y <- x
    if(!is.matrix(x)) x <- as.matrix(x)
    if(!is.matrix(y)) y <- as.matrix(y)
    dx <- dim(x)
    dy <- dim(y)
    x <- as.double(x)
    y <- as.double(y)
    dim(x) <- c(dx[1] , dx[2])
    dim(y) <- c(dy[1] , dy[2])

#    d <- .C("edist",dx[1],dx[2],x,dy[1],dy[2],y,dist = double(dx[1] * dy[1]),DUP = FALSE)$dist
#    dim(d) <- c(dx[1] , dy[1])
#    d

    .Call("edist2new", x, y)
}
