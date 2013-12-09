.as.double <- function(x, stripAttributes=FALSE){
    if(!stripAttributes){
        if(is.double(x)) return(x)   # no duplicate copy
        storage.mode(x) <- 'double'  # yes duplicate copy
        return(x)
    }
    return(as.double(x))             # duplicate copy if has attributes or not double 
}

DXD <- function(d1,X,d2){ 
    
    if(NROW(d1) == NCOL(d1)) d1 <- diag(d1) # reduced to diagonal of the matrix
    if(NROW(d2) == NCOL(d2)) d2 <- diag(d2) # reduced to diagonal of the matrix
    
    n<-length(d1)
    
    if((NROW(X) != n) || (NCOL(X) != n)) 
    if(!is.matrix(X)) X <- as.matrix(X)
    
    
    d1 <- .as.double(d1) # critical to use .as.double here, otherwise it will try to copy
    d2 <- .as.double(d2)
    X  <- .as.double(X)
    
    .C("dxd",n,d1,X,d2,dxd = X*0.0,DUP = FALSE,NAOK = FALSE)$dxd
}


symprod <- function(S,X){

    if(!is.matrix(S) || !is.matrix(X)) stop("Both 'S' and 'X' have to be matrices.")
    S <- .as.double(S)
    X <- .as.double(X)
    Y <- X * 0.0
    if(
        (NCOL(S) != NROW(X)) || 
        (NCOL(S) != NROW(S)) || 
        !all(dim(X) == dim(Y))
    ) stop("Dimension mismatch")
    
    res <- .C( 
        "symprod",  
        M = NROW(Y),
        N = NCOL(Y),
        A = .as.double(S),
        B = as.double(X),
        C = Y
    ) 

    # res <- .Fortran( 
        # "dsymm",  
        # SIDE = 'L',
        # UPLO = 'U',
        # M = NROW(Y),
        # N = NCOL(Y),
        # alpha = as.double(1),
        # A = .as.double(S),
        # LDA = NROW(S),
        # B = as.double(X),
        # LDB = NROW(X),
        # beta = as.double(0),
        # C = Y,
        # LDC = NROW(Y) 
    # )     
    res$C
}


txSy <- function(x,S,y){
    n <- length(x)
    
    if( (n != NROW(S)) || (n != NCOL(S)) || (n != length(y)) )stop("Dimension mismatch")
    
    .C('txSy',length(x),.as.double(S),.as.double(x),.as.double(y),double(n),out=double(1))$out
}
