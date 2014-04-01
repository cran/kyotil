roundup=function (value, digits) {
    format(round(value, digits), nsmall=digits, scientific=FALSE) 
}
format.int=function (x, digits, fill="0", ...) {
    formatC(x, format="d", flag=fill, width=digits) 
}

# don't have to transpose x
mywrite=function(x, ...){
    if (is.list(x)) x=fill.jagged.array(x)
    if (is.null(ncol(x))) i=length(x)
    else i=ncol(x)
    write (t(x), ncolumns=i, ...)
}

# print a matrix/table or a list of them to a latex file as xtable
# note file.name can not have space in it
# e.g. mytex(matrix(0,2,2));
# e.g. mytex(matrix(0,2,2), digits=4);
# e.g. mytex(list(matrix(0,2,2), c(1,1))); 
# default arguments: file.name="temp"; digits=NULL; display=NULL; align="r"; append=FALSE; preamble=""; keep.row.names=TRUE
mytex.begin=function(file.name,preamble=""){
#    if(exists("tablePath") && file.exists(tablePath)) {
#        file.name=tablePath%+%"/"%+%file.name
#    } else {
#        file.name=file.name
#    }    
    if (!endsWith(file.name,".tex")) file.name=file.name%+%".tex"
    cat ("\\documentclass{article}\n", file=file.name, append=FALSE)
    cat (preamble, file=file.name, append=TRUE)
    cat("\n\\usepackage{geometry}\n", file=file.name, append=TRUE)    
    cat("\n\\begin{document}\n", file=file.name, append=TRUE)    
}
mytex.end=function(file.name){
#    if(exists("tablePath") && file.exists(tablePath)) {
#        file.name=tablePath%+%"/"%+%file.name
#    } else {
#        file.name=file.name
#    }    
    if (!endsWith(file.name,".tex")) file.name=file.name%+%".tex"
    cat ("\n\\end{document}", file=file.name, append=TRUE)
}
mytex=function(dat=NULL, file.name="temp", digits=NULL, display=NULL, align="r", append=FALSE, preamble="", include.rownames=TRUE, include.dup.rownames=FALSE, 
    floating=FALSE, lines=TRUE, ...) {
    
#    if(exists("tablePath") && file.exists(tablePath)) {
#        file.name=tablePath%+%"/"%+%file.name
#    } else {
#        file.name=file.name
#    }    

    if (include.dup.rownames) include.rownames=F
    
    if (!endsWith(file.name,".tex")) file.name=file.name%+%".tex"
    
    if(is.data.frame(dat)) dat=list(dat)
    if (!is.list(dat)) dat=list(dat)
    if (!append) {
        mytex.begin(file.name, preamble)
    } 
#    else {
#        fil1 = file(file.name, "r")
#        # copy content to a new file until hit \end{document}, assuming that is the last line
#        n=as.numeric(strsplit(system ("wc -l "%+%file.name, intern=TRUE), " ")[[1]][1])
#        print(n)
#        tmp = readLines(fil1, n-1)
#        close(fil1)
#        cat (concatList(tmp, "\n"), file=file.name, append=FALSE)
#    }
    
    if (length(dat)>0) {
        names(dat)=gsub("_"," ",names(dat))
        for (i in 1:length(dat)) {
            cat ("\\leavevmode\\newline\\leavevmode\\newline\n", file=file.name, append=TRUE)
            dat1 = dat[[i]]        
            .ncol=ncol(dat1)
            if (is.null(.ncol)) {
                if (is.null(nrow(dat1))) .ncol=1
                else .ncol=nrow(dat1)
            }
            
            if (!is.matrix(dat1) & is.character(dat1)) {
                cat (dat1%+%"\n\n\n", file=file.name, append=TRUE)
            } else {        
                if (is.vector(dat1)) dat1=as.matrix(dat1)
                
                cat (names(dat)[i]%+%"\n\n", file=file.name, append=TRUE)
                if (!is.null(dat1)) {
                    if (!is.null(attr(dat1,"caption"))) caption=attr(dat1,"caption") else caption=NULL
                    
                    if (include.dup.rownames & !is.null(rownames(dat1))) {
                        tmp=data.frame(row = rownames(dat1),data.frame(dat1))
                        colnames(tmp)[-1]=colnames(dat1)
                        dat1=tmp
                        .ncol=.ncol+1
                    }
                    if (lines) hline.after=c(-1,0,nrow(dat1)) else hline.after=c()
                    if (length(align)==1) align=rep(align,.ncol+1)
                    print(..., xtable::xtable(dat1, 
                        digits=(if(is.null(digits)) rep(3, .ncol+1) else digits), # cannot use ifelse here!!!
                        display=(if(is.null(display)) rep("f", .ncol+1) else display), # or here
                        align=align, caption=caption, ...), 
                        hline.after=hline.after,
                            type = "latex", file = file.name, append = TRUE, floating = floating, include.rownames=include.rownames )
                }
                cat ("\n", file=file.name, append=TRUE)
            }
        
        }
    }
    
    if(!append) mytex.end(file.name)
    cat ("Saving data to "%+%getwd()%+%"/"%+%file.name%+%"\n")
}
#x=matrix(0,2,2)
#attr(x,"caption")="cap"
#mytex(x, floating=TRUE)


# write a table that contains mean and sd to temp.tex in the current working directory, getwd()
# models can be a list of models, or a single model
make.latex.coef.table = function (models, model.names=NULL, row.major=FALSE, round.digits=NULL) {
# e.g.: models=list(gam1, gam2); round.digits= c(3,3,3,3,3); model.names=c("gam1", "gam2");  row.major=TRUE   
    if (! ("list" %in% class (models) ) ) {models=list(models)}
    
    numParams = nrow (getFixedEf(models[[1]]))
    numModels = length (models)
    
    if (is.null (model.names)) {model.names=rep("",numModels)}
    if (is.null(round.digits)) round.digits=rep(3,numParams)    
    
    coef.table = mysapply (1:numModels, function (i.model) {
        temp = getFixedEf(models[[i.model]]) [,1:2,drop=FALSE]
        for (i.param in 1:numParams) {
            temp[i.param,] = round (temp[i.param,], round.digits[i.param])
        }
        temp2 = paste (format(temp[,1]), "(", format(temp[,2]), ")")
        names (temp2) = dimnames(temp)[[1]]
        temp2
    })
    dimnames (coef.table)[[1]] = model.names
    
    if (row.major) mytex ( coef.table, align="r" ) 
    else mytex (t(coef.table), align="r") 
}


# default row.names to FALSE
# file name needs no file extension
mywrite.csv = function(x, file="tmp", row.names=FALSE, digits=NULL, ...) {  
    if (!is.null(digits)) {
        if(length(digits)==1) {
            x=round(x,digits)
        } else {
            for (i in 1:ncol(x)) {
                x[,i]=round(x[,i], digits[i])
            }                
        }
    }
    print("Writing csv file to "%+%getwd()%+%"/"%+%file%+%".csv", quote=FALSE)
    write.csv(x, file=file%+%".csv", row.names=row.names, ...)
}


myprint <- function(object, ...) UseMethod("myprint") 

# this function is placed at the bottom of the file because it contains "\""), which makes all the following line be miss-interpreted as being in quotes
myprint.default = function (..., newline=TRUE, digits=3) {   
    digits.save=getOption("digits")
    options(digits=digits)
    object <- as.list(substitute(list(...)))[-1]
    x=list(...)
    for (i in 1:length(x)) {
        if (is(x[[i]],"formula")) {cat(as.character(x[[i]])); next}
        tmpname <- deparse(object[[i]])[1]
        #str(tmpname)
        #str(gsub("\\\\","\\",gsub("\"", "", tmpname)))
        #str(x[[i]])
        #if (gsub("\\\\","\\",gsub("\"", "", tmpname))!=x[[i]]) {
        if (contain(tmpname, "\"") | contain(tmpname, "\\")) {
            for (a in x[[i]]) cat(a)
        } else {
            cat (tmpname %+% " = ")
            for (a in x[[i]]) cat(a)
            if (i!=length(x)) cat ("; ")
        }
    }
    if (newline)  cat("\n")
    options(digits=digits.save)
}
#a="hello"; b="you"; myprint (a, b); myprint ("test"); myprint.default ("\t")
