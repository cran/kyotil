# one issue with myfigure/mydev.off is that positioning of legend depends on the graphical window size in R
# bg is needed b/c by default the bg is transparent
myfigure=function (mfrow=c(1,1), mfcol=NULL, width=NULL, height=NULL, oma=NULL, mar=NULL, main.outer=FALSE, bg=NULL) {        
    if (!is.null(mfcol)) {
        nrow=mfcol[1]; ncol=mfcol[2]        
    } else {
        nrow=mfrow[1]; ncol=mfrow[2]
    }        
    if(is.null(width) | is.null(height))  tmp=get.width.height(nrow,ncol) else tmp=c(width,height)
 #   unlockBinding(".mydev", getNamespace("kyotil")) #won't pass rcmdcheck 
    eval(eval(substitute(expression(.mydev <<- list(width=tmp[1],height=tmp[2])))))             
    
    if (!is.null(mfcol)) par(mfcol=mfcol) else par(mfrow=mfrow)    
    #needed for dev.copy
    dev.control(displaylist = "enable")
    if (!is.null(oma)) par(oma=oma)
    if (!is.null(mar)) par(mar=mar)    
    if (main.outer) {
        tmp=par()$oma
        tmp[3]=tmp[3]+1
        par(oma=tmp)
    }    
    if(!is.null(bg)) par(bg=bg)
}
mydev.off=function(file="temp", ext=c("png","pdf","tiff","eps"), res=200, mydev=NULL) {        
    if (!is.null(mydev)) .mydev=mydev
    exts=strsplit(ext, ",")[[1]]
    tmp=strsplit(file,"/")[[1]]
    for (ext in exts) {
        if (ext=="pdf") {
            subfolder=concatList(c(tmp[-length(tmp)], "pdf"), sep="/")
            filename=if(file.exists(subfolder))  subfolder%+%"/"%+%last(tmp) else file
            dev.copy(pdf,        file=filename%+%"."%+%ext, width=.mydev$width, height=.mydev$height, paper="special")
            cat("Saving figure to "%+%paste(getwd(),"/",filename,sep="")%+%"."%+%ext%+%"\n")        
        } else if (ext=="eps") {
            subfolder=concatList(c(tmp[-length(tmp)], "eps"), sep="/")
            filename=if(file.exists(subfolder))  subfolder%+%"/"%+%last(tmp) else file
            dev.copy(postscript, file=filename%+%"."%+%ext, width=.mydev$width, height=.mydev$height, paper="special", horizontal=FALSE)
            cat("Saving figure to "%+%paste(getwd(),"/",filename,sep="")%+%"."%+%ext%+%"\n")        
        } else if (ext=="png") {
            subfolder=concatList(c(tmp[-length(tmp)], "png"), sep="/")
            filename=if(file.exists(subfolder))  subfolder%+%"/"%+%last(tmp) else file
            dev.copy(png,    filename=filename%+%"."%+%ext, width=.mydev$width, height=.mydev$height, units="in", res=res)
            cat("Saving figure to "%+%paste(getwd(),"/",filename,sep="")%+%"."%+%ext%+%"\n")        
        } else if (ext=="tiff") {
            subfolder=concatList(c(tmp[-length(tmp)], "tiff"), sep="/")
            filename=if(file.exists(subfolder))  subfolder%+%"/"%+%last(tmp) else file
            dev.copy(tiff,   filename=filename%+%"."%+%ext, width=.mydev$width, height=.mydev$height, units="in", res=res, compression="jpeg")
            cat("Saving figure to "%+%paste(getwd(),"/",filename,sep="")%+%"."%+%ext%+%"\n")        
        }
        dev.off()
    }
}

get.width.height=function(nrow,ncol){
    if (nrow==1 & ncol==1) {width=6.7; height=6.7
    } else if (nrow==1 & ncol==2) {width=9.7; height=5.2
    } else if (nrow==1 & ncol==3) {width=9.7; height=3.4
    } else if (nrow==1 & ncol==4) {width=14; height=3.4
    } else if (nrow==2 & ncol==3) {width=9.7; height=6.7
    } else if (nrow==4 & ncol==6) {width=15; height=10
    } else if (nrow==2 & ncol==4) {width=13; height=6.7
    } else if (nrow==3 & ncol==6) {width=17.5; height=9
    } else if (nrow==3 & ncol==7) {width=17.5; height=7
    } else if (nrow==4 & ncol==8) {width=17.5; height=9
    } else if (nrow==4 & ncol==7) {width=17.5; height=9
    } else if (nrow==4 & ncol==9) {width=20; height=9
    } else if (nrow==3 & ncol==5) {width=15; height=9.6
    } else if (nrow==3 & ncol==4) {width=12; height=9.6
    } else if (nrow==4 & ncol==5) {width=15; height=12.5
    } else if (nrow==5 & ncol==6) {width=9; height=8.3
    } else if (nrow==2 & ncol==2) {width=8; height=8.5
    } else if (nrow==3 & ncol==3) {width=9.7; height=10.3
    } else if (nrow==4 & ncol==4) {width=9.7; height=10.3
    } else if (nrow==6 & ncol==5) {width=18; height=17
    } else if (nrow==5 & ncol==5) {width=15; height=15
    } else if (nrow==5 & ncol==3) {width=9; height=15
    } else if (nrow==4 & ncol==2) {width=6; height=13
    } else if (nrow==6 & ncol==3) {width=9; height=19
    } else if (nrow==7 & ncol==3) {width=9; height=22
    } else if (nrow==8 & ncol==5) {width=10; height=16
    } else if (nrow==6 & ncol==4) {width=12; height=19
    } else if (nrow==7 & ncol==5) {width=18; height=19
    } else if (nrow==5 & ncol==4) {width=12; height=15
    } else if (nrow==2 & ncol==1) {width=6.7; height=9.7
    } else if (nrow==3 & ncol==1) {width=10; height=9.7
    } else if (nrow==5 & ncol==1) {width=5; height=13
    } else if (nrow==3 & ncol==2) {width=6.7; height=10.3
    } else if (nrow==4 & ncol==3) {width=9; height=12
    } else stop ("nrow x ncol not supported: "%+%nrow%+%" x "%+%ncol)
    return(c(width,height))
}

##test

#mypdf(mfrow=c(1,3),file="test1x3");plot(1:10,main="LUMX",xlab="t",ylab="y");plot(1:10);plot(1:10);dev.off()
#mypdf(mfrow=c(2,3),file="test2x3");plot(1:10,main="LUMX",xlab="t",ylab="y");plot(1:10);plot(1:10);plot(1:10);plot(1:10);plot(1:10);plot(1:10);dev.off()
#mypdf(mfrow=c(4,4),file="test4x4");plot(1:10,main="LUMX",xlab="t",ylab="y");plot(1:10);plot(1:10);plot(1:10);plot(1:10);plot(1:10);plot(1:10);plot(1:10);plot(1:10);plot(1:10);plot(1:10);plot(1:10);plot(1:10);plot(1:10);plot(1:10);plot(1:10,main="Luminex");dev.off()
#mypdf(mfrow=c(1,1),file="test1x1", );plot(1:10,main="LUMX",xlab="t",ylab="y");dev.off()

#    # use convert.ext to convert one format to another
#    # get file name relative to getwd()
#    # .Devices sometimes have "null device", sometimes have "windows"
#    tmp=which(sapply(.Devices, function(x) x=="pdf"))
#    if(length(tmp)==1) {
#        filename=attr(.Devices[[tmp]],"filepath")         
#    }
#    # close pdf device or default device
#    dev.off()
#    # convert file if needed
#    if(png & length(tmp)==1) {
#        system('"C:/Program Files/ImageMagick-7.0.3-Q16/convert.exe" -resize 2000 -density 200 "'%+%getwd()%+%'/'%+%filename%+%'" "'%+%getwd()%+%'/'%+%fileStem(filename)%+%'.png"')
#    }        

# deprecated
# cannot print both to pdf and tiff or print both to screen and pdf, 
mypdf=function (...) mypostscript(ext="pdf",...)
mypng=function(...) mypostscript(ext="png",...)
mytiff=function(...) mypostscript(ext="tiff",...)
mypostscript=function (file="temp", mfrow=c(1,1), mfcol=NULL, width=NULL, height=NULL, ext=c("eps","pdf","png","tiff"), oma=NULL, mar=NULL,main.outer=FALSE, save2file=TRUE, res=200, ...) {    
    
    ext=match.arg(ext)
    
    if (!is.null(mfcol)) {
        nrow=mfcol[1]; ncol=mfcol[2]        
    } else {
        nrow=mfrow[1]; ncol=mfrow[2]
    }
    
    #if (nrow>4) warning ("nrow > 4 will not fit a page without making the figures hard to see")
    
    # sca controls how much to scale down for use in a paper
    if(is.null(width) | is.null(height))  {
        if (nrow==1 & ncol==1) {width=6.7; height=6.7
        } else if (nrow==1 & ncol==2) {width=9.7; height=5.2
        } else if (nrow==1 & ncol==3) {width=9.7; height=3.4
        } else if (nrow==1 & ncol==4) {width=14; height=3.4
        } else if (nrow==2 & ncol==3) {width=9.7; height=6.7
        } else if (nrow==4 & ncol==6) {width=15; height=10
        } else if (nrow==2 & ncol==4) {width=13; height=6.7
        } else if (nrow==3 & ncol==6) {width=17.5; height=9
        } else if (nrow==3 & ncol==7) {width=17.5; height=7
        } else if (nrow==4 & ncol==8) {width=17.5; height=9
        } else if (nrow==4 & ncol==7) {width=17.5; height=9
        } else if (nrow==4 & ncol==9) {width=20; height=9
        } else if (nrow==3 & ncol==5) {width=15; height=9.6
        } else if (nrow==3 & ncol==4) {width=12; height=9.6
        } else if (nrow==4 & ncol==5) {width=15; height=12.5
        } else if (nrow==5 & ncol==6) {width=9; height=8.3
        } else if (nrow==2 & ncol==2) {width=8; height=8.5
        } else if (nrow==3 & ncol==3) {width=9.7; height=10.3
        } else if (nrow==4 & ncol==4) {width=9.7; height=10.3
        } else if (nrow==6 & ncol==5) {width=18; height=17
        } else if (nrow==5 & ncol==5) {width=15; height=15
        } else if (nrow==5 & ncol==3) {width=9; height=15
        } else if (nrow==4 & ncol==2) {width=6; height=13
        } else if (nrow==6 & ncol==3) {width=9; height=19
        } else if (nrow==7 & ncol==3) {width=9; height=22
        } else if (nrow==8 & ncol==5) {width=10; height=16
        } else if (nrow==6 & ncol==4) {width=12; height=19
        } else if (nrow==7 & ncol==5) {width=18; height=19
        } else if (nrow==5 & ncol==4) {width=12; height=15
        } else if (nrow==2 & ncol==1) {width=6.7; height=9.7
        } else if (nrow==3 & ncol==1) {width=10; height=9.7
        } else if (nrow==5 & ncol==1) {width=5; height=13
        } else if (nrow==3 & ncol==2) {width=6.7; height=10.3
        } else if (nrow==4 & ncol==3) {width=9; height=12
        } else stop ("nrow x ncol not supported: "%+%nrow%+%" x "%+%ncol)
    }    
    
    if(save2file){      
        if (ext=="pdf") {
            pdf (paper="special", file=file%+%"."%+%ext, width=width, height=height, ...)
        } else if (ext=="eps") {
            postscript (paper="special", horizontal=FALSE, file=file%+%"."%+%ext, width=width, height=height, ...)
        } else if (ext=="png") {
            png (filename=file%+%"."%+%ext, width=width, height=height, units="in", res=res, ...)
        } else if (ext=="tiff") {
            tiff (filename=file%+%"."%+%ext, width=width, height=height, units="in", res=res, compression="jpeg", ...)
        }
        cat("Saving figure to "%+%paste(getwd(),"/",file,sep="")%+%"\n")        
    } else {
        print("not saving to file")
    }
    
    if (!is.null(mfcol)) par(mfcol=mfcol)
    else par(mfrow=mfrow)    
    
    if (!is.null(oma)) par(oma=oma)
    if (!is.null(mar)) par(mar=mar)
    
    if (main.outer) {
        tmp=par()$oma
        tmp[3]=tmp[3]+1
        par(oma=tmp)
    }
    
}



# if lty is specified, a line will be drawn
mylegend=function(legend, x, y=NULL, lty=NULL,bty="n", ...) {
    if(is.null(y)) x=switch(x, "topleft", "top", "topright", "left", "center" , "right", "bottomleft", "bottom", "bottomright")
    legend(bty=bty,x=x, y=y, legend=legend, lty=lty, ...)
}


# copied from pairs help page
## put (absolute) correlations on the upper panels,
## with size proportional to the correlations.
panel.cor <- function(x, y, digits=2, prefix="", cex.cor,  ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y, method="pearson", use="pairwise.complete.obs"))
    txt <- format(c(r, 0.123456789), digits=digits)[1]
    txt <- paste(prefix, txt, sep="")
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = ifelse(cex.cor<0,-cex.cor, cex.cor * r) )
}
panel.hist <- function(x, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
}
panel.nothing=function(x, ...) {}
mypairs=function(dat, ...){
    pairs(dat, lower.panel=panel.smooth, upper.panel=panel.cor, diag.panel=panel.hist, ...)
}


getMfrow=function (len) {
    ret=NULL
    if (len==1) { 
        ret=c(1,1)
    } else if (len==2) { 
        ret=c(1,2)
    } else if (len==3) { 
        ret=c(1,3)
    } else if (len<=4) { 
        ret=c(2,2)
    } else if (len<=6) { 
        ret=c(2,3)
    } else if (len<=9) { 
        ret=c(3,3)
    } else if (len<=12) { 
        ret=c(3,4)
    } else if (len<=16) { 
        ret=c(4,4)
    } else if (len<=20) { 
        ret=c(4,5)
    } else if (len<=25) { 
        ret=c(5,5)
    }
    ret
}


empty.plot=function () {
    plot(1,1,type="n",xlab="",ylab="",xaxt="n", yaxt="n", bty="n")
}

# dat
myforestplot=function(dat, xlim=NULL, xlab="", main="", col.1="red", col.2="blue",plot.labels=TRUE,order=FALSE,decreasing=FALSE, vline=TRUE,cols=NULL,log="") {
    if (order) dat=dat[order(dat[,1],decreasing=decreasing),] 
    p=nrow(dat)    
    # makes no plot, but helps set the x axis later
    plot(c(dat[,2], dat[,3]),rep(1:p,2), xlim=xlim, yaxt="n", xaxt="s", xlab=xlab, ylab="", main="", type="n", cex.main=1.4, axes=F, log=log)
    mtext(side=3, line=3, adj=0, text=main, cex=1.4, font=2, xpd=NA)
    if (range(dat[,2:3])[1]>0) null.val=1 else null.val=0 # if all values are greater than 0, 1 is probably the null, otherwise, 0 is probably the null
    if(vline) abline(v=null.val, col="gray")
    if(is.null(cols)) cols=ifelse(dat[,4]<0.05, col.1, col.2)
    points(dat[,1], nrow(dat):1, pch=19, col=cols)
    segments(dat[,2], nrow(dat):1, dat[,3], nrow(dat):1, lwd=2, col=cols)
    axis(1, cex.axis=1.4)
    # add labels
    if (plot.labels) axis(4, at=p:1, rownames(dat), tick=F, las=2, col=1, cex.axis=1, xpd=NA, line=-.5)
}


myboxplot <- function(object, ...) UseMethod("myboxplot") 


# this function may fail sometimes, most likely at eval
# myboxplot.formula and myboxplot.list make a boxplot with data points and do inferences for two group comparions. 
# cex=.5; ylab=""; xlab=""; main=""; box=FALSE; highlight.list=NULL; at=NULL;pch=1;col=1;
myboxplot.formula=function(formula, data, cex=.5, xlab="", ylab="", main="", box=TRUE, at=NULL, na.action=NULL,
    pch=1, col=1, test="", reshape.formula=NULL, jitter=TRUE, add.interaction=FALSE,  drop.unused.levels = TRUE, bg.pt=NULL, ...){
    
    save.seed <- try(get(".Random.seed", .GlobalEnv), silent=TRUE) 
    if (class(save.seed)=="try-error") {        
        set.seed(1)
        save.seed <- get(".Random.seed", .GlobalEnv)
    }                        
    set.seed(1)
    
     # removes empty groups formed through model.frame
     mf=model.frame(formula, data)
     response <- attr(attr(mf, "terms"), "response") 
     tmp.dat=split(mf[[response]], mf[-response])
     if(drop.unused.levels) {
         len.n=sapply(tmp.dat, length)
         tmp.dat=tmp.dat[len.n!=0]
     }
     res=boxplot(tmp.dat, range=0, xlab=xlab, at=at, col=NULL, cex=cex, 
        boxlty=if(!box) 0 else NULL,whisklty=if(!box) 0 else NULL,staplelty=if(!box) 0 else NULL,
        #pars = list(boxwex = if(box) 0.8 else 0, staplewex = if(box) 0.5 else 0, outwex = if(box) 0.5 else 0), 
        main=main, ylab=ylab, ...)
    
    dat.tmp=model.frame(formula, data)
    xx=interaction(dat.tmp[,-1])
    if(drop.unused.levels) xx=droplevels(xx)
    if(is.null(at)){        
        xx=as.numeric(xx)
    } else{
        xx=at[xx]
    }      
    if (add.interaction) jitter=FALSE
    if (jitter) xx=jitter(xx)  
    points(xx, dat.tmp[[1]], cex=cex,pch=pch,col=col, bg=bg.pt)
    
    # restore rng state 
    assign(".Random.seed", save.seed, .GlobalEnv)     
    
    # inference
    x.unique=unique(dat.tmp[[2]])
    if (length(test)>0) {
        sub=""
        pvals=NULL
        if ("t" %in% test) {
            p.val=t.test(formula, data)$p.value
            pvals=c(pvals, Student=p.val)
            sub=sub%+%" Student's t "%+%ifelse(length(test)==1,"p-val ","")%+%signif(p.val,2) 
        }
        if ("w" %in% test) {
            p.val=suppressWarnings(wilcox.test(formula, data)$p.value)
            pvals=c(pvals, Wilcoxon=p.val)
            sub=sub%+%" Wilcoxon "%+%ifelse(length(test)==1,"p-val ","")%+%signif(p.val,2)
        }
        if ("k" %in% test) {
            p.val=kruskal.test(formula, data)$p.value
            pvals=c(pvals, Kruskal=p.val)
            sub=sub%+%" Kruskal "%+%ifelse(length(test)==1,"p-val ","")%+%signif(p.val,2)
        }
        if ("f" %in% test) {
            dat.wide=myreshapewide (reshape.formula, data, idvar = NULL)
            #str(dat.wide)# show this so that we know we are using the right data to do the test
            ftest = friedman.test (as.matrix(dat.wide[,-(1)]))
            p.val=ftest$p.value
            pvals=c(pvals, Friedman=p.val)
            if (add.interaction) my.interaction.plot(as.matrix(dat.wide[,-1]), add=T)
            sub=sub%+%" Friedman "%+%ifelse(length(test)==1,"p-val ","")%+%signif(p.val,2)
        }
        title(sub=sub)
        res$pvals=pvals
    }
    
    invisible(res)
    
}

myboxplot.data.frame=function(object, cex=.5, ylab="", xlab="", main="", box=TRUE, at=NULL, pch=1, col=1, test="", ...){
    myboxplot.list(as.list(object), cex=cex, ylab=ylab, xlab=xlab, main=main, box=box, at=at, pch=pch, col=col, test=test, ...)
}
myboxplot.matrix=function(object, cex=.5, ylab="", xlab="", main="", box=TRUE, at=NULL, pch=1, col=1, test="", ...){
    myboxplot.list(as.list(as.data.frame(object)), cex=cex, ylab=ylab, xlab=xlab, main=main, box=box, at=at, pch=pch, col=col, test=test, ...)
}

myboxplot.list=function(object, ...){
    
    # make a dataframe out of list object
    dat=NULL
    if(is.null(names(object))) names(object)=1:length(object)
    for (i in 1:length(object)) {
        dat=rbind(dat,data.frame(y=object[[i]], x=names(object)[i]))
    }
    myboxplot(y~x, dat, ...)
    
}


# can use after myboxplot
# both dat must have two columns, each row is dat from one subject
# x.ori=0; xaxislabels=rep("",2); cex.axis=1; add=FALSE; xlab=""; ylab=""; pcol=NULL; lcol=NULL
my.interaction.plot=function(dat, x.ori=0, xaxislabels=rep("",2), cex.axis=1, add=FALSE, xlab="", ylab="", pcol=NULL, lcol=NULL, ...){
    if (!add) plot(0,0,type="n",xlim=c(1,2),ylim=range(dat), ylab=ylab, xlab=xlab, xaxt="n", ...)
    cex=.25; pch=19
    if (is.null(lcol)) lcol=ifelse(dat[,1]>dat[,2],"red","black") else if (length(lcol)==1) lcol=rep(lcol,nrow(dat))
    if (!is.null(pcol)) if (length(pcol)==1) pcol=matrix(pcol,nrow(dat),2)
    for (i in 1:nrow(dat)) {
        points (1+x.ori, dat[i,1], cex=cex, pch=pch, col=ifelse(is.null(pcol), 1, pcol[i,1]))
        points (2+x.ori, dat[i,2], cex=cex, pch=pch, col=ifelse(is.null(pcol), 1, pcol[i,2]))
        lines (1:2+x.ori, dat[i,], lwd=.25, col=lcol[i])
    }
    axis(side=1, at=1:2+x.ori, labels=xaxislabels, cex.axis=cex.axis)
}

# called butterfly.plot, because it is meant to plot two treatment arms at two time points, the two arms are plotted in a mirror fashion, see "by analyte.pdf" for an example
# if dat2 is null: dat is matrix with four columns. each row is one subject, the columns will be plotted side by side, with lines connecting values from one ptid
# if dat2 is not null, dat has two columns, which are plotted side by side with lines connecting them, same for dat2
# if add is true, no plot function will be called
butterfly.plot=function (dat, dat2=NULL, add=FALSE, xaxislabels=rep("",4), x.ori=0, xlab="", ylab="", cex.axis=1, ...){
    if (!add) plot(0,0,type="n",xlim=c(1,4),ylim=range(dat), xaxt="n", xlab=xlab, ylab=ylab, ...)
    for (i in 1:nrow(dat)) {
        lines (1:2+x.ori, dat[i,1:2], lwd=.25, col=ifelse(dat[i,1]<=dat[i,2],"red","black"))
        if (is.null(dat2)) {
            lines (2:3+x.ori, dat[i,2:3], lwd=.25, col="lightgreen")
            lines (3:4+x.ori, dat[i,3:4], lwd=.25, col=ifelse(dat[i,3]<=dat[i,4],"black","red"))
        }
    }
    if (!is.null(dat2)) {
        for (i in 1:nrow(dat2)) {
            lines (3:4+x.ori, dat2[i,1:2], lwd=.25, col=ifelse(dat2[i,1]<=dat2[i,2],"black","red"))
        }
    }
    axis(side=1, at=1:4+x.ori, labels=xaxislabels, cex.axis=cex.axis)
}


corplot <- function(object, ...) UseMethod("corplot") 

corplot.default=function(object,y,...){
    dat=data.frame(object,y)
    names(dat)=c("x1", "x2")
    corplot(x2~x1, dat, ...)
}

# col can be used to highlight some points
corplot.formula=function(formula,data,main="",method=c("pearson","spearman"),col=1,cex=.5,add.diagonal.line=TRUE,add.lm.fit=FALSE,col.lm=2,add.deming.fit=FALSE,col.deming=4,add=FALSE,
    log="",same.xylim=FALSE,xlim=NULL,ylim=NULL, ...){
    vars=dimnames(attr(terms(formula),"factors"))[[1]]
    cor.=NULL
    if (length(method)>0) {
        cor.=sapply (method, function (method) {
            cor(data[,vars[1]],data[,vars[2]],method=method,use="p")
        })
        main=main%+%ifelse(main=="","",", ")
        main=main%+%"cor: "%+%concatList(round(cor.,2),"|")
    }

    if (!add) {
        if (same.xylim) {
            xlim=range(model.frame(formula, data))
            ylim=range(model.frame(formula, data))
        }
        plot(formula,data,main=main,col=col,cex=cex,log=log,xlim=xlim,ylim=ylim,...)
    } else {
        points(formula,data,main=main,col=col,cex=cex,log=log,...)
    }
    
    if(add.diagonal.line) abline(0,1)
    if(add.lm.fit) {
        fit=lm(formula, data)
        abline(fit,untf=log=="xy", col=col.lm)
    }
    if(add.deming.fit) {
        # this implementation is faster than the one by Therneau, Terry M.
        fit=Deming(model.frame(formula, data)[[2]], model.frame(formula, data)[[1]]) # this function is in Deming.R copied from MethComp package by Bendix Carstensen
        abline(fit["Intercept"], fit["Slope"], untf=log=="xy", col=col.deming)   
        # Therneau, Terry M.'s implementation in a loose R file that is in 3software folder, slower than Deming, but may be more generalized?
        #fit <- deming(model.frame(formula, data)[[2]], model.frame(formula, data)[[1]], xstd=c(1,0), ystd=c(1,0))
        #abline(fit, untf=log=="xy", col=col.deming)        
    }
    
    invisible(cor.)
}

abline.pts=function(pt1, pt2=NULL){
    if (is.null(pt2)) {
        if (nrow(pt1)>=2) {
            pt2=pt1[2,]
            pt1=pt1[1,]
        } else {
            stop("wrong input")
        }
    }
    slope=(pt2-pt1)[2]/(pt2-pt1)[1]
    intercept = pt1[2]-slope*pt1[1]
    abline(intercept, slope)
}
#abline.pts(c(1,1), c(2,2))

abline.pt.slope=function(pt1, slope, x2=NULL, ...){
    intercept = pt1[2]-slope*pt1[1]
    if (is.null(x2)) {
        abline(intercept, slope, ...)
    } else {
        pt2=c(x2, intercept+slope*x2)
        lines(c(pt1[1],pt2[1]), c(pt1[2],pt2[2]), ...)
    }
    
}

# put a shade in a rectangle between a point and one of the four quadrants
# pt is a vector of two values
# col is red blue gree
abline.shade=function(pt, quadrant=c(1,2,3,4), col=c(0,1,0), alpha=0.3){
    usr <- par('usr')   #this may be useful
    # rec: xleft, ybottom, xright, ytop
    # usr: xleft, xright, ybottom, ytop
    if (quadrant==1) {
        rect(pt[1], pt[2], usr[2], usr[4], col=rgb(red=col[1], blue=col[2], green=col[3], alpha=alpha), border=NA) 
    } else if (quadrant==2) {
        rect(pt[1], usr[3], usr[2], pt[2], col=rgb(red=col[1], blue=col[2], green=col[3], alpha=alpha), border=NA) 
    } else if (quadrant==3) {
        rect(usr[1], usr[3], pt[1], pt[2], col=rgb(red=col[1], blue=col[2], green=col[3], alpha=alpha), border=NA) 
    } else if (quadrant==4) {
        rect(usr[1], pt[2], pt[1], usr[4], col=rgb(red=col[1], blue=col[2], green=col[3], alpha=alpha), border=NA) 
    } 
    
}

# put a shade between two lines
# x is a vector of two values
# col is red blue gree
abline.shade.2=function(x, col=c(0,1,0)){
    usr <- par('usr') 
    rect(x[1], usr[3], x[2], usr[4], col=rgb(red=col[1], blue=col[2], green=col[3], alpha=.5), border=NA) 
}

#abline.pt.slope(c(1,1), 1)
mymatplot=function(x, y, type="b", lty=1:5, pch=NULL, col=1:6, xlab=NULL, ylab="", 
    draw.x.axis=TRUE, bg=NA, lwd=1, at=NULL, make.legend=TRUE, legend=NULL, 
    legend.x=9, legend.title=NULL, legend.cex=1, legend.inset=0, xaxt="s", ...) {
    
    missing.y=FALSE
    if (missing(y)) {
        missing.y=TRUE
        y=x
        x=1:nrow(y)
    } 
    
    if (is.null(xlab)) xlab=names(dimnames(y))[1]
    if (is.null(legend.title)) legend.title=names(dimnames(y))[2]
    matplot(x, y, lty=lty, pch=pch, col=col, xlab=xlab, xaxt=xaxt, ylab=ylab, bg=bg, lwd=lwd, type=type, ...)
    if (xaxt=="n") 
        if(missing.y & draw.x.axis) axis(side=1, at=x, labels=rownames(y)) else if (draw.x.axis) axis(side=1, at=x, labels=x)
    if (make.legend) {
        if (is.null(legend)) legend=colnames(y)
        if (length(unique(pch))>1) {
            mylegend(legend, x=legend.x, lty=lty, title=legend.title, col=col, pt.bg=bg, cex=legend.cex, lwd=lwd, inset=legend.inset, pch=pch)
        } else {
            mylegend(legend, x=legend.x, lty=lty, title=legend.title, col=col, pt.bg=bg, cex=legend.cex, lwd=lwd, inset=legend.inset)
        }
    }
}


myhist=function(x, add.norm=TRUE, col.norm="blue", ...){
    if (!add.norm) hist(x, ...) else {
        hist=hist(x,breaks=30, plot=F)
        dnorm=dnorm(seq(range(x)[1],range(x)[2], length=100),mean(x),sd(x))
        hist(x, freq=F, ylim=range(hist$density, dnorm), ...)
        lines(seq(range(x)[1],range(x)[2], length=100), dnorm, col=col.norm)
    }    
}


# eclipse
plot.ellipse=function(x0,y0,a=1,b=1,theta=0,alpha=0,add=TRUE,...) {
    theta <- seq(0, 2 * pi, length=500)
#    x <- x0 + a * cos(theta)
#    y <- y0 + b * sin(theta)    
    x <- x0 + a * cos(theta) * cos(alpha) - b * sin(theta) * sin(alpha)
    y <- y0 + a * cos(theta) * sin(alpha) + b * sin(theta) * cos(alpha)
    if(add) {
        lines(x,y,...)
    } else plot(x, y, type = "l",...)
}

add.mtext.label=function(text, cex=1.4, adj=-0.2) mtext(side=3, line=2, adj=adj, text=text, cex=cex, font=2, xpd=NA)
