"grasp.in" <-
function(Ymat, Xmat, Xpred)
{
    cat("\n")
    cat("*****************************************************", "\n")
    cat("**********           R-GRASP IN            **********", "\n")
    cat("**********       Initializes variables     **********", "\n")
    cat("**********        GRASP by A. Lehmann      **********", "\n")
    cat("**********      Ported to R by F. Fivaz    **********", "\n")
    cat("*****************************************************", "\n")
    cat("\n")
    cat(date(), "\n")
    cat("\n")    
    cat("Loading libraries... ")
        library(mgcv)
        library(MASS)
        library(modreg)
    cat("done", "\n")
    
    cat("Creating YYY... ")
        assign("YYY", Ymat, pos=1)
    cat("done", "\n")
    
    cat("Creating STEPMODEL... ")
        STEPMODEL <- list(gam(rep(c(0,1),50)~runif(100,0,10), family=binomial()))
        assign("STEPMODEL", STEPMODEL, pos=1)
        assign("gam.start", STEPMODEL, pos=1)
    cat("done", "\n")

    cat("Creating gr.selim...")
        assign("gr.selim", c(0), pos = 1)
    cat("done", "\n")    
    cat("Creating XXX... ")
        XXX <- Xmat
        assign("XXX", Xmat, pos=1)
    cat("done", "\n")
    
    cat("Creating XXXpred... ")
        assign("XXXpred", Xpred, pos=1)
    cat("done", "\n")
    
    cat("Initializing gr.selX and gr.selY... ")
        SELXCOR <- NULL
        assign("gr.selY", c(2:length(Ymat)), pos=1)
        assign("gr.selX", c(4:length(Xmat)), pos=1)
        assign("selX", gr.selX, pos=1)
        cat("done", "\n")
        cat("Selected responses (gr.selY): ", gr.selY, "\n")
        cat("Selected predictors (gr.selX): ", gr.selX, "\n")
        cat("Removing past variables if exist... ")
        OPT <- NULL
        assign("OPT", OPT, pos = 1)
        gr.selXCOR = XXX
        assign("gr.selXCOR", gr.selXCOR, pos = 1)
        gr.selXCOR <- XXX
    cat("done", "\n")
        
    cat("Creating gr.modmask...")
        modcol <- length(YYY)
        modrow <- length(YYY[,1])
        gr.modmask <- matrix(TRUE, ncol = dim(YYY)[2], nrow = dim(YYY)[1])
        assign("gr.modmask", gr.modmask, pos = 1)
    cat("done", "\n")
    
    cat("Creating gr.predmask...")
        gr.predmask <- matrix(rep(T, dim(XXXpred)[1] * dim(YYY)[2]), ncol = dim(YYY)[2])
        assign("gr.predmask", gr.predmask, pos = 1)
    cat("done", "\n")
    
    cat("Creating gr.predmat...", "\n")
    gr.predmat <- as.data.frame(matrix(as.single(rep(-99.9, dim(XXXpred)[1]*dim(YYY)[2])), ncol=dim(YYY)[2]))
    assign("gr.predmat", gr.predmat, pos=1)
    fixed <- rep(1, dim(XXX)[1]*dim(YYY)[2])
    assign("fixed", fixed, pos=1)
    Yname <- names(YYY[gr.selY])
    cat("SELECTED RESPONSES:", Yname, "\n")
    WEIGHTS <- data.frame(index=YYY[1], Yname=rep(1, dim(XXX)[1]))
    assign("WEIGHTS", WEIGHTS, pos=1)
    cat("done", "\n")
   cat("RGRASP initialized!", "\n")
    cat("\n")
    cat("**********         R-GRASP IN END          **********", "\n")
    cat("\n")
}
