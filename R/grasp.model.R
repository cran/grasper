"grasp.model" <-
function (gr.Yi = gr.Yi, cX = gr.selX, optest = "F", df = 4, 
    trace = TRUE, calcdf = FALSE, stepfam = "quasibinomial()") 
{
    if (trace) {
        cat("\n")
        cat("*****************************************************", 
            "\n")
        cat("**********          R-GRASP MODEL          **********", 
            "\n")
        cat("**********       Modelling using gam()     **********", 
            "\n")
        cat("**********        GRASP by A. Lehmann      **********", 
            "\n")
        cat("**********      Ported to R by F. Fivaz    **********", 
            "\n")
        cat("*****************************************************", 
            "\n")
        cat("\n")
        cat(date(), "\n")
        cat("\n")
        cat("Initializing variables... ")
    }
    Yname <- names(YYY[gr.Yi])
    gr.selYCOR <- gr.Yi
    gr.selXCOR <- list(gr.selY)
    for (i in gr.Yi) {
        gr.selXCOR[[i]] <- gr.selX
    }
    gr.selXCOR <- gr.selXCOR
    assign("gr.selXCOR", gr.selXCOR, pos = 1)
    assign("gr.Yi", gr.Yi, pos = 1)
    if (trace) {
        cat("done", "\n")
        cat("\n")
        assign("Yname", Yname, pos = 1)
        cat("RESPONSE NAME: ", Yname, "\n")
    }
    if (((stepfam == "binomial") | (stepfam == "quasi")) & ((max(YYY[, 
        gr.Yi]) > 1) | (min(YYY[, gr.Yi]) < 0))) {
        stop("DATA OUT OF RANGE [0,1] TO USE A BINOMIAL MODEL !")
    }
    mkt.keep.AIC <- function(object, AIC) {
        list(df.resid = object$df.resid, deviance = object$deviance, 
            term = as.character(object$formula)[3], AIC = AIC)
    }
    mkt.keep.p <- function(object, pvalue) {
        list(df.resid = object$df.resid, deviance = object$deviance, 
            term = as.character(object$formula)[3], pvalue = pvalue)
    }
    for (xi in gr.selXCOR[[gr.Yi]]) {
        if (is.factor(XXX[, xi]) & length(levels(XXX[, xi])) < 
            2) {
            gr.selXCOR[[gr.Yi]] <- gr.selXCOR[[gr.Yi]][-match(xi, 
                gr.selXCOR[[gr.Yi]])]
            print(paste("factor", names(XXX)[xi], "was removed from the potential predictors of", 
                names(YYY)[gr.Yi], "because it has less than two levels"))
        }
    }
    grasp.scope(gr.selXCOR[[gr.Yi]], trace = FALSE)
    grasp.start(cX, sX = gr.selXCOR[[gr.Yi]], df = df, trace = FALSE, 
        calcdf = calcdf)
    START <- model.formula
    assign("model.formula", model.formula, pos = 1)
    XXX <- XXX[gr.modmask[, gr.Yi], ]
    YYY <- YYY[gr.modmask[, gr.Yi], ]
    WEIGHTS.df <- paste("WEIGHTS[gr.modmask[, ", gr.Yi, "], ", 
        gr.Yi, "]")
    gam.start <- eval(parse(text = paste("gam(", as.character(START[2]), 
        "~", as.character(START[3]), ", family = ", stepfam, 
        ",data = XXX", ", weights = ", WEIGHTS.df, ", control = gam.control(maxit = 50, epsilon=0.001))")))
    assign("gam.start", gam.start, pos = 1)
    STEPMODEL[[gr.Yi]] <- gam.start
    assign("STEPMODEL", STEPMODEL, pos = 1)
    if (trace) 
        cat("Ploting results...", "\n")
    plot.gam(gam.start, pages = 1, scale = 0, n = 1000)
    if (trace) {
        cat("SUMMARY OF MODEL", "\n")
        summary.gam(gam.start)
        cat("\n")
        cat("**********        R-GRASP MODEL END        **********", 
            "\n")
        cat("\n")
    }
}
