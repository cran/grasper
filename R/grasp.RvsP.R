"grasp.RvsP" <-
function (gr.Yi = gr.selY, sX = gr.selX) 
{
    Yname <- names(YYY)[gr.Yi]
    cat("\n")
    cat("********** GRASP RESPONSE VS PREDICTORS **********", 
        "\n")
    cat(date(), "\n")
    cat("RESPONSE NAME: ", Yname, "\n")
    par(mfrow = c(4, 2), mai = c(0.5, 0.5, 0.5, 0.5))
    first <- TRUE
    YYY <- YYY[gr.modmask[, gr.Yi], ]
    XXX <- XXX[gr.modmask[, gr.Yi], ]
    for (xi in sX) {
        plot(XXX[, xi], YYY[, gr.Yi], xlab = names(XXX)[xi], 
            ylab = names(YYY)[gr.Yi])
        if (!is.factor(XXX[, xi])) 
            lines(smooth.spline(XXX[, xi], YYY[, gr.Yi], df = 3))
        if (first) 
            title(paste(OPT$TITLE, "RESPONSE vs. PREDICTIONS", 
                sep = " "))
        first <- FALSE
    }
    cat("\n")
    cat(" ********** GRASP RESPONSE VS PREDICTORS END ********** ", 
        "\n")
    cat("\n")
}
