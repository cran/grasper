"grasp.scope" <-
function (sX = gr.selX, df = 4, calcdf = FALSE, trace = TRUE) 
{
    if (trace) {
        cat("\n")
        cat("*****************************************************", 
            "\n")
        cat("**********        R-GRASP SCOPE            **********", 
            "\n")
        cat("**********      Creates scope list         **********", 
            "\n")
        cat("**********      GRASP by A. Lehmann        **********", 
            "\n")
        cat("**********    Ported to R by F. Fivaz      **********", 
            "\n")
        cat("*****************************************************", 
            "\n")
        cat("\n")
        cat(date(), "\n")
        cat("\n")
        cat("Initializing variables... ")
    }
    vnames <- names(XXX[, sX])
    step.list <- as.list(vnames)
    names(step.list) <- vnames
    assign("vnames", vnames, pos = 1)
    if (trace) {
        cat("done", "\n")
        cat("\n")
        cat("Scope list: ", "\n")
        cat("\n")
    }
    if (calcdf == FALSE) {
        for (Xi in sX) {
            vname <- names(XXX)[Xi]
            junk <- c("1")
            if (!is.factor(XXX[[vname]])) 
                junk <- c("1", paste("s", "(", vname, ", ", df + 
                  1, ", fx = T)", sep = ""))
            junk <- eval(parse(text = paste(" ~ ", paste(junk, 
                collapse = "+"))))
            step.list[[vname]] <- junk
        }
    }
    else {
        for (Xi in sX) {
            vname <- names(XXX)[Xi]
            junk <- c("1")
            if (!is.factor(XXX[[vname]])) 
                junk <- c("1", paste("s", "(", vname, ")", sep = ""))
            junk <- eval(parse(text = paste(" ~ ", paste(junk, 
                collapse = "+"))))
            step.list[[vname]] <- junk
        }
    }
    if (trace) 
        print(step.list)
    assign("step.list", step.list, pos = 1)
    if (trace) 
        cat("**********      R-GRASP SCOPE END          **********", 
            "\n")
}
