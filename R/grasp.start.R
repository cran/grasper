"grasp.start" <-
function(cX, sX, df = 4, calcdf = FALSE, trace = TRUE)
{
    if (trace) {
    cat("\n")
    cat("*****************************************************", "\n")
    cat("**********        R-GRASP START            **********", "\n")
    cat("**********    Creates starting formula     **********", "\n")
    cat("**********      GRASP by A. Lehmann        **********", "\n")
    cat("**********    Ported to R by F. Fivaz      **********", "\n")
    cat("*****************************************************", "\n")
    cat("\n")
    cat(date(), "\n")
    cat("\n")
    cat("Initializing variables... ")
    }
    sX <- intersect(sX, cX)
    vnames <- names(XXX[, c(1, sX)])
    model.formula <- NULL
    if (trace) {
    cat("done", "\n")
    cat("\n")
    cat("Variables used:", "\n")
    }
    if (calcdf==FALSE) {
    for(Xi in sX) {
	if (trace)
        	cat(names(XXX)[Xi], "\n")
	vname <- names(XXX)[Xi]
        # loops through independant variable names
        if(is.factor(XXX[[vname]])) 
            model.formula <- c(model.formula, vname)
        else model.formula <- c(model.formula, paste("s", "(", vname, ", k=", df + 1, ", fx = TRUE)"))
    }
    } else {
    for(Xi in sX) {
        if (trace)
		cat(names(XXX)[Xi], "\n")
        vname <- names(XXX)[Xi]
        # loops through independant variable names
        if(is.factor(XXX[[vname]])) 
            model.formula <- c(model.formula, vname)
        else model.formula <- c(model.formula, paste("s", "(", vname, ")"))
    }
    }
    gam.formula <- model.formula
    assign("gam.formula", gam.formula, pos = 1)
    model.formula <- c(model.formula)
    if(length(sX) > 1) {
        # tests that there is more than one variables to generate the formula by collapsing the names
        model.formula <- eval(parse(text = paste("YYY$", names(YYY)[gr.Yi], "~", paste(model.formula, collapse = "+"))))
    }
    else {
        if(length(sX) == 0) {
            # tests if there is 0 variable
            model.formula <- eval(parse(text = paste("YYY$", names(YYY)[gr.Yi], "~", "0")))
        }
        else {
            # tests if there is 1 variable
            model.formula <- eval(parse(text = paste("YYY$", names(
                YYY)[gr.Yi], "~", model.formula)))
        }
    }
    model.formula
    cat("\n")
    assign("model.formula", model.formula, pos=1)
    if (trace) {
    print(model.formula)
    cat("\n")
    cat("Formula created!", "\n")
    cat("\n")
    cat("**********        R-GRASP START END         **********", "\n")
    cat("\n")
    }
}
