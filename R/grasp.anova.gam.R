"grasp.anova.gam" <-
function (object, ..., dispersion = NULL, test = NULL) 
{
    object$df.residual <- object$df.null - sum(object$edf) - object$nsdf
    dotargs <- list(...)
    named <- if (is.null(names(dotargs))) 
        rep(FALSE, length(dotargs))
    else (names(dotargs) != "")
    if (any(named)) 
        warning(paste("The following arguments to anova.glm(..)", "are invalid and dropped:", paste(deparse(dotargs[named]), collapse = ", ")))
    dotargs <- dotargs[!named]
    is.gam <- unlist(lapply(dotargs, function(x) inherits(x, "gam")))
    dotargs <- dotargs[is.gam]
    if (length(dotargs) == 0) stop('need two or more models to compare')
    for (i in 1:length(dotargs)) {
        class(dotargs[[i]]) <- c(class(dotargs[[i]]), 'glm')
        dotargs[[i]]$df.residual <- dotargs[[i]]$df.null - sum(dotargs[[i]]$edf) - dotargs[[i]]$nsdf 
    }   
   grasp.anova.glmlist(c(list(object), dotargs), test = test, dispersion = dispersion)
}
