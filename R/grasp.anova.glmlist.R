"grasp.anova.glmlist" <-
function (object, ..., dispersion = NULL, test = NULL) 
{
    responses <- as.character(lapply(object, function(x) {
        deparse(formula(x)[[2]])
    }))
    sameresp <- responses == responses[1]
    if (!all(sameresp)) {
        object <- object[sameresp]
        warning(paste("Models with response", deparse(responses[!sameresp]), 
            "removed because response differs from", "model 1"))
    }
    ns <- sapply(object, function(x) length(x$residuals))
    if (any(ns != ns[1])) 
        stop("models were not all fitted to the same size of dataset")
    nmodels <- length(object)
    if (nmodels == 1) 
        return(anova.glm(object[[1]], dispersion = dispersion, 
            test = test))
    resdf <- as.numeric(lapply(object, function(x) x$df.residual))
    resdev <- as.numeric(lapply(object, function(x) x$deviance))
    table <- data.frame(resdf, resdev, c(NA, -diff(resdf)), c(NA, 
        -diff(resdev)))
    variables <- lapply(object, function(x) paste(deparse(formula(x)), 
        collapse = "\n"))
    dimnames(table) <- list(1:nmodels, c("Resid. Df", "Resid. Dev", 
        "Df", "Deviance"))
    title <- "Analysis of Deviance Table\n"
    topnote <- paste("Model ", format(1:nmodels), ": ", variables, 
        sep = "", collapse = "\n")
    if (!is.null(test)) {
        bigmodel <- object[[order(resdf)[1]]]
        df.dispersion <- if (dispersion == 1) 
            Inf
        else min(resdf)
        table <- grasp.stat.anova(table = table, test = test, 
            scale = dispersion, df.scale = df.dispersion, n = length(bigmodel$residuals))
    }
    structure(table, heading = c(title, topnote), class = c("anova", 
        "data.frame"))
}
