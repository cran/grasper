"grasp.step.gam" <-
function (object = gam.start, scope = step.list, scale, direction = c("both", "backward","forward"), trace = TRUE, keep = NULL, steps = 1000, limit = 0.05, test = "F")
{
# Test of a stepwise Selection using ANOVA based on Chi or F tests instead of
# AIC criteria. The function is a modification of the S-Plus step.gam function

LIMIT = limit
OPT$test <- test
cat("direction =", direction, "\n")
cat("steps =", steps, "\n")
cat("limit =", limit, "\n")
cat("test =", test, "\n")
XXX <- XXX[gr.modmask[, gr.Yi], ]
YYY <- YYY[gr.modmask[, gr.Yi], ]
    

convert.gam <- function (object) 
{
    objgam <- vector("list", 3)
    names(objgam) <- c("family", "formula", "terms")
    objgam$formula <- formula(object)
    objgam$family <- vector("list", 3)
    names(objgam$family) <- c("name", "link", "variance")
    objgam$family$name <- object$family[[1]]
    objgam$family$link <- object$family[[2]]
    objgam$family$variance <- object$family[[5]]
    objgam$terms <- vector("list", 2)
    names(objgam$terms) <- c("response","term.labels")
    objgam$terms$response <- 1
    objgam$terms$term.labels <- gam.formula
    return(objgam)
}

scope.char <- function (form) 
{
    for (i in 1:length(form)) {
        form[[i]] <- c("1", gam.formula[i])
    }
    return(form)
}

untangle.scope <- function(terms, regimens)
{
    a <- attributes(terms)
    response <- deparse(terms[[2]])
    term.labels <- a$term.labels
    nt <- length(regimens)
    Select <- integer(nt)
    for (i in seq(nt)) {
        j <- match(regimens[i], term.labels, 0)
        if (any(j)) {
            if (sum(j > 0) > 1)
                stop("problem")
            Select[i] <- seq(j)[j > 0]
            term.labels <- term.labels[ - sum(j)]
        }
        else {
           if(!(j <- match("1", regimens[[i]], 0)))
               stop("problem 2")
            Select[i] <- j
        }
    }
    if(length(term.labels))
            term.labels <- paste(term.labels, "+")
    return(list(response = paste(response, term.labels, sep = " ~ "), Select = Select))
}

make.step <- function(models, fit, scale, object)
{
    chfrom <- sapply(models, "[[", "from")
    chfrom[chfrom == "1"] <- ""
    chto <- sapply(models, "[[", "to")
    chto[chto == "1"] <- ""
    dev <- sapply(models, "[[", "deviance")
    df <- sapply(models, "[[", "df.resid")
    ddev <- c(NA, diff(dev))
    ddf <- c(NA, diff(df))
    pvalue <- sapply(models, "[[", "pvalue")
    heading <- as.list(c("Stepwise Model Path", "\nAnalysis of deviance Table", "\nInitial Model: ", deparse(as.vector(formula(object))), "\nFinal Model: ", deparse(as.vector(formula(fit))), paste("\nScale: ", format(scale), "\n", sep = "")))
    aod <- data.frame(From = chfrom, To = chto, Df = ddf, Deviance = ddev, "Resid. Df" =  df, "Resid. Dev" = dev, pvalue = pvalue, check.names = F)
    cat("\n")
    print(aod)
    cat("\n")
    cat(unlist(heading))
    cat("\n")
    cat("Creating new gam.start model, with results from stewise selection...")
    gam.start <- update(gam.start, formula(fit))
    assign("gam.start", gam.start, pos = 1)
    cat("done", "\n")
}

#grasp.scope(gr.selX)
direction <- match.arg(direction)
scope <- scope.char(scope)
object.real <- object
object <- convert.gam(object)
response <- untangle.scope(object$formula, scope)
object <- object.real
form.y <- response$response
backward <- direction == "both" | direction == "backward"
forward <- direction == "both" | direction == "forward"
items <- response$Select
family <- object$family
cat("\n")
Call <- object$call
term.lengths <- sapply(scope, length)
n.items <- length(items)
visited <- array(F, term.lengths)
visited[array(items, c(1, n.items))] <- T
models <- vector("list", length(visited))
nm <- 2
form.vector <- character(n.items)
items <- items + 1
for (i in seq(n.items)) {
    form.vector[i] <- scope[[i]][items[i]]
}
form <- deparse(object$formula)
cat("form.vector passed", "\n")
#if (trace)
#    cat("Start:", form, "\n")

fit <- object
fit$df.resid <- length(fit$y) - fit$nsdf - sum(fit$edf)
n <- length(fit$fitted)
scale <- fit$sig2
cat("Scale est. =", scale, "\n")
bAIC <- deviance(fit) + 2 * (n - fit$df.null) * scale
#if(trace)
#    cat("; p limit=", LIMIT, "\n")
models[[1]] <- list(deviance = deviance(fit), df.resid = fit$df.resid, pvalue = LIMIT, from = "", to = "")
STOP <- 1
bFRED <- 1
if (test == "Chisq")
    scale <- 1
while(STOP > LIMIT && steps > 0) {
    steps <- steps - 1
    STOP <- bFRED
    bFRED <- LIMIT
    kFRED <- LIMIT
    bitems <- items
    bfit <- fit
    Nstep <- 0
    for (i in seq(n.items)) {
    if (backward) {
            cat("Start backward...", "\n")
        trial <- items
            trial[i] <- trial[i] - 1
            if (trial[i] > 0 && !visited[array(trial, c(1, n.items))]) {
            visited[array(trial, c(1, n.items))] <- T
                tform.vector <- form.vector
                tform.vector[i] <- scope[[i]][trial[i]]
                cat("Trial:", form, "\n")
                form <- paste(form.y, paste(tform.vector, collapse = " + "))
                #if(trace)
                #    cat("Trial:", form, "\n")
                tfit <- update(object.real, eval(parse(text = form)))
                TEST <- grasp.anova.gam(fit, tfit, test = test, dispersion = scale)
                fit$df.resid <- length(fit$y) - fit$nsdf - sum(fit$edf)
            tfit$df.resid <- length(tfit$y) - tfit$nsdf - sum(tfit$edf)
                bfit$df.resid <- length(bfit$y) - bfit$nsdf - sum(bfit$edf)
            assign("fit", fit, pos = 1)
            assign("tfit", tfit, pos = 1)
            #tFRED <- pchisq(abs((fit$deviance-tfit$deviance))/fit$sig2, abs(sum(fit$edf)-sum(tfit$edf)), lower.tail=FALSE)
            if (test == "Chisq")
            tFRED <- TEST[2,5]
        if (test == "F")
            tFRED <- TEST[2,6]
            if (tFRED > bFRED) {
                bFRED <- tFRED
                        kFRED <- tFRED
                        bitems <- trial
                        bfit <- tfit
                        bform.vector <- tform.vector
                        bfrom <- form.vector[i]
                        bto <- tform.vector[i]
                        Nstep <- i
                }
                #if (trace)
                #    cat("; p-value(-) =", format(round(tFRED, 6)), "\n")
            }
    }
    if (forward) {
        cat("Start forward...", "\n")
        trial <- items
        trial[i] <- trial[i] + 1
        if(trial[i] <= term.lengths[i] && !visited[array(trial, c(1, n.items))]) {
            visited[array(trial, c(1, n.items))] <- T
            tform.vector <- form.vector
            tform.vector[i] <- scope[[i]][trial[i]]
            form <- paste(form.y, paste(tform.vector, collapse = " + "))
            #if(trace)
            #   cat("Trial:", form, "\n")
            tfit <- update(object.real, eval(parse(text = form)))
            TEST <- grasp.anova.gam(fit, tfit, test = test, dispersion = scale)
            fit$df.resid <- length(fit$y) - fit$nsdf - sum(fit$edf)
                tfit$df.resid <- length(tfit$y) - tfit$nsdf - sum(tfit$edf)
            bfit$df.resid <- length(bfit$y) - bfit$nsdf - sum(bfit$edf)
            #tFRED <- pchisq(abs((fit$deviance-tfit$deviance))/fit$sig2, abs(sum(fit$edf)-sum(tfit$edf)), lower.tail=FALSE)
            tFRED <- TEST[2, 5]
            if(tFRED < kFRED & tFRED < LIMIT) {
                bFRED <- LIMIT + 0.001
                kFRED <- tFRED
                bitems <- trial
                bfit <- tfit
                bform.vector <- tform.vector
                bfrom <- form.vector[i]
                bto <- tform.vector[i]
                Nstep <- i
            }
            #if (trace)
            #   cat("p.value(+) =", format(round(tFRED, 6)), "\n")
        }
    }
    }   #End of for
    if(STOP <= LIMIT | steps == 0 | Nstep == 0) {
    return(make.step(models[seq(nm - 1)], fit, scale, object))
    }
    else {
        #if(trace)
        #   cat("Step : ", Nstep, "/ ", deparse(bfit$formula), "; p.value =", kFRED, "\n\n")
        items <- bitems
        models[[nm]] <- list(deviance = deviance(bfit), df.resid = bfit$df.resid, pvalue = kFRED, from = bfrom, to = bto)
        nm <- nm + 1
        fit <- bfit
        form.vector <- bform.vector
    }
}
assign("models", models, pos = 1)
}
