"grasp.limits" <-
function(gr.Yi, sX = OPTIONS$SELXLIM, npast = OPTIONS$NPAST, lim = "and")
{
    gr.modmask <- gr.modmask
    gr.predmask <- gr.predmask
    Yname <- names(YYY)[gr.Yi]
    cat("\n")
    cat(" vvvvvvvvvv GRASP LIMITS vvvvvvvvvv ", "\n")
    cat(date(), "\n")
    cat("RESPONSE NAME: ", Yname, "\n")
    cat("\n")
    response <- YYY[, gr.Yi]
    if(is.na(sX))
        stop("no limiting predictors were defined in GRASP OPTIONS")
    if(min(response) != 0)
        stop("there is no response equal to 0, limits can not be calculated !", "\n")
        predictor <- XXX[, sX]
        n.numerics <- 0
        n.factors <- 0
        max.levels <- 0
        limitlabels <- NULL
        countlabels <- NULL
        for(i in 1:length(sX)) {
            if(i == 1) {
                limitlabels <- append(limitlabels, names(predictor)[i], n.numerics)
                n.numerics <- n.numerics + 1
            } else {
                limitlabels <- append(limitlabels, names(predictor)[i], n.numerics)
                n.numerics <- n.numerics + 1
            }
#            else if(is.factor(predictor[, i])) {
# factor variables
#                countlabels <- append(countlabels, names(predictor)[i], n.factors)
#                n.factors <- n.factors + 1
#                max.levels <- max(max.levels, length(levels(predictor[, i])))
#            }
        }
        cat("Number of variables: ", length(sX), fill = T)
        cat("Number of numerics: ", n.numerics, fill = T)
        cat("Number of factors: ", n.factors, fill = T)
        if(lim == "or") {
            mask <- rep(FALSE, length(response))
            predict.template <- rep(0, length(XXXpred[, 1]))
        }
        else {
            mask <- rep(TRUE, length(response))
            predict.template <- rep(1, length(XXXpred[, 1]))
        }
        if(lim == "mixte") {
            mask2 <- rep(FALSE, length(response))
            predict.template2 <- rep(0, length(XXXpred[, 1]))
        }
        limits <- matrix(0, nrow = n.numerics, ncol = 3)
        dimnames(limits) <- list(limitlabels, c("lower", "upper", "mean"))
        counts <- matrix(0, nrow = n.factors, ncol = max.levels)
        dimnames(counts) <- list(countlabels, NULL)
        n.numerics <- 0
        n.factors <- 0  #
        for(i in 1:length(sX)) {
                n.numerics <- n.numerics + 1    #
#  Get the lower limit by sorting the predictor and the response
#  and then finding the lowest sequence with response > 0
                sorted <- cbind(predictor[order(predictor[, i], response), i], 
                  response[order(predictor[, i], response)])
                j <- seq(along = sorted[, 1])
                min.pos <- min(j[sorted[, 2] > 0])
                min.pos2 <- max(1, min.pos)
                min.pos <- max(1, min.pos - npast)
                lower.limit <- sorted[min.pos, 1]   #
                lower.limit2 <- sorted[min.pos2, 1] #
#  Get the upper limit by sorting the predictor and 0 minus the response
#  and then finding the highest sequence with response > 0
                sorted <- cbind(predictor[order(predictor[, i], 0 - response), i], 
                  response[order(predictor[, i], 0 - response)])
                j <- seq(along = sorted[, 1])
                max.pos <- max(j[sorted[, 2] > 0])
                max.pos2 <- min(length(sorted[, 1]), max.pos)
                max.pos <- min(length(sorted[, 1]), max.pos + npast)
                upper.limit <- sorted[max.pos, 1]   #
                upper.limit2 <- sorted[max.pos2, 1] #
#  now update the logical mask
                if(lim == "and") {
                  mask[predictor[, i] < lower.limit] <- FALSE
                  mask[predictor[, i] > upper.limit] <- FALSE
                  k <- match(names(predictor)[i], names(XXXpred))
                  predict.template[XXXpred[, k] > upper.limit] <- 0
                  predict.template[XXXpred[, k] < lower.limit] <- 0
                }
                if(lim == "or") {
                  mask[(predictor[, i] > lower.limit) & (predictor[, i] < upper.limit)
                    ] <- TRUE
                  k <- match(names(predictor)[i], names(XXXpred))
                  predict.template[(XXXpred[, k] > lower.limit) & (XXXpred[, k] < 
                    upper.limit)] <- 1
                }
                if(lim == "mixte") {
                  mask[predictor[, i] < lower.limit2] <- FALSE
                  mask[predictor[, i] > upper.limit2] <- FALSE
                  mask2[(predictor[, i] > lower.limit) & (predictor[, i] < 
                    lower.limit2)] <- TRUE
                  mask2[(predictor[, i] > upper.limit2) & (predictor[, i] < 
                    upper.limit)] <- TRUE
                  k <- match(names(predictor)[i], names(XXXpred))
                  predict.template[XXXpred[, k] > upper.limit2] <- 0
                  predict.template[XXXpred[, k] < lower.limit2] <- 0
                  predict.template2[(XXXpred[, k] > lower.limit) & (XXXpred[, k] < 
                    lower.limit2)] <- 1
                  predict.template2[(XXXpred[, k] > upper.limit2) & (XXXpred[, k] < 
                    upper.limit)] <- 1
                }
                limits[n.numerics,  ] <- c(zapsmall(lower.limit, 4), zapsmall(
                  upper.limit, 4), zapsmall(mean(predictor[response > 0, i]), 4))
                cat("Lower and upper limits, and occupied mean for ", names(predictor[
                  i]), " are ", limits[n.numerics,  ], fill = T)
                limits2 <- limits
                limits2[n.numerics,  ] <- c(zapsmall(lower.limit2, 4), zapsmall(
                  upper.limit2, 4), zapsmall(mean(predictor[response > 0, i]), 4))
        }
        if(lim == "mixte") {
            mask <- mask | mask2
            predict.template <- predict.template | predict.template2
        }
        excluded <- length(mask[mask == FALSE])
        cat("Limits of models (gr.modmask) set and ", excluded, " cases excluded", fill = T)
        gr.modmask[, gr.Yi] <- as.logical(mask)
        assign("gr.modmask", gr.modmask, pos = 1)
        excluded2 <- length(predict.template[predict.template == FALSE])
        cat("Limits of prediction (gr.predmask) set and ", excluded2, " cases excluded", fill
             = T)
        gr.predmask[, gr.Yi] <- as.logical(predict.template)
        assign("gr.predmask", gr.predmask, pos = 1)
        cat("\n")
    cat(" ********** GRASP LIMITS END ********** ", "\n")
    cat("\n")    
}
