"grasp.validate" <-
function ()
{
    STEPMODEL <- gam.start
    OPTIONS$CVGROUPS <- 5
    assign("OPTIONS", OPTIONS, pos = 1)
    Yname <- names(YYY)[gr.Yi]
    cat("RESPONSE NAME: ", Yname, "\n")
    par(mfrow = c(2, 2), mai = c(0.75, 0.75, 0.75, 0.75))
    subsets <- rep(1:OPTIONS$CVGROUPS, length.out = length(XXX[, 1]))
    temppredict <- rep(NA, length(XXX[, 1]))
    assign("temppredict", temppredict, pos = 1)
    for (Nloop in 1:OPTIONS$CVGROUPS) {
        OPTIONS$SUBSET1 <- subsets != Nloop
	if (OPTIONS$CVGROUPS ==1)
		OPTIONS$SUBSET1 <- !OPTIONS$SUBSET1
	OPTIONS$SUBSET1 <- (as.logical(gr.modmask[, gr.Yi]) & OPTIONS$SUBSET1)		
        tempmodel <- update(STEPMODEL)
	if (OPTIONS$CVGROUPS ==1)
		OPTIONS$SUBSET2 <- OPTIONS$SUBSET1
	else OPTIONS$SUBSET2 <- (as.logical(gr.modmask[, gr.Yi]) & !OPTIONS$SUBSET1)		
        temppredict[OPTIONS$SUBSET2] <- predict.gam(tempmodel, XXX[OPTIONS$SUBSET2,], type = "response")
    }
    temp1 <- YYY[!is.na(temppredict), gr.Yi]
    temp2 <- temppredict[!is.na(temppredict)]
    plot(temp1, temp2, ylab = "cross-predicted", xlab = paste("observed ", Yname), pch = "*")
    BIN <- gam.start$family[1] == "quasibinomial"
    if (BIN)
    	if (BIN) {
		nbgroups <- 10
		cvROC <- grasp.roc(temp2, temp1)
		legend(min(YYY[!is.na(YYY[, gr.Yi]), gr.Yi]), max(temp2), paste("N groups : ", OPTIONS$CVGROUPS, ", cvROC =", zapsmall(cvROC$auc, 2)), cex = 0.7)
	}
	else
	{
		legend(min(YYY[!is.na(YYY[, gr.Yi]), gr.Yi]), max(temp2), paste("N groups : ", OPTIONS$CVGROUPS, ", cvROC =", zapsmall(cor(temp1, temp2), 2)), cex = 0.7)
	}
    STEPMODEL.local <- STEPMODEL
    STEPMODEL.local[[gr.Yi]]$crosspred <- temppredict
    assign("STEPMODEL", STEPMODEL.local, pos = 1)
    title("CROSS-VALIDATION", cex = 0.5)
    CVGROUPS <- 1
    subsets <- rep(1:CVGROUPS, length.out = length(XXX[, 1]))
    temppredict <- rep(NA, length(XXX[, 1]))
    for (Nloop in 1:CVGROUPS) {
        OPTIONS$SUBSET1 <- subsets != Nloop
        if (CVGROUPS == 1) 
            OPTIONS$SUBSET1 <- !OPTIONS$SUBSET1
	OPTIONS$SUBSET1 <- (as.logical(gr.modmask[, gr.Yi]) & OPTIONS$SUBSET1)	
        assign("OPTIONS", OPTIONS, pos = 1)
        tempmodel <- update(STEPMODEL)
        if (CVGROUPS == 1) 
            OPTIONS$SUBSET2 <- OPTIONS$SUBSET1
        else OPTIONS$SUBSET2 <- (as.logical(gr.modmask[, gr.Yi]) & !OPTIONS$SUBSET1)
        assign("OPTIONS", OPTIONS, pos = 1)
        temppredict[OPTIONS$SUBSET2] <- predict.gam(tempmodel, XXX[OPTIONS$SUBSET2, ], type = "response")
    }
    temp1 <- YYY[!is.na(temppredict), gr.Yi]
    temp2 <- temppredict[!is.na(temppredict)]
    plot(temp1, temp2, ylab = "predicted", xlab = paste("observed ", Yname), pch = "*")
    if (BIN)
    	if (BIN) {
		nbgroups <- 10
		ROC <- grasp.roc(temp2, temp1)
		legend(min(YYY[!is.na(YYY[, gr.Yi]), gr.Yi]), max(temp2), paste("N groups : ", CVGROUPS, ", ROC =", zapsmall(ROC$auc, 2)), cex = 0.7)
	}
	else
	{
		legend(min(YYY[!is.na(YYY[, gr.Yi]), gr.Yi]), max(temp2), paste("N groups : ", CVGROUPS, ", ROC =", zapsmall(cor(temp1, temp2), 2)), cex = 0.7)
	}
    COR <- cor(temp1, temp2)
    if (exists("DROP.CONTRIB")) {
        DROP.CONTRIB <- DROP.CONTRIB
        endline <- length(gr.selX) + 7
        DROP.CONTRIB[endline + 1, gr.Yi] <- COR
        DROP.CONTRIB[endline + 2, gr.Yi] <- cvCOR
        row.names(DROP.CONTRIB)[c(endline + 1, endline + 2)] <- c("Cor", "cvCor")
        dump(c("DROP.CONTRIB", "ALONE.CONTRIB"), fileout = paste(OPTIONS$PATH, "contributions.txt", sep = ""))
        assign("DROP.CONTRIB", DROP.CONTRIB, pos = 1)
    }
    title("VALIDATION", cex = 0.5)
    qqnorm(resid(gam.start))
    cat("\n")
    cat(" ********** GRASP VALIDATE END ********** ", "\n")
    cat("\n")
}
