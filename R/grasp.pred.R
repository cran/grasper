"grasp.pred" <-
function(gr.Yi = gr.selY)
{
    cat("\n")
    cat("*****************************************************", "\n")
    cat("**********           GRASP PREDICT         **********", "\n")
    cat("**********         Predicts responses      **********", "\n")
    cat("**********        GRASP by A. Lehmann      **********", "\n")
    cat("**********      Ported to R by F. Fivaz    **********", "\n")
    cat("*****************************************************", "\n")
    cat("\n")
    cat(date(), "\n")
    cat("\n")
    Yname <- names(YYY)[gr.Yi]
    cat("Response name: ", Yname, "\n")
    print(gam.start)
    cat("predicting... ")
    prediction <- rep(1, length(XXXpred[, 1]))
    prediction[gr.predmask[, gr.Yi] == FALSE] <- -99.9
    prediction[prediction == 1] <- predict.gam(gam.start, XXXpred[prediction == 1,  ], type = "response")
    cat("done", "\n")
    cat("Saving predictions...")
    print(prediction[1:10])
    gr.predmat[, gr.Yi] <- round(prediction, 4)
    assign("gr.predmat", gr.predmat, pos = 1)
    cat("done", "\n")
    cat("\n")
    cat("**********         GRASP PREDICT END       **********", "\n")
}
