"grasp.export" <-
function (var = gr.predmat) 
{
    cat("*****************************************************", "\n")
    cat("**********        R-GRASP EXPORT           **********", "\n")
    cat("********** Exporting results in txt format **********", "\n")
    cat("**********      GRASP by A. Lehmann        **********", "\n")
    cat("**********    Ported to R by F. Fivaz      **********", "\n")
    cat("*****************************************************", "\n")
    cat("\n")
    cat(date(), "\n")
    cat("\n")
    gr.Yi <- gr.selY
    Yname <- names(YYY[gr.Yi])
    cat("Response name:", Yname, "\n")
    cat("\n")
    file <- paste("pred_", Yname, ".txt", sep = "")
    gr.export <- var[,gr.Yi]
    glmax <- max(var)
    glength <- length(var)
    cat("Converting to percents (and integer)...", "\n")
    gr.export <- gr.export * 100
    gr.export <- as.integer(gr.export)
    to.export <- data.frame(x = XXXpred$x, y = XXXpred$y, pred = gr.export)
    cat("Writing file", file, "to disk...", "\n")
    write.table(to.export, file = file, sep = ",", row.names = F)
    cat("\n")
    cat("**********      R-GRASP EXPORT END         **********", "\n")
    cat("\n")
}
