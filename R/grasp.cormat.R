"grasp.cormat" <-
function(gr.Yi = gr.selY, cols = gr.selX, thin = 1)
{
    x11()
    Yname <- names(YYY)[gr.Yi]
    cat("\n")
    cat("********** GRASP CORRELATION MATRIX CALCULATION **********", "\n")
    cat(date(), "\n")
    cat("\n")
    cat("RESPONSE NAME: ", Yname, "\n")
    #data <- XXX
    data <- XXX[gr.modmask[, gr.Yi], ]
    XXX <- XXX[gr.modmask[, gr.Yi], ]
    assign("data", data, pos=1)
    gr.selXCOR <- gr.selXCOR
    panel <- function(x, y)
    {
        points(x, y, col = 2)
        corr <- zapsmall(cor(x, y), 2)
        text(min(x), max(y), cex = 1.2, as.character(corr), adj = 0)
    }
    ndata <- thin * nrow(data)
    index <- ((1:ndata) * 1)/thin
    thindata <- data[index,  ]
    pairs(thindata[, cols], panel = panel)
    text(0, 1.17, paste("GRASP: ", " ", Yname), adj = 0, cex = 0.7)
    gr.selXcor <- NULL
    gr.selXfac <- NULL
    for(i in gr.selX) {
        if(!is.factor(XXX[, i]))
            gr.selXcor <- c(gr.selXcor, i)
        else gr.selXfac <- gr.selXfac <- c(gr.selXfac, i)
    }
    gr.selXorder <- match(gr.selXcor, gr.selXcor)
    cortemp <- cor(XXX[ , gr.selXcor])
    cortemp[cortemp == 1] <- 0
    cat("STARTING MATRIX:", "\n")
    print(cortemp)
    assign("CorMatrix", cortemp, pos=1)
    cat(names(XXX)[gr.selXcor], "\n")
    while(max(abs(cortemp)) > 0.05) {
        cormax <- abs(cortemp) == max(abs(cortemp))
        selCor <- apply(cormax, 2, sum) * gr.selXorder
        gr.selXcor <- gr.selXcor[ - match(max(selCor), selCor)]
        gr.selXorder <- gr.selXorder[ - match(max(selCor), selCor)]
        cortemp <- cor(XXX[ , gr.selXcor])
        cortemp[cortemp == 1] <- 0
    }
    cat("UNCORRELATED MATRIX:", "\n")
    print(cortemp)
    gr.selXCOR[[gr.Yi]] <- c(gr.selXcor, gr.selXfac)
    assign("gr.selXCOR", gr.selXCOR, pos = 1)
    cat("\n")
    cat("Matrix has been saved under CorMatrix!", "\n")
    cat("\n")
    cat(" ********** GRASP CORMAT END ********** ", "\n")
    cat("\n")
    cat("\n")
}
