"grasp.lut" <-
function (gr.Yi, gam.model = gam.start, path = "pred1.lut")
{
Yname <- names(YYY)[gr.Yi]
cat("\n")
cat(" vvvvvvvvvv GRASP LOOKUP vvvvvvvvvv ", "\n")
cat(date(), "\n")
cat("RESPONSE NAME: ", Yname, "\n")
artificial <- as.data.frame(matrix(rep(1, 200 * length(gr.selX)), 200, length(gr.selX)))
flist <- list(c(0, 0))
for (i in 1:length(gr.selX))
if (!is.factor(XXX[, gr.selX[i]])) {
	artificial[, i] <- seq(min(XXXpred[, gr.selX[i]]), max(XXXpred[, gr.selX[i]]), length = 200)
}
else {
cat("INFO: !!! your factor map in Arcview will have to be reclassify according to the factor levels presented here:", "\n")
print(levels(XXX[gr.modmask[, gr.Yi], gr.selX[i]]))
for (f in 1:length(levels(XXX[, gr.selX[i]]))) {
if (summary(XXX[gr.modmask[, gr.Yi], gr.selX[i]])[f][1] > 0) {
	artificial[f, i] <- levels(XXX[, gr.selX[i]])[f]
	flist[[gr.selX[i]]] <- c(flist[[gr.selX[i]]], f)
}
}
for (f in 1:length(levels(XXX[, gr.selX[i]]))) {
if (summary(XXX[gr.modmask[, gr.Yi], gr.selX[i]])[f][1] == 0)
	artificial[f, i] <- levels(XXX[, gr.selX[i]])[flist[[gr.selX[i]]][1]]
}
artificial[-flist[[gr.selX[i]]], i] <- levels(XXX[, gr.selX[i]])[flist[[gr.selX[i]]][1]]
artificial[, i] <- as.factor(artificial[, i])
print(flist[[gr.selX[i]]])
}
      dimnames(artificial) <- list(1:200, names(XXX[, gr.selX]))
      print(artificial[c(1:5), ])
      cat("", "\n")
      print(artificial[c(196:200), ])
      assign("artificial", artificial, pos = 1)
      LUT <- matrix(0, nrow = 204, ncol = length(gr.selX) + 1)
      LUT[1:204, 1] <- seq(1, 204)
      LUT.SE <- LUT
      tempse <- predict(gam.model, artificial, se.fit = T, type = "terms")
      term.matrix <- tempse$fit
      se.matrix <- tempse$se.fit
      assign("term.matrix", term.matrix, pos = 1)
      assign("se.matrix", se.matrix, pos = 1)
      mod.terms <- dimnames(gam.model$model)[[2]]
      j <- 0
      for (term1 in names(XXX[gr.selX])) {
            pos1 <- grep(term1, names(XXX[gr.selX])) + 1
            testx <- term1
            pos2 <- pmatch(testx, mod.terms)
            pos3 <- pmatch(term1, names(XXX))
            if (!(pmatch(testx, mod.terms, nomatch = "nomatch") == "nomatch")) {
                  LUT[1:200, pos1] <- zapsmall(term.matrix[pos2, 1:200], 6)
                  j <- j + 1
                  LUT.SE[1:200, pos1] <- zapsmall(se.matrix[pos2, 1:200], 6)
         }
            else {
                  LUT[1:200, pos1] <- rep(0, 200)
                  LUT.SE[1:200, pos1] <- rep(0, 200)
         }
            if (!is.factor(XXX[, pos3])) {
                  LUT[201, pos1] <- min(XXXpred[, pos3])
                  LUT[202, pos1] <- max(XXXpred[, pos3])
                  LUT[203, pos1] <- 200
                  LUT[204, pos1] <- zapsmall(term.matrix[1, 1], 6)
         }
            else {
                  pos4 <- pmatch(term1, mod.terms)
                  LUT[, pos1] <- as.single(LUT[, pos1])
                  LUT.SE[, pos1] <- as.single(LUT[1:200, pos1])
                  if (!is.na(pmatch(term1, mod.terms))) {
                        print(term1)
                        LUT[1:200, pos1] <- zapsmall(term.matrix[1:200, pos4], 6)
                        LUT[-flist[[gr.selX[pos1]]], pos1] <- 0
                        j <- j + 1
                        fac.se <- NULL
                        ranklevel <- 1
                        for (level in levels(XXX[, pos3])) {
                           fac.se[ranklevel] <- tempse[[pos4]]$se.y[tempse[[pos4]]$x == level][1]
                           ranklevel <- ranklevel + 1
                     }
                        LUT.SE[1:length(levels(XXX[, pos3])), pos1] <- zapsmall(fac.se, 6)
                        LUT.SE[-flist[[gr.selX[pos1]]], pos1] <- 0
              }
                  else {
                        cat(term1, "\n")
                        cat("FACTOR OUT", "\n")
                        LUT[1:200, pos1] <- rep(0, 200)
                        LUT.SE[1:200, pos1] <- rep(0, 200)
              }
                  LUT[201, pos1] <- 1
                  LUT[202, pos1] <- length(levels(XXX[, pos3]))
                  LUT[203, pos1] <- length(levels(XXX[, pos3]))
                  LUT[204, pos1] <- zapsmall(attr(term.matrix, "constant"), 6)
        }
}
      LUT.SE[201:204, ] <- LUT[201:204, ]
      dimnames(LUT) <- list(NULL, c("row.names", names(XXX)[gr.selX]))
      dimnames(LUT.SE) <- list(NULL, c("row.names", names(XXX)[gr.selX]))
      cat("LUT prediction:", "\n")
      print(LUT[c(1:5), ])
      cat("", "\n")
      print(LUT[c(196:200), ])
      cat("LUT standard error:", "\n")
      print(LUT.SE[c(1:5), ])
      cat("", "\n")
      print(LUT.SE[c(196:200), ])
      pathlut <- paste(path, Yname, "_lut.txt", sep = "")
      print(pathlut)
      write.table(LUT, file = pathlut, sep = "    ", quote = FALSE, row.names = FALSE)
      pathse <- paste(path, Yname, "_se.txt", sep = "")
      print(pathse)
      write.table(LUT.SE, file = pathse, sep = "  ", quote = FALSE, row.names = FALSE)
}
