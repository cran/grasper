"grasp.stat.anova" <-
function (table, test = c("Chisq", "F", "Cp"), scale, df.scale, n) 
{ 
    test <- match.arg(test)
    dev.col <- match("Deviance", colnames(table))
    if (is.na(dev.col)) 
        dev.col <- match("Sum of Sq", colnames(table))
    if (is.na(dev.col))
        stop("Anova objects need a \"Deviance\" or a \"Sum of Sq\" columns for the F test")
    switch(test, Chisq = {
        cbind(table, "P(>|Chi|)" = pchisq(abs(table[, dev.col]/scale), 
            abs(table[, "Df"]), lower.tail = FALSE))
    }, F = {
        Fvalue <- abs((table[, dev.col]/table[, "Df"])/scale)
        Fvalue[table[, "Df"] == 0] <- NA
        cbind(table, F = Fvalue, "Pr(>F)" = pf(Fvalue, abs(table[, "Df"]), abs(df.scale), lower.tail = FALSE))
    }, Cp = {
        cbind(table, Cp = table[, "Resid. Dev"] + 2 * scale * 
            (n - table[, "Resid. Df"]))
    })
}
