"grasp.GUI" <-
function (grass.on = FALSE) 
{
    require(tcltk) || stop("Tcl/Tk is not available, see README for installation and configuration...")
    require(mgcv) || stop("Mgcv library is not available, install it from CRAN...")
    require(MASS) || stop("MASS library is not available, install it from CRAN...")
    require(modreg) || stop("Modreg library is not available, install it from CRAN...")
    if (grass.on) 
        require(GRASS)
    d.erase <- function(...) {
        system("d.erase")
    }
    g.list.rast <- function(...) {
        system("g.list rast")
    }
    d.vect.area <- function(...) {
        system("d.vect map=rivieres color=blue")
        system("d.area map=lacs fillcolor=blue linecolor=blue")
    }
    startmon <- function(...) {
        system("d.mon x0")
    }
    stopmon <- function(...) {
        system("d.mon stop=x0")
    }
    selectgrid.export <- function(...) {
        GRASS.export <- function(...) {
            grasp.GRASS(tclvalue(YYname))
            tkdestroy(gridselexport)
        }
        gridselexport <- tktoplevel()
        tktitle(gridselexport) <- "Select grid"
        Yname <- names(YYY[, gr.Yi])
        YYname <- tclVar(paste(Yname, "_pred"))
        gridselexport.label <- tklabel(gridselexport, text = "Grid name to export to:")
        gridselexport.entry <- tkentry(gridselexport, textvariable = YYname, 
            width = 10)
        gridselexport.but <- tkbutton(gridselexport, text = "Export", 
            command = GRASS.export)
        tkpack(gridselexport.label, gridselexport.entry, gridselexport.but, 
            fill = "x")
    }
    selectgrid.show <- function(...) {
        GRASS.show <- function(...) {
            Gname <- tclvalue(YYname)
            Gfunct <- paste("system(", "\"", "d.rast ", Gname, 
                "\"", ")", sep = "")
            eval(parse(text = Gfunct))
            tkdestroy(gridselshow)
        }
        gridselshow <- tktoplevel()
        tktitle(gridselshow) <- "Select grid"
        Yname <- names(YYY[, gr.Yi])
        YYname <- tclVar(paste(Yname, "pred", sep = ""))
        gridselshow.label <- tklabel(gridselshow, text = "Grid name to show:")
        gridselshow.entry <- tkentry(gridselshow, textvariable = YYname, 
            width = 10)
        gridselshow.but <- tkbutton(gridselshow, text = "Show", 
            command = GRASS.show)
        tkpack(gridselshow.label, gridselshow.entry, gridselshow.but, 
            fill = "x")
    }
    XXX.fix <- function(..) fix(XXX)
    grasp.limits.GUI.spec <- function(...) grasp.limits.GUI()
    grasp.corlim.plot <- function(...) lapply(gr.selY, grasp.corlim)
    grasp.update.packages <- function(...) update.packages()
    close.all <- function(...) q("yes")
    grasp.ascii.export <- function(...) grasp.ascii()
    grasp.export.txt <- function() grasp.export()
    YYY.fix <- function(...) fix(YYY)
    XXXpred.fix <- function(...) fix(XXXpred)
    WEIGHTS.fix <- function(...) fix(WEIGHTS)
    grasp.pred.gui <- function(...) grasp.pred()
    test.apply.ok <- function(...) {
        if (apply.ok == FALSE) 
            stop("Set options first...")
    }
    grasp.histo.plot <- function(...) {
        lapply(gr.selY, grasp.histo)
    }
    grasp.RvsP.plot <- function(...) {
        lapply(gr.selY, grasp.RvsP)
    }
    grasp.pred.plot.plot <- function(...) {
        par(mfrow = c(1, 1), mai = c(0.5, 0.5, 0.5, 0.5), tcl = 0.25)
        grasp.pred.plot(gr.predmat)
    }
    grasp.model.plot <- function(...) {
        par(cex = 0.8, mai = c(0.5, 0.5, 0.5, 0.75))
        plot.gam(gam.start, pages = 1, scale = 0, n = 1000)
        title(main = "GRASP MODEL PLOTS", sub = gam.start$formula)
    }
    grasp.cormat.plot <- function(...) lapply(gr.selY, grasp.cormat)
    grasp.datamap.plot <- function(...) lapply(gr.selY, grasp.datamap)
    grasp.model.go <- function(...) lapply(gr.selY, grasp.model)
    about.msgbox <- function(...) tkmessageBox(icon = "info", 
        message = "R-GRASP - By A. Lehmann. Ported to R by F. Fivaz", 
        type = "ok", parent = main, title = "About...")
    main.close <- function(...) tkdestroy(main)
    grasp.persp.plot <- function(...) persp.gam(gam.start)
    grasp.model.summary <- function(...) {
        cat("Warning : this function only works if you already have created a model", 
            "\n")
        sum.gam <- summary.gam(gam.start)
        print(sum.gam)
    }
    grasp.model.check <- function(...) gam.check(gam.start)
    grasp.step.anova <- function(...) grasp.step.gam(gam.start, 
        direction = "both")
    grasp.scope.list <- function(...) grasp.scope(gr.selX)
    grasp.summary.gui <- function(...) lapply(gr.selY, grasp.summary)
    main <- tktoplevel()
    tktitle(main) <- "GraspeR - Generalized Regression Analysis and Spatial Predictions for R"
    main.statusbar <- tkframe(main)
    main.statusbar.label <- tklabel(main.statusbar, text = "Welcome to GraspeR", 
        relief = "sunken", anchor = "w")
    tkpack(main.statusbar.label, side = "left", padx = 2, expand = "yes", 
        fill = "both")
    tkpack(main.statusbar, side = "bottom", fill = "x", pady = 2)
    main.menu <- tkmenu(main, tearoff = 0)
    main.menu.file <- tkmenu(main, tearoff = 0)
    tkadd(main.menu, "cascade", menu = main.menu.file, label = "File", 
        underline = 0)
    tkadd(main.menu.file, "command", label = "Import...", command = grasp.import.GUI)
    tkadd(main.menu.file, "command", label = "GraspeR in...", 
        command = grasp.in.GUI)
    tkadd(main.menu.file, "separator")
    tkadd(main.menu.file, "command", label = "Export to ASCII...", 
        command = grasp.ascii.export)
    tkadd(main.menu.file, "command", label = "Export to points...", 
        command = grasp.export.txt)
    tkadd(main.menu.file, "separator")
    tkadd(main.menu.file, "command", label = "Close GUI", command = main.close)
    tkadd(main.menu.file, "command", label = "Save workspace and exit", 
        command = close.all)
    main.menu.edit <- tkmenu(main, tearoff = 0)
    tkadd(main.menu, "cascade", menu = main.menu.edit, label = "Edit", 
        underline = 0)
    tkadd(main.menu.edit, "command", label = "Set options...", 
        command = grasp.options.GUI)
    tkadd(main.menu.edit, "command", label = "Set responses...", 
        command = grasp.select.responses)
    tkadd(main.menu.edit, "command", label = "Set predictors...", 
        command = grasp.select.predictors)
    tkadd(main.menu.edit, "command", label = "Set limits...", 
        command = grasp.limits.GUI.spec)
    tkadd(main.menu.edit, "separator")
    tkadd(main.menu.edit, "command", label = "YYY", command = YYY.fix)
    tkadd(main.menu.edit, "command", label = "XXX", command = XXX.fix)
    tkadd(main.menu.edit, "command", label = "XXXpred", command = XXXpred.fix)
    tkadd(main.menu.edit, "command", label = "WEIGHTS", command = WEIGHTS.fix)
    main.menu.analysis <- tkmenu(main, tearoff = 0)
    tkadd(main.menu, "cascade", menu = main.menu.analysis, label = "Analysis", 
        underline = 0)
    tkadd(main.menu.analysis, "command", label = "Summary", command = grasp.summary.gui)
    tkadd(main.menu.analysis, "command", label = "Summary of model", 
        command = grasp.model.summary)
    tkadd(main.menu.analysis, "separator")
    tkadd(main.menu.analysis, "command", label = "Model", command = grasp.model.go)
    tkadd(main.menu.analysis, "command", label = "Create scope list", 
        command = grasp.scope.list)
    tkadd(main.menu.analysis, "command", label = "Stepwise selection using ANOVA", 
        command = grasp.step.anova)
    tkadd(main.menu.analysis, "command", label = "Predict", command = grasp.pred.gui)
    tkadd(main.menu.analysis, "command", label = "Check GAM model",
        command = grasp.model.check)
    tkadd(main.menu.analysis, "separator")
    tkadd(main.menu.analysis, "command", label = "Automated selection, model and predict...", 
        command = grasp.model.GUI)
    main.menu.plot <- tkmenu(main, tearoff = 0)
    tkadd(main.menu, "cascade", menu = main.menu.plot, label = "Plot", 
        underline = 0)
    tkadd(main.menu.plot, "command", label = "Data map", command = grasp.datamap.plot)
    tkadd(main.menu.plot, "command", label = "Histograms", command = grasp.histo.plot)
    tkadd(main.menu.plot, "command", label = "Response vs. predictors", 
        command = grasp.RvsP.plot)
    tkadd(main.menu.plot, "command", label = "Correlations", 
        command = grasp.cormat.plot)
    tkadd(main.menu.plot, "command", label = "Covariate space", 
        command = grasp.corlim.plot)
    tkadd(main.menu.plot, "separator")
    tkadd(main.menu.plot, "command", label = "Model", command = grasp.model.plot)
    tkadd(main.menu.plot, "command", label = "Perspective plot of model", 
        command = grasp.persp.plot)
    tkadd(main.menu.plot, "command", label = "Predictions", command = grasp.pred.plot.plot)
    if (grass.on) {
        main.menu.GRASS <- tkmenu(main, tearoff = 0)
        tkadd(main.menu, "cascade", menu = main.menu.GRASS, label = "GRASS", 
            underline = 0)
        tkadd(main.menu.GRASS, "command", label = "Start monitor X0", 
            command = startmon)
        tkadd(main.menu.GRASS, "command", label = "Stop monitor X0", 
            command = stopmon)
        tkadd(main.menu.GRASS, "command", label = "GraspeR.GRASS (Create grid)...", 
            command = selectgrid.export)
        tkadd(main.menu.GRASS, "command", label = "Convert grid to integer...")
        tkadd(main.menu.GRASS, "command", label = "Display grid...", 
            command = selectgrid.show)
        tkadd(main.menu.GRASS, "command", label = "Display rivers and lakes", 
            command = d.vect.area)
        tkadd(main.menu.GRASS, "command", label = "Erase currently selected monitor", 
            command = d.erase)
        tkadd(main.menu.GRASS, "command", label = "Change grid colors...")
        tkadd(main.menu.GRASS, "command", label = "List available grids", 
            command = g.list.rast)
    }
    main.menu.help <- tkmenu(main, tearoff = 0)
    tkadd(main.menu, "cascade", menu = main.menu.help, label = "Help", 
        underline = 0)
    tkadd(main.menu.help, "command", label = "Contents")
    tkadd(main.menu.help, "command", label = "TODO", command = grasp.TODO.GUI)
    tkadd(main.menu.help, "command", label = "README...", command = grasp.README.GUI)
    tkadd(main.menu.help, "command", label = "License...", command = grasp.gpl.GUI)
    tkadd(main.menu.help, "separator")
    tkadd(main.menu.help, "command", label = "About...", command = grasp.about.GUI)
    tkadd(main.menu.help, "separator")
    tkadd(main.menu.help, "command", label = "Update packages", 
        command = grasp.update.packages)
    tkconfigure(main, menu = main.menu)
    assign("main", main, pos = 1)
    frame1 <- tkframe(main, relief = "groove", borderwidth = 2)
    main.backgrd.label.main <- tklabel(frame1, text = "GraspeR", 
        anchor = "w", font = "arial 20", justify = "left")
    main.backgrd.label.sub <- tklabel(frame1, text = "Generalized Regression Analysis and Spatial Prediction for R", 
        anchor = "w", font = "arial 14", justify = "left")
    tkpack(frame1, main.backgrd.label.main, main.backgrd.label.sub, 
        side = "top", fill = "x")
}
