"grasp.import.GUI" <-
function () 
{
    opendlgYYY <- function(...) {
        file.select.YYY <- tclvalue(tkgetOpenFile(parent = import))
        print(file.select.YYY)
        assign("file.select.YYY", file.select.YYY, pos = 1)
        YYY.local <- read.delim(file = file.select.YYY, sep=OPT$SEL)
        cat("Responses saved into YYY.local", "\n")
        assign("YYY.local", YYY.local, pos = 1)
        YYY.done <- TRUE
        assign("YYY.done", YYY.done, pos = 1)
        label1.full <- paste("YYY saved from ", file.select.YYY, 
            sep = " ")
        tkinsert(frame5.lstbox, "end", label1.full)
    }
    opendlgXXX <- function(...) {
        file.select.XXX <- tclvalue(tkgetOpenFile(parent = import))
        print(file.select.XXX)
        XXX.local <- read.delim(file = file.select.XXX, sep=OPT$SEL)
        cat("Responses saved into XXX.local", "\n")
        assign("XXX.local", XXX.local, pos = 1)
        label2.full <- paste("XXX saved from ", file.select.XXX, 
            sep = " ")
        tkinsert(frame5.lstbox, "end", label2.full)
        XXX.done <- TRUE
        assign("XXX.done", XXX.done, pos = 1)
    }
    opendlgXXXpred <- function(...) {
        file.select.XXXpred <- tclvalue(tkgetOpenFile(parent = import))
        print(file.select.XXXpred)
        XXXpred.local <- read.delim(file = file.select.XXXpred, 
sep=OPT$SEL)
        cat("Responses saved into XXXpred.local", "\n")
        assign("XXXpred.local", XXXpred.local, pos = 1)
        label3.full <- paste("XXXpred saved from ", file.select.XXXpred, 
            sep = " ")
        tkinsert(frame5.lstbox, "end", label3.full)
        XXXpred.done <- TRUE
        assign("XXXpred.done", XXXpred.done, pos = 1)
    }
    destroy.import <- function(...) {
        tkdestroy(import)
    }
    apply.import <- function(...) {
        label.applied <- NULL
        assign("YYY", YYY.local, pos = 1)
        label.applied <- paste(label.applied, "YYY", sep = " ")
        cat("YYY created...", "\n")
        assign("XXX", XXX.local, pos = 1)
        label.applied <- paste(label.applied, "XXX", sep = " ")
        cat("XXX created...", "\n")
        assign("XXXpred", XXXpred.local, pos = 1)
        label.applied <- paste(label.applied, "XXXpred", sep = " ")
        cat("XXXpred created...", "\n")
        label.applied <- paste("Import done for:", label.applied, 
            sep = " ")
        assign("label.applied", label.applied, pos = 1)
        tkinsert(frame5.lstbox, "end", label.applied)
        WEIGHTS <- data.frame(index = YYY[1], ours = rep(1, dim(XXX)[1]))
        assign("WEIGHTS", WEIGHTS, pos = 1)
        tkinsert(frame5.lstbox, "end", "Starting grasp.in...")
        grasp.in(YYY, XXX, XXXpred)
        tkinsert(frame5.lstbox, "end", "Initializing done!")
    }
    import <- tktoplevel()
    tktitle(import) <- "GraspeR data import"
    YYY.done <- FALSE
    assign("YYY.done", YYY.done, pos = 1)
    XXX.done <- FALSE
    assign("XXX.done", XXX.done, pos = 1)
    XXXpred.done <- FALSE
    assign("XXXpred.done", XXXpred.done, pos = 1)
    frame1 <- tkframe(import, relief = "groove", borderwidth = 2)
    frame2 <- tkframe(import, relief = "groove", borderwidth = 2)
    frame3 <- tkframe(import, relief = "groove", borderwidth = 2)
    frame4 <- tkframe(import, relief = "groove", borderwidth = 2)
    frame5 <- tkframe(import, relief = "groove", borderwidth = 2)
    frame1.label <- tklabel(frame1, text = "Import files into GraspeR")
    tkpack(frame1, frame1.label, fill = "x")
    frame2.label1 <- tklabel(frame2, text = "Responses (YYY):")
    frame2.label2 <- tklabel(frame2, text = "Predictors from (XXX):")
    frame2.label3 <- tklabel(frame2, text = "Predict to (XXXpred):")
    frame2.but1 <- tkbutton(frame2, text = "Import...", padx = 20, 
        command = opendlgYYY)
    frame2.but2 <- tkbutton(frame2, text = "import...", padx = 20, 
        command = opendlgXXX)
    frame2.but3 <- tkbutton(frame2, text = "import...", padx = 20, 
        command = opendlgXXXpred)
    tkgrid(frame2.label1, frame2.but1, sticky = "w")
    tkgrid(frame2.label2, frame2.but2, sticky = "w")
    tkgrid(frame2.label3, frame2.but3, sticky = "w")
    tkpack(frame2, fill = "x")
    frame5.lstbox <- tklistbox(frame5, selectmode = "single")
    tkpack(frame5, frame5.lstbox, fill = "x")
    apply.but.import <- tkbutton(frame4, text = "Apply", padx = 30, 
        command = apply.import)
    close.but.import <- tkbutton(frame4, text = "Close", padx = 30, 
        command = destroy.import)
    tkgrid(apply.but.import, close.but.import, sticky = "w")
    tkpack(frame4)
}
