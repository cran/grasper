"grasp.model.GUI" <-
function () 
{

kill.MODEL <- function()
{
    tkdestroy(MODEL)
}

go.model <- function(...)
{
step.fam <- c("gaussian()", "quasibinomial()", "quasibinomial()", "quasipoisson()")
step.test <- c("Chisq", "F", "F", "F")

model.auto$test <- tkcurselection(frame2.list1)
model.auto$test <- tclvalue(model.auto$test)
model.auto$family <- step.fam[as.numeric(model.auto$test) + 1]
model.auto$test <- step.test[as.numeric(model.auto$test) + 1]
model.auto$direction <- tkcurselection(frame2.list2)
model.auto$direction <- tclvalue(model.auto$direction)
model.auto$direction <- dirselect[as.numeric(model.auto$direction) + 1]
model.auto$resolution <- as.numeric(tclvalue(MODEL.RESOLUTION))
model.auto$plimit <- as.numeric(tclvalue(MODEL.P.LIMIT))
model.auto$steps <- as.numeric(tclvalue(MODEL.STEPS))
model.auto$df <- as.numeric(tclvalue(MODEL.DF))
model.auto$stepit <- as.logical(tclvalue(MODEL.STEP.IT))
model.auto$verbose <- as.logical(tclvalue(MODEL.VERBOSE))
model.auto$auto.df <- as.logical(tclvalue(MODEL.AUTO.DF))
model.auto$predictit <- as.logical(tclvalue(MODEL.DO.PRED))
model.auto$plotpredictit <- as.logical(tclvalue(MODEL.PLOT.PRED))
assign("model.auto", model.auto, pos = 1)

grasp.model(trace = model.auto$verbose, df = model.auto$df, calcdf = model.auto$auto.df, stepfam = model.auto$family)
grasp.scope(gr.selX, df = model.auto$df, calcdf = model.auto$auto.df)
if (model.auto$stepit)
    grasp.step.gam(direction = model.auto$direction, steps = model.auto$steps, trace = model.auto$model.auto$verbose, limit = model.auto$plimit, test = model.auto$test)
if (model.auto$predictit)
    grasp.pred()
if (model.auto$plotpredictit)
    grasp.pred.plot(gr.predmat, resolution = model.auto$resolution)
}

MODEL <- tktoplevel()
tktitle(MODEL) <- "GraspeR variable selection, model and predictions"

model.auto <- vector("list", 12)
names(model.auto) <- c("stepit", "predictit", "plotpredictit", "direction", "test", "plimit", "steps", "verbose", "family", "df", "auto.df", "resolution")

stepselect <- c("Biomass, size, ...", "Presence / absence", "Cover, %, ...", "Richness, Abundance, ...")
dirselect <- c("both", "backward", "forward")
famselect <- c("Gaussian", "Poisson", "Binomial", "Quasibinomial")

MODEL.P.LIMIT <- tclVar("0.05")
MODEL.STEPS <- tclVar(1000)
MODEL.VERBOSE <- tclVar("FALSE")
MODEL.STEP.IT <- tclVar("TRUE")
MODEL.DF <- tclVar(4)
MODEL.AUTO.DF <- tclVar("FALSE")
MODEL.DO.PRED <- tclVar("TRUE")
MODEL.PLOT.PRED <- tclVar("TRUE")
MODEL.RESOLUTION <- tclVar(1000)

frame1 <- tkframe(MODEL, relief = "groove", borderwidth = 2)
frame2 <- tkframe(MODEL, relief = "groove", borderwidth = 2)
frame3 <- tkframe(MODEL, relief = "groove", borderwidth = 2)
frame4 <- tkframe(MODEL, relief = "groove", borderwidth = 2)
frame5 <- tkframe(MODEL, relief = "groove", borderwidth = 2)

frame1.label1 <- tklabel(frame1, text = "GraspeR Modeling", font = "Arial 11")
frame1.label2 <- tklabel(frame1, text = "This window lets you define and execute the modelling steps", font = "Arial 10", wraplength = 280)
tkpack(frame1, frame1.label1, frame1.label2, fill = "x")

frame2.label1 <- tklabel(frame2, text = "Stepwise selection", font = "Arial 11")
tkgrid(frame2.label1, columnspan = 2)
frame2.label21 <- tklabel(frame2, text = "Check to do the stepwise selection", font = "Arial 10")
frame2.check2 <- tkcheckbutton(frame2, variable = MODEL.STEP.IT, height = 1, offvalue = "FALSE", onvalue = "TRUE")
tkgrid(frame2.label21, frame2.check2)
frame2.label2 <- tklabel(frame2, text = "Select the kind of data you have", font = "Arial 10")
tkgrid(frame2.label2, columnspan = 2)
frame2.list1 <- tklistbox(frame2, selectmode = "single", height = 4, font = "Arial 10", exportselection = FALSE, width = 25)
for (i in 1:4) {
    tkinsert(frame2.list1, 'end', stepselect[i])
}
tkselection.set(frame2.list1, 1)
tkgrid(frame2.list1, columnspan = 2)
frame2.labela <- tklabel(frame2, text = "Double-click on the selection above to see test and family for the associated data type", font = "Arial 10", wraplength = 250)
tkgrid(frame2.labela, columnspan = 2)
frame2.entrya <- tkentry(frame2, width = 10, font = "Arial 10", state = "disabled", text = "Quasibinomial")
frame2.entryb <- tkentry(frame2, width = 15, font = "Arial 10", state = "disabled", text = "F")
tkgrid(frame2.entrya, frame2.entryb)
frame2.label3 <- tklabel(frame2, text = "Direction", font = "Arial 10")
tkgrid(frame2.label3, columnspan = 2)
frame2.list2 <- tklistbox(frame2, selectmode = "single", height = 3, font = "Arial 10", exportselection = FALSE)
for (i in 1:3) {
    tkinsert(frame2.list2, 'end', dirselect[i])
}
tkselection.set(frame2.list2, 0)
tkgrid(frame2.list2, columnspan = 2)
frame2.label4 <- tklabel(frame2, text = "P.limit for selection", font = "Arial 10")
frame2.entry1 <- tkentry(frame2, textvariable = MODEL.P.LIMIT, width = 10, justify = "left", font = "Arial 10")
tkgrid(frame2.label4, frame2.entry1)
frame2.label5 <- tklabel(frame2, text = "Number of steps", font = "Arial 10")
frame2.entry2 <- tkentry(frame2, textvariable = MODEL.STEPS, width = 10, justify = "left", font = "Arial 10 ")
tkgrid(frame2.label5, frame2.entry2)
frame2.label6 <- tklabel(frame2, text = "Check to force verbose output", font = "Arial 10")
frame2.check1 <- tkcheckbutton(frame2, variable = MODEL.VERBOSE, height = 1, offvalue = "FALSE", onvalue = "TRUE")
tkgrid(frame2.label6, frame2.check1)
tkpack(frame2, fill = "x")

frame3.label1 <- tklabel(frame3, text = "Modeling using GAMs", font = "Arial 11")
tkgrid(frame3.label1, columnspan = 2)
frame3.label2 <- tklabel(frame3, text = "Family depends on the selections above", font = "Arial 10")
tkgrid(frame3.label2, columnspan = 2)
frame3.label3 <- tklabel(frame3, text = "Smoothing degrees of freedom", font = "Arial 10")
frame3.entry1 <- tkentry(frame3, textvariable = MODEL.DF, width = 10, justify = "left", font = "Arial 10")
tkgrid(frame3.label3, frame3.entry1)
frame3.label4 <- tklabel(frame3, text = "Let the gam function calculate df", font = "Arial 10")
frame3.check3 <- tkcheckbutton(frame3, variable = MODEL.AUTO.DF, height = 1, offvalue = "FALSE", onvalue = "TRUE")
tkgrid(frame3.label4, frame3.check3)
tkpack(frame3, fill = "x")

frame4.label1 <- tklabel(frame4, text = "Predict", font = "Arial 11")
tkgrid(frame4.label1, columnspan = 2)
frame4.label2 <- tklabel(frame4, text = "Check to calculate predictions", font = "Arial 10")
frame4.check1 <- tkcheckbutton(frame4, variable = MODEL.DO.PRED, height = 1, offvalue = "FALSE", onvalue = "TRUE")
tkgrid(frame4.label2, frame4.check1)
frame4.label3 <- tklabel(frame4, text = "Check to plot predictions", font = "Arial 10")
frame4.check2 <- tkcheckbutton(frame4, variable = MODEL.PLOT.PRED, height = 1, offvalue = "FALSE", onvalue = "TRUE")
tkgrid(frame4.label3, frame4.check2)
frame4.label4 <- tklabel(frame4, text = "Resolution for output", font = "Arial 10")
frame4.entry1 <- tkentry(frame4, textvariable = MODEL.RESOLUTION, width = 10, justify = "left", font = "Arial 10")
tkgrid(frame4.label4, frame4.entry1)
tkpack(frame4, fill = "x")

frame5.but1 <- tkbutton(frame5, text = "Gogogooo!", padx = 40, font = "Arial 10", command = go.model)
frame5.but2 <- tkbutton(frame5, text = "Loosing my time!", padx = 40, font = "Arial 10", command = kill.MODEL)
tkgrid(frame5.but1, frame5.but2)
tkpack(frame5, fill = "x")

tkbind(frame2.list1, "<1>", function() {
    
    step.fam <- c("Gaussian", "Quasibinomial", "Quasibinomial", "Quasipoisson")
    step.test <- c("Chisq", "F", "F", "F")
    mod.test <- tkcurselection(frame2.list1)
    mod.test <- tclvalue(mod.test)
    mod.test.local <- tclVar(step.test[as.numeric(mod.test) + 1])
    mod.fam <- tclVar(step.fam[as.numeric(mod.test) + 1])
    tkconfigure(frame2.entrya, textvariable = mod.test.local)
    tkconfigure(frame2.entryb, textvariable = mod.fam)
})

}
