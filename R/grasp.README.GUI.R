"grasp.README.GUI" <-
function () 
{
README.close <- function()
	tkdestroy(README)


README <- tktoplevel()
tktitle(README) <- "README for GraspeR"

frame1 <- tkframe(README)
frame2 <- tkframe(README)

frame1.yscroll <- tkscrollbar(frame1)
frame1.text <- tktext(frame1, font = "Arial 10", yscrollcommand = paste(.Tk.ID(frame1.yscroll),"set"), height = 20)
tkconfigure(frame1.yscroll, command = paste(.Tk.ID(frame1.text),"yview"))
tkpack(frame1, frame1.text, side = "left", fill = "x")
tkpack(frame1, frame1.yscroll, side = "right", fill = "y")
zz <- tkopen("README")
zy <- tkread(zz)
tkclose(zz)
tkinsert(frame1.text, 'end', zy)

tkpack(frame2, tkbutton(frame2, text = "Close", font = "Arial 10", command = README.close))
}
