"grasp.about.GUI" <-
function () 
{

about.close <- function()
{
	tkdestroy(about)
}

about <- tktoplevel()
tktitle(about) <- "About GraspeR"

about.platform <- version[[1]]
about.version <- paste(version[[6]], version[[7]], sep = ".")
about.date <- paste(version[[10]], version[[9]], version[[8]], sep = "/")

frame1 <- tkframe(about, relief = "groove", borderwidth = 2)
frame2 <- tkframe(about, relief = "groove", borderwidth = 2)
frame3 <- tkframe(about, relief = "groove", borderwidth = 2)

frame1.label1 <- tklabel(frame1, text = "GraspeR version 0.3-2", font = "Arial 11")
frame1.label2 <- tklabel(frame1, text = "Written by F. Fivaz (fabien.fivaz@bluewin.ch)", font = "Arial 10")
frame1.label3 <- tklabel(frame1, text = "Original programming on S-Plus by Anthony Lehmann,", font = "Arial 10")
frame1.label12 <- tklabel(frame1, text = "John Leathwich and Jake McOverton (Landcare Research Institute, NZ)", font = "Arial 10")
frame1.label4 <- tklabel(frame1, text = "http://www.cscf.ch/grasp", font = "Arial 10")
frame1.label5 <- tklabel(frame1, text = "Released under the terms of the GPL license", font = "Arial 10")
frame1.label6 <- tklabel(frame1, text = "Swiss Center of Faunal Cartography", font = "Arial 9")
frame1.label7 <- tklabel(frame1, text = "Terreaux 14", font = "Arial 9")
frame1.label8 <- tklabel(frame1, text = "2000 Neuchâtel", font = "Arial 9")
frame1.label9 <- tklabel(frame1, text = "Switzerland", font = "Arial 9")
frame1.label10 <- tklabel(frame1, text = "+41 32 725 72 57", font = "Arial 9")
frame1.label11 <- tklabel(frame1, text = "http://www.cscf.ch/", font = "Arial 9")
tkpack(frame1, frame1.label1, frame1.label2, frame1.label3, frame1.label12, frame1.label4, frame1.label5, frame1.label6, frame1.label7, frame1.label8, frame1.label9, frame1.label10, frame1.label11, fill = "x")

frame2.label1 <- tklabel(frame2, text = "Platform", font = "Arial 10")
frame2.label11 <- tklabel(frame2, text = about.platform, font = "Courrier 10")
tkgrid(frame2.label1, frame2.label11)
frame2.label2 <- tklabel(frame2, text = "Version of R", font = "Arial 10")
frame2.label21 <- tklabel(frame2, text = about.version, font = "Courrier 10")
tkgrid(frame2.label2, frame2.label21)
frame2.label3 <- tklabel(frame2, text = "R release date", font = "Arial 10")
frame2.label31 <- tklabel(frame2, text = about.date, font = "Courrier 10")
tkgrid(frame2.label3, frame2.label31)
tkpack(frame2, fill = "x")

frame3.button1 <- tkbutton(frame3, text = "Close", command = about.close, padx = 30, font = "Arial 10")
frame3.button2 <- tkbutton(frame3, text = "License", command = grasp.gpl.GUI, padx = 30, font = "Arial 10")
tkgrid(frame3.button1, frame3.button2)
tkpack(frame3)
}
