"grasp.roc" <-
function (z1, z2) 
{
# Fonction roc.plot written by Antoine Guisan (Swiss Center for Faunal Cartography, 1999)
# See table 2 in Fielding & Bell (1997), Environmental Conservation 24(1): 41
# sens = sensitivity, spec = specificity;
# z1 = vector of predicted values between 0 and 1; z2 = vector of observed values 0/1
grasp.roc.auc <- function(x)
{
    x1 <- 1 - x$spec
    y1 <- x$sens
    trapezint(x1, y1, 0, 1)
	
}

eva <- data.frame(seuil = 0, sens = 1, spec = 1)
k <- 0.01
i <- 2
while(k < 0.9) {
 	a <- table(z1 >= k, z2)[4]
	assign("a", a, pos = 1)
	b <- table(z1 >= k, z2)[2]
	c <- table(z1 >= k, z2)[3]
	assign("c", c, pos = 1)
	d <- table(z1 >= k, z2)[1]
	eva[i,"seuil"] <- k
	eva[i,"sens"] <- ifelse(is.na((a / (a + c))), 0, a / (a + c))
	eva[i,"spec"] <- d / (b + d)
	k <- k + 0.01
	i <- i + 1
}
eva[i,"seuil"] <- 1
eva[i,"sens"] <- 0
eva[i,"spec"] <- 1
assign("eva", eva, pos = 1)
return(list(data = eva, auc = grasp.roc.auc(eva)))
}
