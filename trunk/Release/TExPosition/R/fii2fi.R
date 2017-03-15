fii2fi <-
function(DESIGN,fii,fi){
	Dsup <- fastEucCalc(fii,fi)
	minD <- apply(Dsup,1,min)
	Group_Assigned <- Re(Dsup==matrix(minD,nrow(DESIGN),ncol(DESIGN)))
	return(list(distances=Dsup,assignments=Group_Assigned,confusion=t(Group_Assigned) %*% DESIGN))
}
