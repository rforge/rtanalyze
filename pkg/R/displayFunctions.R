#############################################
# RTanalyze display Functions				#
# Copyright(c) 2009 Wouter D. Weeda			#
# University of Amsterdam					#
#############################################



#CONTAINS
#diffmodGUI


dmgui <- function(sdat) 
{
	#load and make toplevel
	library(tcltk)
	tt <- tktoplevel()
	tktitle(tt) <- paste('rtanalyze Diffusion Model Analysis (using fast-DM)',sep='')
	tkgrid(tklabel(tt,text=paste('Loaded experiment:',.subjects.experimentname(sdat),'\n')),columnspan=8)
	
	#make scroll-bar
	scr <- tkscrollbar(tt, repeatinterval=5,command=function(...)tkyview(tl,...))
	tl<-tklistbox(tt,height=12,selectmode="single",yscrollcommand=function(...)tkset(scr,...),background="white")
		
	tkgrid(tklabel(tt,text="Available conditions"))
	tkgrid(tl,scr)
	tkgrid.configure(scr,rowspan=8,sticky="nsw")
	
	#fill with names of conditions
	fv = which(.subjects.valid(sdat)==TRUE)
	cnames = names(.rtdata.conditions(.subjects.rtdata(sdat)[[fv[1]]]))
	for (i in 1:length(cnames)) tkinsert(tl,"end",cnames[i])
	tkselection.set(tl,0)
	
	dcb = vector('list',7)
	dmc = vector('list',7)
	cn = names(dmc) = c('v','a','t0','z','sz','st0','sv')
	
	tkgrid(tklabel(tt,text="Depends over conditions"))
	tkgrid(tl,scr)
	tkgrid.configure(scr,rowspan=8,sticky="nsw")
	
	#add checkboxes
	for(i in 1:7) 
	{
		
		dcb[[i]] <- tkcheckbutton(tt)
		dmc[[i]] = tclVar("0")
		
		tkconfigure(dcb[[i]],variable=dmc[[i]])
		tkgrid(tklabel(tt,text=cn[i]),dcb[[i]])
		
	}
	
	#use the tclvar character conversion (make matrix with 0 and zeroes), make list of vars, set tclvar for each list item (parameter), update cb values.
	
	#Make list for every condition name
	depmat = vector('list',length(cnames))
	for(i in 1:length(depmat)) {
		depmat[[i]] = dmc
	}
	
	#add display button
	dispCB <- function() 
	{
		ci <- as.integer(tkcurselection(tl))+1
		
		for(i in 1:7) {
			dmc[[i]]=depmat[[ci]][[i]]
			tkconfigure(dcb[[i]],variable=dmc[[i]])
		}
		#browser()
	}
	
	tkbind(tl,"<Button-1>",dispCB)
	
	#add assign button
	pressAssign <- function() 
	{
		ci <- as.integer(tkcurselection(tl))+1
		for(i in 1:7) depmat[[ci]][[i]] = dmc[[i]]
				
	}
	assign.but <- tkbutton(tt, text = "Assign", command = pressAssign)
	tkgrid(assign.but)		# Place the button on the window
	
	
	
	tkfocus(tt)
	
}


dmgui_example <- function() 
{
	library(tcltk)
	
	tt <- tktoplevel()
	tktitle(tt) <- paste('COOL',sep='')
	
	
	scr <- tkscrollbar(tt, repeatinterval=5,command=function(...)tkyview(tl,...))
	tl<-tklistbox(tt,height=4,selectmode="single",yscrollcommand=function(...)tkset(scr,...),background="white")
	
	
	showSelec = function() 
	{
		ci <- as.integer(tkcurselection(tl))
		cat('Selection =',ci+1,'\n')
		
		if(ci==1) {
			
			cbValue <- tclVar("1")
			cbValue2 <- tclVar("0")
			tkconfigure(cb,variable=cbValue)
			tkconfigure(cb2,variable=cbValue2)
		}
		
		if(ci==0) {
			
			cbValue <- tclVar("0")
			cbValue2 <- tclVar("1")
			tkconfigure(cb,variable=cbValue)
			tkconfigure(cb2,variable=cbValue2)
		}
		
		
	}
	
	tkbind(tl,"<Button-1>",showSelec)
	
	tkgrid(tklabel(tt,text="Available conditions"))
	tkgrid(tl,scr)
	
	tkgrid.configure(scr,rowspan=4,sticky="nsw")
	
	fruits <- c("Stimulus","Condition")
	for (i in (1:2))
	{
		tkinsert(tl,"end",fruits[i])
	}
	tkselection.set(tl,2)  
	
	cb <- tkcheckbutton(tt)
	cbValue <- tclVar("0")
	
	tkconfigure(cb,variable=cbValue)
	tkgrid(tklabel(tt,text="v"),cb)
	
	cb2 <- tkcheckbutton(tt)
	cbValue2 <- tclVar("1")
	
	tkconfigure(cb2,variable=cbValue2)
	tkgrid(tklabel(tt,text="a"),cb2)
	
	OK.but <- tkbutton(tt, text = "  OK  ",command = function() tclvalue(done) <- 1)
	Cancel.but <- tkbutton(tt, text = "Cancel",command = function() tclvalue(done) <- 2)
	
	# Place the two buttons on the same row in their assigned window (tt)
	tkgrid(OK.but, Cancel.but)
	
	
	
	#scr <- tkscrollbar(tt, repeatinterval=5,command=function(...)tkyview(txt,...))
	#txt <- tktext(tt,bg="white",font="courier",yscrollcommand=function(...)tkset(scr,...),height=50,width=45)
	#tkgrid(txt,scr)
	#tkgrid.configure(scr,sticky="ns")
	
	#tkinsert(txt,"end","JA")
	#tkconfigure(txt,state="disabled")
	#tkfocus(txt)
	
	
	browser()
	
}
