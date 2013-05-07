#############################################
# RTanalyze preProcessing Functions			#
# Copyright(c) 2009 Wouter D. Weeda			#
# University of Amsterdam					#
#############################################



#CONTAINS
#checkSubjects
#export.rt (longform) #DEFUNCT
#wpr

checkSubjects <-
function(subjects,min.n=10)
#check if subjects have a minimal of VALID responses (overall)
{
	if(class(subjects)!='subjects') stop('input must be of class \'subjects\'')
	
	for(rtdat in 1:length(.subjects.rtdata(subjects))) 	if(length(which(.rtdata.valid(.subjects.rtdata(subjects)[[rtdat]])==TRUE))<10) .subjects.valid(subjects)[rtdat]=FALSE
			
	return(subjects)
	
}

export.rt <- function(subjectdata,which=NULL,ID.ind=NULL) 
#put all RT's in longform (for SPSS export)
{
	if(missing(which)) which=numeric(0)
		
	subs = which(subjectdata@valid==TRUE)
	
	longrt = numeric(0)
	
	for(i in 1:length(subs)) {
		
		rtdat = .subjects.rtdata(subjectdata)[[subs[i]]]
		validdat = .rtdata.rt(rtdat)[which(.rtdata.valid(rtdat)==TRUE)]
		repeatvec = seq(1:length(validdat))
		
		
		#check which independents to use
		if(is.character(which)) which = match(which,names(.rtdata.conditions(rtdat)))
		
		#set subjectindicator variable
		if(is.null(ID.ind)) {
			ID = rep(subs[i],length(validdat))
		} else {
			if(is.numeric(ID.ind)) {
				ID = rep(.subjects.variables(subjectdata)[subs[i],length(validdat)])
			} else {
				ID = rep(.subjects.variables(subjectdata)[subs[i],which(names(.subjects.variables(subjectdata))==as.character(ID.ind))],length(validdat))			
			}
		}
		
		rtout = cbind(ID,as.character(.rtdata.conditions(rtdat)[which(.rtdata.valid(rtdat)==TRUE),which]),repeatvec,.rtdata.rt(rtdat)[which(.rtdata.valid(rtdat)==TRUE)],as.numeric(.rtdata.correct(rtdat)[which(.rtdata.valid(rtdat)==TRUE)]))
		longrt = rbind(longrt,rtout)
		
	}
	
	#structure dataframe and give correct measurement levels
	longrt = as.data.frame(longrt,stringsAsFactors=FALSE)
	names(longrt) = c('ID',names(.rtdata.conditions(rtdat))[which],'REPEATID','RT','RESPONSE')
	longrt$REPEATID = as.numeric(longrt$REPEATID)
	longrt$ID = as.numeric(longrt$ID)
	longrt$RT = as.numeric(longrt$RT)
	longrt$RESPONSE = as.numeric(longrt$RESPONSE)
	
	return(longrt)
	
	
}

wpr <- function(subjectdata,iq.indicator,which.within=numeric(0),quantiles=c(.1,.3,.5,.7,.9),ID.ind=NULL,templatesub=1) 
#calculate worst performance rule (rt*iq correlations per quantile)
{
	
	subs = which(subjectdata@valid==TRUE)
	qframe = matrix(NA,0,length(quantiles))
	
	#calculate quantiles for each valid subject (in long form)
	for(i in 1:length(subs)) {
		
		qd = quantile.rtdata(.subjects.rtdata(subjectdata)[[subs[i]]],which=which.within,quantiles=quantiles)
		qd = qd$quantiles[qd$quantiles$correct==TRUE,]
		
		#set subjectindicator variable
		if(is.null(ID.ind)) {
			ID = rep(subs[i],nrow(qd))
		} else {
			if(is.numeric(ID.ind)) {
				ID = rep(.subjects.variables(subjectdata)[subs[i],nrow(qd)])
			} else {
				ID = rep(.subjects.variables(subjectdata)[subs[i],which(names(.subjects.variables(subjectdata))==as.character(ID.ind))],nrow(qd))			
			}
		}
		qd=cbind(ID,qd)
		qframe = rbind(qframe,qd)
	}
	
	#get IQ vector
	if(is.numeric(iq.indicator)) {
		iqvec = .subjects.variables(subjectdata)[,iq.indicator]
	} else {
		iqvec = .subjects.variables(subjectdata)[,which(names(.subjects.variables(subjectdata))==as.character(iq.indicator))]
	}
	
	#select only valids
	iqvec = iqvec[subs]
	
	#make conditions etc.
	totlev = makelevels(which.within,subjectdata@rtdata[[templatesub]])
	totmat = makeconditionarray(which.within,totlev)
	
	condnames = colnames(totmat)
	dmcondnames = rownames(totmat)
	
	outframe = pframe = as.data.frame(matrix(NA,length(dmcondnames),length(quantiles),dimnames=list(dmcondnames,quantiles)))

	for(dmcond in 1:dim(totmat)[1])	{
		evstring = paste('dat = qframe[qframe$`',condnames[1],'`==\'',totmat[dmcond,1],'\'',sep='')
		
		if(length(condnames)>1) {
			for(i in 2:length(condnames)) {
				evstring = paste(evstring,' & qframe$`',condnames[i],'`==\'',totmat[dmcond,i],'\'',sep='') 
			}
		}
		
		evstring = paste(evstring,',]',sep='')
		eval(parse(text=evstring))
		
		for(j in 1:length(quantiles)) {
			
			col = ncol(qframe)-length(quantiles)+j
			qvec = dat[,col]

			corest = cor.test(iqvec,qvec)
			outframe[dmcond,j] = corest$estimate
			pframe[dmcond,j] = corest$p.value
			
		}
		
	}
	
	## TODO
	# add functionality to add number of observations per bin.
	# or do variables-with-error analysis
	
	return(list(estimates=outframe,pvalues=pframe))
	
}
