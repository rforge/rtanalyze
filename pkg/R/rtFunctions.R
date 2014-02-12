#############################################
# RTanalyze reactiontime Functions			#
# Copyright(c) 2009 Wouter D. Weeda			#
# University of Amsterdam					#
#############################################



#CONTAINS
#summarize.subjects
#aggregate.rtdata
#pc
#summary.rtdata
#quantile.rtdata
#concatenate.rtdata
#within
#sat
#check.agg


summarize.subjects <-
function(subject,which.within=NULL,FUN,useCorrect='true',which.between=numeric(0)) 
#summarize rt data on subjects objects (see methods for Generic), output in long format
{
	if(class(subject)!='subjects') stop('works only on \'subjects\' class objects.')
	
	#getvalids VALID subjects
	whichsubjects = which(.subjects.valid(subject)==TRUE)
	
	#check if PC is called
	PCcall = which(as.character(match.call()$FUN)=='pc')

	#make data.frame
	summary.subject = numeric(0)
	
	#make aggregates for all subjects
	for(i in 1:length(whichsubjects)) {
		
		if(length(PCcall)>0) {
			summary.rtdata = pc(.subjects.rtdata(subject)[[whichsubjects[i]]],which.within,TRUE)
		} else {
			summary.rtdata = aggregate.rtdata(.subjects.rtdata(subject)[[whichsubjects[i]]],which.within,FUN,useCorrect=useCorrect)
		}
		
		#get subject variables
		sdat = .subjects.variables(subject)[whichsubjects[i],]
		if(nrow(summary.rtdata)>1) {
			for(j in 2:nrow(summary.rtdata)) {
				sdat = rbind(sdat,.subjects.variables(subject)[whichsubjects[i],])
			}
		}
	
		#add value
		sdat = cbind(sdat,summary.rtdata)
		summary.subject = rbind(summary.subject,sdat)
		
	}
	
	#set new names
	newname = c(names(.subjects.variables(subject)),names(summary.rtdata))
	newname[length(newname)] = as.character(match.call()$FUN)
	names(summary.subject) = newname
	
	return(summary.subject)
	
}

summarize <- function(...) summarize.subjects(...)
	


long2wide <- function(dframe,ID,indicators) 
#make a long data.frame into a wide one
{
	
	
	
	
	
	
}

aggregate.rtdata <-
function(rtdat,which=NULL,FUN,useCorrect=c('both','true','false','none')) 
#wrapper for aggregate called on rtdata objects (see methods for Generic).
{
	if(class(rtdat)!='rtdata') stop('works only on \'rtdata\' class objects.')
	if(missing(which)) which=NULL
	
	if(is.null(which)) {
		rtdat = overall(rtdat,TRUE)
		which = names(.rtdata.conditions(rtdat))[dim(.rtdata.conditions(rtdat))[2]]
	}
	
	useCorrect = match.arg(useCorrect,c('both','true','false','none'))
	
	#create data.frame for VALID conditions and corrects	
	validcond.data = .rtdata.conditions(rtdat)[.rtdata.valid(rtdat),] 
	correct = .rtdata.correct(rtdat)[.rtdata.valid(rtdat)]
	validcond.data = data.frame(validcond.data,correct)
	
	#create data.frame for VALID RTs
	fulldat = .rtdata.rt(rtdat)[.rtdata.valid(rtdat)]
		
	#check which independents to use
	if(is.character(which)) which = match(which,names(.rtdata.conditions(rtdat)))
	
	#create aggregate (always use correct/incorrect as default, else use either or none)
	if(useCorrect=='both') {
		summary.rtdata = aggregate(fulldat,as.list(data.frame(validcond.data[,which],correct)),FUN) 
		names(summary.rtdata) = c(names(.rtdata.conditions(rtdat))[which],'correct','rt')
	} 
	if(useCorrect=='true') {
		summary.rtdata = aggregate(fulldat[which(correct==TRUE)],as.list(data.frame(validcond.data[which(correct==TRUE),which])),FUN)
		summary.rtdata = cbind(data.frame(summary.rtdata[,-which(names(summary.rtdata)=='x')]),data.frame(rep(TRUE,nrow(summary.rtdata))),data.frame(summary.rtdata[,which(names(summary.rtdata)=='x')]))
		names(summary.rtdata) = c(names(.rtdata.conditions(rtdat))[which],'correct','rt')
	} 
	if(useCorrect=='false') {
		summary.rtdata = aggregate(fulldat[which(correct==FALSE)],as.list(data.frame(validcond.data[which(correct==FALSE),which])),FUN)
		summary.rtdata = cbind(data.frame(summary.rtdata[,-which(names(summary.rtdata)=='x')]),data.frame(rep(FALSE,nrow(summary.rtdata))),data.frame(summary.rtdata[,which(names(summary.rtdata)=='x')]))
		names(summary.rtdata) = c(names(.rtdata.conditions(rtdat))[which],'correct','rt')
	} 
	if(useCorrect=='none') {
		summary.rtdata = aggregate(fulldat,as.list(data.frame(validcond.data[,which])),FUN) 
		names(summary.rtdata) = c(names(.rtdata.conditions(rtdat))[which],'rt')
	} 
	
	#return aggregate
	return(summary.rtdata)

}

pc <-
function(rtdat,which=NULL,answer=TRUE) 
#return percentagecorrect
{
	
	if(class(rtdat)!='rtdata') stop('pc() works only on \'rtdata\' class objects.')

	#call aggregate 
	total = aggregate.rtdata(rtdat,which,length)
	
	errors = total[total$correct==FALSE,]
	correct = total[total$correct==TRUE,]

	#match matrix
	cmat = apply(as.matrix(total[,-which(names(total)=='correct' | names(total)=='rt')]),1,function(x) paste(x,collapse=''))
	
	#select rows in matchmatrix with longest	
	if(nrow(errors)<=nrow(correct)) {
		rownums = which(total$correct==TRUE)
		werr = FALSE
	} else {
		rownums = which(total$correct==FALSE)
		werr = TRUE
	}
	
	#try and match rows of correct and incorrects
	pcrt = numeric(length(rownums))
	
	for(row in 1:length(rownums)) 
	{
		m = which(cmat==cmat[rownums[row]])
				
		#if match is found
		if(length(m)==2) {
			#select if 1 or 2 is number corrects
			pcrt[row] = (total$rt[m[2]]) / (total$rt[m[2]]+total$rt[m[1]])   	
		} else {
			#no match
			#determine which is missing correct or incorrect and set to zero
			if(werr) pcrt[row] = 0 else pcrt[row] = 1 
		}
	}
	
	#select the appropriate outframe (based on longest matchmatrix)
	if(werr) outframe = errors else outframe = correct
	
	#pretty up the dataframe
	outframe = outframe[,-which(names(outframe)=='correct')]
	outframe$rt = pcrt
	names(outframe)[ncol(outframe)]='pc'
	row.names(outframe) = 1:nrow(outframe)

	return(outframe)
	
}


summary.rtdata <-
function(rtdat,which=NULL,pc=FALSE) 
#return a summary data.frame
{
	if(class(rtdat)!='rtdata') stop('summary.rtdata() works only on \'rtdata\' class objects.')
	
	#mean, sd and n
	msl.dat = cbind(aggregate.rtdata(rtdat,which,mean),aggregate.rtdata(rtdat,which,sd)$rt,aggregate.rtdata(rtdat,which,length)$rt)
		
	#proportion of responses per condition
	if(pc) pcs = data.frame(pc=c(pc(rtdat,which,FALSE)$pc,pc(rtdat,which,TRUE)$pc))
	
	#quantile RTs
	quants = quantile.rtdata(rtdat,which,onlyQs=TRUE)
		
	#conacatenate and set column names
	if(pc) {
		summary.dat = cbind(msl.dat,pcs,quants)
		names(summary.dat) = c(names(aggregate.rtdata(rtdat,which,mean)[-length(names(aggregate.rtdata(rtdat,which,mean)))]),'meanRT','sdRT','n','prop',names(quants))
	} else {
		summary.dat = cbind(msl.dat,quants)
		names(summary.dat) = c(names(aggregate.rtdata(rtdat,which,mean)[-length(names(aggregate.rtdata(rtdat,which,mean)))]),'meanRT','sdRT','n',names(quants))
	}
	 
	return(summary.dat)
	
}


quantile.rtdata <-
function(rtdat,which=NULL,quantiles=c(.1,.3,.5,.7,.9),useCorrect='both',onlyQs=FALSE)
{
	if(class(rtdat)!='rtdata') stop('quantile.rtdata() works only on \'rtdata\' class objects.')

	#define mean and quantdata
	meandata = aggregate.rtdata(rtdat,which,length,useCorrect=useCorrect)
	quant.data = n.quant.data = matrix(NA,nrow(meandata),length(quantiles))
	
	#recursive y apply quantile on aggregate (and calculate cumulative n)
	for(i in 1:length(quantiles)) {
		quant.data[,i] = aggregate.rtdata(rtdat,which,useCorrect=useCorrect,FUN=function(x) quantile(x,quantiles[i]))$rt
		n.quant.data[,i] = aggregate.rtdata(rtdat,which,useCorrect=useCorrect,FUN=function(x) npq(x,quantiles[i]))$rt
	}
	
	#make bins
	for(i in 1:nrow(n.quant.data)) n.quant.data[i,] = bin(n.quant.data[i,])
		
	#set dataframes and names
	quant.data = as.data.frame(quant.data)
	n.quant.data = as.data.frame(n.quant.data)
	
	rownames(quant.data) = seq(1,nrow(quant.data))
	rownames(n.quant.data) = seq(1,nrow(n.quant.data))
	
	names(quant.data) = paste('q',as.character(quantiles),sep='')
	names(n.quant.data) = paste('q',as.character(quantiles),sep='')
	
	total = meandata[,ncol(meandata)]
	meandata = meandata[,-ncol(meandata)]
	
	if(!onlyQs) {
		summary.data = data.frame(meandata,quant.data) 
		n.summary.data = data.frame(meandata,n.quant.data,total)
		
		return(list(quantiles=summary.data,n=n.summary.data))	
		
	} else {
		summary.data = quant.data
		return(summary.data)
	}
	
}


npq <- 
function(x,quant) {
	
	q = quantile(x,quant)
	n = length(which(x<q))
	
	return(n)
}

bin <-
function(x)
{
	y = numeric(length(x))
	
	y[1] = x[1]
	for(i in 2:length(x)) y[i] = x[i]-x[i-1]

	return(y)
}


concatenate.rtdata <-
function(rtdat1,rtdat2)
{
	
	#match call and select number of concats
	#make the newrtdata to 1
	rtdata = rtdat1
	newrt = .rtdata.rt(rtdata)
	newcorrect = .rtdata.correct(rtdata)
	newvalid = .rtdata.valid(rtdata)
	newconditions = .rtdata.conditions(rtdata)
	newremarks = .rtdata.remarks(rtdata)
	
	#concatenate data
	crt = rtdat2
		
	newrt = c(newrt,.rtdata.rt(crt))
	newcorrect = c(newcorrect,.rtdata.correct(crt))
	newvalid = c(newvalid,.rtdata.valid(crt))
	newremarks = c(newremarks,.rtdata.remarks(crt))
	newconditions = rbind(newconditions,.rtdata.conditions(crt))		
		
	#make new rtdata object
	crtdata = new('rtdata',rt=newrt,correct=newcorrect,valid=newvalid,remarks=newremarks,conditions=newconditions)

	return(crtdata)
	
}


within.condition <-
function(subject) 
#show within subject conditions of 
{
	if(class(subject)!='subjects') stop('works only on \'subjects\' class objects.')
	cat(names(.rtdata.conditions(.subjects.rtdata(subject)[[1]])),'\n')

}


plot.rtdata <-
function(rtdata) 
#plot rtdata object
{
	layout(matrix(1:4,2,byrow=T))
	minmax = c(min(.rtdata.rt(rtdata)[.rtdata.valid(rtdata)],na.rm=T)/1.1,max(.rtdata.rt(rtdata)[.rtdata.valid(rtdata)],na.rm=T)*1.1)
	hist(.rtdata.rt(rtdata)[.rtdata.valid(rtdata) & .rtdata.correct(rtdata)==TRUE],xlim=minmax,col=3,main='Correct',bty='n',xlab='RT (correct)')
	hist(.rtdata.rt(rtdata)[.rtdata.valid(rtdata) & .rtdata.correct(rtdata)==FALSE],xlim=minmax,col=2,main='Incorrect',bty='n',xlab='RT (incorrect)')
	boxplot(.rtdata.rt(rtdata)[.rtdata.valid(rtdata) & .rtdata.correct(rtdata)==TRUE],col=gray(.5),horizontal=TRUE,bty='n',ylim=minmax)
	boxplot(.rtdata.rt(rtdata)[.rtdata.valid(rtdata) & .rtdata.correct(rtdata)==FALSE],col=gray(.5),horizontal=TRUE,bty='n',ylim=minmax)
	
}

sat <-
function(rtdata,quants=c(.1,.3,.5,.7,.9),condition=NULL)
#calculate speed/accuracy tradeoff
{
	if(is.null(condition)) {
		rts = data.frame(rt=.rtdata.rt(rtdata)[.rtdata.valid(rtdata)],correct=.rtdata.correct(rtdata)[.rtdata.valid(rtdata)])
	} else {
		rts = eval(parse(text=paste('data.frame(rt=.rtdata.rt(rtdata)[.rtdata.valid(rtdata)==TRUE & rtdata$',condition,'],correct=.rtdata.correct(rtdata)[.rtdata.valid(rtdata)==TRUE & rtdata$',condition,'])',sep='')))
	}
	
	qs = quantile(rts$rt,quants,na.rm=T)
	
	block = rep(NA,length(rts$rt))
	
	for(i in length(qs):1) {
		block[which(rts$rt<qs[i])]=i
		
	}
	
	rts = data.frame(rt=rts$rt,correct=rts$correct,block=block)
	
	cns = aggregate(as.numeric(rts$correct),list(rts$block),function(x) sum(x,na.rm=T))
	len = aggregate(as.numeric(rts$correct),list(rts$block),length)
	
	acc = cns[,2]/len[,2]
		
	stdat = data.frame(rt=qs,speed=seq(1,length(qs)),accuracy=acc)
	stdat = stdat[order(qs,decreasing=T),]
	
	return(stdat)	
}

plot.sat <-
function(rtdata,quants=c(.1,.3,.5,.7,.9),ylim=c(.5,1))
#plot SAT
{
	satdata = sat(rtdata,quants)
	
	par(las=1)
	plot(NA,xlim=c(1,nrow(satdata)),ylim=ylim,bty='n',axes=F,xlab='REACTION TIME',ylab='ACCURACY',main='SPEED/ACCURACY TRADEOFF')
	axis(1,at=nrow(satdata):1,labels=round(satdata$rt))
	axis(2)
	lines(satdata$speed,satdata$accuracy,lwd=3,lty=1,col=1)
	points(satdata$speed,satdata$accuracy,pch=19,col=1)
	
	
}

fitexgauss <- function(rtdat,which.condition) 
#using
{
	if(missing(which.condition)) which.condition=NULL
	
	if(is.null(which.condition)) {
		rtdat = overall(rtdat,TRUE)
		which.condition = names(.rtdata.conditions(rtdat))[dim(.rtdata.conditions(rtdat))[2]]
		remov = TRUE
	} else remov = FALSE
	
	if(is.numeric(which.condition)) {
		dat = data.frame(.rtdata.conditions(rtdat)[,which.condition]) 
		names(dat) = which.condition
		totlev = makelevels(names(.rtdata.conditions(rtdat)==which.condition),dat)
		totmat = makeconditionarray(names(.rtdata.conditions(rtdat)==which.condition),totlev)
		
	} else {
		dat = data.frame(.rtdata.conditions(rtdat)[,match(which.condition,names(.rtdata.conditions(rtdat)))])
		names(dat) = which.condition
		totlev = makelevels(which.condition,dat)
		totmat = makeconditionarray(which.condition,totlev)
	}	
	
	condnames = colnames(totmat)
	
	outdata = data.frame(mu=rep(NA,dim(totmat)[1]),sigma=rep(NA,dim(totmat)[1]),tau=rep(NA,dim(totmat)[1]))
	rownames(outdata) = rownames(totmat)
	
	for(cond in 1:dim(totmat)[1])	{
		
		#make selve empty
		selvec = numeric(0)
		
		evstring = paste('selvec = which(rtdat@conditions$`',condnames[1],'`==\'',totmat[cond,1],'\'',sep='')
		if(length(condnames)>1) {
			for(i in 2:length(condnames)) {
				evstring = paste(evstring,' & rtdat@conditions$`',condnames[i],'`==\'',totmat[cond,i],'\'',sep='') 
			}
		}
		
		evstring = paste(evstring,')',sep='')
		eval(parse(text=evstring))
		#selvec is now an object of the selected trials	
	
		rtvec = rtdat@rt[selvec][rtdat@valid[selvec]==TRUE & rtdat@correct[selvec]==TRUE]
		
		out = fitexg(rtvec)
		
		if(out$convergence==0) outdata[cond,] = out$par
		
	}
	
	return(outdata)
	
}



