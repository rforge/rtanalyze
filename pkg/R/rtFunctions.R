#############################################
# RTanalyze reactiontime Functions			#
# Copyright(c) 2009 Wouter D. Weeda			#
# University of Amsterdam					#
#############################################



#CONTAINS
#summarize
#aggregate.rtdata
#pc
#summary.rtdata
#quantile.rtdata
#concatenate.rtdata
#within
#sat

summarize <-
function(subject,which.within=numeric(0),FUN,useCorrect=TRUE,template.subject=1) 
#summarize rt data on subjects objects (see methods for Generic).
{
	if(class(subject)!='subjects') stop('works only on \'subjects\' class objects.')
	
	#getvalids VALID subjects
	whichsubjects = which(.subjects.valid(subject)==TRUE)
	
	#check if PC is called
	PCcall = which(as.character(match.call()$FUN)=='pc')

	#create namevec
	if(length(PCcall)>0) {
		summary.rtdata = pc(.subjects.rtdata(subject)[[whichsubjects[template.subject]]],which.within,TRUE)
	} else summary.rtdata = aggregate.rtdata(.subjects.rtdata(subject)[[whichsubjects[template.subject]]],which.within,FUN,useCorrect=useCorrect)
	
	variables = character(nrow(summary.rtdata))
	for(row in 1:nrow(summary.rtdata)) {
		varname = as.character(match.call()$FUN)
		if(ncol(summary.rtdata)>1) {
			for(col in 1:(ncol(summary.rtdata)-1)) {
				varname = c(varname,as.character(summary.rtdata[row,col]))
			}
		}
		variables[row] = paste(varname,collapse='_')
	}
	
	#create named dataframe
	x = data.frame(matrix(NA,length(whichsubjects),length(variables)))
	colnames(x) = variables
	
	#fill in rows (subjects) give NA if not all conditions are filled
	for(sub in 1:length(whichsubjects)) {
		if(length(PCcall)>0) {
			dat = pc(.subjects.rtdata(subject)[[whichsubjects[sub]]],which.within,TRUE)$pc
		} else dat = aggregate.rtdata(.subjects.rtdata(subject)[[whichsubjects[sub]]],which.within,FUN,useCorrect=useCorrect)$rt
		if(length(dat)!=length(variables)) dat = rep(NA,length(variables))
		x[sub,]= dat
	}
	
	#bind conditions and summaries
	summary.subject = cbind(.subjects.variables(subject)[.subjects.valid(subject),],x)

	return(summary.subject)
	
}

aggregate.rtdata <-
function(rtdat,which=NULL,FUN,useCorrect=TRUE) 
#wrapper for aggregate called on rtdata objects (see methods for Generic).
{
	if(class(rtdat)!='rtdata') stop('works only on \'rtdata\' class objects.')
	if(missing(which)) which=numeric(0)
	
	#create data.frame for VALID conditions and corrects	
	validcond.data = .rtdata.conditions(rtdat)[.rtdata.valid(rtdat),] 
	correct = .rtdata.correct(rtdat)[.rtdata.valid(rtdat)]
	validcond.data = data.frame(validcond.data,correct)
	
	#create data.frame for VALID RTs
	fulldat = .rtdata.rt(rtdat)[.rtdata.valid(rtdat)]
		
	#check which independents to use
	if(is.character(which)) which = match(which,names(.rtdata.conditions(rtdat)))
	conditions = names(.rtdata.conditions(rtdat))[which]
	
	#create aggregate (always use correct/incorrect as default)
	if(useCorrect) {
		summary.rtdata = aggregate(fulldat,as.list(data.frame(validcond.data[,which],correct)),FUN) 
		names(summary.rtdata) = c(names(.rtdata.conditions(rtdat))[which],'correct','rt')
	} else {
		summary.rtdata = aggregate(fulldat,as.list(data.frame(validcond.data[,which])),FUN)
		names(summary.rtdata) = c(names(.rtdata.conditions(rtdat))[which],'rt')
	}
	
	return(summary.rtdata)

}

pc <-
function(rtdat,which=numeric(0),answer=TRUE) 
#return percentagecorrect
{
	
	if(class(rtdat)!='rtdata') stop('pc() works only on \'rtdata\' class objects.')

	#call aggregate on totals
	total = aggregate.rtdata(rtdat,which,length,useCorrect=FALSE)
	
	#call aggregate on corrects
	correct = aggregate.rtdata(rtdat,which,length)
	correct = correct[correct$correct==answer,]
	
	#make totals and replace in data.frame
	nc = correct$rt/total$rt 
	
	total[,ncol(total)] = nc
	names(total)[ncol(total)]='pc'
	
	return(total)
	
}


summary.rtdata <-
function(rtdat,which=numeric(0),pc=FALSE) 
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
function(rtdat,which=numeric(0),quantiles=c(.1,.3,.5,.7,.9),useCorrect=TRUE,onlyQs=FALSE)
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


within <-
function(subject) 
#show within subject conditions of 
{
	if(class(subject)!='subjects') stop('works only on \'subjects\' class objects.')
	cat(names(.rtdata.conditions(.subjects.rtdata(subject)[[1]])),'\n')

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


