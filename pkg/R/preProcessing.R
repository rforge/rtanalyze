#############################################
# RTanalyze Analysis Functions				#
# Copyright(c) 2009 Wouter D. Weeda			#
# University of Amsterdam					#
#############################################



#CONTAINS
#markNA
#markPostError
#markWarmUp
#markOutliers
#markCondition

#markSubjects
#ewma
#cormat.test

markPostError <- function(rtdat) 
#mark post-error trials as invalid
{
	
	count_posterror = 0
	pre.len = length(.rtdata.rt(rtdat)[.rtdata.valid(rtdat)==TRUE])
	pre.shadow = .rtdata.valid(rtdat)
		
	for(i in 2:length(.rtdata.rt(rtdat))) 
	{
		if(.rtdata.correct(rtdat)[i-1]==FALSE) {
			if(.rtdata.valid(rtdat)[i]==TRUE) count_posterror = count_posterror + 1 
			.rtdata.valid(rtdat)[i]=FALSE
			
		}
	}	
	
	outlier = new('outlier')
	
	.outlier.type(outlier) = 'post-error'
	.outlier.method(outlier) = 'remove'
	
	.outlier.minmax(outlier) = numeric(0)
	.outlier.pre.total(outlier) = pre.len
	.outlier.rem.total(outlier) = count_posterror
	.outlier.rem.low(outlier) = numeric(0)
	.outlier.rem.high(outlier) = numeric(0)
	.outlier.rem.prop(outlier) = .outlier.rem.total(outlier) / .outlier.pre.total(outlier)
	.outlier.post.total(outlier) = length(.rtdata.rt(rtdat)[.rtdata.valid(rtdat)==TRUE])
	.outlier.marked.values(outlier) = which(apply(cbind(pre.shadow,.rtdata.valid(rtdat)),1,sum)==1)
	.outlier.remark(outlier) = ''

	.rtdata.outliers(rtdat) = c(.rtdata.outliers(rtdat),outlier)
	
	#add remarks
	.rtdata.remarks(rtdat) = c(.rtdata.remarks(rtdat),'marked Post-Error trials as invalid.')
	
	return(rtdat)	
	
}

markNA <- function(rtdat) 
#mark NA  trials as invalid
{
	
	pre.len = length(.rtdata.rt(rtdat)[.rtdata.valid(rtdat)==TRUE])
	pre.shadow = .rtdata.valid(rtdat)
	
	count_postNA = 0
	
	for(i in 1:length(.rtdata.rt(rtdat))) 
	{
		if(is.na(.rtdata.rt(rtdat)[i])) {
			if(.rtdata.valid(rtdat)[i]==TRUE) count_postNA = count_postNA + 1
			.rtdata.valid(rtdat)[i]=FALSE
		}
	}	
	
	
	outlier = new('outlier')
	
	.outlier.type(outlier) = 'NA'
	.outlier.method(outlier) = 'remove'
	
	.outlier.minmax(outlier) = numeric(0)
	.outlier.pre.total(outlier) = pre.len
	.outlier.rem.total(outlier) = count_postNA
	.outlier.rem.low(outlier) = numeric(0)
	.outlier.rem.high(outlier) = numeric(0)
	.outlier.rem.prop(outlier) = .outlier.rem.total(outlier) / .outlier.pre.total(outlier)
	.outlier.post.total(outlier) = length(.rtdata.rt(rtdat)[.rtdata.valid(rtdat)==TRUE])
	.outlier.marked.values(outlier) = which(apply(cbind(pre.shadow,.rtdata.valid(rtdat)),1,sum)==1)
	.outlier.remark(outlier) = ''
	
	.rtdata.outliers(rtdat) = c(.rtdata.outliers(rtdat),outlier)
	
	#add remarks
	.rtdata.remarks(rtdat) = c(.rtdata.remarks(rtdat),'marked NA\'s as invalid.')
	
	return(rtdat)	
	
}

markWarmUp <- function(rtdat,at.each.condition=NULL,numtrials=5)
#mark trials at beginning of a condition as invalid
{
	
	pre.len = length(.rtdata.rt(rtdat)[.rtdata.valid(rtdat)==TRUE])
	pre.shadow = .rtdata.valid(rtdat)
	
	count_postWU = 0
	
	if(!is.null(at.each.condition)) {
		
		parsetext = paste('condind = levels(as.factor(.rtdata.conditions(rtdat)$`',at.each.condition,'`))',sep='')
		eval(parse(text=parsetext))
		
		for(level in 1:length(condind)) 
		{
			parsetext = paste('.rtdata.valid(rtdat)[.rtdata.conditions(rtdat)$`',at.each.condition,'`==',condind[level],'][1:',numtrials,']=FALSE',sep='')
			eval(parse(text=parsetext))
			
			count_postWU = count_postWU + length(1:numtrials)
		}
		
	} else {
		.rtdata.valid(rtdat)[1:numtrials] = FALSE
		count_postWU = count_postWU + length(1:numtrials)
	}
	
	outlier = new('outlier')
	
	.outlier.type(outlier) = 'Warm-up trials'
	.outlier.method(outlier) = 'remove'
	
	.outlier.minmax(outlier) = numeric(0)
	.outlier.pre.total(outlier) = pre.len
	.outlier.rem.total(outlier) = count_postWU
	.outlier.rem.low(outlier) = numeric(0)
	.outlier.rem.high(outlier) = numeric(0)
	.outlier.rem.prop(outlier) = .outlier.rem.total(outlier) / .outlier.pre.total(outlier)
	.outlier.post.total(outlier) = length(.rtdata.rt(rtdat)[.rtdata.valid(rtdat)==TRUE])
	.outlier.marked.values(outlier) = which(apply(cbind(pre.shadow,.rtdata.valid(rtdat)),1,sum)==1)
	.outlier.remark(outlier) = ''

	.rtdata.outliers(rtdat) = c(.rtdata.outliers(rtdat),outlier)
	
	
	#add remarks
	.rtdata.remarks(rtdat) = c(.rtdata.remarks(rtdat),paste('marked Warm-up trials (',numtrials,') as invalid.',sep=''))
	
	return(rtdat)
	
	
}

markOutliers <- 
function(rtdat,which.condition=NULL,method.min=c('abs','sd','ewma'),method.max=c('abs','sd'),sdfac=3,rtmin=200,rtmax=2500,ewma.control=list(lambda=.01,c0=.5,sigma0=.5,L=1.5,select=F)) 
#mark outliers based on on absolute values or SD
{
	
	method.min = match.arg(method.min,c('abs','sd','ewma'))
	method.max = match.arg(method.max,c('abs','sd','ewma'))
	
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
	
	for(cond in 1:dim(totmat)[1])	{
	
		evstring = paste('selvec = which(rtdat@conditions$`',condnames[1],'`==\'',totmat[cond,1],'\'',sep='')
		if(length(condnames)>1) {
			for(i in 2:length(condnames)) {
				evstring = paste(evstring,' & rtdat@conditions$`',condnames[i],'`==\'',totmat[cond,i],'\'',sep='') 
			}
		}
		
		evstring = paste(evstring,')',sep='')
		eval(parse(text=evstring))
		#selvec is now an object of the selected trials	
		
		pre.shadow = .rtdata.valid(rtdat)
		prelen = length(.rtdata.rt(rtdat)[.rtdata.valid(rtdat)==TRUE])
		
		rtvec = .rtdata.rt(rtdat)[selvec]
		accvec = .rtdata.correct(rtdat)[selvec]
		validvec = .rtdata.valid(rtdat)[selvec]
		
		if(method.min=='sd') {
			rtmin = mean(rtvec[which(validvec==TRUE)])-sd(rtvec[which(validvec==TRUE)])*sdfac
		}
		
		if(method.max=='sd') {
			rtmax = mean(rtvec[which(validvec==TRUE)])+sd(rtvec[which(validvec==TRUE)])*sdfac
		}
		
		
		ewmastat = NULL

		if(method.min=='ewma') {
			ewmastat = ewma(rtvec[validvec==TRUE],accvec[validvec==TRUE],lambda=ewma.control$lambda,c0=ewma.control$c0,sigma0=ewma.control$sigma0,L=ewma.control$L,abslower=rtmin,select=ewma.control$select) 
			rtmin = ewmastat$rtuse
		}

		#check if rtmin is not larger than rtmax (else use rtmax as reference)
		if(rtmin>rtmax) {
			rtmin=rtmax
		}
		
		.rtdata.valid(rtdat)[selvec][rtvec<rtmin] = FALSE
		.rtdata.valid(rtdat)[selvec][rtvec>rtmax] = FALSE
		
		postlen.low = length(which(rtvec[which(validvec==TRUE)]<rtmin))
		postlen.high = length(which(rtvec[which(validvec==TRUE)]>rtmax))

		outlier = new('outlier')
		
		.outlier.type(outlier) = paste('rtoutlier',sep='')
		.outlier.method(outlier) = paste('min=',method.min,';max=',method.max,sep='')
		if(method.min=='sd' | method.max=='sd') .outlier.method(outlier) = paste(.outlier.method(outlier),paste('sdfac=',sdfac,sep=''),sep=';')
		
		.outlier.minmax(outlier) = c(rtmin,rtmax)
		.outlier.pre.total(outlier) = prelen
		.outlier.rem.total(outlier) = postlen.low + postlen.high
		.outlier.rem.low(outlier) = postlen.low
		.outlier.rem.high(outlier) = postlen.high
		.outlier.rem.prop(outlier) = .outlier.rem.total(outlier) / .outlier.pre.total(outlier)
		.outlier.post.total(outlier) = length(.rtdata.rt(rtdat)[.rtdata.valid(rtdat)==TRUE])
		.outlier.marked.values(outlier) = which(apply(cbind(pre.shadow,.rtdata.valid(rtdat)),1,sum)==1)
		.outlier.selection.total(outlier) = length(selvec)
		.outlier.selection.vector(outlier) = selvec
		.outlier.ewma.stats(outlier) = ewmastat
		.outlier.remark(outlier) = paste(' [',rownames(totmat)[cond],'] removed ',.outlier.rem.total(outlier),' out of ',length(rtvec[validvec==TRUE]),' (',round(.outlier.rem.total(outlier) / length(rtvec[validvec==TRUE]),3),')',sep='')
		
		.rtdata.outliers(rtdat) = c(.rtdata.outliers(rtdat),outlier)
			
		#add remarks
		.rtdata.remarks(rtdat) = c(.rtdata.remarks(rtdat),'marked RT outliers as invalid.')
				
	}
	
	if(remov) rtdat = overall(rtdat,FALSE)
	
	return(rtdat)
}

markCondition <- function(rtdat,condition,value) 
#mark an entire condition as invaldi 
{
	pre.len = length(.rtdata.rt(rtdat)[.rtdata.valid(rtdat)==TRUE])
	pre.shadow = .rtdata.valid(rtdat)
	
	count_postCR = 0
	
	
	if(!is.null(condition) & !is.null(value)) {
		for(i in 1:length(condition)) 
		{
			parsetext = paste('selected = which(.rtdata.conditions(rtdat)$`',condition[[i]][1],'`==\'',value[[i]][1],'\')',sep='')
			eval(parse(text=parsetext))
			
			.rtdata.valid(rtdat)[selected]=FALSE
			
			count_postCR = count_postCR + length(selected)
		}
		
	}
	
	
	outlier = new('outlier')
	
	.outlier.type(outlier) = 'conditionremove'
	.outlier.method(outlier) = 'remove'
	
	.outlier.minmax(outlier) = numeric(0)
	.outlier.pre.total(outlier) = pre.len
	.outlier.rem.total(outlier) = count_postCR
	.outlier.rem.low(outlier) = numeric(0)
	.outlier.rem.high(outlier) = numeric(0)
	.outlier.rem.prop(outlier) = .outlier.rem.total(outlier) / .outlier.pre.total(outlier)
	.outlier.post.total(outlier) = length(.rtdata.rt(rtdat)[.rtdata.valid(rtdat)==TRUE])
	.outlier.marked.values(outlier) = which(apply(cbind(pre.shadow,.rtdata.valid(rtdat)),1,sum)==1)
	.outlier.remark(outlier) = ''
	
	.rtdata.outliers(rtdat) = c(.rtdata.outliers(rtdat),outlier)
	
	mat = cbind(unlist(condition),unlist(value))
	txt = character(0)
	for(i in 1:dim(mat)[1]) {
		txt = c(txt,paste(mat[i,1],'=',mat[i,2],sep=''))
	}
	
	
	#add remarks
	.rtdata.remarks(rtdat) = c(.rtdata.remarks(rtdat),paste('marked conditions (',paste(txt,collapse=','),') as invalid.',sep=''))
	
	return(rtdat)	
	
}

markSubjects <- function(subject,FUN,criterionlist,which.within=numeric(0),useCorrect='true',remark=character(0)) 
#mark subjects as invalid based on a summary statistics
{
	
	#check if function is pc
	PCcall = which(as.character(match.call()$FUN)=='pc')
	
	#get valid subjects
	whichsubjects = which(.subjects.valid(subject)==TRUE)
	marked = numeric(0)
	
	for(i in 1:length(whichsubjects)) 
	{
		if(length(PCcall)>0) {
			summary.rtdata = pc(.subjects.rtdata(subject)[[whichsubjects[i]]],which.within)
			summvalue = summary.rtdata$pc
		} else {
			summary.rtdata = aggregate.rtdata(.subjects.rtdata(subject)[[whichsubjects[i]]],which.within,FUN,useCorrect=useCorrect)
			summvalue = summary.rtdata$rt
		}
		
		#cat(whichsubjects[i],summvalue,'\n')
		if(length(summvalue)==length(criterionlist)) {
			
			valid = logical(0)
			for(j in 1:length(summvalue)) {
				 valid = c(valid,eval(parse(text=paste('summvalue[j]',criterionlist[[j]][1],criterionlist[[j]][2],sep=''))))
			}
			 
			#cat(whichsubjects[i],valid,'\n\n')
			if(is.numeric(criterionlist[[j]][3])) {
				valid = valid[criterionlist[[j]][3]]
			} else {
				
				if(criterionlist[[j]][3]=='any') {
					valid = !any(valid)
				} else {
					warning('No valid criterium to select validness of subjects. Using any() to select.')
					valid = !any(valid)	
				}
			}
		} else {
			.subjects.valid(subject)[whichsubjects[i]]=FALSE
			warning('Length of criterionlist does not match length of summary output. Setting subject to invalid.')
		}
		
		#which one is marked
		if(valid==FALSE) marked = c(marked,whichsubjects[i])
		
		.subjects.valid(subject)[whichsubjects[i]]=valid
	}

	#store information on outlier object
	outliers = new(subjectoutlier)
	
	.subjectoutlier.FUN(outliers) = as.character(match.call()$FUN) 
	.subjectoutlier.which(outliers) = which.within
	.subjectoutlier.criteria(outliers) = criterionlist
	.subjectoutlier.pre.total(outliers) = length(whichsubjects)
	.subjectoutlier.post.total(outliers) = which(.subjects.valid(subject)==TRUE)
	
	.subjectoutlier.rem.total(outliers) = .subjectoutlier.pre.total(outliers) - .subjectoutlier.post.total(outliers)  
	.subjectoutlier.rem.prop(outliers) = .subjectoutlier.rem.total(outliers)/.subjectoutlier.pre.total(outliers)
	.subjectoutlier.marked.values(outliers) = marked 
	.subjectoutlier.remark(outliers) = remarks
	
	.subjects.outliers(subject) = c(.subjects.outliers(subject),outliers)

	
	return(subject)
} 

showOutliers <- 
function(rtdat) 
#show outlier summary
{
	ol = .rtdata.outliers(rtdat)
	nlen = length(ol)
	
	if(nlen>0) {
		nframe = data.frame(type=rep(NA,nlen),method=rep(NA,nlen),removed=rep(NA,nlen),proportion=rep(NA,nlen),remarks=rep(NA,nlen))
		
		for(i in 1:nlen) {
			nframe$type[i] = .outlier.type(ol[[i]]) 
			nframe$method[i] = .outlier.method(ol[[i]]) 
			nframe$removed[i] = paste(.outlier.rem.total(ol[[i]]),' (of ',.outlier.pre.total(ol[[i]]),')*',sep='') 
			nframe$proportion[i] = .outlier.rem.prop(ol[[i]]) 
			nframe$remarks[i] = .outlier.remark(ol[[i]])
		}
		
		show(nframe)
		
		cat('\n')
		cat('Total number of remaining trials after outlier removal is ',.outlier.post.total(ol[[nlen]]),' (of original ',.outlier.pre.total(ol[[1]]),' trials) \n',sep='')
		cat('Total proportion of removed trials is ',(.outlier.pre.total(ol[[1]])-.outlier.post.total(ol[[nlen]]))/.outlier.pre.total(ol[[1]]),'\n',sep='')
	} else {
		cat('No outlier removal applied\n')
	}

	return(invisible(TRUE))
}


ewma <-
function(rtdata,accdata,lambda=.01,c0=.5,sigma0=.5,L=1.5,abslower=0,select=F) 
#ewma fast-guess removal (Vandekerckhove & Tuerlincks, 2007)
{

	#input vectors are assumed to be VALID trials
	rtvec = rtdata
	cvec = accdata
	
	#order rtvecs
	o = order(rtvec)
	rtvec = rtvec[o]
	cvec = cvec[o]
	
	#calculate cs and UCL vectors
	cs = UCLs = numeric(length(rtvec))
	cs[1] = c0
	UCLs[1] = c0 + L*sigma0*sqrt((lambda/(2-lambda)*(1-(1-lambda))^(2*1)))
	for(i in 2:length(rtvec)) {
		cs[i] = lambda*cvec[i] + (1-lambda)*cs[i-1]
		UCLs[i] = c0 + L*sigma0*sqrt( (lambda/(2-lambda)) * ( 1-(1-lambda)^(2*i) )  )
	}
	
	#make on entire matrix
	cvma = (cs<UCLs)
	mx = suppressWarnings(max(which(cvma)))
	rtmax = suppressWarnings(rtvec[mx])
	
	cvmi = !(cs<UCLs)
	mn = suppressWarnings(min(which(cvmi)))
	rtmin = suppressWarnings(rtvec[mn])
	
	if(is.na(rtmin)) {
		mn = length(rtvec)
		rtmin = rtvec[mn]
	}
	
	#switch when mn > mx
	if(mn>mx) {
		mx1 = mx
		mx = mn
		mn = mx1
	}

	#check results and return
	if(mn<mx) {
		#check distance to upper/lower bound
		numbelow = sum(cs[mn:mx]<UCLs[mn:mx])
		totdisthalve = length(cs[mn:mx])/2
		
		if(numbelow>totdisthalve) {
			rtuse = rtmin
		} else {
			if(numbelow<totdisthalve) rtuse = rtmax else rtuse = round((rtmin+rtmax)/2)
		}
		
	} else rtuse = rtmin
			
	ewmastat = list(rtuse=rtuse,rtmin=rtmin,rtmax=rtmax,min=mn,max=mx,c=cs,UCL=UCLs,rtvec=rtvec,cvec=cvec)

	if(select) {
		plot(1:length(ewmastat$c),ewmastat$c,type='l',bty='n',xlab='RT',ylab='ewmastat',axes=F)
		lines(ewmastat$UCL,lty=2,col='gray')
		points(ewmastat$min,ewmastat$c[ewmastat$min],pch=19,col=3)
		points(ewmastat$max,ewmastat$c[ewmastat$max],pch=19,col=2)
		axis(2)
		axis(1,at=c(1,round(median(1:length(ewmastat$c))),length(ewmastat$c)),label=c(ewmastat$rtvec[1],ewmastat$rtvec[round(median(1:length(ewmastat$c)))],ewmastat$rtvec[length(ewmastat$c)]))
		ans = readline('Which threshold to use? [1=green][2=red] > ')
		if(ans=='1') ewmastat$rtuse = ewmastat$rtmin
		if(ans=='2') ewmastat$rtuse = ewmastat$rtmax
	} 
	
	return(ewmastat)
	
}


overall <- function(rtdat,add=T) 
#add a 'ghost' overall condtions
{

	if(add) {
		overall = as.factor(rep('overall',dim(.rtdata.conditions(rtdat))[1]))
		.rtdata.conditions(rtdat) = cbind(.rtdata.conditions(rtdat),overall)
	} else {
		if(names(.rtdata.conditions(rtdat))[dim(.rtdata.conditions(rtdat))[2]]=='overall') .rtdata.conditions(rtdat) = .rtdata.conditions(rtdat)[,-dim(.rtdata.conditions(rtdat))[2]] else warning('No ghost condition removed.')
	}
	
	return(rtdat)
}

cormat.test <- 
function(datavecs,pretty.out=FALSE,rnd=2)
#test the correlation matrix for significance
{
	names = colnames(datavecs)
	cm = matrix(NA,ncol(datavecs),ncol(datavecs))
	
	for(row in 1:(ncol(datavecs)-1)) {
		for(col in (row+1):(ncol(datavecs))) {
			ct = cor.test(datavecs[,row],datavecs[,col])
			cm[row,col]=ct$estimate
			cm[col,row]=ct$p.value
		}
	}
	
	dimnames(cm)=list(names,names)
	
	if(pretty.out) {
		names = dimnames(cm)[[1]]
		len = ((ncol(datavecs)^2)-ncol(datavecs))/2
		prdata = data.frame(estimate=numeric(len),p.value=numeric(len))
		
		cnt=1
		for(row in 1:(ncol(datavecs)-1)) {
			for(col in (row+1):(ncol(datavecs))) {
				dimnames(prdata)[[1]][cnt] = paste(names[row],' - ',names[col],sep='')
				prdata[cnt,1] = round(cm[row,col],rnd)
				prdata[cnt,2] = round(cm[col,row],rnd)
				cnt = cnt + 1
			}
		}
		
		return(prdata)
		
	}
	
	return(cm)		
	
}
