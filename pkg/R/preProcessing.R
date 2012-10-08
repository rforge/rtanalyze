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
#markSubjects
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
			count_postNA = count_postNA + 1
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
	
	.rtdata.outliers(rtdat) = c(.rtdata.outliers(rtdat),outlier)
	
	
	#add remarks
	.rtdata.remarks(rtdat) = c(.rtdata.remarks(rtdat),paste('marked Warm-up trials (',numtrials,') as invalid.',sep=''))
	
	return(rtdat)
	
	
}

markOutliers <- 
function(rtdat,method=c('abs','sd','mia-masd'),sdfac=3,rtmin=250,rtmax=2500,plot=F) 
#mark outliers based on on absolute values or SD
{
	rtvec = .rtdata.rt(rtdat)
	method = match.arg(method,c('abs','sd','mia-masd'))
	validvec = .rtdata.valid(rtdat)
	
	prelen = length(.rtdata.rt(rtdat)[.rtdata.valid(rtdat)==TRUE])
	pre.shadow = .rtdata.valid(rtdat)
	
	if(method=='sd') {
		rtmin = mean(rtvec[which(validvec==TRUE)])-sd(rtvec[which(validvec==TRUE)])*sdfac
		rtmax = mean(rtvec[which(validvec==TRUE)])+sd(rtvec[which(validvec==TRUE)])*sdfac
	}
	
	if(method=='mia-masd') {
		rtmax = mean(rtvec[which(validvec==TRUE)])+sd(rtvec[which(validvec==TRUE)])*sdfac
	}
	
	.rtdata.valid(rtdat)[rtvec<rtmin] = FALSE
	.rtdata.valid(rtdat)[rtvec>rtmax] = FALSE
	
	postlen.low = length(which(rtvec[which(validvec==TRUE)]<rtmin))
	postlen.high = length(which(rtvec[which(validvec==TRUE)]>rtmax))
	
	outlier = new('outlier')
	
	.outlier.type(outlier) = 'rtoutlier'
	.outlier.method(outlier) = method
	if(method=='mia-masd') .outlier.remark(outlier) = c(.outlier.remark(outlier),paste('sdfac=',sdfac,sep=''))
	
	.outlier.minmax(outlier) = c(rtmin,rtmax)
	.outlier.pre.total(outlier) = prelen
	.outlier.rem.total(outlier) = postlen.low + postlen.high
	.outlier.rem.low(outlier) = postlen.low
	.outlier.rem.high(outlier) = postlen.high
	.outlier.rem.prop(outlier) = .outlier.rem.total(outlier) / .outlier.pre.total(outlier)
	.outlier.post.total(outlier) = length(.rtdata.rt(rtdat)[.rtdata.valid(rtdat)==TRUE])
	.outlier.marked.values(outlier) = which(apply(cbind(pre.shadow,.rtdata.valid(rtdat)),1,sum)==1)
	
	.rtdata.outliers(rtdat) = c(.rtdata.outliers(rtdat),outlier)
		
	#add remarks
	.rtdata.remarks(rtdat) = c(.rtdata.remarks(rtdat),'marked RT outliers as invalid.')


	if(plot) {
		cat('plotsie')
	}
	
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
		nframe = data.frame(type=rep(NA,nlen),method=rep(NA,nlen),removed=rep(NA,nlen),proportion=rep(NA,nlen))
		
		for(i in 1:nlen) {
			nframe$type[i] = .outlier.type(ol[[i]]) 
			nframe$method[i] = .outlier.method(ol[[i]]) 
			nframe$removed[i] = paste(.outlier.rem.total(ol[[i]]),' (of ',.outlier.pre.total(ol[[i]]),')',sep='') 
			nframe$proportion[i] = .outlier.rem.prop(ol[[i]]) 
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
