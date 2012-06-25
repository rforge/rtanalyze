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
#cormat.test

markPostError <- function(rtdat) 
#mark post-error trials as invalid
{
	
	count_posterror = 0
	pre.len = length(.rtdata.rt(rtdat)[.rtdata.valid(rtdat)==TRUE])
	
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
	
	.rtdata.outliers(rtdat) = c(.rtdata.outliers(rtdat),outlier)
	
	#add remarks
	.rtdata.remarks(rtdat) = c(.rtdata.remarks(rtdat),'marked Post-Error trials as invalid.')
	
	return(rtdat)	
	
}

markNA <- function(rtdat) 
#mark NA  trials as invalid
{
	
	pre.len = length(.rtdata.rt(rtdat)[.rtdata.valid(rtdat)==TRUE])
	
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
	
	.rtdata.outliers(rtdat) = c(.rtdata.outliers(rtdat),outlier)
	
	#add remarks
	.rtdata.remarks(rtdat) = c(.rtdata.remarks(rtdat),'marked NA\'s as invalid.')
	
	return(rtdat)	
	
}

markWarmUp <- function(rtdat,at.each.condition=NULL,numtrials=5)
#mark trials at beginning of a condition as invalid
{
	
	pre.len = length(.rtdata.rt(rtdat)[.rtdata.valid(rtdat)==TRUE])
	
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
	
	if(method=='sd') {
		rtmin = mean(rtvec[which(validvec==TRUE)])-sd(rtvec[which(validvec==TRUE)])*sdfac
		rtmax = mean(rtvec[which(validvec==TRUE)])+sd(rtvec[which(validvec==TRUE)])*sdfac
	}
	
	if(method=='mia-masd') {
		rtmax = mean(rtvec[which(validvec==TRUE)])+sd(rtvec[which(validvec==TRUE)])*sdfac
	}
	
	.rtdata.valid(rtdat)[rtvec<rtmin] = FALSE
	.rtdata.valid(rtdat)[rtvec>rtmax] = FALSE
	#.rtdata.valid(rtdat)[is.na(validvec)] = NA
	
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
	
	.rtdata.outliers(rtdat) = c(.rtdata.outliers(rtdat),outlier)
	
	##[DEFUNT OUTLIER FUNCTIONS]
		#newlen = length(rtvec[which(validvec==TRUE)])-(length(which(rtvec[which(validvec==TRUE)]<rtmin))+length(which(rtvec[which(validvec==TRUE)]>rtmax)))
		#perc = abs((newlen-length(rtvec[which(validvec==TRUE)]))/length(rtvec[which(validvec==TRUE)])*100)
	
		#.rtdata.outlier.method(rtdat) = method
		#.rtdata.outlier.minmax(rtdat) = c(rtmin,rtmax)
		#.rtdata.outlier.abs(rtdat) = list(total=length(rtvec[which(validvec==TRUE)]),lower=length(which(rtvec[which(validvec==TRUE)]<rtmin)),higher=length(which(rtvec[which(validvec==TRUE)]>rtmax)))
		#.rtdata.outlier.percentage(rtdat) = perc	
	
	#add remarks
	.rtdata.remarks(rtdat) = c(.rtdata.remarks(rtdat),'marked RT outliers as invalid.')


	if(plot) {
		cat('plotsie')
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
