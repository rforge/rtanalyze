#############################################
# RTanalyze preProcessing Functions			#
# Copyright(c) 2009 Wouter D. Weeda			#
# University of Amsterdam					#
#############################################



#CONTAINS
#markOutliers
#checkSubjects

markOutliers <- 
function(rtdat,method=c('abs','sd','mia-masd'),sdfac=3,rtmin=250,rtmax=2500,plot=F) 
#mark outliers based on on absolute values or SD
{
	rtvec = .rtdata.rt(rtdat)
	method = match.arg(method,c('abs','sd','mia-masd'))
	
	#mark all as valid (except NA's)
	validvec = .rtdata.valid(rtdat)
	validvec[!is.na(validvec)] = TRUE
	
	if(method=='sd') {
		rtmin = mean(rtvec[which(validvec==TRUE)])-sd(rtvec[which(validvec==TRUE)])*sdfac
		rtmax = mean(rtvec[which(validvec==TRUE)])+sd(rtvec[which(validvec==TRUE)])*sdfac
	}
	
	if(method=='mia-masd') {
		rtmax = mean(rtvec[which(validvec==TRUE)])+sd(rtvec[which(validvec==TRUE)])*sdfac
	}
	
	.rtdata.valid(rtdat)[rtvec<rtmin] = FALSE
	.rtdata.valid(rtdat)[rtvec>rtmax] = FALSE
	.rtdata.valid(rtdat)[is.na(validvec)] = NA
	
	newlen = length(rtvec[which(validvec==TRUE)])-(length(which(rtvec[which(validvec==TRUE)]<rtmin))+length(which(rtvec[which(validvec==TRUE)]>rtmax)))
	perc = abs((newlen-length(rtvec[which(validvec==TRUE)]))/length(rtvec[which(validvec==TRUE)])*100)

	.rtdata.outlier.method(rtdat) = method
	.rtdata.outlier.minmax(rtdat) = c(rtmin,rtmax)
	.rtdata.outlier.percentage(rtdat) = perc
	
	if(plot) {
		cat('plotsie')
	}
	
	return(rtdat)
}


checkSubjects <-
function(subjects,min.n=10)
#check if subjects have a minimal of VALID responses (overall)
{
	if(class(subjects)!='subjects') stop('input must be of class \'subjects\'')
	
	
	for(rtdat in 1:length(.subjects.rtdata(subjects))) 	if(length(which(.rtdata.valid(.subjects.rtdata(subjects)[[rtdat]])==TRUE))<10) .subjects.valid(subjects)[rtdat]=FALSE
			
	return(subjects)
	
}
