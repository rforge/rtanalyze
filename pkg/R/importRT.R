#############################################
# RTanalyze importFunctions 				#
# Copyright(c) 2009 Wouter D. Weeda			#
# University of Amsterdam					#
#############################################


#CONTAINS
#importRTs
#batch.fillSubjects

importRTs <-
function(filename,rtcol='RTclc',correctcol='correct',correct.answer='correct',incorrect.answer='incorrect',ws_conds=c('condition'),autoblock=99,remove.invalidresponse=T,resp_uclim=c('resp_uc',100),stim_uclim=c('stim_uc',100),ms.correction=.1)
{
	if(!file.exists(filename)) stop('file does not exist.')
	
	#read in RawData
	rawdat = read.table(filename,header=T,fill=T,sep='\t')
	
	#determine RTcol and Correctcol
	rtcol = which(!is.na(match(names(rawdat),rtcol)))
	correctcol = which(!is.na(match(names(rawdat),correctcol))) 
	
	#create new Rtdata object
	rtdat = new('rtdata')
	
	#create RT vector
	.rtdata.rt(rtdat) = as.numeric(rawdat[,rtcol])
	
	#set in ms
	.rtdata.rt(rtdat) = .rtdata.rt(rtdat)*ms.correction
		
	#create logical vector of Correct and Incorrect responses
	.rtdata.correct(rtdat) = rep(FALSE,length(.rtdata.rt(rtdat))) 
	.rtdata.correct(rtdat)[rawdat[,correctcol]==correct.answer] = TRUE
	.rtdata.correct(rtdat)[rawdat[,correctcol]!=correct.answer & rawdat[,correctcol]!=incorrect.answer] = NA
	
	#create condition data.frame
	condcols = which(!is.na(match(names(rawdat),ws_conds)))
	.rtdata.conditions(rtdat) = as.data.frame(rawdat[,condcols],stringsAsFactors=T)
	names(.rtdata.conditions(rtdat)) = ws_conds
	
	#if autoblock = TRUE >> create blocks
	if(is.numeric(autoblock)) {
		block = rep(seq(1,length(.rtdata.correct(rtdat))/autoblock),each=autoblock)
		if(length(block)!=length(.rtdata.correct(rtdat))) block = c(block,rep(NA,abs(length(.rtdata.correct(rtdat))-length(block))))
		.rtdata.conditions(rtdat) = cbind(.rtdata.conditions(rtdat),as.factor(block))
		names(.rtdata.conditions(rtdat)) = c(ws_conds,'block2')
	}
	
	#create valid vector for stim/resp uc required + add remarks
	.rtdata.valid(rtdat) = rep(TRUE,length(.rtdata.rt(rtdat)))
	
	#check resp_uc  
	if(length(resp_uclim)>0) {
		resp_uc = rawdat[,which(!is.na(match(names(rawdat),resp_uclim[1])))]
		.rtdata.rt(rtdat)[which(resp_uc>=as.numeric(resp_uclim[2]))]=NA
		.rtdata.remarks(rtdat) = c(.rtdata.remarks(rtdat),paste(length(which(resp_uc>=as.numeric(resp_uclim[2]))),' uc_responses >=',resp_uclim[2],' marked.',sep=''))
	}
	
	#check stim_uc
	if(length(stim_uclim)>0) {
		stim_uc = rawdat[,which(!is.na(match(names(rawdat),stim_uclim[1])))]
		.rtdata.rt(rtdat)[which(stim_uc>=as.numeric(stim_uclim[2]))]=NA
		.rtdata.remarks(rtdat) = c(.rtdata.remarks(rtdat),paste(length(which(resp_uc>=as.numeric(stim_uclim[2]))),' uc_stimuli >=',stim_uclim[2],' marked.',sep=''))
	}
	
	#check wrong responses
	if(remove.invalidresponse)	{
		.rtdata.rt(rtdat)[which(is.na(.rtdata.correct(rtdat)))]=NA	
		.rtdata.remarks(rtdat) = c(.rtdata.remarks(rtdat),paste(length(which(is.na(.rtdata.correct(rtdat)))),'of invalid responses marked.'))
	}
	
	
	return(rtdat)
	
}

batch.fillSubjects <-
function(dir,patt='.Rda',bs_conditions=NULL,expname='default experiment',markOutliers=TRUE,ol.method='abs',ol.rtmin=250,ol.rtmax=2500) 
#create a subjects object from a list of rdata objects (saved as .Rda files) 
{

	#make new data.frame
	subjects = new('subjects')
	
	#make rtdata list (with names)
	numsubs = length(list.files(dir,patt))
	rtdatas = vector('list',numsubs)
	names(rtdatas) = strsplit(list.files(dir,patt,full.names=F),'.Rda')
	
	#read in the files, preprocess and add to rtdata list
	filelist = list.files(dir,patt,full.names=T)
	for(file in 1:numsubs) {
		load(filelist[file])
		if(markOutliers) rtdata = markOutliers(rtdata,method=ol.method,rtmin=ol.rtmin,rtmax=ol.rtmax)
		rtdatas[[file]] = rtdata
	}
	
	#set the appropriate slots
	.subjects.rtdata(subjects) = rtdatas
	.subjects.experimentname(subjects) = expname
	.subjects.valid(subjects) = rep(TRUE,numsubs)
	
	#make rownames and colnames of subjects data.frame
	if(length(bs_conditions)>0) {
		conds = data.frame(matrix(NA,numsubs,length(bs_conditions)))
		colnames(conds) = bs_conditions
		rownames(conds) = names(rtdatas)
		.subjects.variables(subjects) = conds
	}
	return(subjects)
}
