log2text <-
function(logfile)
{
	
	#settings
	stim.mrk='Picture pi'
	resp.mrk='Response'
	within.mrk=matrix(c('pi-dll','pi-ull','pi-drl','pi-url','pi-dlm','pi-ulm','pi-drm','pi-urm','pi-dls','pi-uls','pi-drs','pi-urs'),12,4,byrow=F)
	dimnames(within.mrk)=list(seq(1,12),c('mrk','orientation','left/right','difficulty'))
	within.mrk[,2]=rep(c('down','up'),6)
	within.mrk[,3]=rep(c(rep('left',2),rep('right',2)),3)
	within.mrk[,4]=c(rep('easy',4),rep('medium',4),rep('difficult',4))
	
	#read in data	
	skip=2
	rawdat = read.table(logfile,skip=skip,sep='\t',header=T,fill=T,stringsAsFactors=F)
	startline = grep(within.mrk[1],rawdat$Code)[1]		
	#make rtdataframe
	rtdata = data.frame(trial=0,RT=0,correct=F,orient='none',lr='none',diff='none',stim_uc=0,resp_uc=0,stringsAsFactors=F)
	
	trial=1
	#line-by-line analysis
	for(line in startline:nrow(rawdat)) {
	
		#get initial time
		start.time = rawdat$Time[line]
		
		#get trial type and set conditions (if a stimulus trial)
		type = grep(rawdat$Code[line],within.mrk[,1])
	
		if(length(type)==1) {
			orient = within.mrk[type,2]
			lr = within.mrk[type,3]
			diff = within.mrk[type,4]
			
			stim_uc=rawdat$Uncertainty[line]
			
		
			#get correct or incorrect response
			stop=F
			i=line
			while(!stop) {
				if(i>=nrow(rawdat)) stop=T
				if(rawdat$Event.Type[i]==resp.mrk) {
		 			stop.time=rawdat$Time[i]
		 			response=rawdat$Code[i]
		 			resp_uc=rawdat$Uncertainty[i]
		 			stop=T
		 		} 	
		 		i=i+1
			}
		
			if(i<nrow(rawdat)) {
				#do correc/incorrect (1=left,2=right)
				correct=NA
				if(lr=='left' & response=='1') correct=TRUE
				if(lr=='left' & response=='2') correct=FALSE
				if(lr=='right' & response=='2') correct=TRUE
				if(lr=='right' & response=='1') correct=FALSE
		
				#calculate RT
				RT = stop.time-start.time
		
				#add line to rtdata
				rtdata = rbind(rtdata,c(trial,RT,correct,orient,lr,diff,stim_uc,resp_uc))
		
			}
			trial=trial+1
		}
	}
	
	
	
	#make data.frame and set correct 
	rtdata = rtdata[-1,]
	row.names(rtdata)=seq(1,nrow(rtdata))	
	rtdata$RT = as.numeric(rtdata$RT)
	rtdata$correct = as.logical(rtdata$correct)
	rtdata$trial = as.numeric(rtdata$trial)
	rtdata$orient = as.factor(rtdata$orient)
	rtdata$lr = as.factor(rtdata$lr)
	rtdata$diff = as.factor(rtdata$diff)
	rtdata$stim_uc = as.numeric(rtdata$stim_uc)
	rtdata$resp_uc = as.numeric(rtdata$resp_uc)
	
	
	return(rtdata)
}


batch.log2text <- 
function(dir,newdir=NA,patt='.txt',add='newraw_') {

	filevec = list.files(dir,patt,full=F)
	if(is.na(newdir)) newdir=dir

	for(file in filevec) {
	
		newdat = log2text(paste(dir,'/',file,sep=''))
		fn = paste(newdir,'/',add,file,sep='')
		write.table(newdat,file=fn,quote=F,sep='\t',row.names=F,col.names=T)
		cat(file,'>>>',fn,'\n')	
	
	}	
		
}

batch.importRTs <-
function(dir,newdir=NA,patt='.txt',add='rtdata_',studynum=0)
{
	filevec = list.files(dir,patt,full=F)
	if(is.na(newdir)) newdir=dir
	
	for(file in filevec) {
	
		fs= strsplit(file,'\\.')
		
		## IQ STUDY 1: 
		if(studynum==10) rtdata = importRTs(paste(dir,'/',file,sep=''),rtcol='RT',correctcol='correct',correct.answer=TRUE,incorrect.answer=FALSE,ws_conds=c('orient','lr','diff'),ms.correction=.1)
		
		## IQ STUDY 2:
		if(studynum==20) rtdata = importRTs(paste(dir,'/',file,sep=''),rtcol='RTcmp',correctcol='correct',correct.answer='correct',incorrect.answer='incorrect',ws_conds=c('condition'),ms.correction=1)
		
		## IQ STUDY 2a:
  		if(studynum==21) rtdata = importRTs(paste(dir,'/',file,sep=''),rtcol='RTcmp',correctcol='correct',correct.answer='correct',incorrect.answer='incorrect',ws_conds=c('condition'),ms.correction=1)
		
		## IQ STUDY 3:
  		if(studynum==30) {
  			rtdata = importRTs(paste(dir,'/',file,sep=''),rtcol='RTcmp',correctcol='correct',correct.answer='correct',incorrect.answer='incorrect',ws_conds=c('condition','stimulus'),ms.correction=1)
			rtdata = recondition.stimulus(rtdata)
		}

		## IQ STUDY 3a:
  		if(studynum==31) {
  			rtdata = importRTs(paste(dir,'/',file,sep=''),rtcol='RTcmp',correctcol='correct',correct.answer='correct',incorrect.answer='incorrect',ws_conds=c('condition','stimulus'),ms.correction=1)
			rtdata = recondition.stimulus(rtdata)
		}
				
		fn = paste(newdir,'/',add,fs[[1]][1],'.Rda',sep='')
		save(rtdata,file=fn)
		cat(file,'>>>',fn,'\n')	
		
	}	
	
}

concatenate.subjects <- 
function(subjects1,subjects2) 
{
	
	newrt=vector('list',length(subjects1@rtdata))
	for(i in 1:length(subjects1@rtdata)) {

		subjects1@rtdata[[i]]=concatenate.rtdata(subjects1@rtdata[[i]],subjects2@rtdata[[i]])
		
	}
	

	return(subjects1)
}

recondition.stimulus <-
function(rtdata)
#recondition pi-stimuli into newones
{
	stims = as.character(.rtdata.conditions(rtdata)$stimulus)
	
	newstim = rep(NA,length(stims))
	newstim[grep('eas',stims)]='easy'
	newstim[grep('med',stims)]='medium'
	newstim[grep('dif',stims)]='difficult'
		
	.rtdata.conditions(rtdata)$stimulus = as.factor(newstim)
	
	return(rtdata)
	
}

fillsubjectsdata <-
function(subjects,values)
#fill a subject data.frame with values
{
	
	rnames = rownames(subjects@variables)
	cnames = colnames(subjects@variables)
	
	newfr = values
	rownames(newfr) = rnames
	colnames(newfr) = cnames
	
	subjects@variables = newfr

	return(subjects)
}