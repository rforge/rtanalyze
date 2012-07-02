# Make Generics for all classes (access and replacement functions)
# CAUTION: USE ONLY TO MAKE Generics.R for EXPORT!!!!!!
# Author: Wouter D. Weeda, University of Amsterdam
###############################################################################


setAccRepFile <- function(classobject=NULL,funcnames=slotNames(classobject),filename,methname,packagename) {
	## setAccRep
	## input is an instance of a class (used to determine the slots of a class)
	## creates accessor functions of type '.classname.slotname()' and replacement
	## functions of type '.classname.slotname()<-'
	## function names can be replaced by a vector of names 
	## the call to setAccRep need only be made once (all accessor and replacment
	## functions will be available in .GlobalEnv
	## basic call is setAccRep(new('nameofclass')) 		
	
	doSetAccRep <- function(funcname,classname,slotname,filename,methname,packagename) {
				
		write(paste('classname <-','\'',classname,'\'',sep=''),file=filename,append=T)
		
		# add classname to function names (but not to slotname)
		write(paste('funcname <-','\'',paste('.',classname,'.',funcname,sep=''),'\'',sep=''),file=filename,append=T)
		funcname <- paste('.',classname,'.',funcname,sep='')
		write(paste(funcname,',',sep=''),file=methname,append=T)
		
		#set Generic and Method for slot access
		write(paste('standGen <- function(object) standardGeneric(\'',funcname,'\')',sep=''),file=filename,append=T)
		write(paste('standMethod <- function(object) object@',slotname,sep=''),file=filename,append=T)
		write(paste('setGeneric(funcname,standGen,package=\'',packagename,'\')',sep=''),file=filename,append=T)
		#write(paste('setGeneric(funcname,standGen,package=\'arf3DS4\')',sep=''),file=filename,append=T)
		write(paste('setMethod(funcname,classname,standMethod)'),file=filename,append=T)
		
		#set Generic and Method for slot replacement
		write(paste('slotreplace <-','\'',paste(funcname,'<-',sep=''),'\'',sep=''),file=filename,append=T)
		slotreplace <- paste(funcname,'<-',sep='')
		write(paste('standGen <- function(x, value) standardGeneric(\'',slotreplace,'\')',sep=''),file=filename,append=T)
		write(paste('standMethod <- function(x, value) {x@',slotname,'<- value;x}',sep=''),file=filename,append=T)
		write(paste('setGeneric(slotreplace,standGen)',sep=''),file=filename,append=T)
		write(paste('setReplaceMethod(funcname,classname,standMethod)'),file=filename,append=T)
		
		
		
		return(invisible())
	}
	
	#check if input is a class
	if(is.object(classobject)) 	{
		
		#check if number of function names match number of slots 
		if(length(funcnames)==length(slotNames(classobject))) {
			
			for(i in 1:length(slotNames(classobject))) doSetAccRep(funcnames[i],class(classobject)[1],slotNames(classobject)[i],filename,methname,packagename)
			
		} else stop('Number of slots in class do not match vector of funcnames!') 
		
		return(invisible())
	}		
	
}
source('~/Documents/workspace/RTanalyze/R/ClassDef.R')
filename='~/Documents/workspace/RTanalyze/R/Generics.R'
methname='~/Documents/workspace/RTanalyze/tools/methodnames.txt'
packagename = 'rtanalyze'

if(file.exists(filename)) file.remove(filename)
if(file.exists(methname)) file.remove(methname)

cat('Writing Generic Access and Replacement Methods to:',filename,'\n')
write('exportMethods(',file=methname,append=T)
cat('ready\n')

cat('Writing methodlist to:',methname,'\n')
setAccRepFile(new('version'),filename=filename,methname=methname,packagename=packagename)
setAccRepFile(new('subjects'),filename=filename,methname=methname,packagename=packagename)
setAccRepFile(new('outlier'),filename=filename,methname=methname,packagename=packagename)
setAccRepFile(new('subjectoutlier'),filename=filename,methname=methname,packagename=packagename)
setAccRepFile(new('rtdata'),filename=filename,methname=methname,packagename=packagename)
setAccRepFile(new('rtsummary'),filename=filename,methname=methname,packagename=packagename)
setAccRepFile(new('fastdm'),filename=filename,methname=methname,packagename=packagename)
setAccRepFile(new('fdmoutput'),filename=filename,methname=methname,packagename=packagename)


write(')',file=methname,append=T)

cat('ready\n')
