#############################################
# RTanalyze preProcessing Functions			#
# Copyright(c) 2009 Wouter D. Weeda			#
# University of Amsterdam					#
#############################################



#CONTAINS
#checkSubjects
#longform

checkSubjects <-
function(subjects,min.n=10)
#check if subjects have a minimal of VALID responses (overall)
{
	if(class(subjects)!='subjects') stop('input must be of class \'subjects\'')
	
	for(rtdat in 1:length(.subjects.rtdata(subjects))) 	if(length(which(.rtdata.valid(.subjects.rtdata(subjects)[[rtdat]])==TRUE))<10) .subjects.valid(subjects)[rtdat]=FALSE
			
	return(subjects)
	
}

export.rt <- function() 
#put all RT's in longform (for SPSS export)
{
cat('still defunct\n')	
	
}

