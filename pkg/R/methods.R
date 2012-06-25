#############################################
# RTanalyze Methods							#
# Copyright(c) 2009 Wouter D. Weeda			#
# University of Amsterdam					#
#############################################


setGeneric('aggregate',package='stats',where=.GlobalEnv)

#method for rtdata
setMethod('aggregate','rtdata',
		function(x,which=NULL,FUN,useCorrect=TRUE) {
			aggregate.rtdata(x,which,FUN,useCorrect)
		}
)

#method for rtdata show
setMethod('show','rtdata',
		function(object) {
			dat = data.frame(object@conditions,correct=object@correct,rt=object@rt,valid=object@valid)
			show(dat)
			cat('\n[remarks]:\n')
			for(i in 1:length(object@remarks)) cat('',object@remarks[i],'\n')
			
			cat('\n[outliers]:\n')
			cat(' method   :',object@outlier.method,'\n')
			cat(' minmax   :',object@outlier.minmax,'\n')
			#cat(' abs\n')
			#cat('      tot :',object@outlier.abs$total,'\n')
			#cat('      low :',object@outlier.abs$lower,'\n')
			#cat('      hi  :',object@outlier.abs$higher,'\n')
			cat(' perc/tot :',round(object@outlier.percentage,2),'\n')
			cat('\n')
		}
)

#method for rtdata show
setMethod('show','subjects',
		function(object) {
			if(length(object@variables)==0)	cat(names(object@rtdata),'\n') else {
				x=rep('',nrow(object@variables))
				x[object@valid]='*'
				x=cbind(object@variables,x)
				names(x)[ncol(x)]='(valid)'
				x=cbind(x,seq(1:nrow(x)))
				names(x)[ncol(x)]='(row)'
				show(x)
			}
		}
)


#method for rtdata show
setMethod('names','rtdata',
		function(x) {
			dat = data.frame(x@conditions,correct=x@correct,rt=x@rt,valid=x@valid)
			return(names(dat))
		}
)

#### SUBSETTING DATA.FRAME ACCESS AND REPLACEMENT METHODS #####
#method for rtdata SUBSETTING
setMethod('$','rtdata',
		function(x,name) {
			dat = data.frame(x@conditions,correct=x@correct,rt=x@rt,valid=x@valid)
			return(eval(parse(text=paste('dat$',name,sep=''))))
		}
)
setMethod('[','rtdata',
		function(x,i,j,drop='missing') {
			dat = data.frame(x@conditions,correct=x@correct,rt=x@rt,valid=x@valid)
			if(missing(i)) i = NULL
			if(missing(j)) j = NULL
			return(eval(parse(text=paste('dat[',i,',',j,']',sep=''))))
		}
)
setMethod('[[','rtdata',
		function(x,i,j,drop='missing') {
			dat = data.frame(x@conditions,correct=x@correct,rt=x@rt,valid=object@valid)
			return(eval(parse(text=paste('dat[[',i,']]',sep=''))))
		}
)
setMethod('$<-','rtdata',
		function(x,name,value) {
			dat = data.frame(x@conditions,correct=x@correct,rt=x@rt,valid=object@valid)
			eval(parse(text=paste('dat$',name,'<-value',sep='')))
			return(dat)
		}
)
setMethod('[<-','rtdata',
		function(x,i,j,value,drop='missing') {
			dat = data.frame(x@conditions,correct=x@correct,rt=x@rt,valid=object@valid)
			if(missing(i)) i = NULL
			if(missing(j)) j = NULL
			eval(parse(text=paste('dat[',i,',',j,']<-value',sep='')))
			return(dat)
		}
)
setMethod('[[<-','rtdata',
		function(x,i,j,value,drop='missing') {
			dat = data.frame(x@conditions,correct=x@correct,rt=x@rt,valid=object@valid)
			eval(parse(text=paste('dat[[',i,']]<-value',sep='')))
			return(dat)
		}
)


#### SUBSETTING DATA.FRAME ACCESS AND REPLACEMENT METHODS #####
#method for subjects SUBSETTING
setMethod('$','subjects',
		function(x,name) {
			if(length(which(colnames(x@variables)==name))==0) rtdat = eval(parse(text=paste(' x@rtdata$',name,'',sep='')))
			else rtdat = eval(parse(text=paste(' x@variables$',name,'',sep='')))
			return(rtdat)
		}
)
setMethod('[','subjects',
		function(x,i,j,drop='missing') {
			if(missing(i)) i = NULL
			if(missing(j)) j = NULL
			return(eval(parse(text=paste('x@variables[',i,',',j,']',sep=''))))
		}
)
setMethod('[[','subjects',
		function(x,i,j,drop='missing') {
			return(eval(parse(text=paste('x@variables[[',i,']]',sep=''))))
		}
)
setMethod('$<-','subjects',
		function(x,name,value) {
			rtdat = eval(parse(text=paste(' x@variables$',name,'<-value',sep='')))
			return(rtdat)
		}
)
setMethod('[<-','subjects',
		function(x,i,j,value,drop='missing') {
			if(missing(i)) i = NULL
			if(missing(j)) j = NULL
			return(eval(parse(text=paste('x@variables[',i,',',j,']<-value',sep=''))))
		}
)
setMethod('[[<-','subjects',
		function(x,i,j,value,drop='missing') {
			return(eval(parse(text=paste('x@variables[[',i,']]<-value',sep=''))))
		}
)
