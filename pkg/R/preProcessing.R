#############################################
# RTanalyze Analysis Functions				#
# Copyright(c) 2009 Wouter D. Weeda			#
# University of Amsterdam					#
#############################################



#CONTAINS
#cormat.test


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