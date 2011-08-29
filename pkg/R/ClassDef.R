#############################################
# RTanalyze S4 CLASS DEFINITIONS			#
# Copyright(c) 2009 Wouter D. Weeda			#
# University of Amsterdam					#
#############################################

#[CONTAINS]
#version
#experiment
#subject
#rtdata
#rtsummary


## RTanalyze version class (version is set here)
setClass(
	Class='version',
	representation=representation(
		version='numeric',
		build='numeric',
		update='numeric',
		svnrev='numeric'
	),
	prototype=prototype(
		version=1,
		build=0,
		update=0,
		svnrev=0
	)#,
	#package='RTanalyze'
)


## RTanalyze subject
setClass(
		Class='subjects',
		representation=representation(
				experimentname='character', 	#experiment NAME 
				variables='data.frame',			#between-subject variables values (ID plus factors and covariates)
				rtdata='ANY',					#list of rtdata objects
				valid='logical',				#is subject valid
				remarks='character',			#remarks
				version='ANY'					#version
		
		),
		prototype=prototype(
				version=new('version')
		
		)#,
#package='RTanalyze'
)


## RTanalyze rtdata
setClass(
		Class='rtdata',
		representation=representation(
				rt='numeric',					#vector of ReactionTimes (in ms)
				correct='logical',				#correct or incorrect response
				valid='logical',				#valid RT (FALSE if outlier)
				conditions='data.frame',		#within-subject conditions	
				remarks='character',			#add remarks (pre-procsteps)
				outlier.method='character',		#outlier method
				outlier.minmax='numeric',		#outlier.minmax
				outlier.percentage='numeric',
				summary='ANY',					#summary measures for dataset
				version='ANY'					#version
		
		),
		prototype=prototype(
				version=new('version')
		
		)#,
#package='RTanalyze'
)

## RTanalyze rtsummary
setClass(
		Class='rtsummary',
		representation=representation(
				quantiles='data.frame',			#quantiles (per within condition)
				meanRT='data.frame',			#mean RT (per within condition)
				medianRT='data.frame',			#median RT (per within condition)
				sdRT='data.frame',				#standarddeviation of RT (per within condition)
				pC='data.frame',				#percentage correct (per within condition)	
				version='ANY'					#version
		
		),
		prototype=prototype(
				version=new('version')
		
		)#,
#package='RTanalyze'
)