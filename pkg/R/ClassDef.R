#############################################
# RTanalyze S4 CLASS DEFINITIONS			#
# Copyright(c) 2011 Wouter D. Weeda			#
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
		update=1,
		svnrev=1
	)#,
	#package='RTanalyze'
)


## RTanalyze subject
setClass(
		Class='subjects',
		representation=representation(
				experimentname='character', 	#experiment NAME 
				variables='data.frame',			#between-subject variables values (ID plus factors and covariates)
				variable.levels = 'ANY',		#b-s variable levels
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

## RTanalyze outlier
setClass(
		Class='outlier',
		representation=representation(
				type='character',
				method='character',
				minmax='numeric',
				pre.total='numeric',
				rem.total='numeric',
				rem.low='numeric',
				rem.high='numeric',
				rem.prop='numeric',
				post.total='numeric',
				remark='character',
				version='ANY'
				),
				prototype=prototype(
						version=new('version'),
						type='none',
						method='none',
						minmax=c(-Inf,Inf),
						pre.total=NULL,
						rem.total=NULL,
						rem.low=NULL,
						rem.high=NULL,
						rem.prop=NULL,
						post.total=NULL,
						remark=character(0)
						)
		)

## RTanalyze rtdata
setClass(
		Class='rtdata',
		representation=representation(
				rt='numeric',					#vector of ReactionTimes (in ms)
				correct='logical',				#correct or incorrect response
				valid='logical',				#valid RT (FALSE if outlier)
				conditions='data.frame',		#within-subject conditions	
				condition.levels='ANY',			#within-subject levels
				remarks='character',			#add remarks (pre-procsteps)
				outlier.method='character',		#outlier method [DEFUNCT]
				outlier.minmax='numeric',		#outlier.minmax [DEFUNCT]
				outlier.percentage='numeric',	#outlier percentage [DEFUNCT]
				outliers='list',				#outlier sequence
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

#fastdm
setClass(
		Class='fastdm',
		representation=representation(
				conditions='character',
				format='character',
				depends='list',
				set='list',
				datadir = 'character',
				dataname='character',
				outputname='character',
				subjectlist='character',
				appdir = 'character'
		),
		prototype(
				conditions = c('switch'),
				format = c('TIME','RESPONSE','switch'),
				depends = list(c('v','switch'),c('a','switch'),c('t0','switch')),
				set = list(c('z',.5)),
				dataname = 'ppn*.txt',
				outputname = 'results_ppn*.txt',
				appdir = '/Applications/fast-dm-29/',
				subjectlist= '',
				datadir = ''
		)
)

#fastdmoutput
setClass(
		Class='fdmoutput',
		representation=representation(
				ID='ANY',
				fdmex='ANY',
				data='ANY',
				parameters='ANY',
				estimates='ANY',
				bootstrapdata='ANY'
		)
)

