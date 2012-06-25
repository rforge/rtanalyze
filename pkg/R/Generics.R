classname <-'version'
funcname <-'.version.version'
standGen <- function(object) standardGeneric('.version.version')
standMethod <- function(object) object@version
setGeneric(funcname,standGen,package='swale')
setMethod(funcname,classname,standMethod)
slotreplace <-'.version.version<-'
standGen <- function(x, value) standardGeneric('.version.version<-')
standMethod <- function(x, value) {x@version<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'version'
funcname <-'.version.build'
standGen <- function(object) standardGeneric('.version.build')
standMethod <- function(object) object@build
setGeneric(funcname,standGen,package='swale')
setMethod(funcname,classname,standMethod)
slotreplace <-'.version.build<-'
standGen <- function(x, value) standardGeneric('.version.build<-')
standMethod <- function(x, value) {x@build<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'version'
funcname <-'.version.update'
standGen <- function(object) standardGeneric('.version.update')
standMethod <- function(object) object@update
setGeneric(funcname,standGen,package='swale')
setMethod(funcname,classname,standMethod)
slotreplace <-'.version.update<-'
standGen <- function(x, value) standardGeneric('.version.update<-')
standMethod <- function(x, value) {x@update<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'version'
funcname <-'.version.svnrev'
standGen <- function(object) standardGeneric('.version.svnrev')
standMethod <- function(object) object@svnrev
setGeneric(funcname,standGen,package='swale')
setMethod(funcname,classname,standMethod)
slotreplace <-'.version.svnrev<-'
standGen <- function(x, value) standardGeneric('.version.svnrev<-')
standMethod <- function(x, value) {x@svnrev<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'subjects'
funcname <-'.subjects.experimentname'
standGen <- function(object) standardGeneric('.subjects.experimentname')
standMethod <- function(object) object@experimentname
setGeneric(funcname,standGen,package='swale')
setMethod(funcname,classname,standMethod)
slotreplace <-'.subjects.experimentname<-'
standGen <- function(x, value) standardGeneric('.subjects.experimentname<-')
standMethod <- function(x, value) {x@experimentname<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'subjects'
funcname <-'.subjects.variables'
standGen <- function(object) standardGeneric('.subjects.variables')
standMethod <- function(object) object@variables
setGeneric(funcname,standGen,package='swale')
setMethod(funcname,classname,standMethod)
slotreplace <-'.subjects.variables<-'
standGen <- function(x, value) standardGeneric('.subjects.variables<-')
standMethod <- function(x, value) {x@variables<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'subjects'
funcname <-'.subjects.variable.levels'
standGen <- function(object) standardGeneric('.subjects.variable.levels')
standMethod <- function(object) object@variable.levels
setGeneric(funcname,standGen,package='swale')
setMethod(funcname,classname,standMethod)
slotreplace <-'.subjects.variable.levels<-'
standGen <- function(x, value) standardGeneric('.subjects.variable.levels<-')
standMethod <- function(x, value) {x@variable.levels<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'subjects'
funcname <-'.subjects.rtdata'
standGen <- function(object) standardGeneric('.subjects.rtdata')
standMethod <- function(object) object@rtdata
setGeneric(funcname,standGen,package='swale')
setMethod(funcname,classname,standMethod)
slotreplace <-'.subjects.rtdata<-'
standGen <- function(x, value) standardGeneric('.subjects.rtdata<-')
standMethod <- function(x, value) {x@rtdata<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'subjects'
funcname <-'.subjects.valid'
standGen <- function(object) standardGeneric('.subjects.valid')
standMethod <- function(object) object@valid
setGeneric(funcname,standGen,package='swale')
setMethod(funcname,classname,standMethod)
slotreplace <-'.subjects.valid<-'
standGen <- function(x, value) standardGeneric('.subjects.valid<-')
standMethod <- function(x, value) {x@valid<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'subjects'
funcname <-'.subjects.remarks'
standGen <- function(object) standardGeneric('.subjects.remarks')
standMethod <- function(object) object@remarks
setGeneric(funcname,standGen,package='swale')
setMethod(funcname,classname,standMethod)
slotreplace <-'.subjects.remarks<-'
standGen <- function(x, value) standardGeneric('.subjects.remarks<-')
standMethod <- function(x, value) {x@remarks<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'subjects'
funcname <-'.subjects.version'
standGen <- function(object) standardGeneric('.subjects.version')
standMethod <- function(object) object@version
setGeneric(funcname,standGen,package='swale')
setMethod(funcname,classname,standMethod)
slotreplace <-'.subjects.version<-'
standGen <- function(x, value) standardGeneric('.subjects.version<-')
standMethod <- function(x, value) {x@version<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'outlier'
funcname <-'.outlier.type'
standGen <- function(object) standardGeneric('.outlier.type')
standMethod <- function(object) object@type
setGeneric(funcname,standGen,package='swale')
setMethod(funcname,classname,standMethod)
slotreplace <-'.outlier.type<-'
standGen <- function(x, value) standardGeneric('.outlier.type<-')
standMethod <- function(x, value) {x@type<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'outlier'
funcname <-'.outlier.method'
standGen <- function(object) standardGeneric('.outlier.method')
standMethod <- function(object) object@method
setGeneric(funcname,standGen,package='swale')
setMethod(funcname,classname,standMethod)
slotreplace <-'.outlier.method<-'
standGen <- function(x, value) standardGeneric('.outlier.method<-')
standMethod <- function(x, value) {x@method<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'outlier'
funcname <-'.outlier.minmax'
standGen <- function(object) standardGeneric('.outlier.minmax')
standMethod <- function(object) object@minmax
setGeneric(funcname,standGen,package='swale')
setMethod(funcname,classname,standMethod)
slotreplace <-'.outlier.minmax<-'
standGen <- function(x, value) standardGeneric('.outlier.minmax<-')
standMethod <- function(x, value) {x@minmax<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'outlier'
funcname <-'.outlier.pre.total'
standGen <- function(object) standardGeneric('.outlier.pre.total')
standMethod <- function(object) object@pre.total
setGeneric(funcname,standGen,package='swale')
setMethod(funcname,classname,standMethod)
slotreplace <-'.outlier.pre.total<-'
standGen <- function(x, value) standardGeneric('.outlier.pre.total<-')
standMethod <- function(x, value) {x@pre.total<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'outlier'
funcname <-'.outlier.rem.total'
standGen <- function(object) standardGeneric('.outlier.rem.total')
standMethod <- function(object) object@rem.total
setGeneric(funcname,standGen,package='swale')
setMethod(funcname,classname,standMethod)
slotreplace <-'.outlier.rem.total<-'
standGen <- function(x, value) standardGeneric('.outlier.rem.total<-')
standMethod <- function(x, value) {x@rem.total<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'outlier'
funcname <-'.outlier.rem.low'
standGen <- function(object) standardGeneric('.outlier.rem.low')
standMethod <- function(object) object@rem.low
setGeneric(funcname,standGen,package='swale')
setMethod(funcname,classname,standMethod)
slotreplace <-'.outlier.rem.low<-'
standGen <- function(x, value) standardGeneric('.outlier.rem.low<-')
standMethod <- function(x, value) {x@rem.low<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'outlier'
funcname <-'.outlier.rem.high'
standGen <- function(object) standardGeneric('.outlier.rem.high')
standMethod <- function(object) object@rem.high
setGeneric(funcname,standGen,package='swale')
setMethod(funcname,classname,standMethod)
slotreplace <-'.outlier.rem.high<-'
standGen <- function(x, value) standardGeneric('.outlier.rem.high<-')
standMethod <- function(x, value) {x@rem.high<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'outlier'
funcname <-'.outlier.rem.prop'
standGen <- function(object) standardGeneric('.outlier.rem.prop')
standMethod <- function(object) object@rem.prop
setGeneric(funcname,standGen,package='swale')
setMethod(funcname,classname,standMethod)
slotreplace <-'.outlier.rem.prop<-'
standGen <- function(x, value) standardGeneric('.outlier.rem.prop<-')
standMethod <- function(x, value) {x@rem.prop<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'outlier'
funcname <-'.outlier.post.total'
standGen <- function(object) standardGeneric('.outlier.post.total')
standMethod <- function(object) object@post.total
setGeneric(funcname,standGen,package='swale')
setMethod(funcname,classname,standMethod)
slotreplace <-'.outlier.post.total<-'
standGen <- function(x, value) standardGeneric('.outlier.post.total<-')
standMethod <- function(x, value) {x@post.total<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'outlier'
funcname <-'.outlier.remark'
standGen <- function(object) standardGeneric('.outlier.remark')
standMethod <- function(object) object@remark
setGeneric(funcname,standGen,package='swale')
setMethod(funcname,classname,standMethod)
slotreplace <-'.outlier.remark<-'
standGen <- function(x, value) standardGeneric('.outlier.remark<-')
standMethod <- function(x, value) {x@remark<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'outlier'
funcname <-'.outlier.version'
standGen <- function(object) standardGeneric('.outlier.version')
standMethod <- function(object) object@version
setGeneric(funcname,standGen,package='swale')
setMethod(funcname,classname,standMethod)
slotreplace <-'.outlier.version<-'
standGen <- function(x, value) standardGeneric('.outlier.version<-')
standMethod <- function(x, value) {x@version<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'rtdata'
funcname <-'.rtdata.rt'
standGen <- function(object) standardGeneric('.rtdata.rt')
standMethod <- function(object) object@rt
setGeneric(funcname,standGen,package='swale')
setMethod(funcname,classname,standMethod)
slotreplace <-'.rtdata.rt<-'
standGen <- function(x, value) standardGeneric('.rtdata.rt<-')
standMethod <- function(x, value) {x@rt<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'rtdata'
funcname <-'.rtdata.correct'
standGen <- function(object) standardGeneric('.rtdata.correct')
standMethod <- function(object) object@correct
setGeneric(funcname,standGen,package='swale')
setMethod(funcname,classname,standMethod)
slotreplace <-'.rtdata.correct<-'
standGen <- function(x, value) standardGeneric('.rtdata.correct<-')
standMethod <- function(x, value) {x@correct<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'rtdata'
funcname <-'.rtdata.valid'
standGen <- function(object) standardGeneric('.rtdata.valid')
standMethod <- function(object) object@valid
setGeneric(funcname,standGen,package='swale')
setMethod(funcname,classname,standMethod)
slotreplace <-'.rtdata.valid<-'
standGen <- function(x, value) standardGeneric('.rtdata.valid<-')
standMethod <- function(x, value) {x@valid<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'rtdata'
funcname <-'.rtdata.conditions'
standGen <- function(object) standardGeneric('.rtdata.conditions')
standMethod <- function(object) object@conditions
setGeneric(funcname,standGen,package='swale')
setMethod(funcname,classname,standMethod)
slotreplace <-'.rtdata.conditions<-'
standGen <- function(x, value) standardGeneric('.rtdata.conditions<-')
standMethod <- function(x, value) {x@conditions<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'rtdata'
funcname <-'.rtdata.condition.levels'
standGen <- function(object) standardGeneric('.rtdata.condition.levels')
standMethod <- function(object) object@condition.levels
setGeneric(funcname,standGen,package='swale')
setMethod(funcname,classname,standMethod)
slotreplace <-'.rtdata.condition.levels<-'
standGen <- function(x, value) standardGeneric('.rtdata.condition.levels<-')
standMethod <- function(x, value) {x@condition.levels<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'rtdata'
funcname <-'.rtdata.remarks'
standGen <- function(object) standardGeneric('.rtdata.remarks')
standMethod <- function(object) object@remarks
setGeneric(funcname,standGen,package='swale')
setMethod(funcname,classname,standMethod)
slotreplace <-'.rtdata.remarks<-'
standGen <- function(x, value) standardGeneric('.rtdata.remarks<-')
standMethod <- function(x, value) {x@remarks<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'rtdata'
funcname <-'.rtdata.outlier.method'
standGen <- function(object) standardGeneric('.rtdata.outlier.method')
standMethod <- function(object) object@outlier.method
setGeneric(funcname,standGen,package='swale')
setMethod(funcname,classname,standMethod)
slotreplace <-'.rtdata.outlier.method<-'
standGen <- function(x, value) standardGeneric('.rtdata.outlier.method<-')
standMethod <- function(x, value) {x@outlier.method<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'rtdata'
funcname <-'.rtdata.outlier.minmax'
standGen <- function(object) standardGeneric('.rtdata.outlier.minmax')
standMethod <- function(object) object@outlier.minmax
setGeneric(funcname,standGen,package='swale')
setMethod(funcname,classname,standMethod)
slotreplace <-'.rtdata.outlier.minmax<-'
standGen <- function(x, value) standardGeneric('.rtdata.outlier.minmax<-')
standMethod <- function(x, value) {x@outlier.minmax<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'rtdata'
funcname <-'.rtdata.outlier.percentage'
standGen <- function(object) standardGeneric('.rtdata.outlier.percentage')
standMethod <- function(object) object@outlier.percentage
setGeneric(funcname,standGen,package='swale')
setMethod(funcname,classname,standMethod)
slotreplace <-'.rtdata.outlier.percentage<-'
standGen <- function(x, value) standardGeneric('.rtdata.outlier.percentage<-')
standMethod <- function(x, value) {x@outlier.percentage<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'rtdata'
funcname <-'.rtdata.outliers'
standGen <- function(object) standardGeneric('.rtdata.outliers')
standMethod <- function(object) object@outliers
setGeneric(funcname,standGen,package='swale')
setMethod(funcname,classname,standMethod)
slotreplace <-'.rtdata.outliers<-'
standGen <- function(x, value) standardGeneric('.rtdata.outliers<-')
standMethod <- function(x, value) {x@outliers<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'rtdata'
funcname <-'.rtdata.summary'
standGen <- function(object) standardGeneric('.rtdata.summary')
standMethod <- function(object) object@summary
setGeneric(funcname,standGen,package='swale')
setMethod(funcname,classname,standMethod)
slotreplace <-'.rtdata.summary<-'
standGen <- function(x, value) standardGeneric('.rtdata.summary<-')
standMethod <- function(x, value) {x@summary<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'rtdata'
funcname <-'.rtdata.version'
standGen <- function(object) standardGeneric('.rtdata.version')
standMethod <- function(object) object@version
setGeneric(funcname,standGen,package='swale')
setMethod(funcname,classname,standMethod)
slotreplace <-'.rtdata.version<-'
standGen <- function(x, value) standardGeneric('.rtdata.version<-')
standMethod <- function(x, value) {x@version<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'rtsummary'
funcname <-'.rtsummary.quantiles'
standGen <- function(object) standardGeneric('.rtsummary.quantiles')
standMethod <- function(object) object@quantiles
setGeneric(funcname,standGen,package='swale')
setMethod(funcname,classname,standMethod)
slotreplace <-'.rtsummary.quantiles<-'
standGen <- function(x, value) standardGeneric('.rtsummary.quantiles<-')
standMethod <- function(x, value) {x@quantiles<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'rtsummary'
funcname <-'.rtsummary.meanRT'
standGen <- function(object) standardGeneric('.rtsummary.meanRT')
standMethod <- function(object) object@meanRT
setGeneric(funcname,standGen,package='swale')
setMethod(funcname,classname,standMethod)
slotreplace <-'.rtsummary.meanRT<-'
standGen <- function(x, value) standardGeneric('.rtsummary.meanRT<-')
standMethod <- function(x, value) {x@meanRT<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'rtsummary'
funcname <-'.rtsummary.medianRT'
standGen <- function(object) standardGeneric('.rtsummary.medianRT')
standMethod <- function(object) object@medianRT
setGeneric(funcname,standGen,package='swale')
setMethod(funcname,classname,standMethod)
slotreplace <-'.rtsummary.medianRT<-'
standGen <- function(x, value) standardGeneric('.rtsummary.medianRT<-')
standMethod <- function(x, value) {x@medianRT<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'rtsummary'
funcname <-'.rtsummary.sdRT'
standGen <- function(object) standardGeneric('.rtsummary.sdRT')
standMethod <- function(object) object@sdRT
setGeneric(funcname,standGen,package='swale')
setMethod(funcname,classname,standMethod)
slotreplace <-'.rtsummary.sdRT<-'
standGen <- function(x, value) standardGeneric('.rtsummary.sdRT<-')
standMethod <- function(x, value) {x@sdRT<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'rtsummary'
funcname <-'.rtsummary.pC'
standGen <- function(object) standardGeneric('.rtsummary.pC')
standMethod <- function(object) object@pC
setGeneric(funcname,standGen,package='swale')
setMethod(funcname,classname,standMethod)
slotreplace <-'.rtsummary.pC<-'
standGen <- function(x, value) standardGeneric('.rtsummary.pC<-')
standMethod <- function(x, value) {x@pC<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'rtsummary'
funcname <-'.rtsummary.version'
standGen <- function(object) standardGeneric('.rtsummary.version')
standMethod <- function(object) object@version
setGeneric(funcname,standGen,package='swale')
setMethod(funcname,classname,standMethod)
slotreplace <-'.rtsummary.version<-'
standGen <- function(x, value) standardGeneric('.rtsummary.version<-')
standMethod <- function(x, value) {x@version<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fastdm'
funcname <-'.fastdm.conditions'
standGen <- function(object) standardGeneric('.fastdm.conditions')
standMethod <- function(object) object@conditions
setGeneric(funcname,standGen,package='swale')
setMethod(funcname,classname,standMethod)
slotreplace <-'.fastdm.conditions<-'
standGen <- function(x, value) standardGeneric('.fastdm.conditions<-')
standMethod <- function(x, value) {x@conditions<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fastdm'
funcname <-'.fastdm.format'
standGen <- function(object) standardGeneric('.fastdm.format')
standMethod <- function(object) object@format
setGeneric(funcname,standGen,package='swale')
setMethod(funcname,classname,standMethod)
slotreplace <-'.fastdm.format<-'
standGen <- function(x, value) standardGeneric('.fastdm.format<-')
standMethod <- function(x, value) {x@format<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fastdm'
funcname <-'.fastdm.depends'
standGen <- function(object) standardGeneric('.fastdm.depends')
standMethod <- function(object) object@depends
setGeneric(funcname,standGen,package='swale')
setMethod(funcname,classname,standMethod)
slotreplace <-'.fastdm.depends<-'
standGen <- function(x, value) standardGeneric('.fastdm.depends<-')
standMethod <- function(x, value) {x@depends<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fastdm'
funcname <-'.fastdm.set'
standGen <- function(object) standardGeneric('.fastdm.set')
standMethod <- function(object) object@set
setGeneric(funcname,standGen,package='swale')
setMethod(funcname,classname,standMethod)
slotreplace <-'.fastdm.set<-'
standGen <- function(x, value) standardGeneric('.fastdm.set<-')
standMethod <- function(x, value) {x@set<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fastdm'
funcname <-'.fastdm.datadir'
standGen <- function(object) standardGeneric('.fastdm.datadir')
standMethod <- function(object) object@datadir
setGeneric(funcname,standGen,package='swale')
setMethod(funcname,classname,standMethod)
slotreplace <-'.fastdm.datadir<-'
standGen <- function(x, value) standardGeneric('.fastdm.datadir<-')
standMethod <- function(x, value) {x@datadir<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fastdm'
funcname <-'.fastdm.dataname'
standGen <- function(object) standardGeneric('.fastdm.dataname')
standMethod <- function(object) object@dataname
setGeneric(funcname,standGen,package='swale')
setMethod(funcname,classname,standMethod)
slotreplace <-'.fastdm.dataname<-'
standGen <- function(x, value) standardGeneric('.fastdm.dataname<-')
standMethod <- function(x, value) {x@dataname<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fastdm'
funcname <-'.fastdm.outputname'
standGen <- function(object) standardGeneric('.fastdm.outputname')
standMethod <- function(object) object@outputname
setGeneric(funcname,standGen,package='swale')
setMethod(funcname,classname,standMethod)
slotreplace <-'.fastdm.outputname<-'
standGen <- function(x, value) standardGeneric('.fastdm.outputname<-')
standMethod <- function(x, value) {x@outputname<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fastdm'
funcname <-'.fastdm.subjectlist'
standGen <- function(object) standardGeneric('.fastdm.subjectlist')
standMethod <- function(object) object@subjectlist
setGeneric(funcname,standGen,package='swale')
setMethod(funcname,classname,standMethod)
slotreplace <-'.fastdm.subjectlist<-'
standGen <- function(x, value) standardGeneric('.fastdm.subjectlist<-')
standMethod <- function(x, value) {x@subjectlist<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fastdm'
funcname <-'.fastdm.appdir'
standGen <- function(object) standardGeneric('.fastdm.appdir')
standMethod <- function(object) object@appdir
setGeneric(funcname,standGen,package='swale')
setMethod(funcname,classname,standMethod)
slotreplace <-'.fastdm.appdir<-'
standGen <- function(x, value) standardGeneric('.fastdm.appdir<-')
standMethod <- function(x, value) {x@appdir<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fdmoutput'
funcname <-'.fdmoutput.ID'
standGen <- function(object) standardGeneric('.fdmoutput.ID')
standMethod <- function(object) object@ID
setGeneric(funcname,standGen,package='swale')
setMethod(funcname,classname,standMethod)
slotreplace <-'.fdmoutput.ID<-'
standGen <- function(x, value) standardGeneric('.fdmoutput.ID<-')
standMethod <- function(x, value) {x@ID<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fdmoutput'
funcname <-'.fdmoutput.fdmex'
standGen <- function(object) standardGeneric('.fdmoutput.fdmex')
standMethod <- function(object) object@fdmex
setGeneric(funcname,standGen,package='swale')
setMethod(funcname,classname,standMethod)
slotreplace <-'.fdmoutput.fdmex<-'
standGen <- function(x, value) standardGeneric('.fdmoutput.fdmex<-')
standMethod <- function(x, value) {x@fdmex<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fdmoutput'
funcname <-'.fdmoutput.data'
standGen <- function(object) standardGeneric('.fdmoutput.data')
standMethod <- function(object) object@data
setGeneric(funcname,standGen,package='swale')
setMethod(funcname,classname,standMethod)
slotreplace <-'.fdmoutput.data<-'
standGen <- function(x, value) standardGeneric('.fdmoutput.data<-')
standMethod <- function(x, value) {x@data<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fdmoutput'
funcname <-'.fdmoutput.parameters'
standGen <- function(object) standardGeneric('.fdmoutput.parameters')
standMethod <- function(object) object@parameters
setGeneric(funcname,standGen,package='swale')
setMethod(funcname,classname,standMethod)
slotreplace <-'.fdmoutput.parameters<-'
standGen <- function(x, value) standardGeneric('.fdmoutput.parameters<-')
standMethod <- function(x, value) {x@parameters<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fdmoutput'
funcname <-'.fdmoutput.estimates'
standGen <- function(object) standardGeneric('.fdmoutput.estimates')
standMethod <- function(object) object@estimates
setGeneric(funcname,standGen,package='swale')
setMethod(funcname,classname,standMethod)
slotreplace <-'.fdmoutput.estimates<-'
standGen <- function(x, value) standardGeneric('.fdmoutput.estimates<-')
standMethod <- function(x, value) {x@estimates<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fdmoutput'
funcname <-'.fdmoutput.bootstrapdata'
standGen <- function(object) standardGeneric('.fdmoutput.bootstrapdata')
standMethod <- function(object) object@bootstrapdata
setGeneric(funcname,standGen,package='swale')
setMethod(funcname,classname,standMethod)
slotreplace <-'.fdmoutput.bootstrapdata<-'
standGen <- function(x, value) standardGeneric('.fdmoutput.bootstrapdata<-')
standMethod <- function(x, value) {x@bootstrapdata<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
