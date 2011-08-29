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
