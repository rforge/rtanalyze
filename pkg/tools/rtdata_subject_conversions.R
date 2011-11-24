source("/Users/wweeda2/Documents/workspace/RTanalyze/tools/sourceAll.R")
source("/Users/wweeda2/Documents/workspace/RTanalyze/tools/pi-experiment-data-functions.R")

## IQ STUDY 1:
dir = '/Users/wweeda2/Documents/Wouter/Projects/IQ/REANALYSES/(WPR) OP5218 IQ STUDY 1/RAWTEXT'
batch.log2text('/Users/wweeda2/Documents/Wouter/Projects/IQ/REANALYSES/(WPR) OP5218 IQ STUDY 1/RAWDATA',dir)
batch.importRTs(dir,studynum=10)
sub.dat.s1 = batch.fillSubjects(dir,bs_conditions=c('orgID','level','sex','IQ','age'))
bs.dat.s1 = read.table('/Users/wweeda2/Documents/Wouter/Projects/IQ/REANALYSES/(WPR) OP5218 IQ STUDY 1/SUBJECTS_DATA/resorted_pitaskinfo.txt',header=T,sep='\t')
subject.data.study1 = fillsubjectsdata(sub.dat.s1,bs.dat.s1)


## IQ STUDY 2:
dir = '/Users/wweeda2/Documents/Wouter/Projects/IQ/REANALYSES/(NCP) OP5414 IQ STUDY 2/RAWTEXT'
batch.importRTs(dir,studynum=20)
sub.dat.s2 = batch.fillSubjects(dir,bs_conditions=c('orgID','instruction','IQ','sex','age'))
bs.dat.s2 = read.table('/Users/wweeda2/Documents/Wouter/Projects/IQ/REANALYSES/(NCP) OP5414 IQ STUDY 2/SUBJECTS_DATA/reordered_conditions.txt',header=T,sep='\t')
subject.data.study2 = fillsubjectsdata(sub.dat.s2,bs.dat.s2)


## IQ STUDY 2a:
dir = '/Users/wweeda2/Documents/Wouter/Projects/IQ/REANALYSES/(NCP) MARIEKE IQ STUDY 2/RAWTEXT'
batch.importRTs(dir,studynum=21)
sub.dat.s2a = batch.fillSubjects(dir,bs_conditions=c('orgID','subnum','level','first','age','sex','iq'))
bs.dat.s2a = read.table('/Users/wweeda2/Documents/Wouter/Projects/IQ/REANALYSES/(NCP) MARIEKE IQ STUDY 2/SUBJECTS_DATA/reordered_conditions.txt',header=T,sep='\t')
subject.data.study2a = fillsubjectsdata(sub.dat.s2a,bs.dat.s2a)

## IQ STUDY 3:
dir = '/Users/wweeda2/Documents/Wouter/Projects/IQ/REANALYSES/(IT+RT) JAAP IQ STUDY 3/RAWTEXT/BLOCK1AND2'
batch.importRTs(dir,studynum=30)
sub.dat.s3.12 = batch.fillSubjects(dir,bs_conditions=c('orgID','IQexp','tetris','IQtest','age','sex','IT','itsd'))
dir = '/Users/wweeda2/Documents/Wouter/Projects/IQ/REANALYSES/(IT+RT) JAAP IQ STUDY 3/RAWTEXT/BLOCK3'
batch.importRTs(dir,studynum=30)
sub.dat.s3.3 = batch.fillSubjects(dir,bs_conditions=c('orgID','IQexp','tetris','IQtest','age','sex','IT','itsd'))
sub.dat.s3 = concatenate.subjects(sub.dat.s3.12,sub.dat.s3.3)
bs.dat.s3 = read.table('/Users/wweeda2/Documents/Wouter/Projects/IQ/REANALYSES/(IT+RT) JAAP IQ STUDY 3/SUBJECTS_DATA/reordered_conditions4.txt',header=T,sep='\t')
subject.data.study3 = fillsubjectsdata(sub.dat.s3,bs.dat.s3)

## IQ STUDY 3a:
dir = '/Users/wweeda2/Documents/Wouter/Projects/IQ/REANALYSES/(IT+RT) OP5614 IQ STUDY 3/RAWTEXT/BLOCK1AND2'
batch.importRTs(dir,studynum=30)
sub.dat.s3a.12 = batch.fillSubjects(dir,bs_conditions=c('orgID','sex','tetris','IQ','level'))
dir = '/Users/wweeda2/Documents/Wouter/Projects/IQ/REANALYSES/(IT+RT) OP5614 IQ STUDY 3/RAWTEXT/BLOCK3'
batch.importRTs(dir,studynum=30)
sub.dat.s3a.3 = batch.fillSubjects(dir,bs_conditions=c('orgID','sex','tetris','IQ','level'))
sub.dat.s3a = concatenate.subjects(sub.dat.s3a.12,sub.dat.s3a.3)
bs.dat.s3a = read.table('/Users/wweeda2/Documents/Wouter/Projects/IQ/REANALYSES/(IT+RT) OP5614 IQ STUDY 3/SUBJECTS_DATA/reordered_conditions.txt',header=T,sep='\t')
subject.data.study3a = fillsubjectsdata(sub.dat.s3a,bs.dat.s3a)



save(list=ls(pattern='subject.data.study'),file='all_subjectdata_objects_aut100_study1.Rda')
