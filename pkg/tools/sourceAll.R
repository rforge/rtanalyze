#options and WD
options(warn=1)
wd='/Users/wweeda2/Documents/workspace/RTanalyze/R/'

#files to source
flist=list.files(wd,pattern='.R',full.names=T,ignore=T)
for(i in 1:length(flist)) eval(parse(text=paste('source(\'',flist[i],'\')',sep='')))
