

######one time point treatment ######################

for(sampleSize in c( 200,1000)){
  for(start in seq(1, 500, 100)){
    for (level in c("high", "moderate", "low") ) {
      for(modelType in c("lassoOutcome", "lassoSep", "lassoStepwiseT", "lassoStepwiseY", "stepwise", "allLasso") ) {
        
        #DIREC="M:/Private/varSelection/case4/"
        DIREC=paste0("/home/tkzhou/PSPP_Project/PENCOMP_OneTimePoint/variableSelection5/case2/sampleSize", sampleSize, "/")
        
        #############################################################################  
        batchFile=NULL
        batchFile=c(batchFile, paste(
          
          "#!/bin/sh",
          "#SBATCH --mail-type=ALL",
          "#SBATCH --mail-user=tkzhou@umich.edu",
          "#SBATCH --time=12-10:00:00",
          "",
          
          paste("#SBATCH --workdir=", DIREC, sep=""),
          paste("#SBATCH --job-name=varSelsample", sampleSize,"_", modelType, sep=""),
          
          paste("R CMD BATCH ", modelType,"_", level, "_start_", start, ".R", sep=""),
          sep="\n")  )
        
        write.table(batchFile, paste(DIREC,  modelType,"_", level, "_start_", start, ".txt",sep=""), row.name=F, col.names=F, quote=F)
      }
      
    }
  }
}






######one time point treatment ######################

for(sampleSize in c(200, 1000)){
  for(start in seq(1, 500, 500)){
    for (level in c("high", "moderate", "low" ) ) {
      for(modelType in c("trueOutcomePred","trueConf", "truePropen", "allPot") ) {
        
        #DIREC="M:/Private/varSelection/case4/"
        DIREC=paste0("/home/tkzhou/PSPP_Project/PENCOMP_OneTimePoint/variableSelection5/case2/sampleSize", sampleSize, "/")
        
        #############################################################################  
        batchFile=NULL
        batchFile=c(batchFile, paste(
          
          "#!/bin/sh",
          "#SBATCH --mail-type=ALL",
          "#SBATCH --mail-user=tkzhou@umich.edu",
          "#SBATCH --time=5-10:00:00",
          "",
          
          paste("#SBATCH --workdir=", DIREC, sep=""),
          paste("#SBATCH --job-name=varSelsample", sampleSize,"_", modelType, sep=""),
          
          paste("R CMD BATCH ", modelType,"_", level, "_start_", start, ".R", sep=""),
          sep="\n")  )
        
        write.table(batchFile, paste(DIREC,  modelType,"_", level, "_start_", start, ".txt",sep=""), row.name=F, col.names=F, quote=F)
      }
      
    }
  }
}
