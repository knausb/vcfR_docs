




INFO2df <- function(x){
  # Isolate INFO from meta
  INFO <- x@meta[grep("##INFO=", x@meta)]
  
  # Clean things up a bit.
  INFO <- sub("##INFO=<", "", INFO)
  INFO <- sub(">$", "", INFO)
  INFO <- sub('\"', "", INFO)
  INFO <- sub('\"$', "", INFO)
  
  INFO <- strsplit(INFO, split = ",")
  
  ID     <- unlist( lapply(INFO, function(x){ grep("^ID=", x, value=TRUE) }) )
  Type   <- unlist( lapply(INFO, function(x){ grep("^Type=", x, value=TRUE) }) )
  Number <- unlist( lapply(INFO, function(x){ grep("^Number=", x, value=TRUE) }) )
  
  ID     <- unlist(lapply(strsplit(ID, split = "=" ), function(x){x[2]}))
  Type   <- unlist(lapply(strsplit(Type, split = "=" ), function(x){x[2]}))
  Number <- unlist(lapply(strsplit(Number, split = "=" ), function(x){x[2]}))
    
  INFO.Type <- cbind(ID, Type, Number)
  
  # Initialize a data.frame for the INFO data
  INFOdf <- data.frame( matrix( nrow=nrow(x@fix),  ncol=nrow(INFO.Type) ) )
  names(INFOdf) <- INFO.Type[,'ID']
  
  for( i in 1:nrow(INFO.Type) ){
    tmp <- extract.info(x, element = INFO.Type[,'ID'][i])
    if( INFO.Type[,'Type'][i] == "Integer" & INFO.Type[,'Number'][i] == "1" ){
      tmp <- as.integer( tmp )
    }
    if( INFO.Type[,'Type'][i] == "Float" & INFO.Type[,'Number'][i] == "1" ){
      tmp <- as.numeric( tmp )
    }
    INFOdf[,INFO.Type[,'ID'][i]] <- tmp
  }
  return(INFOdf)
}



data(vcfR_example)
x<-vcf

tmp <- INFO2df(vcf)

head(tmp)

data(vcfR_test)
INFO2df(vcfR_test)

