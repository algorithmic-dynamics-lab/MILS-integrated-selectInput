require(readr)


getKValues <- function(alphabetSize) {
  
  name <- paste0("K", as.character(alphabetSize))
  
  path <- paste0("./data/", name, ".rds")
  
  kData <- as.data.frame(read_rds(path = path))
  
  kData$S <- NULL
  
  return(kData)
  
}


maxKnownKs <- read.csv("data/maxKnownKs.csv")
# erase useless column
maxKnownKs$X <- NULL

# count the number of symbols in a string
countSymbols <- function(string) {
  return(length(table(strsplit(string, NULL))))
}


splitString <- function(string, blockSize, offset){
  
  if(blockSize > nchar(string)){
    return (string)
  }
  
  if(offset > blockSize){
    return ("ERROR: offset cannot be greater than blockSize.")
  }
  
  subs <- character()
  startIndices <- seq(1, nchar(string), offset)
  
  for(i in startIndices){
    
    first <- i
    
    last <- -1
    
    if (last > nchar(string)){
      last <- nchar(string) -1
    } else{
      last <- i + blockSize -1  
    }
    
    sub <- substr(string, first, last)
    subs <- append(subs, sub)
    
    lastStep = FALSE
    if (nchar(sub) == blockSize && last == nchar(string)){
      lastStep = TRUE
    }
    if(lastStep)break
  }
  return (subs)
}


#receives the already splitted vector of input strings
getBDMFromSplittedString <- function (stringsVector, kValues, base) {
  
  stringCounts <- as.data.frame(table(stringsVector))

  stringCounts$stringsVector <- as.character(stringCounts$stringsVector)
    
  stringCounts["ks"] <- kValues[stringCounts$stringsVector, ]
  
  naIndices <- as.integer(which(is.na(stringCounts$ks)))
  
  naStrings <- as.vector(stringCounts$stringsVector[naIndices])
  
  naLengths <- unlist(lapply(naStrings, nchar))
  
  #more complex (+1) than the highest known values 
  naKs <- maxKnownKs[, paste0("K.", toString(base))] + 1
    
  stringCounts[is.na(stringCounts)] <- naKs
  
  bdm <- sum(log2(stringCounts$Freq)) + sum(stringCounts$ks)
  
  return(bdm) 
  
}

# get BDM given a string
evaluateBDM1D <- function(string, blockSize, offset, base, kValues){
  
  splittedString <- splitString(string, blockSize, offset)
  
  #normalize splitted strings
  normalizedString <- unlist(lapply(splittedString, normalize_string))
  
  bdm <- getBDMFromSplittedString(stringsVector = normalizedString, kValues = kValues, base = 2 )
  
  return(bdm)
}


normalize_string <- function (string) {
  
    splitted <- strsplit(string, "")
    elements <- lapply(splitted, unique)
    if (any(vapply(elements, length, 0) > 10)) 
      stop("two many symbols (more than 10)")
  
    exchanged <- mapply(function(x, y) seq(0, length.out = length(x))[match(y, x)], 
                        elements, splitted, SIMPLIFY = FALSE)
    
    return(vapply(exchanged, paste, "", collapse = ""))
  
}






