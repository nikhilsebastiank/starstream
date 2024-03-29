#' Merges all the responses from the UK Music Creators Dataset
#' @return The data set with merged responses
#' @importFrom stats na.omit
#' @importFrom utils View
sortresponse = function(){
  rawData = readxl::read_xlsx("/Users/nikhilsebastian/Dropbox/Projects/MasterThesis/data/UK_Creators__Earnings_Survey_Raw_Data.xlsx")
  colnames(rawData) = rawData[1,] # Changes the column names based on the answer number
  rawData = rawData[-1,] # Removes the first row (corresponds to column names)
  rawData[is.na(rawData)] = "n" # missing values coded as ns

  # Vectors of column names
  namesA = colnames(rawData[1:40])
  namesA = append(namesA, colnames(rawData[194:222]))
  namesA = sort(namesA)
  namesB = colnames(rawData[41:95])
  namesB = sort(namesB)
  namesC = colnames(rawData[96:135])
  namesC = sort(namesC)
  namesD = colnames(rawData[136:159])
  namesD = sort(namesD)
  namesE = colnames(rawData[160:193])
  namesE = sort(namesE)
  namesF = colnames(rawData[223:228])
  namesF = sort(namesF)
  namesQ = namesB[53:55]
  namesB = namesB[-(53:55)]
  namesQ = sort(namesQ)
  rawData2 = rawData[,c(namesA,namesB,namesC,namesD,namesE,namesF,namesQ)]

  undervector = c()
  lettervector = c()
  for (i in colnames(rawData2)){
    d = substring(i,1,3)
    if (substring(d,3,3) == "_"){
      undervector = append(undervector,d)
    }
    else {
      lettervector = append(lettervector,d)
    }
  }
  for (i in lettervector){
    c = stringr::str_extract(colnames(rawData2), stringr::str_c("(",i,").","*"))
    c = as.character(na.omit(c))
    if (length(c) > 1){
      rawData2 = mergecol(rawData2,i)
    }
  }
  for (i in undervector){
    c = stringr::str_extract(colnames(rawData2), stringr::str_c("(",i,").","*"))
    c = as.character(na.omit(c))
    if (length(c) > 1){
      rawData2 = mergecol(rawData2,i)
    }
  }
  rawData2
  saveRDS(rawData2, file =  "/Users/nikhilsebastian/Dropbox/Projects/MasterThesis/results/data/sortedCreators.Rds")
  sortedCreators = readRDS( file = "/Users/nikhilsebastian/Dropbox/Projects/MasterThesis/results/data/sortedCreators.Rds")
  return(sortedCreators)
}
