#' Intermediate Binned Dataset
#'
#' Retrieves the transformed dataset returned from HybridFS function
#' @param input.df Input data frame that contains the target variable and predictor variables with no missing values. Predictors can be either categorical or continuous.
#' @param target.var.name Name of binary target variable. Target variables should be numeric with only two distinct values (0, 1)
#' @return \item{TransformedData}{A data frame that contains the transformed dataset used in the HybridFS function}
#' @export
#' @examples
#' BinnedData=FinalBinnedData(input.df=validation,target.var.name="Survived")



FinalBinnedData <- function(input.df, target.var.name){
  data<- as.data.frame(input.df)

  names(data)[names(data)==target.var.name] <- "DV"
  options(java.parameters = "-Xmx1g")

  #require(woeBinning)


  #subset all integer variables in dataset
  allIntVarDF <- data[,sapply(data,is.integer)]

  #Int variables with levels less than 12
  intVarsLen <- apply(allIntVarDF,2,function(i) length(unique(i))<=12)
  intvar<-names(intVarsLen)

  #Int variables with more than 12 levels
  intbin_var <- allIntVarDF[,names(intVarsLen[intVarsLen==FALSE])]
  intbin_var2<- names(intbin_var)
  numvars <- names(data[,sapply(data,is.numeric)])
  numbin_var<-setdiff(numvars,intvar)

  #Supervised Binning of variables based of woe
  binning <- woeBinning::woe.binning(data, 'DV', c(numbin_var,intbin_var2))
  tabulate.binning <- woeBinning::woe.binning.table(binning)

  #Adding binned variables to dataset
  TransformedData <- woeBinning::woe.binning.deploy(data, binning)

  return(TransformedData)
}

