#' Displaying the Selected Features
#'
#' Displays the selected set of features generated via the HybridFS function
#' @param FS FS is the final list returned from HybridFS function.It contains a data frame of the selected features with the relative rank and the optimal model performance.
#' @details Displays the final selected variables with relative rank and an additional plot of Top 10 significant variables.Continuous features selected are returned as binned variables (e.g. average_volume is returned as average_volume.binned).To retrieve the transformed dataset,use FinalBinnedData function
#' @export
#' @examples
#' FS=HybridFS(input.df=validation,target.var.name="Survived")
#' imp.features(FS)




imp.features <- function(FS){
  a=FS[[1]]
  c=a[,c("VARS","Rank")]
  graphics::par(mai=c(1,3,1,1))
  b=subset(a[,c("VARS","VariableImportance")],a$Rank<=10)
  graphics::barplot(b$VariableImportance,main="Top 10 Variables", horiz=TRUE,xlab="VariableImportance",names.arg=b$VARS,las=1,col=grDevices::rainbow(10))
  print(c,row.names=FALSE)
}
