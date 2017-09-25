#' Displaying the Optimal Model Performance
#'
#' Displays the performance metrics of the optimal model generated via the HybridFS function
#' @param FS FS is the final list returned from HybridFS function.It contains a data frame of the selected features with the relative rank and the optimal model performance.
#' @details Displays the performance metrics of the optimal model returned from the HybridFS function such as F1 Score, Accuracy, Precision and Recall.Performance metrics of the Validation dataset is also displayed to understand model stability
#' @export
#' @examples
#' FS=HybridFS(input.df=validation,target.var.name="Survived")
#' model.perf(FS)




model.perf <- function(FS){
  a=FS[[2]]
  print(a,row.names=FALSE)
}
