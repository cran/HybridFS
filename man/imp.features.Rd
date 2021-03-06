% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/imp.features.R
\name{imp.features}
\alias{imp.features}
\title{Displaying the Selected Features}
\usage{
imp.features(FS)
}
\arguments{
\item{FS}{FS is the final list returned from HybridFS function.It contains a data frame of the selected features with the relative rank and the optimal model performance.}
}
\description{
Displays the selected set of features generated via the HybridFS function
}
\details{
Displays the final selected variables with relative rank and an additional plot of Top 10 significant variables.Continuous features selected are returned as binned variables (e.g. average_volume is returned as average_volume.binned).To retrieve the transformed dataset,use FinalBinnedData function
}
\examples{
FS=HybridFS(input.df=validation,target.var.name="Survived")
imp.features(FS)
}
