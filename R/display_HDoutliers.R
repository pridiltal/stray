#' Display outliers with a scatterplot
#'
#' Animate a 2D tour path with a scatterplot to visualize outliers.
#'
#' @param data A vector, matrix, or data frame consisting of numeric
#' and/or categorical variables.
#' @param outliers A vector of indexes of the observations determined to be
#' outliers by \code{\link[stray]{find_HDoutliers}}
#' @param col color to be plotted.  Defaults to "black"
#' @param pch size of the point to be plotted.  Defaults to 20.
#' @import tourr
#' @export
display_HDoutliers <- function(data, outliers, col, pch) {
  d <- ncol(as.matrix(data))
  if(d==1) {
    print("one dimensional plot")
  } else if (d==2){
    print("two dimensional plot")
  } else {
    print("Have a tour")
  }
}
