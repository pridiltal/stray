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
#' @importFrom colorspace rainbow_hcl
#' @importFrom tibble as.tibble
#' @importFrom dplyr mutate
#' @import tourr
#' @export
display_HDoutliers <- function(data, outliers, col, pch) {
  data <- tibble::as.tibble(data)
  d <- ncol(data)
  outcon <- as.factor(ifelse(1:nrow(data) %in% outliers,
                             "outlier", "non_outlier"))
  data <-dplyr::mutate(data, outcon)
  if(d==1) {

    pal <- colorspace::rainbow_hcl(length(levels(data$outcon)))
    col <- pal[as.numeric(data$outcon)]
    print("one dimensional plot")
  } else if (d==2){
    print("two dimensional plot")
  } else {
    print("Have a tour")
  }
}
