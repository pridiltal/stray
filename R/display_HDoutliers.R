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
  n <- nrow(data)
  outcon <- as.factor(ifelse(1:n %in% outliers,
                             "outlier", "non_outlier"))
  data <-dplyr::mutate(data, outcon)
  if(d==1) {
    data <-dplyr::mutate(data, index = rep(0, n))
    colnames(data) <- c("value", "outcon", "index")
    out_display <- ggplot(data, aes_string(x = "value", y= "index",
                                           colour = "outcon")) +
     geom_point() +
     scale_colour_manual(name = "Type",
                         values = c("outlier" = "red",
                                    "non_outlier"= "black"))
    print(out_display)
  } else if (d==2){
    print("two dimensional plot")
  } else {
    print("Have a tour")
  }
}
