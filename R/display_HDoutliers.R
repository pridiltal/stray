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
#' @importFrom dplyr mutate
#' @import ggplot2
#' @import tourr
#' @export
display_HDoutliers <- function(data, outliers, col, pch) {
  data <- as.data.frame(data)
  d <- ncol(data)
  n <- nrow(data)
  outcon <- as.factor(ifelse(1:n %in% outliers,
                             "outlier", "non_outlier"))
  data <-dplyr::mutate(data, outcon)
  if(d==1) {
    data <-dplyr::mutate(data, index = rep(0, n))
    out_display <- ggplot(data) +
     geom_point(aes_string(x = data[,1], y= data[,3], colour = data[,2])) +
     scale_colour_manual(name = "Type",
                         values = c("outlier" = "red",
                                    "non_outlier"= "black"))+
      xlab("Value") +
      ylab("")+
      theme(axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
    print(out_display)
  } else if (d==2){
    out_display <- ggplot(data) +
      geom_point(aes_string(x = data[,1], y= data[,2], colour = "outcon")) +
      scale_colour_manual(name = "Type",
                          values = c("outlier" = "red",
                                     "non_outlier"= "black"))+
      xlab("Variable 1") +
      ylab("Variable 2")

    print(out_display)
  } else {
    print("Have a tour")
  }
}
