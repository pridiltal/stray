#' Display outliers with a scatterplot
#'
#' Animate a 2D tour path with a scatterplot to visualize outliers.
#'
#' @param data A vector, matrix, or data frame consisting of numeric
#' and/or categorical variables.
#' @param outliers A vector of indexes of the observations determined to be
#' outliers by \code{\link[stray]{find_HDoutliers}}
#' @importFrom colorspace rainbow_hcl
#' @importFrom dplyr mutate
#' @importFrom animation saveGIF
#' @importFrom dplyr as_tibble
#' @importFrom ggplot2  ggplot geom_point scale_colour_manual xlab ylab theme
#' @importFrom tourr animate_xy
#' @export
#' @examples
#' \dontrun{
#' require(tourr)
#' outpoints <- matrix(rnorm(12, mean=200), nrow = 2)
#' colnames(outpoints) <- colnames(flea[,-7])
#' data <- rbind(flea[,-7], outpoints)
#' outliers <- find_HDoutliers(data)
#' display_HDoutliers(data, outliers)}
display_HDoutliers <- function(data, outliers) {
  data <- as.data.frame(data)
  d <- ncol(data)
  n <- nrow(data)
  outcon <- as.factor(ifelse(1:n %in% outliers,
                             "outlier", "non_outlier"))
  data <-dplyr::mutate(data, outcon)
  if(d == 1) {
    data <-dplyr::mutate(data, index = rep(0, n))
    out_display <- ggplot(data) +
     geom_point(aes_string(x = data[,1], y= data[,3], colour = data[,2])) +
     scale_colour_manual(name = "Type", values = c("outlier" = "red", "non_outlier"= "black"))+
     xlab("Value") +
     ylab("")+
     theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())
    out_display
  } else if (d == 2) {
    out_display <- ggplot(data) +
      geom_point(aes_string(x = data[,1], y= data[,2], colour = "outcon")) +
      scale_colour_manual(name = "Type", values = c("outlier" = "red", "non_outlier"= "black"))+
      xlab("Variable 1") +
      ylab("Variable 2")
    out_display
  } else {
    col <- ifelse(1:n %in% outliers, "red", "black")
    tourr::animate_xy(data[,-(d+1)], col=col, pch = 20)
  }
}
