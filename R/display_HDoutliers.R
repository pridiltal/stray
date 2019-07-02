#' Display outliers with a scatterplot
#'
#' Animate a 2D tour path with a scatterplot to visualize outliers.
#'
#' @param data A vector, matrix, or data frame consisting of numeric
#' and/or categorical variables.
#' @param out A list containing output values produced by \code{\link[stray]{find_HDoutliers}}
#' @param alpha Alpha level of the points
#' @importFrom colorspace rainbow_hcl
#' @importFrom animation saveGIF
#' @import ggplot2
#' @importFrom tourr animate_xy
#' @export
#' @examples
#' data <-c( rnorm(100), 7,7.5, rnorm(100,20), 45)
#' out <- find_HDoutliers(data, method = "knn_maxdiff")
#' display_HDoutliers(data, out = out, alpha = out$out_scores )
#'
#' \dontrun{
#' require(tourr)
#' outpoints <- matrix(rnorm(12, mean=200), nrow = 2)
#' colnames(outpoints) <- colnames(flea[,-7])
#' data <- rbind(flea[,-7], outpoints)
#' outliers <- find_HDoutliers(data)
#' display_HDoutliers(data, outliers)}
display_HDoutliers <- function(data, out, alpha = 1) {
  data <- as.data.frame(data)
  d <- ncol(data)
  n <- nrow(data)
  data$outcon <- out$type

  if(d == 1) {
    data$index <- rep(0, n)
    out_display <- ggplot(data) +
     geom_point(aes_string(x = data[,1], y= data[,3], colour = "outcon", shape = "outcon",  alpha = alpha) ) +
     scale_colour_manual(name = "Type", values = c("outlier" = "red", "typical"= "black"))+
     scale_shape_manual(name = "Type", values = c("outlier" = 17, "typical"= 20))+
     xlab("Value") +
     ylab("")+
     theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())
    out_display
  } else if (d == 2) {
    out_display <- ggplot(data) +
      geom_point(aes_string(x = data[,1], y= data[,2], colour = "outcon", alpha = alpha)) +
      scale_colour_manual(name = "Type", values = c("outlier" = "red", "typical"= "black"))+
      xlab("Variable 1") +
      ylab("Variable 2")
    out_display
  } else {
    col <- ifelse(1:n %in% out$outliers, "red", "black")
    tourr::animate_xy(data[,-(d+1)], col=col, pch = 20)
  }
}
