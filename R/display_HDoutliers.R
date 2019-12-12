#' Display outliers with a scatterplot
#'
#' @description Provide a 2D scatterplot of data for visual exploration.
#' For data with more than two dimensions, two dimensional scatterplot is produced
#' using the first two pricipal components.
#' @param data A vector, matrix, or data frame consisting of numerical variables.
#' @param out A list containing output values produced by \code{\link[stray]{find_HDoutliers}}
#' @importFrom colorspace rainbow_hcl
#' @importFrom pcaPP PCAproj
#' @import ggplot2
#' @return A ggplot object of data space with detected outliers (if any).
#' @export
#' @examples
#' data <- c(rnorm(100), 7, 7.5, rnorm(100, 20), 45)
#' output <- find_HDoutliers(data, knnsearchtype = "kd_tree")
#' display_HDoutliers(data, out = output)
#'
#'
#' data <- rbind(matrix(rnorm(96), ncol = 2), c(10,12),c(3,7))
#' output <- find_HDoutliers(data, knnsearchtype = "brute")
#' display_HDoutliers(data, out = output)
#'
#'
#' data <- rbind(matrix(rnorm(144), ncol = 3), c(10,12,10),c(3,7,10))
#' output <- find_HDoutliers(data, knnsearchtype = "brute")
#' display_HDoutliers(data, out = output)
#'
display_HDoutliers <- function(data, out) {
  data <- as.data.frame(data)
  d <- ncol(data)
  n <- nrow(data)
  data$outcon <- out$type

  if (d == 1) {
    data$index <- rep(0, n)
    out_display <- ggplot(data) +
      geom_point(aes_string(x = data[, 1], y = data[, 3], colour = "outcon", shape = "outcon")) +
      scale_colour_manual(name = "Type", values = c("outlier" = "red", "typical" = "black")) +
      scale_shape_manual(name = "Type", values = c("outlier" = 17, "typical" = 20)) +
      xlab("Value") +
      ylab("") +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
    out_display
  } else if (d == 2) {
    out_display <- ggplot(data) +
      geom_point(aes_string(x = data[, 1], y = data[, 2], colour = "outcon")) +
      scale_colour_manual(name = "Type", values = c("outlier" = "red", "typical" = "black")) +
      xlab("Variable 1") +
      ylab("Variable 2") +
      theme(aspect.ratio = 1)
    out_display
  } else if (d > 2)
  {
    rpc <- pcaPP::PCAproj(data, k = 2, scale = sd, center = mean)
    data$PC1 <- rpc$scores[, 1]
    data$PC2 <- rpc$scores[, 2]
    out_display <- ggplot(data) +
      geom_point(aes_string(x = "PC1", y = "PC2", colour = "outcon")) +
      scale_colour_manual(name = "Type", values = c("outlier" = "red", "typical" = "black")) +
      xlab("PC 1") +
      ylab("PC 2") +
      theme(aspect.ratio = 1)
    out_display

  }
}
