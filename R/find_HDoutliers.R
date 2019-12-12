#' Detect Anomalies in High Dimensional Data.
#'
#' @description  Detect anomalies in high dimensional data. This is a modification of
#' \code{\link[HDoutliers]{HDoutliers}}.
#' @param data A vector, matrix, or data frame consisting of numerical variables.
#' @param alpha Threshold for determining the cutoff for outliers. Observations are considered
#'  outliers if they fall in the \eqn{(1- alpha)} tail of the distribution of the nearest-neighbor
#'  distances between exemplars.
#' @param k Number of neighbours considered.
#' @param knnsearchtype A character vector indicating the search type for k- nearest-neighbors.
#' @param normalize Method to normalize the columns of the data. This prevents variables with large variances
#'  having disproportional influence on Euclidean distances. Two options are available "standardize" or "unitize".
#'  Default is set to "unitize"
#' @return The indexes of the observations determined to be outliers.
#' @export
#' @import stats
#' @references {Wilkinson, L. (2018), `Visualizing big data
#' outliers through distributed aggregation', IEEE
#' transactions on visualization and computer graphics 24(1), 256-266.}
#' @examples
#' require(ggplot2)
#' set.seed(1234)
#' data <- c(rnorm(1000, mean = -6), 0, rnorm(1000, mean = 6))
#' outliers <- find_HDoutliers(data, knnsearchtype = "kd_tree")
#'
#'
#'
#' set.seed(1234)
#' n <- 1000 # number of observations
#' nout <- 10 # number of outliers
#' typical_data <- matrix(rnorm(2 * n), ncol = 2, byrow = TRUE)
#' out <- matrix(5 * runif(2 * nout, min = -5, max = 5), ncol = 2, byrow = TRUE)
#' data <- rbind(out, typical_data)
#' outliers <- find_HDoutliers(data, knnsearchtype = "brute")
#'
find_HDoutliers <- function(data, alpha = 0.01, k = 10,
                            knnsearchtype = "brute",
                            normalize = "unitize") {
  data <- as.matrix(data)
  r <- nrow(data)
  data[is.infinite(data)] <- NA
  naomit_data <- na.omit(data)
  na_act <- na.action(naomit_data)
  if (is.null(na_act)) {
    tag <- 1:r
  }
  else {
    tag <- (1:r)[-na_act]
  }

  unitize <- function(z) {
    zrange <- range(z)
    if (!(dif <- diff(zrange))) {
      return(rep(0, length(z)))
    }
    (z - zrange[1]) / dif
  }

  standardize <- function(z) {
    (z - stats::median(z)) / stats::IQR(z)
  }

  data <- apply(as.matrix(naomit_data), 2, normalize)
  out <- use_KNN(data, alpha, k = k, knnsearchtype = knnsearchtype)
  outliers <- tag[out$outliers]
  type <- as.factor(ifelse(1:r %in% outliers,
    "outlier", "typical"
  ))
  return(list(outliers = outliers, out_scores = out$out_scores, type = type))
}


#' Find outliers using kNN distance with maximum gap
#'
#' @param data A vector, matrix, or data frame consisting of numeric and/or categorical variables.
#' @param alpha Threshold for determining the cutoff for outliers. Observations are considered
#'  outliers outliers if they fall in the \eqn{(1- alpha)} tail of the distribution of the nearest-neighbor
#'  distances between exemplars.
#' @param k Number of neighbours considered.
#' @param knnsearchtype A character vector indicating the search type for k- nearest-neighbors.
#' @return The indexes of the observations determined to be outliers and the outlying scores
#' @export
#' @importFrom FNN knn.dist
use_KNN <- function(data, alpha = 0.01, k = 10,
                    knnsearchtype = c("kd_tree", "brute")) {

  # k <- ceiling(length(exemplars) / 20)
  if (k == 1) {
    d <- as.vector(FNN::knn.dist(data, 1))
  } else {
    d_knn <- FNN::knn.dist(data, k, algorithm = knnsearchtype)
    d_knn1 <- cbind(rep(0, nrow(d_knn)), d_knn)
    diff <- t(apply(d_knn1, 1, diff))
    max_diff <- apply(diff, 1, which.max)
    d <- d_knn[cbind(1:nrow(d_knn), max_diff)]
  }

  out_index <- find_theshold(d, alpha = 0.05, outtail = "max")
  return(list(outliers = out_index, out_scores = d))
}
