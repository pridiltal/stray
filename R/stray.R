#' stray: A package for robust anomaly detection in data streams with concept drift
#'
#' This package is a modification of \code{\link[HDoutliers]{HDoutliers}} package. \code{HDoutliers} is a powerful algorithm for the
#' detection of anomalous observations in a dataset, which has (among other advantages) the ability to detect
#' clusters of outliers in multi-dimensional data without requiring a model of the typical behavior of the system.
#' However, it suffers from some limitations that affect its accuracy. In this package, we propose solutions to
#' the limitations of HDoutliers, and propose an extension of the algorithm to deal with data streams that exhibit
#' non-stationary behavior. The results show that our proposed algorithm improves the accuracy, and enables the
#' trade-off between false positives and negatives to be better balanced.
#'
#' @seealso The core functions in this package:  \code{\link{find_HDoutliers}},
#' \code{\link{display_HDoutliers}}
#'
#' Full documentation and demos:
#' @references
#' Talagala, P. D., Hyndman, R. J., & Smith-Miles, K. (2019). Anomaly Detection in High Dimensional Data.
#'  \url{https://www.monash.edu/business/ebs/research/publications/ebs/wp20-2019.pdf}
#'
#' Wilkinson, L. (2017). Visualizing big data outliers through distributed
#' aggregation. IEEE transactions on visualization and computer graphics, 24(1), 256-266.
#' \url{https://www.cs.uic.edu/~wilkinson/Publications/outliers.pdf}
#' @note The name \code{stray} comes from \code{Search and TRace AnomalY}
#' @docType package
#' @name  stray
NULL
