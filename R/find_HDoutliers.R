#' Detect Anomalies in High Dimensional Data.
#'
#' @description  Detect anomalies in high dimensional data. This is a modification of
#' \code{\link[HDoutliers]{HDoutliers}}.
#' @param data A vector, matrix, or data frame consisting of numeric and/or categorical variables.
#' @param alpha Threshold for determining the cutoff for outliers. Observations are considered
#'  outliers if they fall in the \eqn{(1- alpha)} tail of the distribution of the nearest-neighbor
#'  distances between exemplars.
#' @param method Outlier detection method used for detecting outlier in the high dimensional space.
#' @param knnsearchtype A character vector indicating the search type for k- nearest-neighbors.
#' @param normalize Method to normalize the columns of the data. This prevents variables with large variances
#'  having disproportional influence on Euclidean distances. Two options are available "standardize" or "unitize".
#'  Default is set to "unitize"
#' @return The indexes of the observations determined to be outliers.
#' @seealso \code{\link{check_duplicates}}
#' @export
#' @import stats
#' @references {Wilkinson, L. (2018), `Visualizing big data
#' outliers through distributed aggregation', IEEE
#' transactions on visualization and computer graphics 24(1), 256-266.}
#' @examples
#' require(ggplot2)
#' set.seed(1234)
#' data <- c(rnorm(1000, mean = -6), 0, rnorm(1000, mean = 6))
#' outliers <- find_HDoutliers(data, method = "knn_maxdiff", knnsearchtype = "FNN_auto")
#' display_HDoutliers(data, outliers)
#'
#'
#' set.seed(1234)
#' n <- 1000 # number of observations
#' nout <- 10 # number of outliers
#' typical_data <- tibble::as.tibble(matrix(rnorm(2 * n), ncol = 2, byrow = TRUE))
#' out <- tibble::as.tibble(matrix(5 * runif(2 * nout, min = -5, max = 5), ncol = 2, byrow = TRUE))
#' data <- rbind(out, typical_data)
#' outliers <- find_HDoutliers(data, method = "knn_maxdiff", knnsearchtype = "FNN_auto")
#' display_HDoutliers(data, outliers)
find_HDoutliers <- function(data, alpha = 0.01,
                            method = c("knn_maxdiff", "knn_sum", "hdr"),
                            knnsearchtype = c("FNN_auto", "FNN_brute", "nabor_brute"),
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

  if (method %in% c("knn_maxdiff", "knn_sum")) {
    out <- use_KNN(data, alpha, method = method, knnsearchtype = knnsearchtype)
  }
  if (method == "hdr") {
    out <- hdr_outliers(data)
  }

  outliers <- tag[out$outliers]

  type <- as.factor(ifelse(1:r %in% outliers,
    "outlier", "typical"
  ))
  return(list(outliers = outliers, out_scores = out$out_scores, type = type))
}

#' Check duplicates
#'
#' @description  Check duplicates by giving a one-to-one mapping from unique observations to rows of a data matrix
#' @param data A vector, matrix, or data frame consisting of numeric and/or categorical variables.
#' @return  A list in which each component is a vector of
#' observation indexes. The first index in each list is the
#' index of the exemplar defining that list, and any remaining indexes are the associated members, within radius of the exemplar.
#' @export
#' @importFrom mclust partuniq
#' @importFrom FNN get.knnx
check_duplicates <- function(data) {
  n <- nrow(data)
  cl <- mclust::partuniq(data)
  U <- unique(cl)
  m <- length(U)
  if (m != n) {
    members <- rep(list(NULL), m)
    j <- 0
    for (u in U) {
      j <- j + 1
      members[[j]] <- which(cl == u)
    }
  }
  else {
    members <- as.list(1:n)
  }

  members <- members[!sapply(members, is.null)]
  exemplars <- sapply(members, function(x) x[[1]])
  names(members) <- exemplars
  members
}

#' Advanced HDoutliers
#'
#' @param data A vector, matrix, or data frame consisting of numeric and/or categorical variables.
#' @param alpha Threshold for determining the cutoff for outliers. Observations are considered
#'  outliers outliers if they fall in the \eqn{(1- alpha)} tail of the distribution of the nearest-neighbor
#'  distances between exemplars.
#' @param k Number of neighbours considered.
#' @param method Outlier detection method used for detecting outlier in the high dimensional space.
#' @param knnsearchtype A character vector indicating the search type for k- nearest-neighbors.
#' @return The indexes of the observations determined to be outliers.
#' @export
#' @importFrom HDoutliers getHDmembers
#' @importFrom FNN knn.dist
#' @importFrom nabor knn
use_KNN <- function(data, alpha = 0.01, k = 10, method = c("knn_maxdiff", "knn_sum", "hdr"),
                    knnsearchtype = c("FNN_auto", "FNN_brute", "nabor_brute")) {

  # k <- ceiling(length(exemplars) / 20)
  if (k == 1) {
    d <- as.vector(FNN::knn.dist(data, 1))
  } else {
    if (knnsearchtype == "FNN_auto") {
      d_knn <- FNN::knn.dist(data, k, algorithm = "kd_tree")
    }
    if (knnsearchtype == "FNN_brute") {
      d_knn <- FNN::knn.dist(data, k, algorithm = "brute")
    }
    if (knnsearchtype == "nabor_brute") {
      kdist <- nabor::knn(data, k = k + 1, searchtype = "brute")
      d_knn <- kdist$nn.dists[, -1]
    }

    if (method == "knn_maxdiff") {
      d_knn1 <- cbind(rep(0, nrow(d_knn)), d_knn)
      diff <- t(apply(d_knn1, 1, diff))
      max_diff <- apply(diff, 1, which.max)
      d <- d_knn[cbind(1:nrow(d_knn), max_diff)]
    }
    if (method == "knn_sum") {
      d <- t(apply(d_knn, 1, sum))
    }
  }

  out_index <- find_theshold(d, alpha = 0.05, outtail = "max")
  return(list(outliers = out_index, out_scores = d))
}

#' Find anomalies using high density regions of the irst two principal components of the high dimensional data set
#'
#' @param data A vector, matrix, or data frame consisting of numeric and/or categorical variables.
#' @return The indexes of the observations determined to be outliers.
#' @export
#' @importFrom pcaPP PCAproj
#' @importFrom ks kde
#' @importFrom hdrcde hdr.2d
hdr_outliers <- function(data) {
  nvar <- ncol(data)
  if (nvar == 1) {
    d <- ks::kde(data, eval.points = data)
    outlier_score <- d$estimate
  }

  if (nvar > 1) {
    rbt.pca <- pcaPP::PCAproj(data,
      k = 2, center = mean,
      scale = sd
    )
    scores <- rbt.pca$scores[, 1:2]
    hdrinfo <- hdrcde::hdr.2d(x = scores[, 1], y = scores[
      ,
      2
    ], kde.package = "ks")
    outlier_score <- hdrinfo$fxy
  }


  out <- find_theshold(outlier_score, alpha = 0.05, outtail = "min")
  return(out)
}
