#' Detect Anomalies in High Dimensional Data.
#'
#' @description  Detect anomalies in high dimensional data. This is a modification of
#' \code{\link[HDoutliers]{HDoutliers}}.
#' @param data A vector, matrix, or data frame consisting of numeric and/or categorical variables.
#' @param maxrows If the number of observations is greater than \code{maxrows}, \code{outliers} reduces the
#'  number used in k-nearest-neighbor computations to a set of \emph{exemplars}. The default value is 10000.
#' @param radius Threshold for determining membership in the \emph{exemplars}'s lists (used only when the
#' number of observations is greater than \eqn{maxrows}). An observation is added to an \emph{exemplars}'s lists
#' if its distance to that \emph{exemplar} is less than \code{radius}. The default value is \eqn{.1/(log n)^(1/p)},
#'  where \eqn{n} is the number of observations and \eqn{p} is the dimension of the data.
#' @param alpha Threshold for determining the cutoff for outliers. Observations are considered
#'  outliers outliers if they fall in the \eqn{(1- alpha)} tail of the distribution of the nearest-neighbor
#'  distances between exemplars.
#' @param l A user defined parameter to obtain a trade off between the false positives and the false negatives.
#' @return The indexes of the observations determined to be outliers.
#' @details If the number of observations exceeds \code{maxrows}, the data is first partitioned into lists
#' associated with \emph{exemplars} and their \emph{members} within \code{radius} of each \emph{exemplar}, to
#' reduce the number of k-nearest neighbor computations required for outlier detection.
#' @seealso \code{\link[HDoutliers]{getHDmembers}},  \code{\link{get_outliers}}
#' @export
#' @importFrom HDoutliers getHDmembers
#' @importFrom FactoMineR MCA
#' @importFrom gridExtra grid.arrange
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#' @import ggplot2
#' @import stats
#' @references {Wilkinson, L. (2016). Visualizing outliers.}
#' @examples
#' require(ggplot2)
#' set.seed(1234)
#' data <- c(rnorm(1000, mean = -6), 0, rnorm(1000, mean = 6))
#' df <- tibble::tibble( index = rep(0, length(data)), data = data)
#' data_plot <- ggplot(df, aes(x = data, y= index)) +
#' geom_point() +
#' xlab("x") +
#' ylab("") +
#' ggtitle("Original Data")
#' data_out <- find_HDoutliers(data)
#' output_plot  <- data_plot +
#' geom_point(data = df[data_out, ], aes(x=data, y = index),
#' colour = "red", size = 3) +
#' xlab("x") +
#' ylab("") +
#' ggtitle("Output")
#' gridExtra::grid.arrange(data_plot, output_plot )
#'
#'
#' set.seed(1234)
#' n <- 1000 # number of observations
#' nout <- 10 # number of outliers
#' typical_data <- tibble::as.tibble(matrix(rnorm(2*n), ncol = 2, byrow = TRUE))
#' out <- tibble::as.tibble(matrix(5*runif(2*nout,min=-5,max=5), ncol = 2, byrow = TRUE))
#' data <- dplyr::bind_rows(out, typical_data )
#' data_out <- find_HDoutliers(data)
#' data_plot <- ggplot(data, aes(x=V1, y= V2))+
#' geom_point() +
#' ggtitle("Original Data")+
#' theme(aspect.ratio = 1)
#' output_plot <- data_plot +
#' geom_point(data = data[data_out, ], aes(x=V1, y = V2),
#' colour = "red", size = 3) +
#' ggtitle("Output")
#' gridExtra::grid.arrange(data_plot, output_plot , nrow=1 )

find_HDoutliers <- function(data, maxrows = 10000, radius = NULL, alpha = 0.05, l=0.2){
  # look for categorical variables
  if (is.null(dim(data))) {
    CAT <- !is.numeric(data)
    data <- if (CAT)  as.data.frame(data) else as.matrix(data)
  } else {
    if (inherits(data, "matrix")) data <- as.data.frame(data)
    CAT <- sapply(data, function(x) !is.numeric(x))
  }

  if (any(CAT)) {
    # convert each categorical variable to numeric via
    # multiple correspondence analysis using MCA function from FactoMineR
    #  require(FactoMineR)
    data[,CAT] <- sapply(data[, CAT, drop = F], function(x) FactoMineR::MCA(as.matrix(x), ncp = 1, graph = F)$ind$coord)
  }

  unitize <- function(z) {
    median.z <- stats::median(z)
    iqr.z <- stats::IQR(z)
    if (iqr.z == 0) return(rep(0, length(z)))
    (z - median.z) / iqr.z
  }

  udata <- apply(as.matrix(data), 2, unitize)
  members <- HDoutliers::getHDmembers(udata, radius = radius, maxrows = maxrows)
  get_outliers(udata, members, alpha = alpha, l = l)
}



#' Detect Anomalies Based on the Maximum k-Nearest Neighbour Differences.
#'
#' @description  Detect anomalies based on the maximum k-Nearest Neighbour differences. This is a modification of
#' \code{\link[HDoutliers]{getHDoutliers}}.
#' @param data A vector, matrix, or data frame consisting of numeric and/or categorical variables.
#' @param memberLists A list following the structure of the output to \code{\link[HDoutliers]{getHDmembers}}, in
#' which each component is a vector of observation indexes. The first index in each list is the index of the
#' exemplar representing that list, and any remaining indexes are the associated members, considered
#' 'close to' the exemplar.
#' @param alpha Threshold for determining the cutoff for outliers. Observations are considered outliers
#'  outliers if they fall in the (1- alpha) tail of the distribution of the nearest-neighbor distances
#'  between exemplars.
#' @param l A parameter the user can choose to obtain a trade off between the false positives and
#'  the false negatives.
#' @return  The indexes of the observations determined to be outliers.
#' @export
#' @importFrom FNN knn.dist
#' @seealso \code{\link[HDoutliers]{getHDmembers}},  \code{\link{find_HDoutliers}}
get_outliers <- function(data, memberLists, alpha = 0.05, l=0.2) {
  exemplars <- sapply(memberLists, function(x) x[[1]])
  data <- as.matrix(data)

  k = ceiling(length(exemplars)[1] / 20)
  if (k == 1) {
    d <- FNN::knn.dist(data[exemplars, ], k )
  } else {
    d_1 <- FNN::knn.dist(data[exemplars, ], k )
    d_2 <- apply(d_1, 1, diff)
    if(is.null(dim(d_2))) {
      d_3 <- d_2
    } else {
      d_3 <- apply(d_2, 2, max)
    }
    d <- apply(cbind(d_1[, 1], d_3), 1, max)
  }

  n <- length(d)
  ord <- order(d)
  dmin <- min(d)
  dmax <- max(d)
  gaps <- c(0, diff(d[ord]))
  n4 <- max(min(50, floor(n / 4)), 2)
  J <- 1:(n4 - 1)
  start <- max(floor(n / 2), 1) + 1
  K <- n:1
  std.gaps <- (n4:1) * gaps[(length(gaps) - n4 + 1):length(gaps)]
  start.gap <-stats::quantile(std.gaps, 0.75)
  ghat.2 <- start.gap/(n:1) + l*stats::IQR(std.gaps)
  ghat <- ghat.2
  alpha2 <- 0.05
  logAlpha <- log(1 / alpha)
  start.2 <- max(floor(n * (1 - alpha2)), 1) + 1
  use <- start.2:n
  bound <- Inf
  outlier.condition <- FALSE
  for (i in start.2:n) {
    if (gaps[i] > logAlpha * ghat[i]) {
      bound <- d[ord][i - 1]
      outlier.condition <- TRUE
      break
    }
  }

  if (outlier.condition) {
    pending.outlier.index <- i:length(gaps)
    pending.outlier.vals <- gaps[pending.outlier.index]
    num.pending.outliers <- length(pending.outlier.index)
    spacing.index <- pending.outlier.index[which.max(pending.outlier.vals * (num.pending.outliers:1))]:length(gaps)
    ex1 <- ord[spacing.index]
    ex <- exemplars[ex1]
  } else {
    ex <- exemplars[which(d > bound)]
  }

  mem1 <- sapply(memberLists, function(x) x[1])
  out <- unlist(memberLists[match(ex, mem1)])
  return(out)
}

