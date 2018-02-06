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
#' @return The indexes of the observations determined to be outliers.
#' @details If the number of observations exceeds \code{maxrows}, the data is first partitioned into lists
#' associated with \emph{exemplars} and their \emph{members} within \code{radius} of each \emph{exemplar}, to
#' reduce the number of k-nearest neighbor computations required for outlier detection.
#' @seealso \code{\link[HDoutliers]{getHDmembers}},  \code{\link{get_outliers}}
#' @export
#' @importFrom HDoutliers getHDmembers
#' @importFrom FactoMineR MCA
#' @importFrom gridExtra grid.arrange
#' @importFrom dplyr bind_rows
#' @import tibble
#' @import ggplot2
#' @import stats
#' @references {Wilkinson, L. (2018), `Visualizing big data
#' outliers through distributed aggregation', IEEE
#' transactions on visualization and computer graphics 24(1), 256-266.}
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

find_HDoutliers <- function(data, maxrows = 1000, radius = NULL, alpha = 0.01){
# look for categorical variables
if (is.null(dim(data))) {
  CAT <- !is.numeric(data)
  data <- if (CAT)  tibble::as.tibble(data) else as.matrix(data)
} else {
    if (inherits(data, "matrix")) data <- tibble::as.tibble(data)
    CAT <- sapply(data, function(x) !is.numeric(x))
}

if (any(CAT)) {
  # convert each categorical variable to numeric via
  # multiple correspondence analysis using MCA function from FactoMineR
  #  require(FactoMineR)
   data[,CAT] <- sapply(data[, CAT, drop = F], function(x) FactoMineR::MCA(as.matrix(x), ncp = 1, graph = F)$ind$coord)
}

standardize <- function(z) {
  (z-median(z))/IQR(z)
}

zdata <- apply(as.matrix(data), 2, standardize)
members <- get_leader_cluster(zdata, radius = radius, maxrows = maxrows)
get_outliers(zdata, members, alpha = alpha)
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
#' @param alpha Threshold for determining the cutoff for outliers. Observations are considered
#'  outliers if they fall in the (1- alpha) tail of the distribution of the nearest-neighbor distances
#'  between exemplars.
#' @return  The indexes of the observations determined to be outliers.
#' @export
#' @importFrom FNN knn.dist
#' @importFrom purrr flatten
#' @seealso \code{\link[HDoutliers]{getHDmembers}},  \code{\link{find_HDoutliers}}
get_outliers <- function(data, memberLists, alpha = 0.01) {

  if(length(memberLists)==1){
    out = NULL
  } else{

    break_list<-function(x){
      max <- floor( nrow(data) / 20)
      seq <- seq_along(x)
      split(x, ceiling(seq/max))
    }
    memberLists<- lapply(memberLists, break_list)
    memberLists<- purrr::flatten(memberLists)
    name <- sapply(memberLists, function(x) x[[1]])
    names(memberLists) <- name

    exemplars <- sapply(memberLists, function(x) x[[1]])
    data <- as.matrix(data)

    k <- ceiling(length(exemplars)[1] / 20)

    if(k==1){
      d <- as.vector(FNN::knn.dist(data[exemplars, ], 1 ))
    } else{
      d_knn <- FNN::knn.dist(data[exemplars, ], k )
      d_knn1<-cbind(rep(0, nrow(d_knn)), d_knn)
      diff<- t(apply(d_knn1, 1, diff))
      max_diff <- apply(diff, 1, which.max)
      d<-d_knn[cbind(1:nrow(d_knn), max_diff)]
      #d <- apply(diff, 1, max)
    }
    n <- length(d)
    ord <- order(d)
    gaps <- c(0, diff(d[ord]))
    n4 <- max(min(50, floor(n/4)), 2)
    J <- 2:n4
    start <- max(floor(n/2), 1) + 1
    ghat <- numeric(n)
    for (i in start:n) ghat[i] <- sum((J/(n4-1)) * gaps[i - J+1 ]) # check i - j +1
    logAlpha <- log(1/alpha)
    use <- start:n
    bound <- Inf
    for (i in start:n) {
      if (gaps[i] > logAlpha * ghat[i]) {
        bound <- d[ord][i - 1]
       # outlier.condition<- TRUE
        break
      }
    }

   # if(outlier.condition){
    #  pending.outlier.index<- i: length(gaps)
    #  pending.outlier.vals <- gaps[pending.outlier.index]
    #  num.pending.outliers <- length(pending.outlier.index)
    #  spacing.index <- pending.outlier.index[which.max(pending.outlier.vals *
          #                                               (num.pending.outliers:1))]:length(gaps)
     #ex1<-ord[spacing.index]
    #  ex<- exemplars[ex1]


    #} else {
    ex <- exemplars[which(d > bound)]

    #}
    mem1 <- sapply(memberLists, function(x) x[1])
    out <- unlist(memberLists[match(ex, mem1)])
    names(out) <- NULL
    return(out)
  }
}



#' Form clusters of datapoints using Hartigan's Leader Algorithm.
#'
#' @description  Form clusters of datapoints using Hartigan's Leader Algorithm. This is a modification of
#' \code{\link[HDoutliers]{getHDmembers}}.
#' @param data A vector, matrix, or data frame consisting of numeric and/or categorical variables.
#' @param maxrows If the number of observations is greater than \code{maxrows}, \code{outliers} reduces the
#'  number used in k-nearest-neighbor computations to a set of \emph{exemplars}. The default value is 10000.
#' @param radius Threshold for determining membership in the \emph{exemplars}'s lists (used only when the
#' number of observations is greater than \eqn{maxrows}). An observation is added to an \emph{exemplars}'s lists
#' if its distance to that \emph{exemplar} is less than \code{radius}. The default value is \eqn{1/2(1/n)^(1/p)},
#'  where \eqn{n} is the number of observations and \eqn{p} is the dimension of the data.
#' @return  A list in which each component is a vector of
#' observation indexes. The first index in each list is the
#' index of the exemplar defining that list, and any remaining indexes are the associated members, within radius of the exemplar.
#' @export
#' @importFrom mclust partuniq
#' @importFrom FNN get.knnx
#' @seealso \code{\link[HDoutliers]{getHDmembers}}
#' @references {Hartigan, John A. "Clustering algorithms." (1975).}
#' @references {Kantardzic, Mehmed. Data mining: concepts, models, methods, and algorithms.
#'  John Wiley & Sons, 2011.}
get_leader_cluster <- function( data, maxrows = 10000, radius = NULL)
{
  data <- as.matrix(data)
  n <- nrow(data)
  p <- ncol(data)
  if (is.null(radius))
   # radius <- 0.1/(log(n)^(1/p))
    radius <- 1/(2*((1/n)^(1/p)))
  if (n <= maxrows) {
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
    else members <- as.list(1:n)
  }
  else {
    members <- rep(list(NULL), n)
    exemplars <- 1
    members[[1]] <- 1
    for (i in 2:n) {
      KNN <- FNN::get.knnx(data = data[exemplars, , drop = F],
                      query = data[i, , drop = F], k = 1)
      m <- KNN$nn.index[1, 1]
      d <- KNN$nn.dist[1, 1]
      if (d < radius) {
        l <- exemplars[m]
        members[[l]] <- c(members[[l]], i)
        next
      }
      exemplars <- c(exemplars, i)
      members[[i]] <- i
    }
  }
  members <- members[!sapply(members, is.null)]
  exemplars <- sapply(members, function(x) x[[1]])
  names(members) <- exemplars
  members
}

