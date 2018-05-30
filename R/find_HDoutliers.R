#' Detect Anomalies in High Dimensional Data.
#'
#' @description  Detect anomalies in high dimensional data. This is a modification of
#' \code{\link[HDoutliers]{HDoutliers}}.
#' @param data A vector, matrix, or data frame consisting of numeric and/or categorical variables.
#' @param maxrows If the number of observations is greater than \code{maxrows}, \code{outliers} reduces the
#'  number used in k-nearest-neighbor computations to a set of \emph{exemplars}. The default value is 10000.
#' @param alpha Threshold for determining the cutoff for outliers. Observations are considered
#'  outliers outliers if they fall in the \eqn{(1- alpha)} tail of the distribution of the nearest-neighbor
#'  distances between exemplars.
#' @return The indexes of the observations determined to be outliers.
#' @details If the number of observations exceeds \code{maxrows}, the data is first partitioned into lists
#' associated with \emph{exemplars} and their \emph{members} within \code{radius} of each \emph{exemplar}, to
#' reduce the number of k-nearest neighbor computations required for outlier detection.
#' @seealso \code{\link{get_leader_clusters}}
#' @export
#' @importFrom HDoutliers getHDmembers
#' @importFrom FNN knn.dist
#' @import ggplot2
#' @references {Wilkinson, L. (2018), `Visualizing big data
#' outliers through distributed aggregation', IEEE
#' transactions on visualization and computer graphics 24(1), 256-266.}
#' @examples
#' require(ggplot2)
#' set.seed(1234)
#' data <- c(rnorm(1000, mean = -6), 0, rnorm(1000, mean = 6))
#' outliers <- find_HDoutliers(data)
#' display_HDoutliers(data,outliers )
#'
#'
#' set.seed(1234)
#' n <- 1000 # number of observations
#' nout <- 10 # number of outliers
#' typical_data <- tibble::as.tibble(matrix(rnorm(2*n), ncol = 2, byrow = TRUE))
#' out <- tibble::as.tibble(matrix(5*runif(2*nout,min=-5,max=5), ncol = 2, byrow = TRUE))
#' data <- rbind(out, typical_data )
#' outliers <- find_HDoutliers(data)
#' display_HDoutliers(data, outliers)
find_HDoutliers <- function(data, maxrows = 1000, alpha = 0.01){

#standardize <- function(z) {(z-stats::median(z))/stats::IQR(z)}
#standardize <- function(z) {(z-mean(z))/stats::sd(z)}
unitization <- function(z) {(z-min(z))/(max(z)-min(z))}
data <- as.matrix(data)
zdata <- apply(data, 2, unitization)
members <- get_leader_clusters(zdata, maxrows = maxrows)

if(length(members)==1){
  out = NULL
} else {
  break_list<-function(x){
    max <- floor(nrow(data) / 20)
    seq <- seq_along(x)
    split(x, ceiling(seq/max))
  }

  members <- lapply(members, break_list)
  members <- unlist(members, recursive= FALSE, use.names = FALSE)
  exemplars <- sapply(members, function(x) x[[1]])
  names(members) <- exemplars


  k <- ceiling(length(exemplars)/ 20)
  if(k==1){
    d <- as.vector(FNN::knn.dist(zdata[exemplars, ], 1 ))
  } else{
    d_knn <- FNN::knn.dist(zdata[exemplars, ], k )
    d_knn1 <-cbind(rep(0, nrow(d_knn)), d_knn)
    diff <- t(apply(d_knn1, 1, diff))
    max_diff <- apply(diff, 1, which.max)
    d <- d_knn[cbind(1:nrow(d_knn), max_diff)]
  }
  n <- length(d)
  ord <- order(d)
  gaps <- c(0, diff(d[ord]))
  n4 <- max(min(50, floor(n/4)), 2)
  J <- 2:n4
  start <- max(floor(n/2), 1) + 1
  ghat <- numeric(n)
  for (i in start:n) ghat[i] <- sum((J/(n4-1)) * gaps[i - J+1 ]) # check i - j +1
  #J <- 1:n4
  #start <- max(floor(n/2), 1) + 1
  #ghat <- numeric(n)
  #for (i in start:n) ghat[i] <- sum((J/(n4)) * gaps[i - J+1 ]) # check i - j +1
  logAlpha <- log(1/alpha)
  bound <- Inf

  for (i in start:n) {
    if (gaps[i] > logAlpha * ghat[i]) {
      bound <- d[ord][i - 1]
      break
    }
  }
  ex <- exemplars[which(d > bound)]
  out <- unlist(members[match(ex, exemplars)])
  names(out) <- NULL
}
return(out)
}


#' Form clusters of datapoints using Hartigan's Leader Algorithm.
#'
#' @description  Form clusters of datapoints using Hartigan's Leader Algorithm. This is a modification of
#' \code{\link[HDoutliers]{getHDmembers}}.
#' @param data A vector, matrix, or data frame consisting of numeric and/or categorical variables.
#' @param maxrows If the number of observations is greater than \code{maxrows}, \code{outliers} reduces the
#'  number used in k-nearest-neighbor computations to a set of \emph{exemplars}. The default value is 1000.
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
get_leader_clusters <- function( data, maxrows = 1000)
{

  n <- nrow(data)
  p <- ncol(data)
# radius <- 1/2*((1/n)^(1/p))
 # radius <- 0.5
  radius<- 0.1/(log(n)^(1/p)) #HD

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

