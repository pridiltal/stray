#' Find Outlier Threshold
#'
#' @param outlier_score A vector of outlier scores. Can be a named vector or a vector with no names.
#' @param alpha Threshold for determining the cutoff for outliers. Observations are considered
#'  outliers if they fall in the \eqn{(1- alpha)} tail of the distribution of the nearest-neighbor
#'  distances between exemplars.
#' @param outtail Direction of the outlier tail.
#' @return The indexes (or names, if the input is named vactor) of the observations determined to be outliers.
#' @export
find_theshold <- function(outlier_score, alpha = 0.05, outtail = c("max", "min")) {

  n <- length(outlier_score)
  if(outtail == "min")
  {
    outlier_score <- - outlier_score
  }

  ord <- order(outlier_score)
  gaps <- c(0, diff(outlier_score[ord]))
  n4 <- max(min(50, floor(n / 4)), 2)
  J <- 2:n4
  start <- max(floor(n / 2), 1) + 1
  ghat <- numeric(n)
  for (i in start:n) ghat[i] <- sum((J / (n4 - 1)) * gaps[i - J + 1 ]) # check i - j +1
  # J <- 1:n4
  #  start <- max(floor(n/2), 1) + 1
  #  ghat <- numeric(n)
  #  for (i in start:n) ghat[i] <- sum((J/(n4)) * gaps[i - J+1 ]) # check i - j +1
  logAlpha <- log(1 / alpha)
  bound <- Inf

  for (i in start:n) {
    if (gaps[i] > logAlpha * ghat[i]) {
      bound <- outlier_score[ord][i - 1]
      break
    }
  }
  ex <- which(outlier_score > bound)

  return(ex)
}
