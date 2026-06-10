#' Implementation of MULTIMOORA Method for Multi-Criteria Decision Making Problems.
#'
#' @description The \code{MMOORA} function implements both the Multi-Objetive Optimization by Ration Analysis (MOORA) and the "Full Multiplicative Form" (MULTIMOORA).
#' @param decision The decision matrix (\emph{m} x \emph{n}) with the values of the \emph{m} alternatives, for the \emph{n} criteria.
#' @param weights A vector of length \emph{n}, containing the weights for the criteria. The sum of the weights has to be 1.
#' @param cb A vector of length \emph{n}. Each component is either \code{cb(i)='max'} if the \emph{i-th} criterion is benefit or \code{cb(i)='min'} if the \emph{i-th} criterion is a cost.
#' @return \code{MMOORA} returns a data frame which contains the scores and the four rankings calculated (Ratio System, Reference Point, Multiplicative Form and Multi-MOORA ranking).
#' @references Brauers, W. K. M.; Zavadskas, E. K. Project management by MULTIMOORA as an instrument for transition economies. Technological and Economic Development of Economy, 16(1), 5-24, 2010.
#' @examples
#'
#'  d <- matrix(c(60,6.35,6.8,10,2.5,4.5,3,0.4,0.15,0.1,0.2,0.1,0.08,0.1,2540,1016,1727.2,
#'  1000,560,1016,1778,500,3000,1500,2000,500,350,1000,990,1041,1676,965,915,508,920),
#'  nrow=7,ncol=5)
#'  w <- c(0.036,0.192,0.326,0.326,0.12)
#'  cb <- c('max','min','max','max','max')
#'  MMOORA(d,w,cb)
#'
#' @export
MMOORA <- function(decision, weights, cb) {
  if (!is.matrix(decision)) stop("'decision' must be a matrix")
  if (missing(weights)) stop("weights required")
  if (missing(cb)) stop("cb required")

  # Normalize decision matrix
  N <- apply(decision, 2, function(x) x / sqrt(sum(x^2)))

  # 1. Ratio System
  NW <- sweep(N, 2, weights, "*")
  Y_rs <- rep(0, nrow(NW))
  if (any(cb == "max")) {
    Y_rs <- Y_rs + rowSums(NW[, cb == "max", drop = FALSE])
  }
  if (any(cb == "min")) {
    Y_rs <- Y_rs - rowSums(NW[, cb == "min", drop = FALSE])
  }
  rank_rs <- rank(-Y_rs, ties.method = "first")

  # 2. Reference Point Approach
  ref_point <- sapply(1:ncol(NW), function(i) {
    if (cb[i] == "max") max(NW[, i]) else min(NW[, i])
  })
  distances <- sweep(NW, 2, ref_point, "-")
  distances <- apply(distances, 2, abs)
  Y_rp <- apply(distances, 1, max)
  rank_rp <- rank(Y_rp, ties.method = "first")

  # 3. Full Multiplicative Form
  U <- rep(1, nrow(decision))
  V <- rep(1, nrow(decision))
  if (any(cb == "max")) {
    U <- apply(decision[, cb == "max", drop = FALSE], 1, prod)
  }
  if (any(cb == "min")) {
    V <- apply(decision[, cb == "min", drop = FALSE], 1, prod)
  }
  Y_mf <- U / V
  rank_mf <- rank(-Y_mf, ties.method = "first")

  # 4. Multi-MOORA Ranking
  rank_all <- data.frame(RS = rank_rs, RP = rank_rp, MF = rank_mf)
  rank_mm <- rank(rowSums(rank_all), ties.method = "first")

  return(data.frame(
    Alternatives = 1:nrow(decision),
    RatioSystemScore = Y_rs, RatioSystemRank = rank_rs,
    ReferencePointScore = Y_rp, ReferencePointRank = rank_rp,
    MultiplicativeFormScore = Y_mf, MultiplicativeFormRank = rank_mf,
    MultiMOORARank = rank_mm
  ))
}
