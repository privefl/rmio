\dontrun{ # CRAN is super slow when parallelism.

  X <- big_attachExtdata()

  ### Computation on all the matrix
  true <- big_counts(X)

  big_counts_sub <- function(X, ind) {
    rmio::big_counts(X, ind.col = ind)
  }
  # 1. the computation is split along all columns
  # 2. for each part the computation is done, using `big_colstats`
  # 3. the results (data.frames) are combined via `rbind`.
  test <- big_parallelize(X, p.FUN = big_counts_sub,
                          p.combine = "cbind", ncores = 2)
  all.equal(test, true)

  ### Computation on a part of the matrix
  n <- nrow(X)
  m <- ncol(X)
  rows <- sort(sample(n, n/2)) # sort to provide some locality in accesses
  cols <- sort(sample(m, m/2)) # idem

  true2 <- big_counts(X, ind.row = rows, ind.col = cols)

  big_counts_sub2 <- function(X, ind, rows, cols) {
    rmio::big_counts(X, ind.row = rows, ind.col = cols[ind])
  }
  # This doesn't work because, by default, the computation is spread
  # along all columns. We must explictly specify the `ind` parameter.
  tryCatch(big_parallelize(X, p.FUN = big_counts_sub2,
                           p.combine = "cbind", ncores = 2,
                           rows = rows, cols = cols),
           error = function(e) message(e))

  # This now works, using `ind = seq_along(cols)`.
  test2 <- big_parallelize(X, p.FUN = big_counts_sub2,
                           p.combine = "cbind", ncores = 2,
                           ind = seq_along(cols),
                           rows = rows, cols = cols)
  all.equal(test2, true2)

}
