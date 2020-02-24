################################################################################

#' @importFrom bigparallelr nb_cores
#' @export
bigparallelr::nb_cores

################################################################################

#' Split-parApply-Combine
#'
#' A Split-Apply-Combine strategy to parallelize the evaluation of a function.
#'
#' This function splits indices in parts, then apply a given function to each
#' part and finally combine the results.
#'
#' @inheritParams rmio-package
#' @param p.FUN The function to be applied to each subset matrix.
#'   It must take a [Filebacked Big Matrix][FBM-class] as first argument and
#'   `ind`, a vector of indices, which are used to split the data.
#'   For example, if you want to apply a function to \code{X[ind.row, ind.col]},
#'   you may use \code{X[ind.row, ind.col[ind]]} in `p.FUN`.
#' @param p.combine Function to combine the results with `do.call`.
#'   This function should accept multiple arguments (`...`). For example, you
#'   can use `c`, `cbind` and `rbind`. This package also provides function `plus`
#'   to add multiple arguments together. The default is `NULL`, in which case
#'   the results are not combined and are returned as a list, each element being
#'   the result of `p.FUN` applied to one block.
#' @param ind Initial vector of subsetting indices.
#'   Default is the vector of all column indices.
#' @param ... Extra arguments to be passed to `p.FUN`.
#'
#' @return Return a list of `ncores` elements, each element being the result of
#'   one of the cores, computed on a block. The elements of this list are then
#'   combined with `do.call(p.combine, .)` if `p.combined` is given.
#' @export
#'
#' @importFrom bigparallelr register_parallel
#'
#' @seealso [big_apply] [bigparallelr::split_parapply]
#'
#' @example examples/example-parallelize.R
#'
big_parallelize <- function(X, p.FUN,
                            p.combine = NULL,
                            ind = cols_along(X),
                            ncores = nb_cores(),
                            ...) {

  bigparallelr::split_parapply(
    p.FUN, ind, X, ...,
    .combine = p.combine,
    ncores = ncores,
    opts_cluster = list(type = getOption("bigstatsr.cluster.type"))
  )
}

################################################################################

big_applySeq <- function(X, a.FUN, block.size, ind, ...) {

  intervals <- bigparallelr::split_len(total_len = length(ind),
                                       block_len = block.size)

  lapply(rows_along(intervals), function(ic) {
    seq_ic <- bigparallelr::seq_range(intervals[ic, ])
    a.FUN(X, ind = ind[seq_ic], ...)
  })
}

################################################################################

#' Split-Apply-Combine
#'
#' A Split-Apply-Combine strategy to apply standard R functions to a
#' Filebacked Big Matrix.
#'
#' This function splits indices in parts, then apply a given function to each
#' subset matrix and finally combine the results. If parallelization is used,
#' this function splits indices in parts for parallelization, then split them
#' again for each core, apply the given function to each part and finally
#' combine the results (within each cluster and then between each cluster).
#'
#' @inheritParams rmio-package
#' @param a.FUN The function to be applied to each subset matrix.
#'   It must take a [Filebacked Big Matrix][FBM-class] as first argument and
#'   `ind`, a vector of indices, which are used to split the data.
#'   For example, if you want to apply a function to \code{X[ind.row, ind.col]},
#'   you may use \code{X[ind.row, ind.col[ind]]} in `a.FUN`.
#' @param a.combine Function to combine the results with `do.call`.
#'   This function should accept multiple arguments (`...`). For example, you
#'   can use `c`, `cbind` and `rbind`. This package also provides function `plus`
#'   to add multiple arguments together. The default is `NULL`, in which case
#'   the results are not combined and are returned as a list, each element being
#'   the result of `a.FUN` applied to one block.
#' @param ind Initial vector of subsetting indices.
#'   Default is the vector of all column indices.
#' @param block.size Maximum number of columns (or rows, depending on how you
#'   use `ind` for subsetting) accessed at once. Default uses [block_size].
#' @param ... Extra arguments to be passed to `a.FUN`.
#'
#' @export
#'
#' @seealso [big_parallelize] [bigparallelr::split_parapply]
#'
#' @example examples/example-apply.R
#'
big_apply <- function(X, a.FUN,
                      a.combine = NULL,
                      ind = cols_along(X),
                      ncores = 1,
                      block.size = block_size(nrow(X), ncores),
                      ...) {

  assert_args(a.FUN, "ind")

  res <- big_parallelize(X = X,
                         p.FUN = big_applySeq,
                         p.combine = NULL,
                         ind = ind,
                         ncores = ncores,
                         a.FUN = a.FUN,
                         block.size = block.size,
                         ...)

  res <- unlist(res, recursive = FALSE)

  `if`(is.null(a.combine), res, do.call(a.combine, res))
}

################################################################################

#' @importFrom bigparallelr plus
#' @export
bigparallelr::plus

################################################################################
