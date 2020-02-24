################################################################################

#' Replace extension '.bk'
#'
#' @param path String with extension '.bk'.
#' @param replacement Replacement of '.bk'. Default replaces by nothing.
#' @param stop_if_not_ext If `replacement != ""`, whether to error if
#'   replacement is not an extension (i.e. starting with a dot).
#'
#' @return String with extension '.bk' replaced by `replacement`.
#' @export
#'
#' @examples
#' path <- "toto.bk"
#' sub_bk(path)
#' sub_bk(path, ".rds")
sub_bk <- function(path, replacement = "", stop_if_not_ext = TRUE) {
  pattern <- "\\.bk$"
  if (!grepl(pattern, path))
    stop2("Path '%s' must have 'bk' extension.", path)
  if (stop_if_not_ext && nchar(replacement) > 0 && substr(replacement, 1, 1) != ".")
    stop2("Replacement must be an extension starting with '.' if provided.")
  sub(pattern, replacement, path)
}

################################################################################

#' Determine a good default value for the block.size parameter
#'
#' It determines the value of `block.size` such that a matrix of doubles of
#' size `n` x `block.size` takes less memory than
#' `getOption("bigstatsr.block.sizeGB")` GigaBytes (default is 1GB).
#'
#' @param n The number of rows.
#' @param ncores The number of cores.
#'
#' @return An integer >= 1.
#'
#' @export
#'
#' @examples
#' block_size(1e3)
#' block_size(1e6)
#' block_size(1e6, 6)
#'
block_size <- function(n, ncores = 1) {
  block.max <- getOption("bigstatsr.block.sizeGB") / ncores
  # 8 * n * m < opt * 1024^3
  # m < opt * 1024^3 / (8 * n)
  max(1, floor(block.max * 1024^3 / (8 * n)))
}

################################################################################
#### Sequence generation ####

#' @importFrom bigparallelr rows_along
#' @export
bigparallelr::rows_along

#' @importFrom bigparallelr cols_along
#' @export
bigparallelr::cols_along

################################################################################

#' Increment an FBM
#'
#' @param X An `FBM` (of type double) to increment.
#' @param add A matrix of same dimensions as `X`. Or a vector of same size.
#' @param use_lock Whether to use locks when incrementing. Default is `FALSE`.
#'   This is useful when incrementing in parallel.
#'
#' @return Returns nothing (`NULL`, invisibly).
#'
#' @export
#'
#' @examples
#' X <- FBM(10, 10, init = 0)
#' mat <- matrix(rnorm(100), 10, 10)
#'
#' big_increment(X, mat)
#' all.equal(X[], mat)
#'
#' big_increment(X, mat)
#' all.equal(X[], 2 * mat)
#'
big_increment <- function(X, add, use_lock = FALSE) {

  if (use_lock) {
    locked <- bigparallelr::lock(X$backingfile)
    on.exit(bigparallelr::unlock(locked), add = TRUE)
  }

  if (is.matrix(add)) incr_FBM_mat(X, add) else incr_FBM_vec(X, add)
}

################################################################################

#' Temporarily disable downcast warning
#'
#' @param expr The expression to evaluate without downcast warning.
#'
#' @return The result of the evaluated expression.
#' @export
#'
#' @examples
#' without_downcast_warning(FBM(10, 10, type = "integer", init = 1.5))
without_downcast_warning <- function(expr) {

  opt.save <- options(bigstatsr.downcast.warning = FALSE)
  on.exit(options(opt.save), add = TRUE)

  eval.parent(substitute(expr))
}

################################################################################
