################################################################################

#' @useDynLib rmio, .registration = TRUE
#'
#' @importFrom Rcpp sourceCpp
#' @importFrom bigassertr message2 warning2 stop2
#' @importFrom bigassertr assert_class assert_int assert_pos assert_all
#' @importFrom bigassertr assert_args assert_dir assert_exist assert_noexist
#'
#' @param X An object of class [FBM][FBM-class].
#' @param X.code An object of class [FBM.code256][FBM.code256-class].
#'
#' @param ind.row An optional vector of the row indices that are used.
#' If not specified, all rows are used. __Don't use negative indices.__
#' @param ind.col An optional vector of the column indices that are used.
#' If not specified, all columns are used. __Don't use negative indices.__
#'
#' @param block.size Maximum number of columns read at once.
#'   Default uses [block_size].
#'
#' @param ncores Number of cores used. Default doesn't use parallelism.
#'   You may use [nb_cores].
#'
"_PACKAGE"

################################################################################
