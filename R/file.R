################################################################################

#' Create or resize files
#'
#' @param file Path name.
#' @param size Size in bytes.
#'
#' @importFrom bigassertr assert_noexist assert_dir assert_exist stop2
#'
#' @return Input `file`, invisibly.
#' @export
#'
#' @examples
#' tmp <- tempfile()
#' file_create(tmp, 10)
#' file.size(tmp)
#' file_resize(tmp, 5)
#' file.size(tmp)
#' file_resize_off(tmp, 10)
#' file.size(tmp)
#'
file_create <- function(file, size) {
  file <- path.expand(file)
  assert_noexist(file)
  assert_dir(dirname(file))
  file.create(file)
  ff::file.resize(file, size)
  invisible(file)
}

################################################################################

#' @rdname file_create
#' @export
file_resize <- function(file, size) {
  file <- path.expand(file)
  assert_exist(file)
  success <- ff::file.resize(file, size)
  if (!success) stop2("Problem when resizing '%s'.", file)
  invisible(file)
}

################################################################################

#' @rdname file_create
#' @export
file_resize_off <- function(file, size) {
  file_resize(file, file.size(file) + size)
}

################################################################################
