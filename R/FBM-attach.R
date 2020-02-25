################################################################################

FBM_FIELDS <- c("extptr", "extptr_rw", "nrow", "ncol", "type", "backingfile",
                "is_read_only", "address", "address_rw", "bk", "rds", "is_saved",
                "type_chr", "type_size")

FBM_METHODS <- c("bm.desc", "bm", "add_columns", "save", "check_write_permissions")

################################################################################

#' Reconstruct FBM from old version
#'
#' @param X An FBM.
#' @param msg1 Message before reconstructing.
#' @param msg2 Message after reconstructing.
#'
#' @return New FBM object (or old one if no need for reconstruction).
#' @export
#'
#' @keywords internal
#'
#' @examples
#' rds <- system.file("testdata", "before_readonly.rds", package = "rmio")
#' big_attach(rds)
reconstruct_if_old <- function(X,
                               msg1 = "FBM from an old version? Reconstructing..",
                               msg2 = "You should `$save()` it again.") {

  obj.fields  <- names(X$getClass()@fieldClasses)
  obj.methods <- names(X$getClass()@refMethods)

  # In case it was generated from old versions
  if (attr(class(X), "package") != "rmio" ||
      !all(FBM_FIELDS  %in% obj.fields) ||
      !all(FBM_METHODS %in% obj.methods)) {

    message2(msg1)

    new_X <- FBM(
      nrow = X$nrow,
      ncol = X$ncol,
      type = names(X$type),
      init = NULL,
      backingfile = sub_bk(X$backingfile),
      create_bk = FALSE,
      is_read_only = `if`(exists("is_read_only", X), X$is_read_only, FALSE)
    )

    if (inherits(X, "FBM.code256"))
      new_X <- add_code256(new_X, code = X$code256)

    message2(msg2)

    new_X

  } else X
}

################################################################################

#' Attach a Filebacked Big Matrix
#'
#' @param rdsfile Path to a ".rds" file.
#'
#' @return The [FBM] object stored in the rdsfile.
#' @export
#' @rdname big_attach
#'
#' @examples
#' # temporary FBM
#' X <- FBM(10, 10)$save()
#'
#' rdsfile <- sub_bk(X$backingfile, ".rds")
#' X2 <- big_attach(rdsfile)
#'
#' all.equal(X[], X2[])
big_attach <- function(rdsfile) {

  assert_exist(rdsfile)
  rdsfile <- normalizePath(rdsfile)
  X <- readRDS(rdsfile)

  # In case of moving files
  if (!file.exists(X$backingfile <- sub("\\.rds$", ".bk", rdsfile)))
    stop2("The backingfile associated with this FBM can't be found.")

  reconstruct_if_old(X)
}

#' @rdname big_attach
#' @export
#' @keywords internal
big_attachExtdata <- function() {
  tmp <- tempfile()
  EXTS <- c(".rds", ".bk")
  file.copy(system.file("extdata", paste0("example", EXTS), package = "rmio"),
            paste0(tmp, EXTS))
  big_attach(paste0(tmp, ".rds"))
}

################################################################################
