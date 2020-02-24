################################################################################

.onLoad <- function(libname, pkgname) {

  pkg.opt <- list(
    bigstatsr.cluster.type     = "PSOCK",
    bigstatsr.block.sizeGB     = 1,
    bigstatsr.downcast.warning = TRUE
  )

  toset <- !(names(pkg.opt) %in% names(.Options))
  if (any(toset)) options(pkg.opt[toset])
}

################################################################################
