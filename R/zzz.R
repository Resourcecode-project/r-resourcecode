.pkgenv <- new.env(parent = emptyenv())

.onLoad  <- function(libname, pkgname) {
  has_data <- requireNamespace("resourcecodedata", quietly = TRUE)
  .pkgenv[["has_data"]] <- has_data
}

.onAttach <- function(libname, pkgname) {

  if (!.pkgenv$has_data) {
    msg <- paste("To use this package, you must install the",
                 "`{resourcecodedata}` package. To install that ",
                 "package, run `install.packages('resourcecodedata',",
                 "repos='https://resourcecode-project.github.io/drat/', type='source')`.",
                 "See the `resourcecode` vignette for more details.")
    msg <- paste(strwrap(msg), collapse = "\n")
    packageStartupMessage(msg)
  }
}

has_data <- function(has_data = .pkgenv$has_data) {
  if (!has_data) {
    msg <- paste("To use this function, you must have the",
                 "`{resourcecodedata}` package installed. See the",
                 "`resourcecode` package vignette for more details.")
    msg <- paste(strwrap(msg), collapse = "\n")
    stop(msg)
  }
}
