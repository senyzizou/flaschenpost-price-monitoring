# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  FLASCHENPOST PRICE MONITORING
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261125

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 0) ESSENTIALS
# <-----------------------------------------------------------------------------

args = commandArgs(trailingOnly = F)
script = sub("^--file=", "", args[grepl("^--file=", args)][1])
root = normalizePath(file.path(dirname(script), ".."), mustWork = T)
setwd(root)
source("scripts/bootstrap.R")
load_regional(root)
fp = fp_load_preparation(root)
sys.source("R/publication_scan.R", envir = fp)

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) VERIFY THE EXACT RELEASE CONTENT
# <-----------------------------------------------------------------------------

expected = readLines("REPOSITORY_FILES.txt", encoding = "UTF-8")
stopifnot(length(expected) > 0L, !anyDuplicated(expected),
  all(grepl("^[A-Za-z0-9_.][A-Za-z0-9_./-]*$", expected)),
  !any(grepl("(^|/)[.]{1,2}(/|$)", expected)))
# An initialized checkout may hold Git metadata outside the release.
actual = list.files(all.files = T, recursive = T, include.dirs = F)
actual = actual[!grepl("^[.]git/", actual)]
if (!setequal(expected, actual)) {
  stop("Release file list differs from staging. Inspect the file selection.")
}
stopifnot(!any(nzchar(Sys.readlink(expected))),
  !any(grepl("^(data|results|[.]git)/", expected)))
for (path in expected) {
  tryCatch(fp$fp_ci_privacy(path), error = \(err) {
    stop("Release inspection failed for ", path, ": ",
      conditionMessage(err), call. = F)
  })
}
stopifnot(fp_preparation_verify(root)$ok)
cat(length(expected), "release files verified, including provenance.\n")
