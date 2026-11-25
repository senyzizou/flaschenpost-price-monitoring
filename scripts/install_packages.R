# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING - FLASCHENPOST PREPARATION
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261027

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 0) ESSENTIALS
# <-----------------------------------------------------------------------------

# Explicit CI setup only; never sourced by load_project() or offline tests.
args = commandArgs(trailingOnly = F)
script = sub("^--file=", "", args[grepl("^--file=", args)][1])
root = normalizePath(file.path(dirname(script), ".."), mustWork = T)
if (!identical(commandArgs(trailingOnly = T), "--install-for-preflight") ||
    Sys.getenv("GITHUB_ACTIONS") != "true" ||
    unname(Sys.info()["sysname"]) != "Linux") stop(
  "Installation is restricted to explicit isolated Ubuntu Actions setup.")
library_dir = Sys.getenv("FP_ISOLATED_R_LIBRARY")
runner_temp = Sys.getenv("RUNNER_TEMP")
if (!nzchar(runner_temp) || !startsWith(library_dir,
    paste0(runner_temp, "/")) || grepl("(^|/)[.]{2}(/|$)", library_dir)) stop(
  "Package library must be below the isolated runner temp directory.")
dir.create(library_dir, recursive = T, showWarnings = F)
.libPaths(c(library_dir, .libPaths()))
packages = read.csv(
  file.path(root, "preparation", "config", "ubuntu_packages.csv"),
  colClasses = "character")

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) PINNED SOURCE INSTALLATION IN THE EPHEMERAL JOB LIBRARY
# <-----------------------------------------------------------------------------

version_in_job = \(pkg) tryCatch(as.character(packageVersion(pkg,
  lib.loc = library_dir)), error = \(e) "")
if (!identical(version_in_job("remotes"), "2.5.0") &&
    any(vapply(packages$package, version_in_job, "") != packages$version)) {
  source_file = tempfile(fileext = ".tar.gz")
  urls = paste0("https://cran.r-project.org/src/contrib/",
    c("", "Archive/remotes/"), "remotes_2.5.0.tar.gz")
  downloaded = F
  for (url in urls) {
    downloaded = tryCatch(
      suppressWarnings(download.file(url, source_file, mode = "wb",
        quiet = T)) == 0L, error = \(e) F)
    if (downloaded) break
  }
  if (!downloaded) stop("Pinned remotes 2.5.0 could not be downloaded.")
  install.packages(source_file, repos = NULL, type = "source",
    lib = library_dir, quiet = T)
  unlink(source_file)
  stopifnot(identical(version_in_job("remotes"), "2.5.0"))
}
for (i in seq_len(nrow(packages))) {
  pkg = packages$package[i]
  installed = version_in_job(pkg)
  if (installed != packages$version[i]) {
    remotes::install_version(pkg, version = packages$version[i],
      lib = library_dir, repos = "https://cloud.r-project.org",
      dependencies = F, upgrade = "never", quiet = T)
  }
  stopifnot(identical(as.character(packageVersion(pkg,
    lib.loc = library_dir)), packages$version[i]))
}
cat("Pinned runtime packages installed in isolated CI library.\n")
