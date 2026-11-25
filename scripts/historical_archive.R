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
opts = commandArgs(trailingOnly = T)
if (!identical(opts, "--probe") &&
    (length(opts) != 2L || !opts[1] %in% c("--verify", "--mock", "--seed"))) {
  stop("Use --probe, or --verify, --mock, --seed with a private bundle path.")
}
source(file.path(root, "scripts", "bootstrap.R"))
load_regional(root)
source(file.path(root, "R", "historical.R"))
if (identical(opts, "--probe")) {
  if (Sys.getenv("FP_ARCHIVE_TRANSFER_ENABLED") != "true") {
    stop("WebDAV probe is disabled.")
  }
  probe = fp_rm_capability_probe(fp_rm_http())
  print(probe$checks)
  quit(status = if (probe$all_ok) 0L else 1L)
}
bundle = normalizePath(opts[2], mustWork = T)

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) EXPLICIT OFFLINE OR AUTHORIZED REMOTE OPERATION
# <-----------------------------------------------------------------------------

if (opts[1] == "--verify") {
  cat("Verified historical raw files:", fp_hist_verify(bundle)$raw_files, "\n")
} else if (opts[1] == "--mock") {
  source(file.path(root, "tests", "helpers_remote.R"))
  dest = tempfile("fp_mock_")
  n = fp_hist_seed(bundle, mock_remote(dest))
  unlink(dest, recursive = T)
  cat("Historical transfer and restore verified in local mock:", n, "\n")
} else {
  if (Sys.getenv("FP_ARCHIVE_TRANSFER_ENABLED") != "true") {
    stop("Historical transfer is disabled.")
  }
  cat("Transferred and verified historical raw files:",
    fp_hist_seed(bundle, fp_rm_http()), "\n")
}
