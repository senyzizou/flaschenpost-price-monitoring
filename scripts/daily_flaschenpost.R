# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261102

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 0) ESSENTIALS
# <-----------------------------------------------------------------------------

args = commandArgs(trailingOnly = F)
script = sub("^--file=", "", args[grepl("^--file=", args)][1])
root = normalizePath(file.path(dirname(script), ".."), mustWork = T)
mode = commandArgs(trailingOnly = T)
if (length(mode) != 1L || !mode %in% c("--plan", "--run")) {
  stop("Use --plan or --run.", call. = F)
}
source(file.path(root, "scripts", "bootstrap.R"))
cfg = load_regional(root)
auto = fp_dy_config(root)
mw = rp_mw_load(root)
verified = fp_preparation_verify(root)
stopifnot(verified$ok, length(auto$warehouse_order) > 0L,
  all(unlist(auto$warehouse_order) %in% rp_mw_ids(mw)))
fp = fp_load_preparation(root)
date = fp_dy_today(auto)
cat("Berlin date:", date, "\n")
cat("Configured warehouses:", unlist(auto$warehouse_order), "\n")
if (mode == "--plan") {
  cat("Status: offline plan verified\n")
  quit(status = 0L)
}
if (!fp_dy_authorized(auto, date)) {
  stop("Daily live operation has no active authorization.", call. = F)
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) RUN WITH PRIVATE WORKING FILES
# <-----------------------------------------------------------------------------

run = \() {
  parent = file.path(root, "data", "runtime")
  dir.create(parent, recursive = T, showWarnings = F, mode = "0700")
  work = tempfile("day_", tmpdir = parent)
  stopifnot(dir.create(work, mode = "0700"))
  on.exit(unlink(work, recursive = T), add = T)
  store = file.path(work, "versioned_store")
  result = fp_dy_run_day(root, fp, mw, cfg, auto, store, work, fp_rm_http())
  cat("Status:", result$status, "\n")
  if (!is.null(result$warehouses)) {
    print(result$warehouses, row.names = F)
  }
  if (identical(result$status, "complete")) {
    print(fp_dy_summary(store, result$date, root), row.names = F)
  }
  result$status
}
outcome = run()
quit(status = if (outcome == "complete") 0L else 1L)
