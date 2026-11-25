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
opts = commandArgs(trailingOnly = T)
mode = opts[1]
reassess = identical(mode, "--reassess")
if (reassess) {
  stopifnot(length(opts) == 5L,
    identical(Sys.getenv("FP_REASSESS_AUTHORIZED"), "true"),
    Sys.getenv("GITHUB_EVENT_NAME") == "workflow_dispatch",
    grepl("^[0-9a-f]{40}$", opts[4]), grepl("^[0-9]+$", opts[5]))
} else if (length(opts) != 1L || !mode %in% c("--plan", "--run")) {
  stop("Use --plan, --run or the manual reassessment workflow.", call. = F)
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
if (!reassess && !fp_dy_authorized(auto, date)) {
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
  if (reassess) {
    revision = Sys.getenv("GITHUB_SHA")
    git = c("-C", shQuote(root))
    head = system2("git", c(git, "rev-parse", "HEAD"), stdout = T)
    stopifnot(identical(head, revision),
      system2("git", c(git, "merge-base", "--is-ancestor",
        opts[4], head)) == 0L,
      system2("git", c(git, "diff", "--quiet", opts[4], "--", "preparation",
        "config", "R/preparation_api.R")) == 0L,
      system2("git", c(git, "diff", "--quiet", "HEAD", "--",
        "R", "scripts", "config", "preparation")) == 0L,
      length(auto$warehouse_order) == 1L)
    outcome = fp_dy_reassess(root, fp, mw, cfg, store, work, fp_rm_http(),
      opts[2], as.integer(auto$warehouse_order[[1]]), opts[3], opts[4],
      revision, as.integer(opts[5]), Sys.getenv("GITHUB_RUN_ID"))
    cat("Offline reassessment verified:", outcome$commit,
      "| SKUs:", outcome$skus, "| raw responses:", outcome$responses,
      "| manifest SHA-256:", outcome$manifest_sha256, "\n")
    return("complete")
  }
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
outcome = tryCatch(suppressMessages(suppressWarnings(run())),
  fp_access_blocked = \(e) {
    fp_dy_report_codes(c("access_blocked", paste0("http_", e$code)))
    "incomplete"
  }, fp_capture_incomplete = \(e) {
    fp_dy_report_codes("interrupted_collection")
    "incomplete"
  }, error = \(e) {
    fp_dy_report_codes("runtime_failure")
    "incomplete"
  })
quit(status = if (outcome == "complete") 0L else 1L)
