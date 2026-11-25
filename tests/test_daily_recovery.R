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

source("scripts/bootstrap.R")
root = normalizePath(".")
cfg = load_regional(root)
fp = fp_load_preparation(root)
mw = rp_mw_load(root)
source("tests/helpers_remote.R")
scratch = tempfile("recovery_")
dir.create(scratch)
auto = fp_dy_config(root, get = \(key) "")
auto$enabled = T
auto$authorization = list(valid_from = "2026-11-25",
  valid_until = "2026-12-10")
approval = auto
approval$authorization$valid_until = ""
stopifnot(fp_dy_authorized(approval, "2026-11-25"),
  fp_dy_authorized(approval, "2029-01-01"),
  !fp_dy_authorized(approval, "2026-11-24"))
approval$authorization$valid_until = NULL
stopifnot(fp_dy_authorized(approval, "2029-01-01"))
approval$enabled = F
stopifnot(!fp_dy_authorized(approval, "2029-01-01"))
approval$enabled = T
approval$authorization$valid_until = "2027-11-25"
stopifnot(fp_dy_authorized(approval, "2027-11-25"),
  !fp_dy_authorized(approval, "2027-11-26"))
for (end in c("2026-11-24", "2027-02-30", "2027-1-01", " ")) {
  approval$authorization$valid_until = end
  stopifnot(!fp_dy_authorized(approval, "2026-11-25"))
}
approval$authorization$valid_until = ""
approval$authorization$valid_from = ""
stopifnot(!fp_dy_authorized(approval, "2029-01-01"))
now = as.POSIXct("2026-11-25 09:15:00", tz = "UTC")
date = "2026-11-25"
tr = mock_remote(file.path(scratch, "remote"))
fake = new.env(parent = emptyenv())
fake$http = 0L
fake$fail_path = ""
fake$after = F
fake$block = F

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) SYNTHETIC OBSERVATIONS AND TRANSPORT FAILURE
# <-----------------------------------------------------------------------------

put_day = \(store, date, run_id) {
  data = read.csv("tests/fixtures/synthetic_w36.csv",
    colClasses = "character")
  data$retailer = "flaschenpost"
  data$warehouse_id = "36"
  data$daily_date = date
  data$run_id = run_id
  data$observed_at = paste0(date, "T09:15:00Z")
  data$product_key = data$sku
  data$observation_key = paste("flaschenpost", "w36", data$sku, date, sep = "|")
  data$listing_state = "listed"
  required = union(fp_vs_observation_columns(), fp_vs_tracked_columns())
  data[setdiff(required, names(data))] = NA_character_
  shard = tempfile(fileext = ".csv.gz")
  receipt = tempfile(fileext = ".json")
  on.exit(unlink(c(shard, receipt)), add = T)
  fp_vs_write_gz(data, shard)
  fp_vs_write_json(list(date = date, warehouse_id = 36L, run_id = run_id,
    n_products = nrow(data), observations = list(sha256 = fp_vs_hash(shard),
      path = "observations.csv.gz")), receipt)
  fp_vs_ingest(store, shard, receipt, 36L)
}

seed = file.path(scratch, "baseline")
fp_vs_init(seed)
stopifnot(put_day(seed, "2026-11-24", "baseline") == "committed")
fp_rm_publish_store(tr, seed)

transport = \(method, path, input = NULL, output = NULL) {
  if (method == "PUT_NEW" && nzchar(fake$fail_path) &&
      grepl(fake$fail_path, path)) {
    if (fake$after) tr(method, path, input, output)
    stop("Injected upload interruption.")
  }
  tr(method, path, input, output)
}
site = \(url, query, collector) {
  fake$http = fake$http + 1L
  if (fake$block) return(list(status = 429L))
  list(status = 200L, body = '{"sku":"SYN001"}')
}

capture_run = \(root, fp, release, cfg, wid, date, store, out,
  transport, clock, sleep, run_id) {
  budget = fp$fp_budget(fp$runner, 5L, 100, sleep = \(seconds) NULL)
  raw = file.path(out, "raw")
  ctx = fp$fp_ctx(fp$collector, fp$runner, budget, transport = transport,
    raw_dir = raw, sleep = sleep, clock = clock)
  fp$fp_http_json(ctx, "synthetic_1")
  fp$fp_http_json(ctx, "synthetic_2")
  list(status = put_day(store, date, run_id),
    raw_dir = raw, calls = do.call(rbind, ctx$state$calls))
}

run = \(at = now) {
  local = tempfile("runner_", tmpdir = scratch)
  dir.create(local)
  on.exit(unlink(local, recursive = T), add = T)
  fp_dy_run_day(scratch, fp, mw, cfg, auto, file.path(local, "store"), local,
    transport, now = at, site = site, capture_run = capture_run,
    clock = \() at)
}
failed = \(code) inherits(tryCatch(force(code), error = identity), "error")

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 2) REPLAY, REPEAT, INTEGRITY AND ACCESS LIMITS
# <-----------------------------------------------------------------------------

fake$fail_path = "/capture_[0-9a-f]{64}[.]rds$"
stopifnot(failed(run()), fake$http == 2L,
  identical(fp_rm_remote_units(tr)$id, "c0001"))
fake$fail_path = "/commits/c0002/observations.csv.gz"
stopifnot(failed(run()), fake$http == 2L,
  identical(fp_rm_remote_units(tr)$id, "c0001"))
fake$fail_path = "/markers/commits/c0002.json"
fake$after = T
stopifnot(failed(run()), fake$http == 2L,
  identical(fp_rm_remote_units(tr)$id, c("c0001", "c0002")))
fake$fail_path = ""
fake$after = F
stopifnot(run()$status == "complete", run()$status == "complete",
  fake$http == 2L, identical(fp_rm_remote_units(tr)$id,
    c("c0001", "c0002")))
raw = paste0(fp_rm_base, "/runs/", date, "/w36/a01/responses/0001.rds")
attr(tr, "state")$hide = raw
stopifnot(failed(run()), fake$http == 2L)
attr(tr, "state")$hide = character()
raw_path = file.path(scratch, "remote", raw)
valid_record = readRDS(raw_path)
broken_record = valid_record
broken_record$body = '{"sku":"CHANGED"}'
saveRDS(broken_record, raw_path)
stopifnot(failed(run()), fake$http == 2L)
saveRDS(valid_record, raw_path)
stopifnot(run()$status == "complete")

tomorrow = now + 86400
fake$fail_path = "/responses/0002.rds"
stopifnot(failed(run(tomorrow)), fake$http == 4L)
fake$fail_path = ""
stopifnot(run(tomorrow)$status == "complete", fake$http == 6L)
stopifnot(identical(fp_rm_remote_units(tr)$id, c("c0001", "c0002", "c0003")))

fake$block = T
stopifnot(failed(run(now + 2 * 86400)), fake$http == 7L,
  run(now + 2 * 86400)$status == "incomplete", fake$http == 7L)

probe = fp_rm_capability_probe(tr)
stopifnot(probe$all_ok, all(probe$checks))
overwriting = \(method, path, input = NULL, output = NULL) {
  if (method == "PUT_NEW" && endsWith(path, "/first.txt")) {
    target = file.path(scratch, "remote", path)
    if (file.exists(target)) {
      file.copy(input, target, overwrite = T)
      return(204L)
    }
  }
  tr(method, path, input, output)
}
bad = fp_rm_capability_probe(overwriting)
stopifnot(!bad$all_ok, !bad$checks["rejected"], !bad$checks["readback"])

use = fp_dy_inclusion(data.frame(alcohol_content = c(0, 0.5, 0.51, NA),
  analysis_ready = T, analysis_ready_gt_0_5 = T))
stopifnot(identical(use$main, c(F, F, T, F)),
  identical(use$sensitivity, c(F, T, T, F)))
unlink(scratch, recursive = T)
cat("Daily recovery, raw integrity, WebDAV, and ABV checks passed.\n")
