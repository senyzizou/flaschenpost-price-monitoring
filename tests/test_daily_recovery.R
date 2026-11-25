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

put_day = \(store, date, run_id, reassessment = NULL) {
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
      path = "observations.csv.gz"), reassessment = reassessment), receipt)
  fp_vs_ingest(store, shard, receipt, 36L)
}

seed = file.path(scratch, "baseline")
fp_vs_init(seed)
stopifnot(put_day(seed, "2026-11-24", "baseline") == "committed")
fp_rm_publish_store(tr, seed)
prior_raw = file.path(scratch, "historic_raw")
dir.create(prior_raw)
writeLines("{}", file.path(prior_raw, "0001_category.json"))
fp_rm_archive_raw(tr, prior_raw, "2026-11-24", 36L, "baseline")

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
  transport, clock, sleep, run_id, reassessment = NULL) {
  budget = fp$fp_budget(fp$runner, 5L, 100, sleep = \(seconds) NULL)
  raw = file.path(out, "raw")
  ctx = fp$fp_ctx(fp$collector, fp$runner, budget, transport = transport,
    raw_dir = raw, sleep = sleep, clock = clock)
  fp$fp_http_json(ctx, "synthetic_1")
  fp$fp_http_json(ctx, "synthetic_2")
  list(status = put_day(store, date, run_id, reassessment),
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
verified = run()
old = verified$warehouses[verified$warehouses$date == "2026-11-24", ]
stopifnot(old$commit == "c0001", old$status == "verified_historical")
stopifnot(verified$status == "complete", run()$status == "complete",
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
# <=============================================================================
# <=============================================================================
# <=============================================================================

# 3) CATEGORY CHANGE EVIDENCE AND SANITIZED FAILURES
# <-----------------------------------------------------------------------------

tree = data.frame(root_id = c("19", "23", "24", "19"),
  sub_id = c("101", "102", "103", "104"), sub_name = letters[1:4],
  root_name = c("Bier", "Wein", "Spirituosen", "Bier"), fetch = T,
  in_scope = T, nonbeverage_excluded = F)
variants = data.frame(sku = c("stay", "moved", "wine", "spirit", "absent"),
  product_name = "Synthetic", alcohol_content = c("5", "6", "12", "40", "4.8"),
  price_eur = "2", total_volume_l = "1", price_per_unit_text = "2,00",
  abv_filter_min_0_5 = "yes", observed_at = "2026-11-24T09:00:00Z")
prior = list(meta = list(warehouse_id = 36L), categories = tree,
  variants = variants,
  membership = data.frame(sku = variants$sku,
    sub_id = c("101", "104", "102", "103", "104")))
current = variants[1:4, ]
current = rbind(current, transform(variants[1, ], sku = "new"))
listed = data.frame(sku = current$sku,
  sub_id = c("101", "101", "102", "103", "102"))
run_data = file.path(scratch, "category_fixture")
dir.create(run_data)
fp_vs_write_json(list(started_at = "2026-11-25T09:00:00Z",
  warehouse_id = 36L, scope_limited = F,
  inventory_blocking = fp_cv_category_blocker),
  file.path(run_data, "meta.json"))
save_table = \(name, x) {
  write.csv(x, file.path(run_data, paste0(name, ".csv")), row.names = F)
}
catalog = tree[1:3, ]
counts = data.frame(sub_id = catalog$sub_id, variants = c(2, 2, 1))
save_table("categories", catalog)
save_table("variants", current)
save_table("membership", listed)
save_table("listing", counts)
save_table("replicates", data.frame(same_sku_set = T))
save_table("outside_probe", data.frame(alcohol_hits = 0, detail_missing = 0))
a = fp_cv_assess(run_data, prior = prior)
gone = a$assortment_changes[a$assortment_changes$sku == "absent", ]
stopifnot(a$pass, identical(a$category_changes$sub_id, "104"),
  gone$change_type == "removed_with_category",
  gone$alcohol_evidence == "alcoholic", gone$prior_abv_pct == "4.8",
  !("moved" %in% a$assortment_changes$sku),
  a$assortment_changes$change_type[a$assortment_changes$sku == "new"] ==
    "new_in_assortment")
save_table("listing", counts[1:2, ])
a = fp_cv_assess(run_data, prior = prior)
stopifnot(!a$pass, "all_current_categories_fetched" %in% a$failed)
save_table("listing", counts)
save_table("membership", rbind(listed,
  data.frame(sku = "moved", sub_id = "104")))
a = fp_cv_assess(run_data, prior = prior)
stopifnot(!a$pass, "membership_uses_fetched_categories" %in% a$failed)
save_table("membership", listed)
hidden = tree
hidden$fetch[4] = F
save_table("categories", hidden)
a = fp_cv_assess(run_data, prior = prior)
stopifnot(!a$pass, "removed_categories_gone_from_tree" %in% a$failed)
line = capture.output(fp_dy_report_codes(c(a$failed, "PRIVATE_PRODUCT_TEXT")))
stopifnot(any(grepl("removed_categories_gone_from_tree", line)),
  !any(grepl("PRIVATE_PRODUCT_TEXT", line)))

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 4) REVISION-BOUND REASSESSMENT WITH IMMUTABLE INPUTS
# <-----------------------------------------------------------------------------

retry_dir = file.path(scratch, "reassessment")
retry = mock_remote(retry_dir)
fp_rm_publish_store(retry, seed)
attempt = paste0(fp_rm_base, "/runs/", date, "/w36/a01")
fp_rm_mkcols(retry, paste0(attempt, "/responses/placeholder"))
capture_revision = paste(rep("a", 40), collapse = "")
assessment_revision = paste(rep("b", 40), collapse = "")
fp_rc_put(retry, paste0(attempt, "/started.rds"),
  list(time = now, revision = capture_revision))
fp_rc_put(retry, paste0(attempt, "/rejected.rds"), "blocked_coverage")
recorder = fp_rc_capture(retry, attempt, now, http = \(...) {
  list(status = 200L, body = '{"sku":"SYN001"}')
}, clock = \() now)
for (i in seq_len(2L)) {
  url = paste(fp$collector$api_base, paste0("synthetic_", i), sep = "/")
  recorder$request(url, list(), fp$collector)
}
immutable = list.files(file.path(retry_dir, attempt), recursive = T,
  full.names = T)
hashes = vapply(immutable, fp_vs_hash, "")
do_reassess = \(revision = capture_revision, expected = 12L) {
  local = tempfile("runner_", tmpdir = scratch)
  dir.create(local)
  on.exit(unlink(local, recursive = T), add = T)
  fp_dy_reassess(root, fp, mw, cfg, file.path(local, "store"), local,
    retry, date, 36L, "a01", revision, assessment_revision, expected, "1",
    capture_run = capture_run)
}
stopifnot(failed(do_reassess(assessment_revision)),
  failed(do_reassess(expected = 13L)),
  identical(fp_rm_remote_units(retry)$id, "c0001"))
result = do_reassess()
stopifnot(result$commit == "c0002", result$skus == 12L, result$responses == 2L,
  identical(result, do_reassess()),
  identical(hashes, vapply(immutable, fp_vs_hash, "")),
  identical(fp_rm_remote_units(retry)$id, c("c0001", "c0002")))
proof = jsonlite::read_json(file.path(retry_dir, fp_rm_base, "commits",
  "c0002", "origin_receipt.json"))$reassessment
stopifnot(identical(proof$capture_revision, capture_revision),
  identical(proof$assessment_revision, assessment_revision),
  identical(proof$previous_status, "blocked_coverage"))
unlink(scratch, recursive = T)
cat("Daily recovery, raw integrity, WebDAV, and ABV checks passed.\n")
