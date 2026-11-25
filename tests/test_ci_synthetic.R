# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING - FLASCHENPOST PREPARATION
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261028

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 0) ESSENTIALS
# <-----------------------------------------------------------------------------

args = commandArgs(trailingOnly = F)
script = sub("^--file=", "", args[grepl("^--file=", args)][1])
root = normalizePath(file.path(dirname(script), ".."), mustWork = T)
bootstrap = new.env(parent = globalenv())
sys.source(file.path(root, "scripts", "bootstrap.R"), envir = bootstrap)
bootstrap$load_regional(root)
fp = fp_load_preparation(root)
sys.source(file.path(root, "R", "publication_scan.R"), envir = fp)
count = new.env(parent = emptyenv())
count$n = 0L
check = \(condition, label) {
  if (!isTRUE(condition)) stop("FAILED: ", label)
  count$n = count$n + 1L
  cat("PASS:", label, "\n")
}
reject = \(code) inherits(tryCatch(force(code), error = identity), "error")
scratch = tempfile("fp_synthetic_ci_")
dir.create(scratch)

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) PINNED RULES AND ARTIFICIAL OBSERVATIONS
# <-----------------------------------------------------------------------------

rules = fp$fp_classifier(fp$collector)
provenance = fp$read_json(file.path(root, "preparation", "rules",
  "apm_20261027", "provenance.json"))
check(length(provenance$definitions) == 23L &&
  rules$classification_version == "beverage-0.7.1" &&
  rules$price_analysis_version == "price-analysis-0.4.1",
  "23 pinned definitions and expected classification versions")
products = data.frame(product_name = c("Synthetic Pils", "Synthetic Wine"),
  designation_raw = c("bier", "wein"),
  retailer_category = c("bier", "weißwein"),
  description_raw = "", alcohol_content = c(5, 12), stringsAsFactors = F)
classified = rules$env$assign_price_categories(
  rules$env$classify_products(products))
check(identical(classified$analysis_category, c("Bier", "Wein")),
  "synthetic beer and wine use the unchanged classification functions")
x = as.data.frame(setNames(lapply(fp$fp_panel_fields,
  \(field) rep(NA_character_, 20L)), fp$fp_panel_fields), stringsAsFactors = F)
x$sku = sprintf("SYN%03d", 1:20)
x$product_key = paste0("synthetic_", 1:20)
x$product_name = paste("Synthetic fixture", 1:20)
x$warehouse_id = 36L
x$price_cents = 420
x$price_eur = 4.2
x$deposit_total_eur = 0.1
x$deposit_per_frame_eur = 0.1
x$pack_count = 1
x$unit_volume_l = 1
x$total_volume_l = 1
x$volume_attribute_l = 1
x$volume_text_l = 1
x$available = T
x$content_kind = "volume"
x$alcohol_content = 5
x$abv_source = "synthetic_test_only"
x$abv_status = "explicit_exact"
x$abv_text_conflict = F
x$abv_filter_min_0_5 = "yes"
x$abv_filter_gt_0_5 = "yes"
x$volume_status = "consistent"
x$price_category = "Bier"
x$category_state = "assigned"
x$inclusion_state = "included"
x$analysis_ready = T
x$analysis_ready_gt_0_5 = T
x$classification_version = rules$classification_version
x$price_analysis_version = rules$price_analysis_version
x$project_status = "none"
x$observed_at = "2026-10-28T08:00:00Z"
x$alcohol_content[2] = 0.5
x$abv_boundary_status[2] = "documented_numeric_no_qualifier"
x$abv_filter_gt_0_5[2] = "no"
x$analysis_ready_gt_0_5[2] = F
x$total_volume_l[3] = NA_real_
x$volume_status[3] = "unresolved_conflict"
x$alcohol_content[4] = NA_real_
x$abv_filter_min_0_5[4] = "unknown"
x$abv_filter_gt_0_5[4] = "unknown"
x$analysis_ready[4] = F
x$analysis_ready_gt_0_5[4] = F
x$available[5] = F
prices = fp$fp_price_readiness(x)
check(sum(prices$price_eligible_primary) == 16L &&
  sum(prices$price_eligible_sensitivity) == 17L,
  "synthetic eligibility distinguishes ABV, volume and availability")
check(is.na(prices$price_eur_per_10g[3]) &&
  identical(prices$analysis_ready, x$analysis_ready),
  "unknown volume remains NA without rewriting classification flags")
check(!prices$price_eligible_primary[2] &&
  prices$price_eligible_sensitivity[2], "half-percent boundary stays explicit")
check(!anyDuplicated(prices$sku) && all(nzchar(
  prices$price_exclusions_primary[!prices$price_eligible_primary])),
  "every excluded artificial observation has explicit reasons")
check(all(grepl("^[0-9a-f]{64}$", fp$fp_load_abv_evidence()$evidence_sha256)),
  "versioned ABV evidence references carry hashes")

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 2) DAILY PANEL, IDEMPOTENCY AND SAFE CHECKPOINTS
# <-----------------------------------------------------------------------------

result = list(status = "inventory_proof_passed", scope_limited = F,
  warehouse_assignment = "verified_for_delivery_address", warehouse_id = 36L,
  reviewed_set = list(match = T), variants = x,
  checks = list(blocking = character(), feature_gaps = "synthetic_only"),
  categories = data.frame(sub_id = 397L, fetch = T),
  listing = data.frame(sub_id = 397L, products = 20L),
  membership = data.frame(sub_id = 397L, sku = x$sku),
  started_at = x$observed_at[1], collector_version = fp$collector$version,
  calls = data.frame(path = "category/warehouse/36", sha256 =
    fp$digest("synthetic evidence only", algo = "sha256", serialize = F)))
store = file.path(scratch, "panel")
check(reject(fp$fp_panel_read(store)), "missing history never auto-initializes")
fp$fp_panel_init(store, mode = "offline_preview")
check(fp$fp_panel_append(result, store)$status == "committed",
  "synthetic inventory can enter an explicitly initialized preview")
head = fp$fp_panel_read(store)$head_sha256
check(fp$fp_panel_append(result, store)$status == "already_committed" &&
  identical(head, fp$fp_panel_read(store)$head_sha256),
  "identical day repetition is byte-preserving and idempotent")
for (case in c("price", "partial", "warehouse", "duplicate", "scope")) {
  bad = result
  if (case == "price") {
    bad$variants$price_cents[1] = 421
    bad$variants$price_eur[1] = 4.21
  }
  if (case == "partial") bad$status = "partial_not_a_result"
  if (case == "warehouse") bad$variants$warehouse_id[1] = 1L
  if (case == "duplicate") bad$variants$sku[1] = bad$variants$sku[2]
  if (case == "scope") bad$scope_limited = T
  check(reject(fp$fp_panel_append(bad, store)) &&
    identical(head, fp$fp_panel_read(store)$head_sha256),
    paste("failed candidate preserves history:", case))
}
checkpoint = file.path(scratch, "checkpoint")
hash = fp$fp_panel_checkpoint(store, checkpoint)
restored = file.path(scratch, "restored")
fp$fp_panel_restore(checkpoint, restored, hash)
check(identical(fp$fp_panel_read(store)$panel,
  fp$fp_panel_read(restored)$panel), "checkpoint restores identical rows")
check(reject(fp$fp_panel_restore(checkpoint, restored, hash)),
  "restore cannot overwrite an existing successful history")
check(reject(fp$fp_panel_restore(checkpoint, file.path(scratch, "bad"),
  paste(rep("0", 64), collapse = ""))), "wrong checkpoint checksum is rejected")
next_day = result
next_day$started_at = "2026-10-29T08:00:00Z"
next_day$variants$observed_at = next_day$started_at
next_day$variants = next_day$variants[-20, ]
next_day$membership = next_day$membership[-20, ]
fp$fp_panel_append(next_day, store)
day = fp$fp_panel_read(store)$rows[[2]]
check(nrow(day) == 20L && day$listing_state[day$sku == "SYN020"] ==
  "not_listed" && is.na(day$price_eur[day$sku == "SYN020"]),
  "missing SKU remains distinct without carrying yesterday's price")

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 3) EXISTING WEBDAV MOCK LOGIC, REUSED WITHOUT ARCHIVED PRODUCT DATA
# <-----------------------------------------------------------------------------
mock_dav = \() {
  state = new.env(parent = emptyenv())
  state$objects = new.env(parent = emptyenv())
  state$directories = character()
  state$fail_method = ""
  state$fail_path = ""
  transport = \(method, path, input = NULL, output = NULL) {
    if (identical(method, state$fail_method) &&
        grepl(state$fail_path, path, fixed = T)) return(500L)
    if (method %in% c("MKCOL", "MKCOL_LOCK")) {
      if (path %in% state$directories) return(405L)
      state$directories = c(state$directories, path)
      return(201L)
    }
    if (method == "DELETE_LOCK") {
      state$directories = setdiff(state$directories, path)
      return(204L)
    }
    if (method == "PROPFIND") {
      keys = ls(state$objects, all.names = T)
      keys = keys[startsWith(keys, paste0(path, "/"))]
      xml = paste0('<d:multistatus xmlns:d="DAV:">', paste0(
        '<d:response><d:href>/', keys, '</d:href></d:response>',
        collapse = ""), '</d:multistatus>')
      writeLines(xml, output)
      return(207L)
    }
    if (method == "GET") {
      if (!exists(path, state$objects, inherits = F)) return(404L)
      writeBin(state$objects[[path]], output)
      return(200L)
    }
    if (method == "PUT_NEW") {
      if (exists(path, state$objects, inherits = F)) return(412L)
      state$objects[[path]] = readBin(input, "raw", file.size(input))
      return(201L)
    }
    stop("Unexpected mock method.")
  }
  list(state = state, transport = transport)
} 
remote = mock_dav()
check(reject(fp$fp_dav_publish(restored, file.path(scratch, "no_seed"),
  remote$transport)), "missing remote state needs explicit initialization")
fp$fp_dav_publish(restored, file.path(scratch, "seed"), remote$transport,
  initialize = T)
snapshot = as.list(remote$state$objects, all.names = T)
fp$fp_dav_publish(restored, file.path(scratch, "repeat"), remote$transport)
check(identical(snapshot, as.list(remote$state$objects, all.names = T)),
  "identical mock upload is idempotent")
remote$state$fail_method = "PUT_NEW"
remote$state$fail_path = "price_panel.rds"
check(reject(fp$fp_dav_publish(store, file.path(scratch, "failed_upload"),
  remote$transport)) && all(vapply(names(snapshot), \(path)
    identical(snapshot[[path]], remote$state$objects[[path]]), logical(1))),
  "interrupted upload preserves all previously successful mock objects")
remote$state$fail_method = "GET"
remote$state$fail_path = "day.rds"
check(reject(fp$fp_dav_restore("2026-10-28", file.path(scratch, "bad_remote"),
  remote$transport, mode = "offline_preview")) &&
  !file.exists(file.path(scratch, "bad_remote")),
  "failed mock restore never activates a partial target")
remote$state$fail_method = ""
fp$fp_dav_publish(store, file.path(scratch, "recovery"), remote$transport)
check(identical(fp$fp_dav_dates(remote$transport),
  c("2026-10-28", "2026-10-29")), "retry preserves both committed mock days")
check(reject(fp$fp_dav_publish(restored, file.path(scratch, "stale"),
  remote$transport)), "stale history cannot replace a newer remote state")

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 4) DEEP PRIVACY INSPECTION AND PORTABILITY
# <-----------------------------------------------------------------------------

safe_rds = file.path(scratch, "safe.rds")
saveRDS(data.frame(sku = "SYN001", price_eur = 4.2), safe_rds)
check(fp$fp_ci_privacy(safe_rds)$status == "passed",
  "compressed RDS data are inspected structurally")
secret_rds = file.path(scratch, "blocked.rds")
saveRDS(list(customer_id = "synthetic"), secret_rds)
check(reject(fp$fp_ci_privacy(secret_rds)),
  "sensitive structured fields are rejected inside RDS")
for (extension in c("json", "csv", "log")) {
  path = file.path(scratch, paste0("blocked.", extension, ".gz"))
  connection = gzfile(path, "wt")
  writeLines(paste0("/", "Users/synthetic/blocked"), connection)
  close(connection)
  check(reject(fp$fp_ci_privacy(path)),
    paste("user paths are rejected after gzip decompression:", extension))
}
binary = file.path(scratch, "unknown.bin")
writeBin(as.raw(c(0, 1, 2)), binary)
check(reject(fp$fp_ci_privacy(binary)), "unreviewed binary formats fail closed")
check(fp$fp_portability_check()$status == "passed",
  "UTF-8, gzip, RDS, temporary writes and Berlin dates pass")
check(identical(fp$fp_portability_check()$linux_executed,
  identical(unname(Sys.info()["sysname"]), "Linux")),
  "actual host is reported without claiming a Linux result")
for (path in c(list.files(file.path(root, "R"), "[.]R$", full.names = T),
    list.files(file.path(root, "scripts"), "[.]R$", full.names = T),
    list.files(file.path(root, "tests"), "[.]R$", full.names = T))) {
  tokens = getParseData(parse(path, keep.source = T))
  check(all(nchar(readLines(path, warn = F)) <= 80) &&
    !any(tokens$token %in% c("LEFT_ASSIGN", "PIPE", "FUNCTION")) &&
    !any(tokens$text[tokens$token == "NUM_CONST"] %in% c("TRUE", "FALSE")),
    paste("active R source style:", basename(path)))
}
unlink(scratch, recursive = T)
cat("Passed", count$n, "synthetic CI checks; no replay or network.\n")
