# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING - FLASCHENPOST PREPARATION
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261027

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) DAILY CONTRACT AND OBSERVATION STATES
# <-----------------------------------------------------------------------------

fp_panel_version = "flaschenpost-panel-0.1.0"

fp_panel_fields = c("sku", "product_key", "product_name", "price_cents",
  "price_eur", "deposit_total_eur", "deposit_per_frame_eur", "pack_count",
  "unit_volume_l", "total_volume_l", "available", "content_kind",
  "alcohol_content", "abv_source", "abv_status", "abv_text_conflict",
  "abv_boundary_status", "abv_filter_min_0_5", "abv_filter_gt_0_5",
  "volume_status", "volume_attribute_l", "volume_text_l", "price_category",
  "category_state", "inclusion_state", "analysis_ready",
  "analysis_ready_gt_0_5", "classification_version", "price_analysis_version",
  "tax_class", "tax_basis", "product_type", "project_status", "category_rule",
  "refinement_decision", "observed_at")

fp_panel_date = \(time) {
  parsed = as.POSIXct(time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  if (anyNA(parsed)) stop("Invalid observation timestamp.", call. = F)
  format(parsed, "%Y-%m-%d", tz = "Europe/Berlin")
}

fp_panel_candidate = \(result, mode) {
  allowed = if (mode == "production") "complete_candidate" else
    "inventory_proof_passed"
  if (!identical(result$status, allowed) || isTRUE(result$scope_limited) ||
      length(result$checks$blocking) || !isTRUE(result$reviewed_set$match) ||
      result$warehouse_assignment != "verified_for_delivery_address") {
    stop("Only a complete verified inventory can enter a panel.", call. = F)
  }
  x = result$variants
  if (!nrow(x) || anyNA(x$sku) || any(!nzchar(x$sku)) ||
      anyDuplicated(x$sku) || anyNA(x$product_key) ||
      !all(fp_panel_fields %in% names(x)) ||
      any(x$warehouse_id != result$warehouse_id) ||
      anyNA(x$warehouse_id) || length(fp_variant_consistency(x))) {
    stop("Invalid panel identities, schema or analysis variants.", call. = F)
  }
  if (!is.logical(x$available) || anyNA(x$available) ||
      !is.logical(x$analysis_ready) || anyNA(x$analysis_ready) ||
      !is.logical(x$analysis_ready_gt_0_5) ||
      anyNA(x$analysis_ready_gt_0_5) ||
      anyNA(x$price_cents) || any(x$price_cents <= 0) ||
      anyNA(x$price_eur) || any(abs(x$price_eur * 100 -
        x$price_cents) > 1e-6)) stop("Invalid panel values.", call. = F)
  if (any(x$analysis_ready & (is.na(x$price_category) |
      !x$price_category %in% classifier_levels(x)))) stop(
    "Unknown project category.", call. = F)
  todo = result$categories$sub_id[result$categories$fetch]
  if (!setequal(todo, result$listing$sub_id) ||
      anyDuplicated(result$listing$sub_id) ||
      !setequal(x$sku, result$membership$sku)) stop(
    "Panel inventory and category evidence differ.", call. = F)
  paths = result$calls$path
  prefix = paste0("/warehouse/", result$warehouse_id)
  hashes = result$calls$sha256
  if (!length(paths) || any(!endsWith(paths, prefix)) ||
      length(hashes) != length(paths) || anyNA(hashes) ||
      any(!grepl("^[0-9a-f]{64}$", hashes))) stop(
    "Warehouse differs in HTTP evidence.", call. = F)
  date = fp_panel_date(result$started_at)
  if (length(date) != 1L || any(fp_panel_date(x$observed_at) != date)) stop(
    "Collection crosses a Berlin calendar day.", call. = F)
  x = x[order(x$sku), fp_panel_fields, drop = F]
  numeric_fields = c("price_cents", "price_eur", "deposit_total_eur",
    "deposit_per_frame_eur", "pack_count", "unit_volume_l", "total_volume_l",
    "alcohol_content", "volume_attribute_l", "volume_text_l")
  for (field in numeric_fields) x[[field]] = as.numeric(x[[field]])
  rownames(x) = NULL
  x$retailer = "flaschenpost"
  x$warehouse_id = as.integer(result$warehouse_id)
  x$run_date = date
  x$listing_state = "listed"
  x$availability_state = ifelse(x$available, "available", "unavailable")
  x$quality_status = ifelse(grepl("unresolved", x$volume_status),
    "volume_conflict", ifelse(x$analysis_ready, "analysis_ready",
    ifelse(x$content_kind != "volume", "excluded_nonbeverage",
      ifelse(is.na(x$alcohol_content), "missing_abv",
        ifelse(grepl("unresolved", x$volume_status), "volume_conflict",
          "excluded_or_classification_review")))))
  x
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 2) IMMUTABLE SHARDS, HASH CHAIN AND EXPLICIT INITIALIZATION
# <-----------------------------------------------------------------------------

fp_panel_init = \(store, warehouse_id = collector$warehouse_id,
  mode = c("production", "offline_preview")) {
  mode = match.arg(mode)
  fp_private_dir(store, runner)
  if (file.exists(store)) stop("Panel store already exists.", call. = F)
  parent = dirname(store)
  dir.create(parent, recursive = T, showWarnings = F)
  pending = tempfile(".fp_init_", tmpdir = parent)
  dir.create(pending)
  on.exit(unlink(pending, recursive = T), add = T)
  write_json(list(version = fp_panel_version, retailer = "flaschenpost",
    warehouse_id = as.integer(warehouse_id), mode = mode),
    file.path(pending, "identity.json"), auto_unbox = T, pretty = T)
  if (!file.rename(pending, store)) stop("Cannot initialize panel.")
  invisible(store)
}

fp_panel_read = \(store) {
  identity_file = file.path(store, "identity.json")
  if (!file.exists(identity_file)) stop(
    "Panel missing: explicit initialization or verified restore required.",
    call. = F)
  files = list.files(store, recursive = T, full.names = T, all.files = T,
    no.. = T)
  if (any(nzchar(Sys.readlink(c(store, files))))) stop(
    "Symlinks are forbidden in panel state.", call. = F)
  identity = read_json(identity_file, simplifyVector = T)
  if (!identical(identity$version, fp_panel_version) ||
      !identical(identity$retailer, "flaschenpost") ||
      !identity$mode %in% c("production", "offline_preview") ||
      length(identity$warehouse_id) != 1L || is.na(identity$warehouse_id)) {
    stop("Invalid panel identity.", call. = F)
  }
  days = sort(list.files(store, "^[0-9]{4}-[0-9]{2}-[0-9]{2}$"))
  receipts = list()
  rows = list()
  previous = fp_hash(identity_file)
  for (date in days) {
    path = file.path(store, date)
    receipt_file = file.path(path, "receipt.json")
    payload = file.path(path, "day.rds")
    if (!all(file.exists(c(receipt_file, payload)))) stop(
      "Incomplete committed day.", call. = F)
    receipt = read_json(receipt_file, simplifyVector = T)
    if (!identical(receipt$previous_sha256, previous) ||
        !identical(receipt$date, date) ||
        !identical(receipt$mode, identity$mode) ||
        !identical(receipt$version, identity$version) ||
        receipt$warehouse_id != identity$warehouse_id ||
        !identical(receipt$sha256, fp_hash(payload))) stop(
      "Panel receipt or payload checksum mismatch.", call. = F)
    day = readRDS(payload)
    required = c(fp_panel_fields, "retailer", "warehouse_id", "run_date",
      "listing_state", "availability_state", "quality_status")
    if (!is.data.frame(day) || !nrow(day) ||
        !identical(names(day), required) || anyNA(day$sku) ||
        anyDuplicated(day$sku) || any(day$retailer != identity$retailer) ||
        any(day$warehouse_id != identity$warehouse_id) ||
        any(day$run_date != date) || nrow(day) != receipt$n_rows ||
        length(fp_variant_consistency(day))) stop(
      "Panel shard identities or values differ.", call. = F)
    listed = day[day$listing_state == "listed", , drop = F]
    rownames(listed) = NULL
    absent = day[day$listing_state == "not_listed", , drop = F]
    if (anyNA(day$listing_state) || any(!day$listing_state %in%
        c("listed", "not_listed")) || nrow(listed) != receipt$n_listed ||
        !identical(digest(listed, algo = "sha256"),
          receipt$candidate_sha256) ||
        any(fp_panel_date(listed$observed_at) != date) ||
        any(!is.na(absent$price_eur)) || any(!is.na(absent$available)) ||
        any(absent$analysis_ready) || any(absent$analysis_ready_gt_0_5)) stop(
      "Panel observation states or candidate identity differ.", call. = F)
    rows[[date]] = day
    receipts[[date]] = receipt
    previous = fp_hash(receipt_file)
  }
  # Pending directories are deliberately ignored after a process interruption.
  list(identity = identity, days = days, rows = rows, receipts = receipts,
    head_sha256 = previous, panel = fp_rows(rows))
}

# A derived view preserves immutable shards and their historical flags.
fp_panel_prices = \(store, availability_policy = "available_only") {
  history = fp_panel_read(store)
  if (!nrow(history$panel)) stop("No observations for price analysis.")
  fp_price_readiness(history$panel, availability_policy)
}

fp_panel_append = \(result, store) {
  fp_private_dir(store, runner)
  lock = paste0(store, ".writer_lock")
  if (!dir.create(lock, showWarnings = F)) stop(
    "Panel writer lock exists; inspect interrupted writer first.", call. = F)
  on.exit(unlink(lock, recursive = T), add = T)
  history = fp_panel_read(store)
  day = fp_panel_candidate(result, history$identity$mode)
  if (any(day$warehouse_id != history$identity$warehouse_id)) stop(
    "Warehouse differs from panel history.", call. = F)
  date = unique(day$run_date)
  # Hash includes timestamps and all retained values: changed retries conflict.
  fingerprint = digest(day, algo = "sha256")
  if (date %in% history$days) {
    if (!identical(fingerprint,
        history$receipts[[date]]$candidate_sha256)) stop(
      "Conflicting repeat day: historical observations are immutable.",
      call. = F)
    return(invisible(list(status = "already_committed", date = date)))
  }
  if (length(history$days) && date < tail(history$days, 1L)) stop(
    "Out-of-order daily append rejected.", call. = F)
  if (length(history$rows)) {
    last = history$rows[[length(history$rows)]]
    linked = match(day$sku, last$sku)
    if (any(day$product_key != last$product_key[linked], na.rm = T)) stop(
      "Known SKU changed product identity; review before appending.")
    missing = last[!last$sku %in% day$sku, , drop = F]
    listed = last$sku[last$listing_state == "listed"]
    if (length(setdiff(listed, day$sku)) / max(1L, length(listed)) > 0.1) {
      stop("Over 10 percent of last listed SKUs missing; review inventory.",
        call. = F)
    }
    if (nrow(missing)) {
      # Absence is an observation state, never yesterday's measured price.
      for (field in c("price_cents", "price_eur", "deposit_total_eur",
          "deposit_per_frame_eur", "alcohol_content", "pack_count",
          "unit_volume_l", "total_volume_l", "volume_attribute_l",
          "volume_text_l")) missing[[field]] = NA_real_
      missing$available = NA
      missing$observed_at = NA_character_
      missing$run_date = date
      missing$listing_state = "not_listed"
      missing$availability_state = "not_observed"
      missing$quality_status = "not_observed"
      missing$abv_source = NA_character_
      missing$abv_status = "not_observed"
      missing$abv_filter_min_0_5 = "unknown"
      missing$abv_filter_gt_0_5 = "unknown"
      missing$inclusion_state = "not_observed"
      missing$category_state = "not_observed"
      missing$analysis_ready = F
      missing$analysis_ready_gt_0_5 = F
      day = rbind(day, missing)
    }
  }
  day = day[order(day$sku), , drop = F]
  rownames(day) = NULL
  pending = tempfile(".pending_", tmpdir = store)
  dir.create(pending)
  on.exit(unlink(pending, recursive = T), add = T)
  saveRDS(day, file.path(pending, "day.rds"), version = 3)
  sources = c(list.files(file.path(preparation_root, "R"), "[.]R$",
    full.names = T), list.files(file.path(preparation_root, "config"),
    full.names = T),
    file.path(dirname(preparation_root), "scripts", "bootstrap.R"))
  receipt = list(date = date, mode = history$identity$mode,
    version = fp_panel_version, warehouse_id = history$identity$warehouse_id,
    n_rows = nrow(day), n_listed = nrow(result$variants),
    candidate_sha256 = fingerprint,
    previous_sha256 = history$head_sha256,
    sha256 = fp_hash(file.path(pending, "day.rds")),
    source_calls = result$calls,
    source_code = as.list(setNames(vapply(sources, fp_hash, ""),
      substring(sources, nchar(preparation_root) + 2L))),
    classifier_source_sha256 = as.list(fp_classifier(collector)$sha256),
    inventory_status = result$status, feature_gaps = result$checks$feature_gaps,
    collector_version = result$collector_version)
  write_json(receipt, file.path(pending, "receipt.json"),
    auto_unbox = T, pretty = T, na = "null")
  if (!identical(fp_panel_read(store)$head_sha256, history$head_sha256)) {
    stop("History changed during append.", call. = F)
  }
  if (!file.rename(pending, file.path(store, date))) stop(
    "Cannot atomically commit panel day.", call. = F)
  invisible(list(status = "committed", date = date, n_rows = nrow(day)))
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 3) PORTABLE CHECKPOINT AND FAIL-CLOSED RESTORE (NO NETWORK)
# <-----------------------------------------------------------------------------

fp_checkpoint_reports = c("price_panel.rds", "price_quality.csv",
  "price_metadata.json")

fp_checkpoint_member = \(paths) paths %in% c("identity.json",
  fp_checkpoint_reports) | grepl(paste0(
    "^[0-9]{4}-[0-9]{2}-[0-9]{2}/(day[.]rds|receipt[.]json)$"), paths)

fp_panel_checkpoint = \(store, destination) {
  history = fp_panel_read(store)
  if (!length(history$days)) stop("Cannot checkpoint empty history.")
  if (file.exists(destination)) stop("Checkpoint destination exists.")
  dir.create(destination, recursive = T)
  on.exit(if (!file.exists(file.path(destination, "checkpoint.json")))
    unlink(destination, recursive = T), add = T)
  paths = c("identity.json", unlist(lapply(history$days, \(date)
    file.path(date, c("day.rds", "receipt.json")))))
  for (path in paths) {
    target = file.path(destination, path)
    dir.create(dirname(target), recursive = T, showWarnings = F)
    if (!file.copy(file.path(store, path), target, overwrite = F)) stop(
      "Checkpoint copy failed.")
  }
  copied = fp_panel_read(destination)
  if (!identical(copied$head_sha256, history$head_sha256)) stop(
    "Checkpoint history changed during copy.")
  prices = fp_price_readiness(copied$panel)
  saveRDS(prices, file.path(destination, "price_panel.rds"), version = 3)
  write.csv(fp_price_exclusions(prices), file.path(destination,
    "price_quality.csv"), row.names = F, na = "")
  write_json(list(version = fp_analysis_version,
    availability_policy = "available_only", source_head = history$head_sha256,
    panel_sha256 = fp_hash(file.path(destination, "price_panel.rds")),
    rules_sha256 = as.list(fp_classifier(collector)$sha256)),
    file.path(destination, "price_metadata.json"), pretty = T, auto_unbox = T)
  paths = c(paths, fp_checkpoint_reports)
  write_json(list(version = fp_panel_version, days = history$days,
    head_sha256 = history$head_sha256, files = lapply(paths, \(path)
      list(path = path, sha256 = fp_hash(file.path(destination, path))))),
    file.path(destination, "checkpoint.json"), pretty = T, auto_unbox = T)
  invisible(fp_hash(file.path(destination, "checkpoint.json")))
}

fp_panel_restore = \(checkpoint, target, expected_sha256) {
  # Caller obtains the expected hash from an immutable durable commit.
  manifest = file.path(checkpoint, "checkpoint.json")
  if (!file.exists(manifest) || !identical(fp_hash(manifest),
      expected_sha256)) stop("Checkpoint manifest checksum mismatch.")
  receipt = read_json(manifest, simplifyVector = F)
  paths = vapply(receipt$files, `[[`, "", "path")
  safe = fp_checkpoint_member(paths)
  if (!length(paths) || any(!safe) || anyDuplicated(paths) ||
      !"identity.json" %in% paths) stop("Unsafe checkpoint paths.")
  actual = list.files(checkpoint, recursive = T, all.files = T,
    full.names = T, no.. = T)
  if (any(nzchar(Sys.readlink(c(checkpoint, actual))))) stop(
    "Checkpoint symlinks are forbidden.")
  actual = substring(actual[!dir.exists(actual)], nchar(checkpoint) + 2L)
  if (!setequal(actual, c(paths, "checkpoint.json"))) stop(
    "Checkpoint file inventory differs.")
  for (i in seq_along(paths)) {
    if (!file.exists(file.path(checkpoint, paths[i])) || !identical(
        fp_hash(file.path(checkpoint, paths[i])),
        receipt$files[[i]]$sha256)) stop("Checkpoint file checksum mismatch.")
  }
  checked = fp_panel_read(checkpoint)
  expected_paths = c("identity.json", unlist(lapply(checked$days, \(date)
    file.path(date, c("day.rds", "receipt.json")))))
  if (any(paths %in% fp_checkpoint_reports)) {
    expected_paths = c(expected_paths, fp_checkpoint_reports)
    if (!all(fp_checkpoint_reports %in% paths)) stop(
      "Incomplete checkpoint price reports.")
    prices = readRDS(file.path(checkpoint, "price_panel.rds"))
    metadata = read_json(file.path(checkpoint, "price_metadata.json"),
      simplifyVector = T)
    quality = read.csv(file.path(checkpoint, "price_quality.csv"),
      stringsAsFactors = F, colClasses = "character")
    if (!identical(metadata$version, fp_analysis_version) ||
        !identical(metadata$source_head, checked$head_sha256) ||
        !identical(metadata$panel_sha256,
          fp_hash(file.path(checkpoint, "price_panel.rds"))) ||
        !identical(unlist(metadata$rules_sha256),
          fp_classifier(collector)$sha256) ||
        !identical(prices, fp_price_readiness(checked$panel)) ||
        !identical(quality, fp_price_exclusions(prices))) stop(
      "Checkpoint analytical view differs from immutable observations.")
  }
  if (!setequal(paths, expected_paths) ||
      !identical(checked$days, unlist(receipt$days)) ||
      !identical(checked$head_sha256, receipt$head_sha256)) stop(
    "Checkpoint semantic inventory differs.")
  fp_private_dir(target, runner)
  if (file.exists(target)) stop("Restore target must not exist.")
  dir.create(dirname(target), recursive = T, showWarnings = F)
  pending = tempfile(".fp_restore_", tmpdir = dirname(target))
  dir.create(pending)
  on.exit(unlink(pending, recursive = T), add = T)
  for (path in paths) {
    destination = file.path(pending, path)
    dir.create(dirname(destination), recursive = T, showWarnings = F)
    if (!file.copy(file.path(checkpoint, path), destination)) stop(
      "Restore copy failed.")
  }
  if (!identical(fp_panel_read(pending)$head_sha256,
      checked$head_sha256)) stop("Restored history differs.")
  if (!file.rename(pending, target)) stop("Cannot activate restored history.")
  invisible(fp_panel_read(target))
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 4) DAILY HTTP ORCHESTRATION (NO SCHEDULE, RELEASES STILL REQUIRED)
# <-----------------------------------------------------------------------------

fp_collect_day = \(ctx, store, clock = Sys.time) {
  # Restore must already have succeeded; never fall back to an empty checkout.
  history = fp_panel_read(store)
  if (history$identity$mode != "production") stop("Preview is not production.")
  date = format(clock(), "%Y-%m-%d", tz = "Europe/Berlin")
  if (date %in% history$days) return(invisible(list(
    status = "already_committed", date = date)))
  if (!isTRUE(ctx$runner$live_authorized) ||
      !isTRUE(ctx$runner$terms_cleared)) stop("Daily release is missing.")
  if (is.null(ctx$raw_dir) || is.null(ctx$log_file)) stop(
    "Daily collection needs private raw evidence and a separate run log.")
  fp_private_dir(ctx$raw_dir, ctx$runner)
  fp_private_dir(dirname(ctx$log_file), ctx$runner)
  if (file.exists(ctx$log_file) || length(list.files(ctx$raw_dir,
      all.files = T, no.. = T))) stop("Daily run evidence paths must be fresh.")
  dir.create(dirname(ctx$log_file), recursive = T, showWarnings = F)
  result = tryCatch(fp_collect(ctx, history$identity$warehouse_id,
    "production", classifier = fp_classifier(ctx$collector),
    brand_checks = 8L, replicate_lists = 3L,
    probe_ids = as.integer(ctx$collector$probe_outside_scope_ids)),
    error = \(e) {
      fp_log(ctx$log_file, "daily_failed", fp_no_secret,
        reason = conditionMessage(e))
      stop(e)
    })
  if (!identical(fp_panel_date(result$started_at), date)) stop(
    "Collector started on another day.")
  fp_panel_append(result, store)
}
