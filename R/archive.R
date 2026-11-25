# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261030

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) INCREMENTAL UKEBOX ARCHIVE (ADAPTED FROM CONALCO)
# <-----------------------------------------------------------------------------

fp_ar_pack_day = \(root, commit_id, dest) {
  commit = file.path(root, "commits", commit_id)
  if (!dir.exists(commit)) {
    stop("No commit: ", commit_id, call. = F)
  }
  if (file.exists(dest)) {
    stop("Pack destination exists: ", dest, call. = F)
  }
  dir.create(dest, recursive = T)
  files = list.files(commit, full.names = T)
  for (f in files) {
    stopifnot(file.copy(f, file.path(dest, basename(f))))
  }
  manifest = jsonlite::read_json(
    file.path(dest, "manifest.json"), simplifyVector = T
  )
  stopifnot(!is.null(manifest$date))
  fp_vs_hash(file.path(dest, "manifest.json"))
}

fp_ar_pack_increment = \(root, dest,
  last_archived = NULL) {
  history = fp_vs_history(root)
  cids = names(history)
  if (!is.null(last_archived)) {
    cids = cids[cids > last_archived]
  }
  if (!length(cids)) {
    return(list(
      packed = character(),
      hashes = character()
    ))
  }
  hashes = character(length(cids))
  for (i in seq_along(cids)) {
    day_dest = file.path(dest, cids[i])
    hashes[i] = fp_ar_pack_day(root, cids[i], day_dest)
  }
  schema_dest = file.path(dest, "schema.json")
  if (!file.exists(schema_dest)) {
    stopifnot(file.copy(
      file.path(root, "schema.json"), schema_dest
    ))
  }
  index = list(
    packed_at_utc = format(
      Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"
    ),
    last_archived = last_archived,
    dates = cids,
    manifest_hashes = setNames(
      as.list(hashes), cids
    ),
    schema_sha256 = fp_vs_hash(
      file.path(root, "schema.json")
    )
  )
  fp_vs_write_json(index, file.path(dest, "index.json"))
  list(packed = cids, hashes = hashes)
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 2) RESTORE FROM INCREMENTAL ARCHIVE
# <-----------------------------------------------------------------------------

fp_ar_restore_increment = \(archive, target_root) {
  if (dir.exists(target_root)) {
    stop("Target store exists", call. = F)
  }
  index = jsonlite::read_json(
    file.path(archive, "index.json"), simplifyVector = T
  )
  dates = index$dates
  if (!length(dates)) {
    stop("Empty archive", call. = F)
  }
  schema_src = file.path(archive, "schema.json")
  if (!file.exists(schema_src)) {
    stop("Missing schema in archive", call. = F)
  }

  dir.create(target_root, recursive = T)
  dir.create(file.path(target_root, "commits"))
  dir.create(file.path(target_root, "transactions"))
  stopifnot(file.copy(
    schema_src,
    file.path(target_root, "schema.json")
  ))

  for (cid in dates) {
    src = file.path(archive, cid)
    if (!dir.exists(src)) {
      stop("Missing commit in archive: ", cid,
        call. = F)
    }
    dest = file.path(target_root, "commits", cid)
    dir.create(dest)
    files = list.files(src, full.names = T)
    for (f in files) {
      stopifnot(
        file.copy(f, file.path(dest, basename(f)))
      )
    }
  }

  history = fp_vs_history(target_root)
  stopifnot(
    identical(names(history), dates),
    length(history) == length(dates)
  )
  invisible(target_root)
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 3) CHECKPOINT AND INTEGRITY VERIFICATION
# <-----------------------------------------------------------------------------

fp_ar_checkpoint = \(root) {
  history = fp_vs_history(root)
  if (!length(history)) {
    stop("No committed days", call. = F)
  }
  dates = names(history)
  schema_hash = fp_vs_hash(
    file.path(root, "schema.json")
  )

  versions = fp_vs_table(
    root, "product_versions.csv.gz"
  )
  all_obs = fp_vs_table(root, "observations.csv.gz")
  changes = fp_vs_table(root, "product_changes.csv.gz")

  list(
    dates = dates,
    n_days = length(dates),
    n_observations = nrow(all_obs),
    n_products = length(unique(all_obs$sku)),
    n_product_versions = nrow(versions),
    n_changes = nrow(changes),
    n_warehouses = length(unique(all_obs$warehouse_id)),
    warehouses = sort(unique(all_obs$warehouse_id)),
    schema_sha256 = schema_hash,
    head_manifest_sha256 = fp_vs_hash(file.path(
      root, "commits",
      tail(dates, 1L), "manifest.json"
    ))
  )
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 4) WEBDAV ADAPTER (REUSES EXISTING TRANSPORT)
# <-----------------------------------------------------------------------------

fp_ar_webdav_publish = \(root, transport,
  remote_base, last_archived = NULL) {
  tmp = tempfile("fp_increment_")
  on.exit(unlink(tmp, recursive = T), add = T)
  dir.create(tmp)
  inc = fp_ar_pack_increment(
    root, tmp, last_archived
  )
  if (!length(inc$packed)) {
    return(list(
      status = "nothing_to_publish",
      packed = character()
    ))
  }
  files = list.files(tmp, recursive = T,
    full.names = T)
  for (f in files) {
    rel = substring(f, nchar(tmp) + 2L)
    remote = paste0(remote_base, "/", rel)
    transport(f, remote)
  }
  list(
    status = "published",
    packed = inc$packed,
    hashes = inc$hashes
  )
}

fp_ar_webdav_fetch = \(transport, remote_base,
  target, dates = NULL) {
  index_remote = paste0(remote_base, "/index.json")
  index_local = file.path(target, "index.json")
  dir.create(target, recursive = T, showWarnings = F)
  transport(index_remote, index_local, "GET")
  index = jsonlite::read_json(
    index_local, simplifyVector = T
  )
  if (is.null(dates)) dates = index$dates

  schema_remote = paste0(remote_base, "/schema.json")
  schema_local = file.path(target, "schema.json")
  transport(schema_remote, schema_local, "GET")

  for (date in dates) {
    remote_dir = paste0(remote_base, "/", date)
    local_dir = file.path(target, date)
    dir.create(local_dir, recursive = T,
      showWarnings = F)
    transport(
      paste0(remote_dir, "/manifest.json"),
      file.path(local_dir, "manifest.json"), "GET"
    )
    listed = jsonlite::read_json(
      file.path(local_dir, "manifest.json"),
      simplifyVector = T
    )$files$file
    for (f in listed) {
      transport(
        paste0(remote_dir, "/", f),
        file.path(local_dir, f), "GET"
      )
    }
  }
  invisible(target)
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 5) MULTI-WAREHOUSE DAY STATUS
# <-----------------------------------------------------------------------------

fp_ar_day_completeness = \(root, date,
  required_warehouses) {
  history = fp_vs_history(root)
  cid_dates = vapply(
    history, \(m) m$date, ""
  )
  day_idx = which(cid_dates == date)
  if (!length(day_idx)) {
    return(list(
      complete = F,
      date = date,
      committed_warehouses = integer(),
      missing_warehouses = required_warehouses
    ))
  }
  committed = vapply(
    history[day_idx],
    \(m) as.integer(m$warehouse_id),
    integer(1)
  )
  committed = sort(unique(committed))
  missing = setdiff(required_warehouses, committed)
  list(
    complete = length(missing) == 0L,
    date = date,
    committed_warehouses = committed,
    missing_warehouses = missing
  )
}

fp_ar_safe_day_ingest = \(root, shards, receipts,
  warehouse_ids) {
  stopifnot(
    length(shards) == length(receipts),
    length(shards) == length(warehouse_ids)
  )
  results = list()
  for (i in seq_along(shards)) {
    results[[i]] = tryCatch(
      fp_vs_ingest(
        root, shards[i], receipts[i],
        warehouse_ids[i]
      ),
      error = \(e) list(
        status = "failed",
        warehouse_id = warehouse_ids[i],
        error = conditionMessage(e)
      )
    )
  }
  names(results) = paste0("w", warehouse_ids)
  committed = vapply(results, \(r) {
    identical(r, "committed") ||
      identical(r, "already_committed") ||
      identical(r$status, "committed")
  }, logical(1))
  list(results = results, all_committed = all(committed))
}
