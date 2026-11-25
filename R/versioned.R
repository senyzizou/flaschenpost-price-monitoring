# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261030

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) VERSIONED STORAGE CORE (ADAPTED FROM CONALCO-VERSIONED-1.0)
# <-----------------------------------------------------------------------------

fp_vs_dependencies = \() {
  for (pkg in c("jsonlite", "digest")) {
    if (!requireNamespace(pkg, quietly = T)) {
      stop("Missing: ", pkg, call. = F)
    }
  }
  invisible(T)
}

fp_vs_read_gz = \(path) {
  con = gzfile(path, "rb")
  on.exit(close(con))
  chunks = list()
  repeat {
    bytes = readBin(con, "raw", n = 8L * 1024L^2)
    if (!length(bytes)) break
    chunks[[length(chunks) + 1L]] = bytes
  }
  text = rawToChar(do.call(c, chunks))
  Encoding(text) = "UTF-8"
  lines = strsplit(text, "\n", fixed = T)[[1]]
  quotes = lengths(regmatches(
    lines, gregexpr('"', lines, fixed = T, useBytes = T)
  ))
  ends = which(cumsum(quotes) %% 2L == 0L)
  stopifnot(length(ends) > 0L, tail(ends, 1L) == length(lines))
  starts = c(1L, head(ends, -1L) + 1L)
  records = vapply(seq_along(ends), \(i) {
    paste(lines[starts[i]:ends[i]], collapse = "\n")
  }, "")
  marker = "__APM_UNQUOTED_MISSING__"
  while (any(grepl(marker, records, fixed = T))) {
    marker = paste0(marker, "_")
  }
  pattern = paste0(
    '"(?:[^"]++|"")*"(*SKIP)(*F)|',
    '(?<=,)(?=,|\\r?$)|^(?=,|\\r?$)'
  )
  records = gsub(pattern, marker, records,
    perl = T, useBytes = T)
  read.csv(text = records, colClasses = "character",
    na.strings = marker, check.names = F,
    strip.white = F, comment.char = "",
    blank.lines.skip = F, fill = F, encoding = "UTF-8")
}

fp_vs_write_gz = \(x, path, level = 6L) {
  if (file.exists(path)) stop("Refusing overwrite: ", path)
  con = gzfile(path, "wt", compression = level)
  on.exit(close(con))
  write.table(x, con, sep = ",", row.names = F, na = "",
    qmethod = "double", fileEncoding = "UTF-8")
  invisible(path)
}

fp_vs_write_json = \(x, path) {
  if (file.exists(path)) {
    stop("Refusing overwrite: ", path, call. = F)
  }
  jsonlite::write_json(x, path, pretty = T,
    auto_unbox = T, na = "null", null = "null")
}

fp_vs_hash = \(path) digest::digest(file = path, algo = "sha256")

fp_vs_content_hash = \(values) {
  encoded = ifelse(is.na(values), "N",
    paste0("V", nchar(values, type = "bytes"), ":", values))
  digest::digest(paste0(encoded, collapse = ""),
    algo = "sha256", serialize = F)
}

fp_vs_version_id = \(product_key, previous, content) {
  fp_vs_content_hash(c(product_key, previous, content))
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 2) SCHEMA AND STORE INITIALIZATION
# <-----------------------------------------------------------------------------

fp_vs_observation_columns = \() {
  c("retailer", "warehouse_id", "sku", "product_key",
    "observation_key", "daily_date", "run_id",
    "observed_at", "price_cents", "price_eur",
    "deposit_total_eur", "deposit_per_frame_eur",
    "available", "listing_state", "availability_state",
    "article_status", "sales_status",
    "regional_price", "reduced_price_cents",
    "reduced_10_cents", "reduced_20_cents",
    "reduced_30_cents", "reduced_40_cents",
    "quality_status", "collection_method",
    "listing_metadata_file", "raw_sha256",
    "product_version_id")
}

fp_vs_tracked_columns = \() {
  c("product_name", "brand", "article_description",
    "container_type", "subcategory_ids",
    "alcohol_content", "abv_source", "abv_status",
    "abv_text_conflict", "abv_boundary_status",
    "pack_count", "unit_volume_l", "total_volume_l",
    "content_kind", "volume_status",
    "volume_attribute_l", "volume_text_l",
    "price_category", "category_state",
    "classification_version", "price_analysis_version",
    "tax_class", "tax_basis", "product_type",
    "category_rule", "refinement_decision",
    "inclusion_state", "abv_filter_min_0_5",
    "abv_filter_gt_0_5", "project_status")
}

fp_vs_core_columns = \() {
  c("product_name", "brand", "article_description",
    "container_type", "alcohol_content", "abv_source",
    "abv_status", "pack_count", "unit_volume_l",
    "total_volume_l", "content_kind", "volume_status",
    "price_category", "classification_version",
    "tax_class", "product_type", "inclusion_state",
    "abv_filter_min_0_5", "abv_filter_gt_0_5")
}

fp_vs_schema = \() {
  obs = fp_vs_observation_columns()
  tracked = fp_vs_tracked_columns()
  core = fp_vs_core_columns()
  list(
    format_version = "flaschenpost-versioned-1.0",
    observation_columns = obs,
    tracked = tracked,
    core = core,
    moved = setdiff(tracked, core),
    product_id_column = "sku",
    product_key_column = "product_key",
    warehouse_column = "warehouse_id",
    import = paste0(
      "base R; quote-aware empty fields; ",
      "character; UTF-8"
    ),
    compression_level = 6L,
    versions = paste0(
      "Immutable episodes; ",
      "warehouse-independent product identity"
    )
  )
}

fp_vs_init = \(root) {
  fp_vs_dependencies()
  if (dir.exists(root)) stop("Store already exists", call. = F)
  dir.create(root, recursive = T)
  dir.create(file.path(root, "commits"))
  dir.create(file.path(root, "transactions"))
  schema = fp_vs_schema()
  fp_vs_write_json(schema, file.path(root, "schema.json"))
  invisible(root)
}

fp_vs_read_schema = \(root) {
  jsonlite::read_json(
    file.path(root, "schema.json"), simplifyVector = T
  )
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 3) COMMIT CHAIN VALIDATION
# <-----------------------------------------------------------------------------

fp_vs_commit_id = \(date, warehouse_id) {
  paste0(date, "_w", warehouse_id)
}

fp_vs_parse_commit_id = \(id) {
  m = regmatches(
    id, regexec("^(\\d{4}-\\d{2}-\\d{2})_w(\\d+)$", id)
  )[[1]]
  if (length(m) != 3L) {
    stop("Invalid commit id: ", id, call. = F)
  }
  list(date = m[2], warehouse_id = as.integer(m[3]))
}

fp_vs_seq_dir = \(n) sprintf("c%04d", n)

fp_vs_find_commit = \(root, date, warehouse_id) {
  history = fp_vs_history(root)
  wid = as.integer(warehouse_id)
  for (i in seq_along(history)) {
    m = history[[i]]
    if (identical(m$date, date) &&
        identical(
          as.integer(m$warehouse_id), wid
        )) {
      return(names(history)[i])
    }
  }
  NULL
}

fp_vs_optional_files = \() {
  c("category_tree.csv.gz", "category_membership.csv.gz",
    "category_changes.csv.gz", "assortment_changes.csv.gz",
    "quality_warnings.csv.gz", "regional_differences.csv.gz",
    "product_details.csv.gz", "detail_links.csv.gz",
    "classification_versions.csv.gz", "volume_resolutions.csv.gz",
    "version_assessments.csv.gz")
}

fp_vs_validate_commit = \(path, schema_hash, previous) {
  manifest = jsonlite::read_json(
    file.path(path, "manifest.json"), simplifyVector = T
  )
  stopifnot(
    identical(manifest$schema_sha256, schema_hash),
    identical(manifest$previous_commit_sha256, previous)
  )
  files = manifest$files
  expected = c(
    "observations.csv.gz", "product_versions.csv.gz",
    "product_changes.csv.gz", "version_usage.csv.gz",
    "origin_receipt.json"
  )
  stopifnot(
    !anyDuplicated(files$file),
    all(expected %in% files$file),
    all(files$file %in% c(
      expected, "statistics.csv", fp_vs_optional_files()
    )),
    !any(grepl("[/\\\\]", files$file))
  )
  for (i in seq_len(nrow(files))) {
    p = file.path(path, files$file[i])
    if (!file.exists(p) || file.size(p) != files$bytes[i] ||
        fp_vs_hash(p) != files$sha256[i]) {
      stop("Corrupt or missing: ", p, call. = F)
    }
  }
  manifest
}

fp_vs_history = \(root) {
  dirs = sort(list.dirs(
    file.path(root, "commits"),
    recursive = F, full.names = T
  ))
  schema_hash = fp_vs_hash(
    file.path(root, "schema.json")
  )
  previous = "GENESIS"
  result = vector("list", length(dirs))
  for (i in seq_along(dirs)) {
    result[[i]] = fp_vs_validate_commit(
      dirs[i], schema_hash, previous
    )
    previous = fp_vs_hash(
      file.path(dirs[i], "manifest.json")
    )
  }
  names(result) = basename(dirs)
  result
}

fp_vs_table = \(root, filename,
  commit_ids = names(fp_vs_history(root))) {
  if (!length(commit_ids)) return(NULL)
  fp_an_bind(lapply(commit_ids, \(cid) {
    path = file.path(root, "commits", cid, filename)
    if (!file.exists(path) &&
        filename %in% fp_vs_optional_files()) {
      return(NULL)
    }
    fp_vs_read_gz(path)
  }))
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 4) CHANGE TYPE CLASSIFICATION
# <-----------------------------------------------------------------------------

fp_vs_change_type = \(old, new, previously_known = T) {
  if (is.na(old) && !is.na(new) && new == "") {
    return("missing_to_empty")
  }
  if (!is.na(old) && old == "" && is.na(new)) {
    return("empty_to_missing")
  }
  if (!is.na(old) && old == "" && !is.na(new) && new != "") {
    return("empty_to_known")
  }
  if (!is.na(old) && old != "" && !is.na(new) && new == "") {
    return("known_to_empty")
  }
  if (is.na(old) && !is.na(new)) {
    return(if (previously_known) "missing_to_known" else
      "first_known_after_missing")
  }
  if (!is.na(old) && is.na(new)) return("known_to_missing")
  "observed_value_change"
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 5) MULTI-WAREHOUSE PRODUCT IDENTITY
# <-----------------------------------------------------------------------------

fp_vs_product_key = \(sku) {
  paste0("flaschenpost:", sku)
}

fp_vs_warehouse_divergence = \(versions, sku,
  warehouse_ids) {
  pv = versions[versions$sku == sku, , drop = F]
  if (!nrow(pv)) return(NULL)
  latest = pv[!duplicated(pv$sku, fromLast = T), ,
    drop = F]
  list(
    sku = sku,
    n_versions = nrow(pv),
    latest_hash = latest$content_sha256[1],
    warehouses_observed = unique(pv$source_warehouse_id)
  )
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 6) DAILY INGEST: ATOMIC, IDEMPOTENT, SEQUENTIAL
# <-----------------------------------------------------------------------------

fp_vs_ingest = \(root, shard, receipt, warehouse_id,
  statistics = NULL, fail_at = NULL, extra_tables = NULL) {
  if (length(extra_tables)) {
    stopifnot(
      all(names(extra_tables) %in% fp_vs_optional_files()),
      !anyDuplicated(names(extra_tables))
    )
  }
  fp_vs_dependencies()
  lock = file.path(root, ".writer_lock")
  if (!dir.create(lock, showWarnings = F)) {
    stop("Writer lock exists", call. = F)
  }
  on.exit(unlink(lock, recursive = T), add = T)
  fp_vs_write_json(
    list(
      pid = Sys.getpid(),
      acquired_at_utc = format(
        Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"
      )
    ),
    file.path(lock, "owner.json")
  )

  history = fp_vs_history(root)
  s = fp_vs_read_schema(root)
  meta = jsonlite::read_json(receipt, simplifyVector = T)
  meta$raw_archive = NULL
  meta$created_at_utc = NULL
  receipt_hash = fp_vs_content_hash(
    jsonlite::toJSON(meta, auto_unbox = T, null = "null")
  )
  input_hash = fp_vs_hash(shard)
  date = meta$date
  run_id = meta$run_id

  if (!is.null(statistics)) {
    statistics = as.data.frame(
      lapply(statistics, as.character),
      stringsAsFactors = F
    )
    stopifnot(nrow(statistics) > 0L,
      all(statistics$date == date))
  }

  logical_key = fp_vs_commit_id(date, warehouse_id)

  if (!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", date) ||
      is.na(as.Date(date))) {
    stop("Invalid commit date", call. = F)
  }

  wid_int = as.integer(warehouse_id)
  existing_idx = NULL
  for (i in seq_along(history)) {
    m = history[[i]]
    if (identical(m$date, date) &&
        identical(
          as.integer(m$warehouse_id), wid_int
        )) {
      existing_idx = i
      break
    }
  }

  if (!is.null(existing_idx)) {
    prior = history[[existing_idx]]
    if (prior$input_sha256 != input_hash ||
        prior$run_id != run_id ||
        prior$receipt_sha256 != receipt_hash) {
      stop(
        "Conflicting input for committed date",
        call. = F
      )
    }
    return(invisible("already_committed"))
  }

  commit_seq = length(history) + 1L
  commit_dir = fp_vs_seq_dir(commit_seq)

  x = fp_vs_read_gz(shard)
  tracked = s$tracked

  x$retailer = "flaschenpost"
  x$warehouse_id = as.character(warehouse_id)
  x$daily_date = date

  if (!"observation_key" %in% names(x) ||
      anyNA(x$observation_key)) {
    x$observation_key = paste(
      "flaschenpost",
      paste0("w", warehouse_id),
      x$sku, date, sep = "|"
    )
  }
  if (!"product_key" %in% names(x) ||
      anyNA(x$product_key)) {
    x$product_key = fp_vs_product_key(x$sku)
  }

  obs_cols = setdiff(
    s$observation_columns, "product_version_id"
  )
  all_cols = unique(c(obs_cols, tracked))
  for (mc in setdiff(all_cols, names(x))) {
    x[[mc]] = NA_character_
  }

  x = x[order(x$sku), ]
  rownames(x) = NULL

  stopifnot(
    !anyNA(x$sku), all(nzchar(x$sku)),
    !anyDuplicated(x$sku),
    !anyDuplicated(x$observation_key),
    all(x$warehouse_id == as.character(warehouse_id))
  )

  if (length(history)) {
    if (run_id %in% vapply(history, \(r) r$run_id, "")) {
      stop("Run-ID already committed on another date",
        call. = F)
    }
  }

  versions = fp_vs_table(
    root, "product_versions.csv.gz", names(history)
  )
  prior_usage = fp_vs_table(
    root, "version_usage.csv.gz", names(history)
  )

  plan = fp_an_plan(
    root, x, tracked, versions, prior_usage, warehouse_id, date,
    run_id
  )
  ids = plan$ids
  old_at = plan$old_at
  changed = plan$changed
  new_v = plan$new_v
  change_rows = plan$change_rows
  extra_tables = c(
    extra_tables,
    plan$tables[!vapply(plan$tables, is.null, logical(1))]
  )
  change_cols = c(
    "retailer", "sku", "product_key", "warehouse_id",
    "date", "observed_at", "run_id", "field",
    "old_version_id", "new_version_id", "change_type"
  )
  changes = if (length(change_rows)) {
    do.call(rbind, change_rows)
  } else {
    as.data.frame(
      setNames(
        rep(list(character()), length(change_cols)),
        change_cols
      )
    )
  }

  obs_out_cols = intersect(obs_cols, names(x))
  obs = x[obs_out_cols]
  obs$product_version_id = ids

  usage = data.frame(
    product_key = x$product_key,
    sku = x$sku,
    observation_key = x$observation_key,
    warehouse_id = as.character(warehouse_id),
    daily_date = date,
    observed_at = x$observed_at,
    run_id = run_id,
    product_version_id = ids,
    content_changed = ifelse(changed, "TRUE", "FALSE"),
    stringsAsFactors = F
  )

  dir.create(
    file.path(root, "transactions"), showWarnings = F
  )
  pending = tempfile(
    "pending_",
    tmpdir = file.path(root, "transactions")
  )
  dir.create(pending)

  fp_vs_write_gz(
    new_v, file.path(pending, "product_versions.csv.gz")
  )
  if (identical(fail_at, "after_versions")) {
    stop("Injected interruption", call. = F)
  }
  fp_vs_write_gz(
    obs, file.path(pending, "observations.csv.gz")
  )
  fp_vs_write_gz(
    changes, file.path(pending, "product_changes.csv.gz")
  )
  fp_vs_write_gz(
    usage, file.path(pending, "version_usage.csv.gz")
  )
  stopifnot(file.copy(
    receipt, file.path(pending, "origin_receipt.json")
  ))

  if (!is.null(statistics)) {
    write.csv(statistics,
      file.path(pending, "statistics.csv"),
      row.names = F, na = "")
  }
  for (nm in names(extra_tables)) {
    fp_vs_write_gz(extra_tables[[nm]], file.path(pending, nm))
  }

  files = sort(list.files(pending))
  records = data.frame(
    file = files,
    bytes = file.size(file.path(pending, files)),
    sha256 = vapply(
      file.path(pending, files), fp_vs_hash, ""
    )
  )
  rownames(records) = NULL

  prev_hash = if (!length(history)) "GENESIS" else {
    last_cid = tail(names(history), 1L)
    fp_vs_hash(file.path(
      root, "commits", last_cid, "manifest.json"
    ))
  }

  manifest = list(
    commit_seq = commit_seq,
    logical_key = logical_key,
    date = date,
    run_id = run_id,
    warehouse_id = as.integer(warehouse_id),
    committed_at_utc = format(
      Sys.time(), "%Y-%m-%dT%H:%M:%SZ",
      tz = "UTC"
    ),
    n_products = nrow(x),
    new_products = sum(is.na(old_at)),
    new_versions = nrow(new_v),
    changed_products = sum(changed & !is.na(old_at)),
    first_capture_links = plan$counts$first_capture_links,
    classification_rows = plan$counts$classification_rows,
    input_sha256 = input_hash,
    receipt_sha256 = receipt_hash,
    schema_sha256 = fp_vs_hash(
      file.path(root, "schema.json")
    ),
    previous_commit_sha256 = prev_hash,
    files = records
  )
  fp_vs_write_json(
    manifest, file.path(pending, "manifest.json")
  )
  fp_vs_validate_commit(
    pending, manifest$schema_sha256, prev_hash
  )

  available = c(
    if (!is.null(versions)) {
      versions$product_version_id
    },
    new_v$product_version_id
  )
  stopifnot(
    !anyDuplicated(available),
    all(ids %in% available)
  )

  if (identical(fail_at, "before_commit")) {
    stop("Injected interruption", call. = F)
  }

  target = file.path(root, "commits", commit_dir)
  if (dir.exists(target) ||
      !file.rename(pending, target)) {
    stop(
      "Atomic commit failed; pending retained",
      call. = F
    )
  }
  invisible("committed")
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 7) HISTORICAL RECONSTRUCTION
# <-----------------------------------------------------------------------------

fp_vs_restore = \(root, from, to,
  warehouses = NULL) {
  history = fp_vs_history(root)
  cids = names(history)
  stopifnot(
    length(c(from, to)) == 2L,
    !anyNA(as.Date(c(from, to))),
    from <= to
  )
  cid_dates = vapply(
    history, \(m) m$date, ""
  )
  selected = cids[cid_dates >= from & cid_dates <= to]
  if (!is.null(warehouses)) {
    sel_wh = vapply(
      history[selected],
      \(m) as.integer(m$warehouse_id),
      integer(1)
    )
    selected = selected[
      sel_wh %in% as.integer(warehouses)
    ]
  }
  if (!length(selected)) {
    stop("No observations in range", call. = F)
  }

  s = fp_vs_read_schema(root)
  up_to = cids[cid_dates <= to]
  versions = fp_vs_table(
    root, "product_versions.csv.gz", up_to
  )
  stopifnot(!anyDuplicated(
    versions$product_version_id
  ))

  ann = list(
    cls = fp_an_table(root, "classification_versions.csv.gz"),
    links = fp_an_table(root, "detail_links.csv.gz"),
    det = fp_an_table(root, "product_details.csv.gz"),
    vol = fp_an_table(root, "volume_resolutions.csv.gz"),
    asm = fp_an_table(root, "version_assessments.csv.gz")
  )
  restored = lapply(selected, \(cid) {
    obs = fp_vs_read_gz(file.path(
      root, "commits", cid, "observations.csv.gz"
    ))
    if (!nrow(obs)) return(NULL)
    at = match(
      obs$product_version_id,
      versions$product_version_id
    )
    if (anyNA(at)) {
      stop("Missing product version", call. = F)
    }
    for (field in s$tracked) {
      if (field %in% names(versions) &&
          !field %in% names(obs)) {
        obs[[field]] = versions[[field]][at]
      } else if (field %in% names(versions) &&
          field %in% names(obs)) {
        in_obs = obs[[field]]
        in_ver = versions[[field]][at]
        both = !is.na(in_obs) & !is.na(in_ver)
        if (any(both & in_obs[both] != in_ver[both])) {
          stop(
            paste0("Conflict in field: ", field),
            call. = F
          )
        }
        na_obs = is.na(in_obs)
        obs[[field]][na_obs] = in_ver[na_obs]
      }
    }
    day = history[[cid]]$date
    eff = fp_an_effective(ann$cls, "product_version_id", as_of = day)
    if (!is.null(eff)) {
      at2 = match(obs$product_version_id, eff$product_version_id)
      for (cl in c(fp_an_class_columns(), fp_an_class_extra())) {
        if (!cl %in% names(eff)) next
        if (!cl %in% names(obs)) obs[[cl]] = NA_character_
        has = !is.na(at2)
        obs[[cl]][has] = eff[[cl]][at2[has]]
      }
      # Makes a retroactive (analytical) classification recognizable.
      obs$classification_provenance = eff$provenance[at2]
      obs$classification_retroactive = if ("retroactive" %in% names(eff)) {
        ifelse(is.na(eff$retroactive[at2]), "FALSE",
          eff$retroactive[at2])
      } else ifelse(is.na(at2), NA_character_, "FALSE")
    }
    veff = fp_an_effective(ann$vol, "resolution_key", as_of = day)
    if (!is.null(veff)) {
      at3 = match(paste(obs$product_version_id, obs$warehouse_id, sep = "|"),
        veff$resolution_key)
      has3 = !is.na(at3)
      for (cl in fp_an_volume_columns()) {
        obs[[cl]][has3] = veff[[cl]][at3[has3]]
      }
    }
    aeff = fp_an_effective(ann$asm, "product_version_id", as_of = day)
    if (!is.null(aeff)) {
      at4 = match(obs$product_version_id, aeff$product_version_id)
      obs$version_assessment = aeff$assessment[at4]
      obs$technical_artifact = ifelse(is.na(at4), NA_character_,
        ifelse(aeff$assessment[at4] == "artifact_derived_volume",
          "TRUE", "FALSE"))
    }
    leff = fp_an_effective(ann$links, "product_version_id", as_of = day)
    if (!is.null(leff) && !is.null(ann$det)) {
      d_id = leff$detail_id[match(obs$product_version_id,
        leff$product_version_id)]
      d_at = match(d_id, ann$det$detail_id)
      for (cl in fp_an_detail_columns()) {
        obs[[cl]] = ann$det[[cl]][d_at]
      }
    }
    obs
  })
  restored = restored[!vapply(
    restored, is.null, logical(1)
  )]
  if (!length(restored)) {
    stop("No observations after filtering", call. = F)
  }
  x = do.call(rbind, restored)
  rownames(x) = NULL
  stopifnot(!anyDuplicated(x$observation_key))
  x
}

fp_vs_panel = \(root, from, to,
  warehouses = NULL, output = NULL) {
  x = fp_vs_restore(root, from, to, warehouses)
  if (!is.null(output)) fp_vs_write_gz(x, output)
  invisible(list(
    rows = nrow(x),
    days = length(unique(x$daily_date)),
    warehouses = length(unique(x$warehouse_id)),
    products = length(unique(x$sku))
  ))
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 8) VERSION VIEW AND CHANGE LOOKUP
# <-----------------------------------------------------------------------------

fp_vs_version_view = \(root, to = "9999-12-31") {
  history = fp_vs_history(root)
  cids = names(history)
  cid_dates = vapply(
    history, \(m) m$date, ""
  )
  cids = cids[cid_dates <= to]
  versions = fp_vs_table(
    root, "product_versions.csv.gz", cids
  )
  usage = fp_vs_table(
    root, "version_usage.csv.gz", cids
  )
  groups = split(
    seq_len(nrow(usage)),
    usage$product_version_id
  )
  at = groups[versions$product_version_id]
  versions$last_observed_date = vapply(at, \(i) {
    max(usage$daily_date[i])
  }, "")
  versions$last_observed_at = vapply(at, \(i) {
    max(usage$observed_at[i])
  }, "")
  versions$warehouses_observed = vapply(at, \(i) {
    paste(sort(unique(usage$warehouse_id[i])),
      collapse = ",")
  }, "")
  versions
}

fp_vs_change_values = \(root, change) {
  stopifnot(
    nrow(change) == 1L,
    change$field != "__initial_product__"
  )
  versions = fp_vs_table(
    root, "product_versions.csv.gz"
  )
  s = fp_vs_read_schema(root)
  stopifnot(change$field %in% s$tracked)
  old = match(
    change$old_version_id,
    versions$product_version_id
  )
  new = match(
    change$new_version_id,
    versions$product_version_id
  )
  stopifnot(!is.na(new))
  list(
    old_value = versions[[change$field]][old],
    new_value = versions[[change$field]][new]
  )
}
