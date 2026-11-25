# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261030

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) PROOF-TO-SHARD ADAPTER
# <-----------------------------------------------------------------------------

fp_mg_proof_to_shard = \(readiness_csv, proof_meta,
  date, run_id, warehouse_id, output_gz) {
  r = read.csv(readiness_csv, stringsAsFactors = F,
    colClasses = "character")
  stopifnot(
    !anyDuplicated(r$sku),
    all(r$warehouse_id == as.character(warehouse_id))
  )

  obs = fp_vs_observation_columns()
  tracked = fp_vs_tracked_columns()
  all_cols = unique(c(
    setdiff(obs, "product_version_id"), tracked
  ))

  s = data.frame(
    retailer = "flaschenpost",
    sku = r$sku,
    warehouse_id = as.character(warehouse_id),
    product_key = r$product_key,
    observation_key = paste(
      "flaschenpost",
      paste0("w", warehouse_id),
      r$sku, date, sep = "|"
    ),
    daily_date = date,
    run_id = run_id,
    observed_at = r$observed_at,
    stringsAsFactors = F
  )

  direct_obs = c(
    "price_cents", "price_eur",
    "deposit_total_eur", "deposit_per_frame_eur",
    "available", "article_status",
    "regional_price",
    "reduced_price_cents",
    "reduced_10_cents", "reduced_20_cents",
    "reduced_30_cents", "reduced_40_cents",
    "raw_sha256"
  )
  for (col in direct_obs) {
    s[[col]] = if (col %in% names(r)) {
      r[[col]]
    } else NA_character_
  }

  s$listing_state = ifelse(
    !is.na(s$available), "listed", "unlisted"
  )
  s$availability_state = ifelse(
    s$available == "TRUE", "available",
    ifelse(s$available == "FALSE",
      "unavailable", NA_character_)
  )

  if ("distribution_status" %in% names(r)) {
    s$sales_status = r$distribution_status
  } else {
    s$sales_status = NA_character_
  }

  s$quality_status = if ("analysis_ready" %in% names(r)) {
    ifelse(
      r$analysis_ready == "TRUE",
      "analysis_ready", "not_ready"
    )
  } else NA_character_

  s$collection_method = "api"
  s$listing_metadata_file = NA_character_

  for (col in tracked) {
    s[[col]] = if (col %in% names(r)) {
      r[[col]]
    } else NA_character_
  }

  for (mc in setdiff(all_cols, names(s))) {
    s[[mc]] = NA_character_
  }

  for (col in names(s)) {
    if (is.character(s[[col]])) {
      empty = !is.na(s[[col]]) & s[[col]] == ""
      s[[col]][empty] = NA_character_
    }
  }

  fp_vs_write_gz(s, output_gz)
  invisible(output_gz)
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 2) RECEIPT FROM PROOF METADATA
# <-----------------------------------------------------------------------------

fp_mg_make_receipt = \(shard_gz, proof_meta_json,
  date, run_id, warehouse_id, output_json,
  extra = NULL) {
  meta = jsonlite::read_json(
    proof_meta_json, simplifyVector = T
  )
  receipt = list(
    date = date,
    run_id = run_id,
    n_products = as.integer(meta$variants),
    warehouse_id = as.integer(warehouse_id),
    proof_status = meta$status,
    collector_version = meta$collector_version,
    observations = list(
      sha256 = fp_vs_hash(shard_gz),
      path = "observations.csv.gz"
    )
  )
  if (length(extra)) receipt = c(receipt, extra)
  fp_vs_write_json(receipt, output_json)
  invisible(output_json)
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 3) COLLECTOR-TO-INGEST BRIDGE
# <-----------------------------------------------------------------------------

fp_mg_collector_bridge = \(root,
  readiness_csv, proof_meta_json,
  date, run_id, warehouse_id,
  work_dir = tempfile("fp_bridge_")) {
  dir.create(work_dir, recursive = T)
  shard = file.path(work_dir, "shard.csv.gz")
  receipt = file.path(work_dir, "receipt.json")

  fp_mg_proof_to_shard(
    readiness_csv, proof_meta_json,
    date, run_id, warehouse_id, shard
  )
  fp_mg_make_receipt(
    shard, proof_meta_json,
    date, run_id, warehouse_id, receipt
  )

  result = fp_vs_ingest(
    root, shard, receipt, warehouse_id
  )
  list(
    status = result,
    shard = shard,
    receipt = receipt,
    work_dir = work_dir
  )
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 4) DAILY COLLECTOR ADAPTER
# <-----------------------------------------------------------------------------

fp_mg_category_tables = \(collector_output, date, warehouse_id) {
  tables = list()
  tree = file.path(collector_output, "categories.csv")
  member = file.path(collector_output, "membership.csv")
  rd = \(p) read.csv(p, colClasses = "character",
    na.strings = character())
  if (file.exists(tree)) {
    tables[["category_tree.csv.gz"]] = cbind(
      date = date, warehouse_id = as.character(warehouse_id),
      rd(tree), stringsAsFactors = F
    )
  }
  if (file.exists(member)) {
    tables[["category_membership.csv.gz"]] = cbind(
      date = date, warehouse_id = as.character(warehouse_id),
      rd(member), stringsAsFactors = F
    )
  }
  tables
}

fp_mg_daily_adapter = \(root, collector_output,
  date, run_id, warehouse_id,
  work_dir = tempfile("fp_daily_"),
  receipt_extra = NULL, extra_tables = NULL) {
  variants = file.path(
    collector_output, "variants.csv"
  )
  meta = file.path(collector_output, "meta.json")
  if (!file.exists(variants)) {
    stop("No variants.csv in collector output",
      call. = F)
  }
  if (!file.exists(meta)) {
    stop("No meta.json in collector output",
      call. = F)
  }

  dir.create(work_dir, recursive = T)
  shard = file.path(work_dir, "shard.csv.gz")
  receipt = file.path(work_dir, "receipt.json")

  fp_mg_variants_to_shard(
    variants, date, run_id, warehouse_id, shard,
    texts_csv = file.path(collector_output, "texts.csv")
  )
  fp_mg_make_receipt(
    shard, meta, date, run_id,
    warehouse_id, receipt, extra = receipt_extra
  )

  tables = fp_mg_category_tables(
    collector_output, date, warehouse_id
  )
  for (nm in names(extra_tables)) {
    tables[[nm]] = extra_tables[[nm]]
  }
  result = fp_vs_ingest(
    root, shard, receipt, warehouse_id,
    extra_tables = if (length(tables)) tables else NULL
  )
  list(status = result, work_dir = work_dir)
}

fp_mg_variants_to_shard = \(variants_csv,
  date, run_id, warehouse_id, output_gz, texts_csv = NULL) {
  r = read.csv(variants_csv, stringsAsFactors = F,
    colClasses = "character")
  if (!is.null(texts_csv) && file.exists(texts_csv)) {
    tx = read.csv(texts_csv, stringsAsFactors = F,
      colClasses = "character")
    for (cl in fp_an_detail_columns()) {
      if (cl %in% names(tx) && !cl %in% names(r)) {
        r[[cl]] = tx[[cl]][match(r$sku, tx$sku)]
      }
    }
  }
  stopifnot(
    !anyDuplicated(r$sku),
    all(r$warehouse_id == as.character(warehouse_id))
  )

  obs = fp_vs_observation_columns()
  tracked = fp_vs_tracked_columns()
  all_cols = unique(c(
    setdiff(obs, "product_version_id"), tracked
  ))

  obs_at = if ("observed_at" %in% names(r)) {
    r$observed_at
  } else {
    rep(paste0(date, "T12:00:00Z"), nrow(r))
  }

  s = data.frame(
    retailer = "flaschenpost",
    sku = r$sku,
    warehouse_id = as.character(warehouse_id),
    product_key = if ("product_key" %in% names(r)) {
      r$product_key
    } else fp_vs_product_key(r$sku),
    observation_key = paste(
      "flaschenpost",
      paste0("w", warehouse_id),
      r$sku, date, sep = "|"
    ),
    daily_date = date,
    run_id = run_id,
    observed_at = obs_at,
    stringsAsFactors = F
  )

  direct_obs = c(
    "price_cents", "price_eur",
    "deposit_total_eur", "deposit_per_frame_eur",
    "available", "article_status",
    "regional_price",
    "reduced_price_cents",
    "reduced_10_cents", "reduced_20_cents",
    "reduced_30_cents", "reduced_40_cents",
    "raw_sha256"
  )
  for (col in direct_obs) {
    s[[col]] = if (col %in% names(r)) {
      r[[col]]
    } else NA_character_
  }

  s$listing_state = ifelse(
    !is.na(s$available), "listed", "unlisted"
  )
  s$availability_state = ifelse(
    s$available == "TRUE", "available",
    ifelse(s$available == "FALSE",
      "unavailable", NA_character_)
  )

  if ("distribution_status" %in% names(r)) {
    s$sales_status = r$distribution_status
  } else {
    s$sales_status = NA_character_
  }

  s$quality_status = NA_character_
  for (cl in c(fp_an_detail_columns(), fp_an_class_extra())) {
    if (cl %in% names(r)) s[[cl]] = r[[cl]]
  }
  s$collection_method = "api"
  s$listing_metadata_file = NA_character_

  for (col in tracked) {
    s[[col]] = if (col %in% names(r)) {
      r[[col]]
    } else NA_character_
  }

  for (mc in setdiff(all_cols, names(s))) {
    s[[mc]] = NA_character_
  }

  for (col in names(s)) {
    if (is.character(s[[col]])) {
      empty = !is.na(s[[col]]) & s[[col]] == ""
      s[[col]][empty] = NA_character_
    }
  }

  fp_vs_write_gz(s, output_gz)
  invisible(output_gz)
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 5) CHANGE DETECTION STRATEGY
# <-----------------------------------------------------------------------------

fp_mg_detail_check_needed = \(root, warehouse_id,
  current_listing_skus, max_age_days = 7L) {
  history = fp_vs_history(root)
  if (!length(history)) return(current_listing_skus)

  versions = fp_vs_table(
    root, "product_versions.csv.gz"
  )
  if (is.null(versions) || !nrow(versions)) {
    return(current_listing_skus)
  }

  latest = versions[
    !duplicated(versions$sku, fromLast = T), ,
    drop = F
  ]
  known_skus = latest$sku
  new_skus = setdiff(current_listing_skus, known_skus)

  stale_cutoff = as.character(
    Sys.Date() - max_age_days
  )
  stale = latest$sku[
    !is.na(latest$first_observed_date) &
      latest$first_observed_date < stale_cutoff
  ]
  stale = intersect(stale, current_listing_skus)

  unique(c(new_skus, stale))
}
