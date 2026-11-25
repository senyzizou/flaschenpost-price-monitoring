# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261029

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) WAREHOUSE SNAPSHOT: IMPORT AND NORMALISE
# <-----------------------------------------------------------------------------

rp_import_snapshot = \(variants_csv, warehouse_id = NULL,
  label = NA_character_) {
  if (!file.exists(variants_csv)) {
    stop("Snapshot file not found: ", variants_csv, call. = F)
  }
  v = read.csv(variants_csv, stringsAsFactors = F)
  required = c("sku", "warehouse_id", "product_key", "product_name",
    "price_cents", "price_eur", "available", "pack_count",
    "unit_volume_l", "total_volume_l", "content_kind")
  missing = setdiff(required, names(v))
  if (length(missing)) {
    stop("Missing columns: ", paste(missing, collapse = ", "),
      call. = F)
  }
  if (!is.null(warehouse_id)) {
    if (!all(v$warehouse_id == warehouse_id)) {
      stop("Warehouse ID mismatch in snapshot.", call. = F)
    }
  }
  if (anyDuplicated(v$sku)) {
    stop("Duplicate SKUs in snapshot.", call. = F)
  }
  v$snapshot_label = label
  v$snapshot_file = variants_csv
  v
}

rp_import_existing_proof = \(cfg = config) {
  prep = cfg$preparation_dir
  proof_dir = file.path(prep, "output", "inventory_proofs",
    "reprocessed_volume_rules")
  if (!dir.exists(proof_dir)) return(NULL)
  dirs = list.dirs(proof_dir, recursive = F, full.names = T)
  if (!length(dirs)) return(NULL)
  results = list()
  for (d in dirs) {
    f = file.path(d, "variants.csv")
    if (!file.exists(f)) next
    wid = as.integer(sub(".*_w(\\d+)_.*", "\\1", basename(d)))
    results[[length(results) + 1]] = rp_import_snapshot(f,
      warehouse_id = wid, label = basename(d))
  }
  if (!length(results)) return(NULL)
  results
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 2) SNAPSHOT NORMALISATION FOR COMPARISON
# <-----------------------------------------------------------------------------

rp_price_fields = c("sku", "warehouse_id", "product_key",
  "product_name", "price_cents", "price_eur",
  "regional_price", "reduced_price_cents",
  "reduced_10_cents", "reduced_20_cents",
  "reduced_30_cents", "reduced_40_cents",
  "deposit_total_eur", "deposit_per_frame_eur",
  "pack_count", "unit_volume_l", "total_volume_l",
  "available", "content_kind",
  "article_description", "container_type",
  "brand", "subcategory_ids")

rp_normalise = \(snapshot, fields = rp_price_fields) {
  present = intersect(fields, names(snapshot))
  out = snapshot[, present, drop = F]
  out$sku = as.character(out$sku)
  out$product_key = as.character(out$product_key)
  out$warehouse_id = as.integer(out$warehouse_id)
  if ("price_cents" %in% names(out)) {
    out$price_cents = as.numeric(out$price_cents)
  }
  if ("available" %in% names(out)) {
    out$available = as.logical(out$available)
  }
  out
}

rp_variant_key = \(snapshot) {
  paste0(snapshot$sku, "_w", snapshot$warehouse_id)
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 3) SNAPSHOT QUALITY CHECKS
# <-----------------------------------------------------------------------------

rp_snapshot_quality = \(snapshot) {
  n_total = nrow(snapshot)
  n_available = sum(snapshot$available, na.rm = T)
  n_unavailable = n_total - n_available
  n_volume = sum(snapshot$content_kind == "volume", na.rm = T)
  n_products = length(unique(snapshot$product_key))
  n_regional = if ("regional_price" %in% names(snapshot)) {
    sum(snapshot$regional_price, na.rm = T)
  } else NA_integer_

  has_abv = "alcohol_content" %in% names(snapshot)
  n_abv = if (has_abv) {
    sum(!is.na(snapshot$alcohol_content))
  } else NA_integer_

  has_cat = "price_category" %in% names(snapshot)
  n_classified = if (has_cat) {
    sum(!is.na(snapshot$price_category) &
      nchar(snapshot$price_category) > 0)
  } else NA_integer_

  has_ready = "analysis_ready" %in% names(snapshot)
  n_ready = if (has_ready) {
    sum(snapshot$analysis_ready, na.rm = T)
  } else NA_integer_

  data.frame(
    warehouse_id = snapshot$warehouse_id[1],
    total_skus = n_total, products = n_products,
    available = n_available, unavailable = n_unavailable,
    volume_items = n_volume,
    regional_priced = n_regional,
    with_abv = n_abv, classified = n_classified,
    analysis_ready = n_ready, stringsAsFactors = F)
}

rp_snapshot_incomplete = \(snapshot) {
  issues = character()
  if (anyDuplicated(snapshot$sku)) {
    issues = c(issues, "duplicate_skus")
  }
  if (any(is.na(snapshot$price_cents) | snapshot$price_cents <= 0)) {
    issues = c(issues, "missing_or_zero_prices")
  }
  if (any(is.na(snapshot$sku) | !nzchar(snapshot$sku))) {
    issues = c(issues, "missing_sku")
  }
  issues
}

rp_save_snapshot = \(snapshot, out_dir, label) {
  dir.create(out_dir, showWarnings = F, recursive = T)
  file = file.path(out_dir, paste0("snapshot_", label, ".csv"))
  write.csv(snapshot, file, row.names = F)
  file
}
