# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261029

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) PAIRWISE WAREHOUSE COMPARISON
# <-----------------------------------------------------------------------------

rp_sku_overlap = \(snap_a, snap_b) {
  skus_a = unique(snap_a$sku)
  skus_b = unique(snap_b$sku)
  shared = intersect(skus_a, skus_b)
  only_a = setdiff(skus_a, skus_b)
  only_b = setdiff(skus_b, skus_a)
  list(shared = shared, only_a = only_a, only_b = only_b,
    n_shared = length(shared), n_only_a = length(only_a),
    n_only_b = length(only_b),
    jaccard = length(shared) /
      length(union(skus_a, skus_b)),
    overlap_pct_a = length(shared) / max(1, length(skus_a)),
    overlap_pct_b = length(shared) / max(1, length(skus_b)))
}

rp_price_diff = \(snap_a, snap_b) {
  shared = intersect(snap_a$sku, snap_b$sku)
  if (!length(shared)) {
    return(data.frame(sku = character(),
      warehouse_a = integer(), warehouse_b = integer(),
      price_a = numeric(), price_b = numeric(),
      diff_cents = numeric(), diff_pct = numeric(),
      stringsAsFactors = F))
  }
  a = snap_a[match(shared, snap_a$sku), ]
  b = snap_b[match(shared, snap_b$sku), ]
  diff_cents = a$price_cents - b$price_cents
  diff_pct = ifelse(b$price_cents > 0,
    (a$price_cents - b$price_cents) / b$price_cents * 100,
    NA_real_)
  out = data.frame(sku = shared,
    product_name = a$product_name,
    warehouse_a = a$warehouse_id[1],
    warehouse_b = b$warehouse_id[1],
    price_a = a$price_cents, price_b = b$price_cents,
    diff_cents = diff_cents, diff_pct = round(diff_pct, 2),
    stringsAsFactors = F)
  out[order(-abs(out$diff_cents)), ]
}

rp_availability_diff = \(snap_a, snap_b) {
  shared = intersect(snap_a$sku, snap_b$sku)
  if (!length(shared)) {
    return(data.frame(sku = character(),
      available_a = logical(), available_b = logical(),
      stringsAsFactors = F))
  }
  a = snap_a[match(shared, snap_a$sku), ]
  b = snap_b[match(shared, snap_b$sku), ]
  differs = a$available != b$available
  if (!any(differs)) {
    return(data.frame(sku = character(),
      product_name = character(),
      available_a = logical(), available_b = logical(),
      stringsAsFactors = F))
  }
  data.frame(sku = shared[differs],
    product_name = a$product_name[differs],
    warehouse_a = a$warehouse_id[1],
    warehouse_b = b$warehouse_id[1],
    available_a = a$available[differs],
    available_b = b$available[differs],
    stringsAsFactors = F)
}

rp_pack_diff = \(snap_a, snap_b) {
  shared = intersect(snap_a$sku, snap_b$sku)
  if (!length(shared)) return(NULL)
  a = snap_a[match(shared, snap_a$sku), ]
  b = snap_b[match(shared, snap_b$sku), ]
  pack_differs = !is.na(a$pack_count) & !is.na(b$pack_count) &
    a$pack_count != b$pack_count
  vol_differs = !is.na(a$unit_volume_l) & !is.na(b$unit_volume_l) &
    abs(a$unit_volume_l - b$unit_volume_l) > 1e-6
  differs = pack_differs | vol_differs
  if (!any(differs)) return(NULL)
  data.frame(sku = shared[differs],
    product_name = a$product_name[differs],
    pack_a = a$pack_count[differs],
    pack_b = b$pack_count[differs],
    vol_a = a$unit_volume_l[differs],
    vol_b = b$unit_volume_l[differs],
    stringsAsFactors = F)
}

rp_deposit_diff = \(snap_a, snap_b) {
  shared = intersect(snap_a$sku, snap_b$sku)
  if (!length(shared)) return(NULL)
  if (!all(c("deposit_total_eur") %in% names(snap_a)) ||
      !all(c("deposit_total_eur") %in% names(snap_b))) {
    return(NULL)
  }
  a = snap_a[match(shared, snap_a$sku), ]
  b = snap_b[match(shared, snap_b$sku), ]
  differs = !is.na(a$deposit_total_eur) &
    !is.na(b$deposit_total_eur) &
    abs(a$deposit_total_eur - b$deposit_total_eur) > 0.001
  if (!any(differs)) return(NULL)
  data.frame(sku = shared[differs],
    product_name = a$product_name[differs],
    deposit_a = a$deposit_total_eur[differs],
    deposit_b = b$deposit_total_eur[differs],
    stringsAsFactors = F)
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 2) PAIRWISE COMPARISON SUMMARY
# <-----------------------------------------------------------------------------

rp_compare_pair = \(snap_a, snap_b) {
  overlap = rp_sku_overlap(snap_a, snap_b)
  prices = rp_price_diff(snap_a, snap_b)
  avail = rp_availability_diff(snap_a, snap_b)
  packs = rp_pack_diff(snap_a, snap_b)
  deposits = rp_deposit_diff(snap_a, snap_b)

  n_price_diff = sum(prices$diff_cents != 0)
  mean_abs_diff = if (n_price_diff > 0) {
    mean(abs(prices$diff_cents[prices$diff_cents != 0]))
  } else 0
  max_abs_diff = if (n_price_diff > 0) {
    max(abs(prices$diff_cents))
  } else 0

  summary = data.frame(
    warehouse_a = snap_a$warehouse_id[1],
    warehouse_b = snap_b$warehouse_id[1],
    skus_a = length(unique(snap_a$sku)),
    skus_b = length(unique(snap_b$sku)),
    shared_skus = overlap$n_shared,
    only_a = overlap$n_only_a,
    only_b = overlap$n_only_b,
    jaccard = round(overlap$jaccard, 4),
    n_price_diff = n_price_diff,
    mean_abs_diff_cents = round(mean_abs_diff, 1),
    max_abs_diff_cents = max_abs_diff,
    n_avail_diff = nrow(avail),
    n_pack_diff = if (is.null(packs)) 0L else nrow(packs),
    n_deposit_diff = if (is.null(deposits)) 0L
      else nrow(deposits),
    stringsAsFactors = F)

  list(summary = summary, overlap = overlap,
    price_diff = prices, availability_diff = avail,
    pack_diff = packs, deposit_diff = deposits)
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 3) MULTI-WAREHOUSE COMPARISON
# <-----------------------------------------------------------------------------

rp_compare_all = \(snapshots) {
  if (length(snapshots) < 2) {
    stop("At least two snapshots required.", call. = F)
  }
  pairs = combn(length(snapshots), 2)
  results = list()
  summaries = list()
  for (col in seq_len(ncol(pairs))) {
    i = pairs[1, col]
    j = pairs[2, col]
    key = paste0("w", snapshots[[i]]$warehouse_id[1],
      "_vs_w", snapshots[[j]]$warehouse_id[1])
    results[[key]] = rp_compare_pair(snapshots[[i]],
      snapshots[[j]])
    summaries[[col]] = results[[key]]$summary
  }
  list(pairs = results, summary = do.call(rbind, summaries))
}

rp_product_matrix = \(snapshots) {
  all_skus = sort(unique(unlist(lapply(snapshots,
    \(s) unique(s$sku)))))
  wids = vapply(snapshots, \(s) s$warehouse_id[1], integer(1))
  mat = matrix(F, nrow = length(all_skus),
    ncol = length(snapshots))
  rownames(mat) = all_skus
  colnames(mat) = paste0("w", wids)
  for (j in seq_along(snapshots)) {
    mat[all_skus %in% snapshots[[j]]$sku, j] = T
  }
  mat
}

rp_price_matrix = \(snapshots) {
  all_skus = sort(unique(unlist(lapply(snapshots,
    \(s) unique(s$sku)))))
  wids = vapply(snapshots, \(s) s$warehouse_id[1], integer(1))
  mat = matrix(NA_real_, nrow = length(all_skus),
    ncol = length(snapshots))
  rownames(mat) = all_skus
  colnames(mat) = paste0("w", wids)
  for (j in seq_along(snapshots)) {
    idx = match(snapshots[[j]]$sku, all_skus)
    mat[idx, j] = snapshots[[j]]$price_cents
  }
  mat
}

rp_price_variation = \(price_matrix) {
  n_present = rowSums(!is.na(price_matrix))
  range_cents = apply(price_matrix, 1, \(row) {
    vals = row[!is.na(row)]
    if (length(vals) < 2) return(0)
    max(vals) - min(vals)
  })
  cv = apply(price_matrix, 1, \(row) {
    vals = row[!is.na(row)]
    if (length(vals) < 2) return(NA_real_)
    sd(vals) / mean(vals)
  })
  data.frame(sku = rownames(price_matrix),
    n_warehouses = n_present,
    price_range_cents = range_cents,
    price_cv = round(cv, 6), stringsAsFactors = F)
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 4) INTRA-WAREHOUSE COMPARISON
# <-----------------------------------------------------------------------------

rp_intra_warehouse_check = \(snap_a, snap_b,
  location_a = NA_character_, location_b = NA_character_,
  skus = NULL) {
  if (snap_a$warehouse_id[1] != snap_b$warehouse_id[1]) {
    stop("Snapshots are from different warehouses.",
      call. = F)
  }
  comp = rp_compare_pair(snap_a, snap_b)
  comp$semantic = rp_semantic_compare(snap_a, snap_b, skus)
  comp$same_warehouse = T
  comp$warehouse_id = snap_a$warehouse_id[1]
  comp$location_a = location_a
  comp$location_b = location_b
  comp$conclusion = if (comp$summary$n_price_diff == 0 &&
      comp$summary$only_a == 0 &&
      comp$summary$only_b == 0 &&
      comp$summary$n_avail_diff == 0 &&
      comp$semantic$identical) {
    "identical"
  } else if (comp$summary$n_price_diff == 0 &&
      comp$summary$only_a == 0 &&
      comp$summary$only_b == 0) {
    "same_catalog_availability_differs"
  } else {
    "differences_detected"
  }
  comp
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 5) REGIONAL PRICE TIER ANALYSIS
# <-----------------------------------------------------------------------------

rp_regional_tier_summary = \(snapshot) {
  tier_fields = c("reduced_price_cents", "reduced_10_cents",
    "reduced_20_cents", "reduced_30_cents", "reduced_40_cents")
  present = intersect(tier_fields, names(snapshot))
  if (!length(present)) {
    return(data.frame(tier = character(), n_with_value = integer(),
      n_differ_from_base = integer(), stringsAsFactors = F))
  }
  rows = lapply(present, \(field) {
    vals = snapshot[[field]]
    n_with = sum(!is.na(vals) & vals > 0)
    n_diff = sum(!is.na(vals) & vals > 0 &
      vals != snapshot$price_cents, na.rm = T)
    data.frame(tier = field, n_with_value = n_with,
      n_differ_from_base = n_diff, stringsAsFactors = F)
  })
  do.call(rbind, rows)
}

rp_regional_flag_summary = \(snapshot) {
  if (!"regional_price" %in% names(snapshot)) {
    return(data.frame(regional_price = logical(),
      count = integer(), stringsAsFactors = F))
  }
  tbl = table(snapshot$regional_price, useNA = "always")
  data.frame(regional_price = names(tbl),
    count = as.integer(tbl), stringsAsFactors = F)
}

rp_price_per_litre = \(snapshot) {
  idx = snapshot$content_kind == "volume" &
    !is.na(snapshot$total_volume_l) &
    snapshot$total_volume_l > 0 &
    !is.na(snapshot$price_cents)
  if (!any(idx)) return(snapshot)
  snapshot$price_per_l_cents = NA_real_
  snapshot$price_per_l_cents[idx] = round(
    snapshot$price_cents[idx] / snapshot$total_volume_l[idx], 2)
  snapshot
}

rp_price_per_alcohol = \(snapshot) {
  if (!"alcohol_content" %in% names(snapshot)) {
    return(snapshot)
  }
  idx = snapshot$content_kind == "volume" &
    !is.na(snapshot$total_volume_l) &
    snapshot$total_volume_l > 0 &
    !is.na(snapshot$alcohol_content) &
    snapshot$alcohol_content > 0 &
    !is.na(snapshot$price_cents)
  snapshot$price_per_10g_alc_cents = NA_real_
  if (any(idx)) {
    pure_alc_l = snapshot$total_volume_l[idx] *
      snapshot$alcohol_content[idx] / 100
    pure_alc_10g = pure_alc_l * 789 / 10
    snapshot$price_per_10g_alc_cents[idx] = round(
      snapshot$price_cents[idx] / pure_alc_10g, 2)
  }
  snapshot
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 6) SEMANTIC COMPARISON
# <-----------------------------------------------------------------------------

rp_comparison_fields = \() {
  c("sku", "product_name", "price_cents", "available",
    "pack_count", "unit_volume_l", "deposit_total_eur",
    "reduced_price_cents", "reduced_10_cents",
    "reduced_20_cents", "reduced_30_cents",
    "reduced_40_cents", "regional_price")
}

rp_semantic_compare = \(snap_a, snap_b, skus = NULL) {
  fields = rp_comparison_fields()
  present_a = intersect(fields, names(snap_a))
  present_b = intersect(fields, names(snap_b))
  common_fields = intersect(present_a, present_b)
  if (is.null(skus)) {
    skus = intersect(snap_a$sku, snap_b$sku)
  } else {
    skus = intersect(skus,
      intersect(snap_a$sku, snap_b$sku))
  }
  if (!length(skus)) {
    return(list(skus_compared = 0L,
      identical = T, diffs = data.frame()))
  }
  a = snap_a[match(skus, snap_a$sku), common_fields,
    drop = F]
  b = snap_b[match(skus, snap_b$sku), common_fields,
    drop = F]
  diffs = list()
  for (f in setdiff(common_fields, "sku")) {
    va = a[[f]]
    vb = b[[f]]
    if (is.numeric(va)) {
      ne = which(!is.na(va) & !is.na(vb) &
        abs(va - vb) > 1e-6)
    } else if (is.logical(va)) {
      ne = which(!is.na(va) & !is.na(vb) & va != vb)
    } else {
      ne = which(!is.na(va) & !is.na(vb) & va != vb)
    }
    if (length(ne)) {
      diffs[[length(diffs) + 1]] = data.frame(
        sku = skus[ne], field = f,
        value_a = as.character(va[ne]),
        value_b = as.character(vb[ne]),
        stringsAsFactors = F)
    }
  }
  diff_df = if (length(diffs)) {
    do.call(rbind, diffs)
  } else {
    data.frame(sku = character(), field = character(),
      value_a = character(), value_b = character(),
      stringsAsFactors = F)
  }
  list(skus_compared = length(skus),
    identical = nrow(diff_df) == 0,
    n_diffs = nrow(diff_df),
    diffs = diff_df,
    fields_compared = setdiff(common_fields, "sku"),
    warehouse_a = snap_a$warehouse_id[1],
    warehouse_b = snap_b$warehouse_id[1])
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 7) COMPARISON TEMPLATE
# <-----------------------------------------------------------------------------

rp_template_fields = \() {
  c("sku", "product_name", "warehouse_id", "location_id",
    "centAmount", "regular_price", "discount_pct",
    "tier_10", "tier_20", "tier_reduced",
    "tier_30", "tier_40",
    "quantity_discount",
    "qd_tier_10", "qd_tier_20", "qd_tier_reduced",
    "qd_tier_30", "qd_tier_40",
    "shop_price_cents", "shop_deposit_cents",
    "shop_streichpreis",
    "available", "regional",
    "pack_count", "unit_volume_l", "deposit_total_eur",
    "source", "timestamp_utc", "session_context")
}

rp_empty_template = \(n_rows = 0L) {
  fields = rp_template_fields()
  df = as.data.frame(
    setNames(
      replicate(length(fields), character(n_rows),
        simplify = F),
      fields),
    stringsAsFactors = F)
  df
}

rp_fill_template_from_api = \(snapshot, location_id,
  timestamp_utc, session_context = NA_character_,
  skus = NULL) {
  if (!is.null(skus)) {
    snapshot = snapshot[snapshot$sku %in% skus, ]
  }
  n = nrow(snapshot)
  tpl = rp_empty_template(n)
  tpl$sku = snapshot$sku
  tpl$product_name = snapshot$product_name
  tpl$warehouse_id = as.character(snapshot$warehouse_id)
  tpl$location_id = location_id
  tpl$centAmount = as.character(snapshot$price_cents)
  if ("reduced_10_cents" %in% names(snapshot)) {
    tpl$tier_10 = as.character(snapshot$reduced_10_cents)
  }
  if ("reduced_20_cents" %in% names(snapshot)) {
    tpl$tier_20 = as.character(snapshot$reduced_20_cents)
  }
  if ("reduced_price_cents" %in% names(snapshot)) {
    tpl$tier_reduced = as.character(
      snapshot$reduced_price_cents)
  }
  if ("reduced_30_cents" %in% names(snapshot)) {
    tpl$tier_30 = as.character(snapshot$reduced_30_cents)
  }
  if ("reduced_40_cents" %in% names(snapshot)) {
    tpl$tier_40 = as.character(snapshot$reduced_40_cents)
  }
  tpl$available = as.character(snapshot$available)
  if ("regional_price" %in% names(snapshot)) {
    tpl$regional = as.character(snapshot$regional_price)
  }
  tpl$pack_count = as.character(snapshot$pack_count)
  tpl$unit_volume_l = as.character(snapshot$unit_volume_l)
  if ("deposit_total_eur" %in% names(snapshot)) {
    tpl$deposit_total_eur = as.character(
      snapshot$deposit_total_eur)
  }
  tpl$source = "api"
  tpl$timestamp_utc = timestamp_utc
  tpl$session_context = session_context
  tpl
}

rp_save_comparison = \(template_df, path) {
  dir.create(dirname(path), showWarnings = F,
    recursive = T)
  utils::write.csv(template_df, path, row.names = F)
  invisible(path)
}

rp_load_comparison = \(path) {
  utils::read.csv(path, stringsAsFactors = F,
    colClasses = "character")
}

rp_validate_comparison = \(template_df) {
  required = c("sku", "warehouse_id", "location_id",
    "centAmount", "timestamp_utc")
  missing = setdiff(required, names(template_df))
  if (length(missing)) {
    return(list(valid = F,
      reason = paste("Missing:", paste(missing,
        collapse = ", "))))
  }
  empty_ts = is.na(template_df$timestamp_utc) |
    template_df$timestamp_utc == ""
  if (all(empty_ts)) {
    return(list(valid = F,
      reason = "All timestamps empty"))
  }
  has_private = any(vapply(template_df, \(col) {
    any(grepl("(str|stra(ss|ß)e|weg|allee|gasse)",
      tolower(col)))
  }, logical(1)))
  if (has_private) {
    return(list(valid = F,
      reason = "Contains private address data"))
  }
  list(valid = T, rows = nrow(template_df),
    locations = length(unique(template_df$location_id)),
    warehouses = length(unique(
      template_df$warehouse_id)),
    skus = length(unique(template_df$sku)))
}
