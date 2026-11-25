# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING - FLASCHENPOST PREPARATION
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261027

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) CLASSIFICATION, NUMERIC COMPUTABILITY AND STATISTICAL INCLUSION
# <-----------------------------------------------------------------------------

fp_analysis_version = "flaschenpost-price-eligibility-0.2.0"

# Historical analysis_ready is a classification/filter flag, not a price gate.
fp_price_readiness = \(variants,
  availability_policy = c("available_only", "all_priced")) {
  availability_policy = match.arg(availability_policy)
  x = variants
  n = nrow(x)
  required = c("sku", "product_key", "price_eur", "price_cents",
    "total_volume_l", "volume_status", "alcohol_content", "price_category",
    "analysis_ready", "analysis_ready_gt_0_5", "available", "content_kind",
    "abv_filter_min_0_5", "abv_filter_gt_0_5", "refinement_decision",
    "product_type", "project_status")
  if (!all(required %in% names(x))) stop("Incomplete price analysis schema.")
  numeric_abv = is.finite(x$alcohol_content) & x$alcohol_content > 0 &
    x$alcohol_content <= 100
  abv_conflict = if ("abv_text_conflict" %in% names(x))
    x$abv_text_conflict %in% T else rep(F, n)
  numeric_price = is.finite(x$price_eur) & x$price_eur > 0 &
    is.finite(x$price_cents) & x$price_cents > 0 &
    x$price_cents == floor(x$price_cents) &
    x$price_eur == x$price_cents / 100
  volume_ok = is.finite(x$total_volume_l) & x$total_volume_l > 0 &
    x$volume_status %in% c("consistent", "attribute_only", "text_only",
      "resolved_text", "resolved_attribute")
  # Repeated SKUs on different panel dates are distinct observations.
  date_keys = c("run_date", "daily_date")
  keys = intersect(c("retailer", "warehouse_id", "sku", date_keys),
    names(x))
  duplicate = duplicated(x[, keys, drop = F]) |
    duplicated(x[, keys, drop = F], fromLast = T)
  identity_ok = !is.na(x$sku) & nzchar(x$sku) &
    !is.na(x$product_key) & nzchar(as.character(x$product_key)) & !duplicate
  category_ok = !is.na(x$price_category) &
    x$price_category %in% classifier_levels(x) & x$content_kind %in% "volume"
  comparable = !x$refinement_decision %in% "assigned_bundle_not_ready" &
    !x$product_type %in% "spirit_with_accessory" &
    !x$project_status %in% c("separate_product_type", "proposed_not_applied")
  listed = if ("listing_state" %in% names(x)) x$listing_state %in% "listed"
    else rep(T, n)
  availability_ok = if (availability_policy == "available_only")
    !is.na(x$available) & x$available %in% T else rep(T, n)
  x$classification_ready = category_ok
  x$product_comparable = comparable
  x$filter_included_primary = numeric_abv &
    x$abv_filter_gt_0_5 %in% "yes" & x$alcohol_content > 0.5
  x$filter_included_sensitivity = numeric_abv &
    x$abv_filter_min_0_5 %in% "yes" & x$alcohol_content >= 0.5
  x$price_calculable = numeric_abv & !abv_conflict & numeric_price &
    volume_ok & identity_ok & comparable & listed
  x$price_eligible_primary = x$price_calculable & x$classification_ready &
    x$filter_included_primary & x$analysis_ready_gt_0_5 %in% T &
    availability_ok
  x$price_eligible_sensitivity = x$price_calculable & x$classification_ready &
    x$filter_included_sensitivity & x$analysis_ready %in% T &
    availability_ok
  failures = list(not_listed = !listed, invalid_identity = !identity_ok,
    missing_or_invalid_abv = !numeric_abv, abv_conflict = abv_conflict,
    missing_or_invalid_price = !numeric_price,
    unresolved_or_missing_sale_volume = !volume_ok,
    category_missing_or_outside_project = !category_ok,
    noncomparable_product = !comparable,
    not_currently_available = !availability_ok)
  reasons = \(filter, legacy) vapply(seq_len(n), \(i) {
    bad = names(failures)[vapply(failures, \(flag) flag[i], logical(1))]
    if (!filter[i]) bad = c(bad, "abv_filter_not_satisfied")
    if (!legacy[i]) bad = c(bad, "historical_classification_filter_not_ready")
    paste(bad, collapse = ";")
  }, "")
  x$price_exclusions_primary = reasons(x$filter_included_primary,
    x$analysis_ready_gt_0_5 %in% T)
  x$price_exclusions_sensitivity = reasons(x$filter_included_sensitivity,
    x$analysis_ready %in% T)
  density = fp_classifier(collector)$env$ETHANOL_DENSITY_G_ML
  x$ethanol_g = NA_real_
  x$price_eur_per_10g = NA_real_
  use = x$price_calculable
  x$ethanol_g[use] = x$total_volume_l[use] * 1000 *
    x$alcohol_content[use] / 100 * density
  x$price_eur_per_10g[use] = x$price_eur[use] * 10 / x$ethanol_g[use]
  stopifnot(all(is.finite(x$price_eur_per_10g[use])),
    all(x$price_eur_per_10g[use] > 0),
    all(x$price_eligible_primary <= x$price_eligible_sensitivity),
    identical(x$price_eligible_primary, !nzchar(x$price_exclusions_primary)),
    identical(x$price_eligible_sensitivity,
      !nzchar(x$price_exclusions_sensitivity)))
  x$price_eligibility_version = fp_analysis_version
  x$availability_policy = availability_policy
  x
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 2) EXCLUSION LEDGER AND COUNTS WITHOUT RECLASSIFICATION
# <-----------------------------------------------------------------------------

fp_price_summary = \(x) {
  groups = c(classifier_levels(x), "Total")
  fp_rows(lapply(groups, \(group) {
    use = if (group == "Total") rep(T, nrow(x)) else
      x$price_category %in% group
    data.frame(category = group, availability_policy = x$availability_policy[1],
      abv_gt_0_5_ready = sum(use & x$analysis_ready_gt_0_5 %in% T),
      abv_ge_0_5_ready = sum(use & x$analysis_ready %in% T),
      price_primary = sum(use & x$price_eligible_primary),
      price_sensitivity = sum(use & x$price_eligible_sensitivity),
      stringsAsFactors = F)
  }))
}

fp_price_exclusions = \(x) {
  rows = list()
  for (analysis in c("primary", "sensitivity")) {
    field = paste0("price_exclusions_", analysis)
    for (i in which(nzchar(x[[field]]))) {
      reasons = strsplit(x[[field]][i], ";", fixed = T)[[1]]
      rows[[length(rows) + 1L]] = data.frame(sku = x$sku[i],
        analysis = analysis, availability_policy = x$availability_policy[i],
        reason = reasons, stringsAsFactors = F)
    }
  }
  if (!length(rows)) return(data.frame(sku = character(),
    analysis = character(), availability_policy = character(),
    reason = character(), stringsAsFactors = F))
  fp_rows(rows)
}
