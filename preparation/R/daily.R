# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING - FLASCHENPOST PREPARATION
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261023

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) VERIFIED ABV REUSE AND CHANGE DETECTION
# <-----------------------------------------------------------------------------

fp_resolve_abv = \(current, master, as_of,
  max_age_days = config$abv_cache_max_age_days) {
  stopifnot(!anyDuplicated(master$variant_key))
  current$abv_resolution = "needs_detail"
  identity = c("product_name", "retailer_parent_product_id", "pack_count",
    "unit_volume_l", "volume_l")
  for (i in seq_len(nrow(current))) {
    if (current$abv_status[i] == "explicit_exact" &&
        !is.na(current$alcohol_content[i]) &&
        current$alcohol_content[i] >= 0 && current$alcohol_content[i] <= 100) {
      current$abv_resolution[i] = "current_detail"
      next
    }
    if (!current$abv_status[i] %in% c("missing_exact_abv", "not_collected")) {
      current$abv_resolution[i] = "qualified_or_conflicting_needs_detail"
      next
    }
    j = match(current$variant_key[i], master$variant_key)
    if (is.na(j)) next
    same = vapply(identity, \(field) {
      !is.na(current[[field]][i]) && !is.na(master[[field]][j]) &&
        identical(current[[field]][i], master[[field]][j])
    }, logical(1))
    if (!all(same)) {
      current$abv_resolution[i] = "identity_changed_needs_detail"
      next
    }
    source = master$abv_raw_file[j]
    age = as.numeric(as.Date(as_of) - as.Date(substr(
      master$abv_observed_at[j], 1, 10)))
    valid = master$abv_status[j] == "explicit_exact" &&
      !is.na(master$alcohol_content[j]) && master$alcohol_content[j] >= 0 &&
      master$alcohol_content[j] <= 100 && !is.na(age) && age >= 0 &&
      age <= max_age_days && !is.na(source) && file.exists(source)
    if (valid) valid = identical(fp_hash(source), master$abv_raw_sha256[j])
    if (!valid) next
    for (field in c("alcohol_content", "alcohol_content_raw", "abv_status",
        "abv_raw_file", "abv_raw_sha256", "abv_observed_at",
        "alcohol_content_source")) {
      current[[field]][i] = master[[field]][j]
    }
    current$abv_resolution[i] = "verified_cache"
  }
  current$statistics_eligible_abv_only = ifelse(
    is.na(current$alcohol_content), NA, current$alcohol_content >= 0.5)
  current$parse_status[current$abv_resolution == "verified_cache"] =
    "fields_with_verified_abv_cache"
  current$price_analysis_eligible = F
  current
}

fp_changes = \(master, current) {
  fields = c("product_name", "retailer_parent_product_id", "pack_count",
    "unit_volume_l", "volume_l", "alcohol_content")
  rows = list()
  for (i in seq_len(nrow(current))) {
    j = match(current$variant_key[i], master$variant_key)
    if (is.na(j)) {
      rows[[length(rows) + 1L]] = data.frame(
        variant_key = current$variant_key[i], change = "new_variant",
        field = NA_character_, previous = NA_character_,
        current = current$product_name[i], previous_raw_file = NA_character_,
        current_raw_file = current$raw_file[i], stringsAsFactors = F)
      next
    }
    for (field in fields) {
      a = master[[field]][j]
      b = current[[field]][i]
      if (is.na(a) || is.na(b) || identical(a, b)) next
      rows[[length(rows) + 1L]] = data.frame(
        variant_key = current$variant_key[i], change = "changed",
        field = field, previous = as.character(a), current = as.character(b),
        previous_raw_file = if (field == "alcohol_content")
          master$abv_raw_file[j] else master$raw_file[j],
        current_raw_file = if (field == "alcohol_content")
          current$abv_raw_file[i] else current$raw_file[i],
        stringsAsFactors = F)
    }
  }
  fp_rows(rows)
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 2) DAY PANEL AND CONSERVATIVE REMOVAL
# <-----------------------------------------------------------------------------

fp_tracking = \(products) {
  defaults = list(first_seen = NA_character_, last_seen = NA_character_,
    lifecycle = "active", not_seen_complete_count = 0L,
    last_absence_date = NA_character_)
  for (field in names(defaults)) {
    if (!field %in% names(products)) products[[field]] = rep(
      defaults[[field]], nrow(products))
  }
  products
}

fp_daily_panel = \(master, current, run_date, inventory_complete = F,
  removal_confirmations = NULL) {
  if (is.null(removal_confirmations)) removal_confirmations =
    config$removal_requires_distinct_complete_inventories
  stopifnot(removal_confirmations >= 2L,
    !anyNA(current$variant_key), !anyDuplicated(current$variant_key),
    !anyNA(master$variant_key), !anyDuplicated(master$variant_key))
  current = fp_tracking(current)
  master = fp_tracking(master)
  stopifnot(length(run_date) == 1L, !is.na(as.Date(run_date)))
  if (nrow(master) && any(c(master$last_seen, master$last_absence_date) >
      run_date, na.rm = T)) stop("Out-of-order master update rejected")
  fields = union(names(master), names(current))
  for (field in setdiff(fields, names(current))) current[[field]] = NA
  for (field in setdiff(fields, names(master))) master[[field]] = NA
  current = current[, fields, drop = F]
  master = master[, fields, drop = F]
  state = list()
  panel = list()
  for (key in union(master$variant_key, current$variant_key)) {
    j = match(key, master$variant_key)
    i = match(key, current$variant_key)
    if (!is.na(i)) {
      row = current[i, , drop = F]
      row$first_seen = if (!is.na(j)) master$first_seen[j] else run_date
      if (is.na(row$first_seen)) row$first_seen = run_date
      row$last_seen = run_date
      row$lifecycle = "active"
      row$not_seen_complete_count = 0L
      row$last_absence_date = NA_character_
      day = row
    } else {
      row = master[j, , drop = F]
      distinct_day = is.na(row$last_absence_date) ||
        row$last_absence_date != run_date
      if (inventory_complete && distinct_day) {
        row$not_seen_complete_count = row$not_seen_complete_count + 1L
        row$last_absence_date = run_date
      }
      row$lifecycle = if (row$not_seen_complete_count >=
        removal_confirmations) "removed_confirmed" else "not_seen"
      day = row
      # Never carry yesterday's price/availability into an unseen day row.
      for (field in intersect(c("price_current", "price_reference",
          "price_lowest_30_days", "price_per_l", "price_per_l_displayed",
          "deposit", "multi_buy_price"), names(day))) day[[field]] = NA_real_
      day$availability = "not_observed"
      day$availability_raw = NA_character_
      day$observed_at = NA_character_
      day$region_confirmed = F
      for (field in intersect(c("raw_file", "raw_sha256", "html_file",
          "html_sha256", "capture_id"), names(day))) day[[field]] =
            NA_character_
      day$parse_status = "not_observed_this_run"
    }
    day$run_date = run_date
    day$inventory_complete = inventory_complete
    day$price_analysis_eligible = F
    state[[length(state) + 1L]] = row
    panel[[length(panel) + 1L]] = day
  }
  list(master = fp_rows(state), panel = fp_rows(panel))
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 3) COMPLETENESS AND STATISTICS GATES
# <-----------------------------------------------------------------------------

fp_assess_run = \(categories, variants, candidates, observations, controls,
  captures, frontier_exhausted = F, classification_approved = F,
  ruleset_verified = F) {
  reasons = character()
  if (!frontier_exhausted) reasons = c(reasons, "frontier_not_exhausted")
  if (any(categories$scope_review == "pending_beverage_scope_review")) {
    reasons = c(reasons, "category_scope_review_pending")
  }
  required = categories$scope_review != "excluded_nonbeverage"
  if (!nrow(categories) || any(required & (!categories$visited_regional |
      !categories$terminal_verified))) reasons = c(reasons,
    "category_terminal_evidence_missing")
  if (nrow(controls) && any(!controls$target_verified &
      controls$region_confirmed)) reasons = c(reasons,
    "unresolved_navigation_controls")
  if (nrow(candidates) && any(candidates$region_confirmed &
      candidates$identity_status != "unique_id_pack_match")) reasons =
        c(reasons, "unresolved_snapshot_variants")
  if (any(variants$identity_conflict)) reasons = c(reasons,
    "conflicting_variant_identity")
  if (any(captures$region_confirmed & (captures$html_truncated |
      captures$dom_truncated | captures$text_truncated))) reasons =
        c(reasons, "truncated_capture")
  if (anyDuplicated(observations$variant_key) ||
      anyDuplicated(observations$variant_id)) reasons = c(reasons,
    "duplicate_variant_observations")
  missing = setdiff(variants$variant_id, observations$variant_id)
  if (length(missing)) reasons = c(reasons, "variant_observations_missing")
  if (!nrow(observations) || any(!observations$region_confirmed) ||
      anyNA(observations$postal_code) ||
      any(observations$postal_code != "10997")) reasons =
        c(reasons, "postcode_unproven")
  invalid_price = is.na(observations$price_current) |
    observations$price_current <= 0 | is.na(observations$volume_l) |
    observations$volume_l <= 0 | observations$availability == "unknown"
  invalid_price[is.na(invalid_price)] = T
  if (any(invalid_price)) reasons = c(reasons, "price_or_availability_gap")
  pack_gap = is.na(observations$pack_count) |
    is.na(observations$unit_volume_l) | observations$pack_count < 1 |
    observations$unit_volume_l <= 0 |
    abs(observations$volume_l - observations$pack_count *
      observations$unit_volume_l) > 1e-6
  pack_gap[is.na(pack_gap)] = T
  if (any(pack_gap)) reasons = c(reasons, "unresolved_or_conflicting_pack")
  base_conflict = !is.na(observations$price_per_l_displayed) &
    abs(observations$price_current / observations$volume_l -
      observations$price_per_l_displayed) > 0.011
  base_conflict[is.na(base_conflict)] = T
  if (any(base_conflict)) reasons = c(reasons, "displayed_base_price_conflict")
  inventory_complete = !length(reasons)
  statistics_reasons = reasons
  if (anyNA(observations$alcohol_content)) statistics_reasons =
    c(statistics_reasons, "missing_exact_abv")
  if (any(observations$alcohol_content < 0 |
      observations$alcohol_content > 100, na.rm = T)) statistics_reasons =
    c(statistics_reasons, "invalid_abv")
  eligible = !is.na(observations$alcohol_content) &
    observations$alcohol_content >= 0.5
  if (!classification_approved || !ruleset_verified ||
      anyNA(observations$analysis_category[eligible]) ||
      anyNA(observations$classification_version[eligible])) statistics_reasons =
    c(statistics_reasons, "classification_not_approved")
  list(inventory_complete = inventory_complete,
    complete_statistics_publishable = !length(statistics_reasons),
    inventory_reasons = unique(reasons),
    statistics_reasons = unique(statistics_reasons),
    expected_known_variants = nrow(variants),
    observed_detail_variants = nrow(observations),
    missing_known_variant_ids = missing,
    status = "offline_preparation_not_productive")
}
