# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261102

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) PANEL PREPARATION
# <-----------------------------------------------------------------------------

# Three separate questions are never mixed:
#   level   : same SKU, same product identity, same sale-unit descriptors,
#             valid sale price (direct comparison of price_cents)
#   unit    : level + secured sale volume + no base-price conflict
#   ethanol : unit + project eligibility (main or sensitivity analysis),
#             numeric ABV > 0, equal ABV and volume in both warehouses

fp_rp_numeric_columns = \() {
  c("price_eur", "price_cents", "total_volume_l", "alcohol_content",
    "pack_count", "unit_volume_l")
}

fp_rp_prepare = \(p) {
  for (cl in intersect(fp_rp_numeric_columns(), names(p))) {
    p[[cl]] = suppressWarnings(as.numeric(p[[cl]]))
  }
  for (cl in c("available", "analysis_ready", "analysis_ready_gt_0_5",
    "abv_text_conflict")) {
    if (cl %in% names(p)) p[[cl]] = p[[cl]] %in% c("TRUE", T)
  }
  p
}

fp_rp_same = \(a, b) (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b)

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 2) COMPARISON OF TWO WAREHOUSES ON ONE DAY
# <-----------------------------------------------------------------------------

# pa, pb: restored panels (character columns as stored). readiness_fn is the
# project's fp_price_readiness(x, policy). conflicts_*: SKUs with a displayed
# base-price conflict. density: ethanol density (g/ml) of the classifier.
fp_rp_compare = \(pa, pb, conflicts_a, conflicts_b, readiness_fn, density,
  label_a = "w36", label_b = "w26", time_a = NA_character_,
  time_b = NA_character_) {
  sh = intersect(pa$sku, pb$sku)
  a = fp_rp_prepare(pa[match(sh, pa$sku), ])
  b = fp_rp_prepare(pb[match(sh, pb$sku), ])
  n = length(sh)
  ident_cols = c("product_name", "brand", "article_description",
    "container_type", "content_kind")
  ident = rep(T, n)
  for (cl in ident_cols) ident = ident & fp_rp_same(a[[cl]], b[[cl]])
  abv_equal = fp_rp_same(a$alcohol_content, b$alcohol_content)
  ident = ident & abv_equal
  unit_cols = c("article_description", "container_type",
    "volume_attribute_l", "volume_text_l")
  sale_unit = rep(T, n)
  for (cl in unit_cols) {
    if (cl %in% names(a)) {
      sale_unit = sale_unit & fp_rp_same(a[[cl]], b[[cl]])
    }
  }
  valid_price = \(x) !is.na(x$price_cents) & x$price_cents > 0 &
    !is.na(x$price_eur) & x$price_eur == x$price_cents / 100
  both_price = valid_price(a) & valid_price(b)
  conf_a = sh %in% conflicts_a
  conf_b = sh %in% conflicts_b
  vol_ok = \(x) !is.na(x$total_volume_l) & x$total_volume_l > 0 &
    x$volume_status %in% c("consistent", "attribute_only", "text_only",
      "resolved_text", "resolved_attribute")
  vol_equal = vol_ok(a) & vol_ok(b) & a$total_volume_l == b$total_volume_l
  level = ident & sale_unit & both_price
  unit = level & vol_equal & !conf_a & !conf_b
  ra = readiness_fn(a, "available_only")
  rb = readiness_fn(b, "available_only")
  ra_all = readiness_fn(a, "all_priced")
  rb_all = readiness_fn(b, "all_priced")
  eth_main = unit & abv_equal & ra$price_eligible_primary &
    rb$price_eligible_primary
  eth_sens = unit & abv_equal & ra$price_eligible_sensitivity &
    rb$price_eligible_sensitivity
  eth_main_all = unit & abv_equal & ra_all$price_eligible_primary &
    rb_all$price_eligible_primary
  eth_sens_all = unit & abv_equal & ra_all$price_eligible_sensitivity &
    rb_all$price_eligible_sensitivity
  stopifnot(all(eth_main <= eth_sens), all(eth_main_all <= eth_sens_all))
  ethanol_g = ifelse(eth_sens_all, a$total_volume_l * 1000 *
    a$alcohol_content / 100 * density, NA_real_)
  p10 = \(x) ifelse(eth_sens_all, x$price_eur * 10 / ethanol_g, NA_real_)
  eth_a = p10(a)
  eth_b = p10(b)
  stopifnot(all(is.finite(eth_a[eth_main_all])), all(eth_a[eth_main_all] > 0))
  reasons = \(i) {
    r = c(
      if (!ident[i]) "identity_differs",
      if (ident[i] && !sale_unit[i]) "sale_unit_differs",
      if (!abv_equal[i]) "abv_differs",
      if (!both_price[i]) "invalid_price",
      if (level[i] && (conf_a[i] || conf_b[i])) "base_price_conflict",
      if (level[i] && !vol_equal[i]) "sale_volume_unresolved_or_different",
      if (unit[i] && !(ra$price_calculable[i] && rb$price_calculable[i])) {
        "not_price_calculable"
      },
      if (unit[i] && !(a$analysis_ready_gt_0_5[i] &&
          b$analysis_ready_gt_0_5[i])) "not_in_main_analysis",
      if (unit[i] && !(a$analysis_ready[i] && b$analysis_ready[i])) {
        "not_in_sensitivity_analysis"
      },
      if (unit[i] && !(is.finite(a$alcohol_content[i]) &&
          a$alcohol_content[i] > 0)) "abv_zero_or_missing",
      if (unit[i] && !(a$available[i] && b$available[i])) "not_available_both"
    )
    paste(r, collapse = ";")
  }
  minutes = if (is.na(time_a) || is.na(time_b)) NA_real_ else {
    round(abs(as.numeric(difftime(
      as.POSIXct(time_b, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      as.POSIXct(time_a, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      units = "mins"
    ))), 1)
  }
  first = if (is.na(time_a) || is.na(time_b)) NA_character_ else {
    if (time_a <= time_b) label_a else label_b
  }
  out = data.frame(
    sku = sh, product_name = a$product_name,
    price_a = a$price_cents, price_b = b$price_cents,
    price_diff_cents = b$price_cents - a$price_cents,
    price_diff_pct = round(100 * (b$price_cents - a$price_cents) /
      a$price_cents, 2),
    available_a = a$available, available_b = b$available,
    main_a = a$analysis_ready_gt_0_5, main_b = b$analysis_ready_gt_0_5,
    sens_a = a$analysis_ready, sens_b = b$analysis_ready,
    identity_equal = ident, sale_unit_equal = sale_unit,
    abv_equal = abv_equal, conflict_a = conf_a, conflict_b = conf_b,
    level_comparable = level, unit_comparable = unit,
    ethanol_main = eth_main, ethanol_sens = eth_sens,
    ethanol_main_incl_unavailable = eth_main_all,
    ethanol_sens_incl_unavailable = eth_sens_all,
    eth10g_price_a = eth_a, eth10g_price_b = eth_b,
    exclusion_reasons = vapply(seq_len(n), reasons, ""),
    label_a = label_a, label_b = label_b,
    observed_at_a = time_a, observed_at_b = time_b,
    minutes_between = minutes, observed_first = first,
    not_simultaneous = T,
    stringsAsFactors = F
  )
  out
}

fp_rp_summary = \(cmp) {
  list(
    shared = nrow(cmp),
    main_both = sum(cmp$main_a & cmp$main_b),
    sens_both = sum(cmp$sens_a & cmp$sens_b),
    level_comparable = sum(cmp$level_comparable),
    level_comparable_conflict_skus = sum(cmp$level_comparable &
      (cmp$conflict_a | cmp$conflict_b)),
    unit_comparable = sum(cmp$unit_comparable),
    ethanol_main = sum(cmp$ethanol_main),
    ethanol_sens = sum(cmp$ethanol_sens),
    ethanol_main_incl_unavailable = sum(cmp$ethanol_main_incl_unavailable),
    ethanol_sens_incl_unavailable = sum(cmp$ethanol_sens_incl_unavailable),
    level_equal_price = sum(cmp$level_comparable & cmp$price_diff_cents == 0),
    level_b_higher = sum(cmp$level_comparable & cmp$price_diff_cents > 0),
    level_b_lower = sum(cmp$level_comparable & cmp$price_diff_cents < 0)
  )
}
