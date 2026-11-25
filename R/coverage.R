# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261030

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) READ A COLLECTOR RUN DIRECTORY
# <-----------------------------------------------------------------------------

fp_cv_read_csv = \(path) {
  if (!file.exists(path)) return(NULL)
  read.csv(path, colClasses = "character", na.strings = character())
}

fp_cv_read_run = \(dir) {
  meta_file = file.path(dir, "meta.json")
  if (!file.exists(meta_file)) {
    stop("No meta.json in: ", dir, call. = F)
  }
  list(
    dir = dir,
    meta = jsonlite::read_json(meta_file, simplifyVector = T),
    variants = fp_cv_read_csv(file.path(dir, "variants.csv")),
    categories = fp_cv_read_csv(file.path(dir, "categories.csv")),
    membership = fp_cv_read_csv(file.path(dir, "membership.csv")),
    listing = fp_cv_read_csv(file.path(dir, "listing.csv")),
    replicates = fp_cv_read_csv(file.path(dir, "replicates.csv")),
    probe = fp_cv_read_csv(file.path(dir, "outside_probe.csv"))
  )
}

fp_cv_flag = \(x) !is.na(x) & x %in% c("TRUE", "T", "true")

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 2) ALCOHOL EVIDENCE FOR PRODUCTS IN REMOVED CATEGORIES
# <-----------------------------------------------------------------------------

fp_cv_alcohol_pattern = paste0(
  "alkohol|ethanol|wein|bier|\\brum\\b|lik(ö|oe)r|branntwein|",
  "weinbrand|whisk|vodka|wodka|\\bgin\\b|sekt|champagner|cognac|",
  "kirsch|\\bkorn\\b|spirituose|schnaps|\\d\\s*%\\s*vol"
)

# Returns one row per SKU: evidence class and reason.
fp_cv_alcohol_evidence = \(prior, texts) {
  n = nrow(prior)
  ingredients = if (is.null(texts)) {
    rep(NA_character_, n)
  } else {
    texts$ingredients_text[match(prior$sku, texts$sku)]
  }
  filter = prior$abv_filter_min_0_5
  has_text = !is.na(ingredients) & nzchar(trimws(ingredients))
  hit = has_text &
    grepl(fp_cv_alcohol_pattern, tolower(ingredients), perl = T)
  evidence = rep("alcohol_not_excluded", n)
  reason = rep("no_exclusion_evidence", n)
  not_applicable = filter == "not_applicable"
  evidence[not_applicable] = "non_alcoholic"
  reason[not_applicable] = "classified_abv_not_applicable"
  by_text = !not_applicable & filter == "unknown" & has_text & !hit
  evidence[by_text] = "non_alcoholic"
  reason[by_text] = "ingredients_without_alcohol_terms"
  reason[!not_applicable & hit] = "ingredients_with_alcohol_terms"
  data.frame(
    sku = prior$sku,
    evidence = evidence,
    reason = reason,
    ingredients = ifelse(has_text, ingredients, NA_character_),
    stringsAsFactors = F
  )
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 3) DISPLAYED BASE PRICE CONFLICTS
# <-----------------------------------------------------------------------------

# The sales price (price_cents) is never touched. Affected SKUs only
# get a warning so that derived per-litre or per-ethanol metrics stay
# suppressed while the sold volume is unresolved.
fp_cv_base_price_conflicts = \(variants) {
  num = \(x) suppressWarnings(as.numeric(x))
  per_l = num(variants$price_eur) / num(variants$total_volume_l)
  shown = suppressWarnings(as.numeric(sub(",", ".", gsub(
    "[^0-9,]", "", variants$price_per_unit_text
  ))))
  bad = !is.na(shown) & !is.na(per_l) & abs(per_l - shown) > 0.011
  variants$sku[bad]
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 4) COVERAGE ASSESSMENT OF A BLOCKED RUN
# <-----------------------------------------------------------------------------

fp_cv_category_blocker = "category_tree_differs_from_reviewed_set"

fp_cv_check = \(name, ok, detail = "") {
  data.frame(
    check = name, ok = isTRUE(ok),
    detail = paste(as.character(detail), collapse = ","),
    stringsAsFactors = F
  )
}

fp_cv_fetch_ids = \(categories) {
  sort(unique(categories$sub_id[
    fp_cv_flag(categories$fetch)
  ]))
}

fp_cv_assess = \(run_dir, prior_dir = NULL, prior_texts_csv = NULL,
  scope_roots = c("19", "23", "24"), prior = NULL,
  require_classification = F, reference = NULL) {
  cur = fp_cv_read_run(run_dir)
  old = if (!is.null(prior)) prior else if (!is.null(prior_dir)) {
    fp_cv_read_run(prior_dir)
  } else NULL
  baseline = is.null(old)
  if (baseline) {
    old = list(meta = list(warehouse_id = cur$meta$warehouse_id),
      categories = cur$categories, membership = cur$membership,
      variants = cur$variants)
  }
  texts = if (is.null(prior_texts_csv)) NULL else {
    fp_cv_read_csv(prior_texts_csv)
  }
  started = as.POSIXct(cur$meta$started_at,
    format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  date = format(started, "%Y-%m-%d", tz = "Europe/Berlin")
  wid = as.character(cur$meta$warehouse_id)
  acc = new.env(parent = emptyenv())
  acc$rows = list()
  add = \(...) {
    acc$rows[[length(acc$rows) + 1L]] = fp_cv_check(...)
  }

  blocking = as.character(cur$meta$inventory_blocking)
  add("collector_blocker_is_category_only",
    !length(blocking) || identical(blocking, fp_cv_category_blocker),
    paste(blocking, collapse = ","))
  add("full_scope_run", identical(cur$meta$scope_limited, F),
    cur$meta$scope_limited)
  add("same_warehouse",
    identical(as.character(old$meta$warehouse_id), wid), wid)

  cat_cur = cur$categories
  cat_old = old$categories
  add("scope_roots_in_tree",
    all(scope_roots %in% cat_cur$root_id),
    paste(setdiff(scope_roots, cat_cur$root_id), collapse = ","))

  now_ids = fp_cv_fetch_ids(cat_cur)
  old_ids = fp_cv_fetch_ids(cat_old)
  listed = sort(unique(cur$listing$sub_id))
  add("all_current_categories_fetched",
    setequal(now_ids, listed),
    paste(c(setdiff(now_ids, listed), setdiff(listed, now_ids)),
      collapse = ","))
  add("no_skipped_relevant_category", {
    skipped = cat_cur$sub_id[
      fp_cv_flag(cat_cur$in_scope) &
        !fp_cv_flag(cat_cur$nonbeverage_excluded) &
        !fp_cv_flag(cat_cur$fetch)
    ]
    !length(skipped)
  })
  add("replicate_lists_stable",
    !is.null(cur$replicates) && nrow(cur$replicates) > 0L &&
      all(fp_cv_flag(cur$replicates$same_sku_set)))
  add("outside_scope_probe_clean",
    !is.null(cur$probe) && nrow(cur$probe) > 0L &&
      all(as.integer(cur$probe$alcohol_hits) == 0L) &&
      all(as.integer(cur$probe$detail_missing) == 0L))
  v = cur$variants
  add("sku_unique", !anyDuplicated(v$sku) && all(nzchar(v$sku)))
  add("membership_matches_variants",
    setequal(unique(cur$membership$sku), v$sku))
  per_sub = tapply(
    cur$membership$sku, cur$membership$sub_id,
    \(x) length(unique(x))
  )
  list_n = setNames(as.integer(cur$listing$variants),
    cur$listing$sub_id)
  add("listing_counts_match_membership",
    all(per_sub[names(list_n)] == list_n, na.rm = F),
    "")
  tree_ids = unique(cat_cur$sub_id)

  removed = setdiff(old_ids, now_ids)
  added = setdiff(now_ids, old_ids)
  add("removed_categories_gone_from_tree",
    !any(removed %in% tree_ids), paste(
      intersect(removed, tree_ids), collapse = ","))

  prior_member = old$membership
  prior_v = old$variants
  miss = setdiff(prior_v$sku, v$sku)
  fresh = setdiff(v$sku, prior_v$sku)
  cats_of = \(m, s) tapply(
    m$sub_id[m$sku %in% s], m$sku[m$sku %in% s],
    \(x) paste(sort(unique(x)), collapse = "+")
  )
  old_cats = if (length(miss)) {
    cats_of(prior_member, miss)
  } else character()
  only_removed = vapply(
    strsplit(old_cats[miss], "+", fixed = T),
    \(x) all(x %in% removed), logical(1)
  )
  names(only_removed) = miss

  p_miss = prior_v[match(miss, prior_v$sku), ]
  evid = fp_cv_alcohol_evidence(p_miss, texts)
  covered = only_removed & evid$evidence == "non_alcoholic"
  uncovered = only_removed & !covered
  add("removed_category_skus_not_alcoholic",
    !any(uncovered), paste(miss[uncovered], collapse = ","))

  assort_cols = c("date", "warehouse_id", "change_type", "sku",
    "product_name", "prior_categories", "last_observed_date",
    "alcohol_evidence", "evidence_reason", "note")
  last_date = substr(p_miss$observed_at, 1, 10)
  assort = if (!length(miss)) {
    as.data.frame(setNames(
      rep(list(character()), length(assort_cols)), assort_cols
    ))
  } else data.frame(
    date = date, warehouse_id = wid,
    change_type = ifelse(
      only_removed, "removed_with_category",
      "absent_from_active_category"
    ),
    sku = miss,
    product_name = p_miss$product_name,
    prior_categories = unname(old_cats[miss]),
    last_observed_date = last_date,
    alcohol_evidence = evid$evidence,
    evidence_reason = evid$reason,
    note = ifelse(
      only_removed,
      "category removed from shop tree; not alcoholic",
      "absence is not proof of unavailability or delisting"
    ),
    stringsAsFactors = F
  )
  if (length(fresh)) {
    f_cats = cats_of(cur$membership, fresh)
    assort = rbind(assort, data.frame(
      date = date, warehouse_id = wid,
      change_type = "new_in_assortment", sku = fresh,
      product_name = v$product_name[match(fresh, v$sku)],
      prior_categories = unname(f_cats[fresh]),
      last_observed_date = NA_character_,
      alcohol_evidence = NA_character_,
      evidence_reason = NA_character_,
      note = "first seen in this run",
      stringsAsFactors = F
    ))
  }
  rownames(assort) = NULL

  cat_changes = data.frame()
  lost_any = c(removed, added)
  if (length(lost_any)) {
    src = rbind(
      cbind(cat_old[match(removed, cat_old$sub_id),
        c("sub_id", "sub_name", "root_name")],
        change_type = rep("removed", length(removed))),
      cbind(cat_cur[match(added, cat_cur$sub_id),
        c("sub_id", "sub_name", "root_name")],
        change_type = rep("added", length(added)))
    )
    members = \(id) unique(prior_member$sku[prior_member$sub_id == id])
    src$prior_member_skus = vapply(src$sub_id, \(id) {
      length(members(id))
    }, 1L)
    src$still_listed_elsewhere = vapply(src$sub_id, \(id) {
      sum(members(id) %in% v$sku)
    }, 1L)
    src$removed_only_skus = vapply(src$sub_id, \(id) {
      sum(members(id) %in% miss[only_removed])
    }, 1L)
    cat_changes = cbind(date = date, warehouse_id = wid, src)
    rownames(cat_changes) = NULL
  }

  conflicts = fp_cv_base_price_conflicts(v)
  warn = data.frame(
    date = character(), warehouse_id = character(),
    warning = character(), sku = character(),
    product_name = character(), detail = character(),
    stringsAsFactors = F
  )
  mk = \(w, s, nm, d) data.frame(
    date = date, warehouse_id = wid, warning = w, sku = s,
    product_name = nm, detail = d, stringsAsFactors = F
  )
  if (length(conflicts)) {
    warn = rbind(warn, mk(
      "displayed_base_price_conflict", conflicts,
      v$product_name[match(conflicts, v$sku)],
      paste0("price_cents unchanged; derived price metrics ",
        "suppressed while sold volume unresolved")
    ))
  }
  unexplained = assort$sku[
    assort$change_type == "absent_from_active_category" &
      assort$alcohol_evidence != "non_alcoholic"
  ]
  if (length(unexplained)) {
    warn = rbind(warn, mk(
      "alcoholic_sku_absent_from_active_category", unexplained,
      assort$product_name[match(unexplained, assort$sku)],
      "absence is not proof of unavailability or delisting"
    ))
  }
  if (length(removed)) {
    warn = rbind(warn, mk(
      "category_removed_from_shop_tree", NA_character_,
      NA_character_, paste(removed, collapse = ",")
    ))
  }
  if (length(added)) {
    warn = rbind(warn, mk(
      "category_added_to_shop_tree", NA_character_,
      NA_character_, paste(added, collapse = ",")
    ))
  }
  if (baseline) {
    warn = rbind(warn, mk(
      "baseline_run_without_prior_reference", NA_character_,
      NA_character_, paste0(
        "first run of warehouse; category set and assortment ",
        "become the reviewed starting point"
      )
    ))
  }
  if (require_classification) {
    need = c("price_category", "category_state", "inclusion_state",
      "analysis_ready", "analysis_ready_gt_0_5")
    levels7 = c("Bier", "Wein", "Schaumwein", "Spirituosen",
      "Zwischenerzeugnisse ≤15 % vol",
      "Zwischenerzeugnisse >15 % vol", "Alkopops")
    cats_ok = all(need %in% names(v)) &&
      !anyNA(v$category_state) &&
      all(v$price_category[!is.na(v$price_category) &
        nzchar(v$price_category)] %in% levels7)
    add("classification_rules_applied", cats_ok,
      paste(setdiff(need, names(v)), collapse = ","))
  }
  rownames(warn) = NULL
  regional = NULL
  if (!is.null(reference) &&
      !identical(as.character(reference$warehouse_id), wid)) {
    regional = fp_cv_regional_diff(
      cur, reference, date, wid
    )
  }

  checks = do.call(rbind, acc$rows)
  rownames(checks) = NULL
  list(
    pass = all(checks$ok),
    baseline = baseline,
    date = date,
    warehouse_id = wid,
    checks = checks,
    failed = checks$check[!checks$ok],
    regional_differences = regional,
    category_changes = cat_changes,
    assortment_changes = assort,
    quality_warnings = warn,
    category_tree = cbind(date = date, warehouse_id = wid,
      cat_cur),
    category_membership = cbind(date = date, warehouse_id = wid,
      cur$membership)
  )
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 4b) PER-WAREHOUSE REFERENCE FROM THE STORE AND REGIONAL DIFFERENCES
# <-----------------------------------------------------------------------------

# Latest committed day of one warehouse as reference for its next run.
# Returns NULL if the warehouse has no commit yet (baseline run).
fp_cv_prior_from_store = \(root, warehouse_id, before = "9999-12-31") {
  history = fp_vs_history(root)
  if (!length(history)) return(NULL)
  wid = as.integer(warehouse_id)
  hit = which(vapply(history, \(m) {
    as.integer(m$warehouse_id) == wid && m$date < before
  }, logical(1)))
  if (!length(hit)) return(NULL)
  dates = vapply(history[hit], \(m) m$date, "")
  day = max(dates)
  cid = names(history)[hit][which(dates == day)[1]]
  tree_file = file.path(root, "commits", cid, "category_tree.csv.gz")
  member_file = file.path(
    root, "commits", cid, "category_membership.csv.gz"
  )
  if (!file.exists(tree_file) || !file.exists(member_file)) {
    stop("Reference commit has no category tree: ", cid, call. = F)
  }
  drop = c("date", "warehouse_id")
  tree = fp_vs_read_gz(tree_file)
  member = fp_vs_read_gz(member_file)
  panel = fp_vs_restore(root, day, day, wid)
  list(
    warehouse_id = wid,
    meta = list(warehouse_id = wid, date = day),
    categories = tree[, setdiff(names(tree), drop)],
    membership = member[, setdiff(names(member), drop)],
    variants = panel,
    commit = cid
  )
}

# Fetchable category ids as the collector's reviewed-set check sees them.
fp_cv_reviewed_ids = \(categories) {
  ok = fp_cv_flag(categories$in_scope) &
    fp_cv_flag(categories$visible) &
    !fp_cv_flag(categories$nonbeverage_excluded)
  as.integer(sort(unique(categories$sub_id[ok])))
}

# Category-level comparison with a reference warehouse; documents regional
# differences without treating them as errors.
fp_cv_regional_diff = \(cur, reference, date, wid) {
  ref_cat = reference$categories
  ids = sort(unique(c(
    fp_cv_fetch_ids(cur$categories), fp_cv_fetch_ids(ref_cat)
  )))
  n_in = \(m, id) length(unique(m$sku[m$sub_id == id]))
  nm = \(id) {
    a = c(cur$categories$sub_name[cur$categories$sub_id == id],
      ref_cat$sub_name[ref_cat$sub_id == id])
    if (length(a)) a[1] else NA_character_
  }
  rows = data.frame(
    date = date, warehouse_id = wid,
    reference_warehouse_id = as.character(reference$warehouse_id),
    scope = "category", id = ids,
    name = vapply(ids, nm, ""),
    in_reference = ids %in% fp_cv_fetch_ids(ref_cat),
    in_warehouse = ids %in% fp_cv_fetch_ids(cur$categories),
    skus_reference = vapply(ids, \(i) n_in(reference$membership, i), 1L),
    skus_warehouse = vapply(ids, \(i) n_in(cur$membership, i), 1L),
    stringsAsFactors = F
  )
  ref_sku = unique(reference$membership$sku)
  cur_sku = unique(cur$membership$sku)
  total = data.frame(
    date = date, warehouse_id = wid,
    reference_warehouse_id = as.character(reference$warehouse_id),
    scope = "sku", id = c("shared", "only_reference", "only_warehouse"),
    name = NA_character_, in_reference = NA, in_warehouse = NA,
    skus_reference = c(
      length(intersect(ref_sku, cur_sku)),
      length(setdiff(ref_sku, cur_sku)), 0L
    ),
    skus_warehouse = c(
      length(intersect(ref_sku, cur_sku)), 0L,
      length(setdiff(cur_sku, ref_sku))
    ),
    stringsAsFactors = F
  )
  out = rbind(rows, total)
  rownames(out) = NULL
  out
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 5) SUPPRESSED PRICE METRICS LOOKUP
# <-----------------------------------------------------------------------------

# Derived price per 10 g ethanol: only for rows whose sold volume is secured
# and whose displayed base price does not conflict. The sales price itself is
# never modified.
fp_cv_ethanol_price = \(panel, suppressed_skus, density) {
  num = \(x) suppressWarnings(as.numeric(x))
  volume = num(panel$total_volume_l)
  abv = num(panel$alcohol_content)
  price = num(panel$price_eur)
  secure = panel$volume_status %in% c("consistent", "attribute_only",
    "text_only", "resolved_text", "resolved_attribute")
  use = !is.na(volume) & volume > 0 & !is.na(abv) & abv > 0 &
    !is.na(price) & secure & !panel$sku %in% suppressed_skus
  panel$ethanol_g = NA_real_
  panel$price_eur_per_10g = NA_real_
  panel$ethanol_g[use] = volume[use] * 1000 * abv[use] / 100 * density
  panel$price_eur_per_10g[use] = price[use] * 10 / panel$ethanol_g[use]
  panel$price_metrics_suppressed = panel$sku %in% suppressed_skus
  panel
}

fp_cv_suppressed_price_skus = \(root, date, warehouse_id) {
  cid = fp_vs_find_commit(root, date, warehouse_id)
  if (is.null(cid)) return(character())
  path = file.path(root, "commits", cid, "quality_warnings.csv.gz")
  if (!file.exists(path)) return(character())
  w = fp_vs_read_gz(path)
  w$sku[w$warning == "displayed_base_price_conflict"]
}
