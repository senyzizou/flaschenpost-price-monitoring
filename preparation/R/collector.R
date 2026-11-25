# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING - FLASCHENPOST PREPARATION
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261026

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) HTTP TRANSPORT WITH BUDGET, PACING AND LIMITED RETRIES
# <-----------------------------------------------------------------------------

# The collector knows only a warehouse id. The delivery address never enters
# this module; it is not an argument, a field or a log entry here.

fp_http_transport = \(url, query, collector) {
  req = do.call(req_url_query, c(list(request(url)), query))
  req = req_user_agent(req, collector$user_agent)
  req = req_timeout(req, collector$timeout_seconds)
  req = req_options(req, followlocation = F, maxredirs = 0L)
  req = req_error(req, is_error = \(resp) F)
  out = tryCatch({
    resp = req_perform(req)
    list(status = resp_status(resp), body = resp_body_string(resp))
  }, error = \(e) list(status = NA_integer_, body = ""))
  out
}

fp_no_secret = fp_secret(list(street = "", number = ""))

fp_ctx = \(collector, runner, budget, transport = fp_http_transport,
  sleep = Sys.sleep, raw_dir = NULL, log_file = NULL, clock = Sys.time) {
  state = new.env(parent = emptyenv())
  state$calls = list()
  list(collector = collector, runner = runner, budget = budget,
    transport = transport, sleep = sleep, clock = clock, raw_dir = raw_dir,
    log_file = log_file, state = state)
}

# One logical request; returns parsed JSON. Blocks (403/429) never retry.
fp_http_json = \(ctx, path, query = list(), label = path) {
  collector = ctx$collector
  url = paste0(collector$api_base, "/", path)
  if (!fp_url_allowed(url, ctx$runner)) stop("URL not allowed.", call. = F)
  tries = collector$max_retries + 1L
  for (attempt in seq_len(tries)) {
    fp_budget_step(ctx$budget)
    res = ctx$transport(url, query, collector)
    status = as.integer(res$status)
    if (!is.null(ctx$log_file)) fp_log(ctx$log_file, "api", fp_no_secret,
      path = path, status = status, attempt = attempt)
    if (!is.na(status) && status %in% c(403L, 429L)) stop(
      "Access limit signal HTTP ", status, "; stopping without retry.",
      call. = F)
    if (!is.na(status) && status == 200L) break
    retryable = is.na(status) || status >= 500L
    if (!retryable || attempt == tries) stop("HTTP request failed for ",
      path, " (status ", status, ").", call. = F)
    ctx$sleep(collector$retry_pause_seconds * attempt)
  }
  parsed = tryCatch(fromJSON(res$body, simplifyVector = F),
    error = \(e) NULL)
  if (is.null(parsed) || !is.list(parsed)) stop("Response is not JSON for ",
    path, ".", call. = F)
  seq_id = length(ctx$state$calls) + 1L
  sha = NA_character_
  if (!is.null(ctx$raw_dir)) {
    dir.create(ctx$raw_dir, showWarnings = F, recursive = T)
    file = file.path(ctx$raw_dir, sprintf("%04d_%s.json", seq_id,
      gsub("[^A-Za-z0-9]+", "_", label)))
    if (file.exists(file)) stop("Raw evidence file already exists.", call. = F)
    sha = fp_write_exact(res$body, file)
  }
  ctx$state$calls[[seq_id]] = data.frame(seq = seq_id, path = path,
    query = paste(names(query), unlist(query), sep = "=", collapse = "&"),
    status = status, chars = nchar(res$body), sha256 = sha,
    attempts = attempt, at = format(ctx$clock(), "%Y-%m-%dT%H:%M:%SZ",
      tz = "UTC"), stringsAsFactors = F)
  parsed
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 2) SCHEMA CHECKS AND CATEGORY TREE
# <-----------------------------------------------------------------------------

fp_schema_fail = \(what) stop("Schema change: ", what, call. = F)

fp_de = \(x) {
  value = if (is.list(x)) x[["de-DE"]] else NULL
  if (is.null(value)) NA_character_ else as.character(value)
}

fp_category_table = \(tree, collector) {
  if (!is.list(tree) || !length(tree)) fp_schema_fail("empty category tree")
  rows = list()
  for (root in tree) {
    if (is.null(root$externalId) || is.null(root$name) ||
        !is.list(root$children)) fp_schema_fail("category node fields")
    for (child in root$children) {
      if (is.null(child$externalId) || is.null(child$name)) fp_schema_fail(
        "subcategory node fields")
      visible = isTRUE(child$custom$fields$IsVisible)
      in_scope = as.integer(root$externalId) %in%
        collector$scope_root_external_ids
      excluded = as.integer(child$externalId) %in%
        collector$nonbeverage_subcategory_ids
      rows[[length(rows) + 1L]] = data.frame(
        root_id = as.integer(root$externalId), root_name = fp_de(root$name),
        sub_id = as.integer(child$externalId), sub_name = fp_de(child$name),
        sub_slug = fp_de(child$slug), visible = visible,
        in_scope = in_scope, nonbeverage_excluded = excluded,
        fetch = in_scope && visible && !excluded, stringsAsFactors = F)
    }
  }
  fp_rows(rows)
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 3) PRODUCT LISTS AND DETAILS: VARIANTS STAY SEPARATE BY SKU
# <-----------------------------------------------------------------------------

fp_attr = \(variant, name) {
  for (attribute in variant$attributes) {
    if (identical(attribute$name, name)) return(attribute$value)
  }
  NULL
}

fp_cents = \(value) {
  if (is.null(value$centAmount)) NA_real_ else as.numeric(value$centAmount)
}

fp_unit_factor = \(unit) {
  key = if (is.list(unit)) unit$key else unit
  switch(as.character(key), ml = 1e-3, cl = 1e-2, l = 1, NA_real_)
}

# Aggregate lists such as Bierwelt also hold snacks sold by weight.
fp_unit_kind = \(unit) {
  key = as.character(if (is.list(unit)) unit$key else unit)
  if (!is.na(fp_unit_factor(unit))) "volume" else
    if (key %in% c("g", "kg")) "mass" else "other"
}

fp_check_product = \(product, collector) {
  if (is.null(product$key) || is.null(product$name) ||
      is.null(product$masterVariant)) fp_schema_fail("product fields")
  for (variant in c(list(product$masterVariant), product$variants)) {
    if (is.null(variant$sku) || is.null(variant$price$value$centAmount) ||
        !is.list(variant$attributes)) fp_schema_fail("variant fields")
    names_seen = vapply(variant$attributes, \(a) as.character(a$name), "")
    missing = setdiff(collector$required_attributes, names_seen)
    if (length(missing)) fp_schema_fail(paste("attributes missing:",
      paste(missing, collapse = ",")))
    unit = fp_attr(variant, "VolumeUnit")
    if (identical(fp_unit_kind(unit), "other")) fp_schema_fail(
      "unknown volume unit")
    if (!identical(variant$price$value$currencyCode, "EUR")) fp_schema_fail(
      "currency")
    if (!is.logical(fp_attr(variant, "IsAvailable")) ||
        length(fp_attr(variant, "IsAvailable")) != 1L ||
        is.na(fp_attr(variant, "IsAvailable"))) fp_schema_fail(
      "availability must be a boolean")
  }
  invisible(T)
}

fp_brand = \(product) {
  for (category in product$categories) {
    if (identical(category$obj$custom$type$key, "fp-category-brand")) {
      return(list(id = as.integer(category$obj$externalId),
        name = fp_de(category$obj$name), slug = fp_de(category$obj$slug)))
    }
  }
  list(id = NA_integer_, name = NA_character_, slug = NA_character_)
}

fp_subcategory_ids = \(product) {
  ids = integer()
  for (category in product$categories) {
    if (identical(category$obj$custom$type$key, "fp-category-subcategory")) {
      ids = c(ids, as.integer(category$obj$externalId))
    }
  }
  ids
}

fp_parse_products = \(products, collector, warehouse_id, observed_at,
  warehouse_status) {
  rows = list()
  for (product in products) {
    fp_check_product(product, collector)
    brand = fp_brand(product)
    variants = c(list(product$masterVariant), product$variants)
    for (index in seq_along(variants)) {
      v = variants[[index]]
      kind = fp_unit_kind(fp_attr(v, "VolumeUnit"))
      unit = fp_unit_factor(fp_attr(v, "VolumeUnit"))
      unit_l = as.numeric(fp_attr(v, "Volume")) * unit
      pack = as.numeric(fp_attr(v, "ItemsInFrame"))
      described = fp_attr(v, "ArticleDescription")
      text_pack = fp_pack(if (is.character(described)) described else "")
      custom = v$price$custom$fields
      tiers = vapply(c("", "10", "20", "30", "40"), \(t) fp_cents(
        fp_attr(v, paste0("ZipCodeReducedPrice", t))), numeric(1))
      row = list(warehouse_id = warehouse_id,
        warehouse_status = warehouse_status, product_key = product$key,
        sku = as.character(v$sku), is_master_variant = index == 1L,
        product_name = fp_de(product$name), brand_id = brand$id,
        brand = brand$name, product_slug = fp_de(product$slug),
        product_url = if (is.na(brand$slug)) NA_character_ else
          paste0("https://www.flaschenpost.de/p/", brand$slug, "/",
            fp_de(product$slug)),
        price_cents = as.numeric(v$price$value$centAmount),
        price_eur = as.numeric(v$price$value$centAmount) / 100,
        price_per_unit_text = custom$PricePerUnitText,
        regional_price = identical(custom$Regional, "YES"),
        reduced_price_cents = tiers[[1]], reduced_10_cents = tiers[[2]],
        reduced_20_cents = tiers[[3]], reduced_30_cents = tiers[[4]],
        reduced_40_cents = tiers[[5]],
        lowest_30d_cents = fp_cents(fp_attr(v, "LowestPriceInLastMonth")),
        content_kind = kind, pack_count = pack, unit_volume_l = unit_l,
        total_volume_l = pack * unit_l,
        text_pack_count = text_pack$pack_count,
        text_unit_volume_l = text_pack$unit_volume_l,
        text_volume_l = text_pack$volume_l,
        article_description = as.character(fp_attr(v,
          "ArticleDescription")),
        container_type = fp_attr(v, "ContainerType")$key,
        deposit_total_eur = fp_cents(fp_attr(v, "DepositOverall")) / 100,
        deposit_per_frame_eur = fp_cents(fp_attr(v,
          "DepositPerFrame")) / 100,
        available = isTRUE(fp_attr(v, "IsAvailable")),
        delivery_state_info = as.character(fp_attr(v,
          "DeliveryStateInfo")),
        article_status = custom$ArticleStatus,
        distribution_status = custom$DistributionStatus,
        description_abv = fp_abv(fp_clean(gsub("<[^>]+>", " ",
          fp_de(product$description))))$alcohol_content,
        description_text = fp_clean(gsub("<[^>]+>", " ",
          fp_de(product$description))),
        subcategory_ids = paste(fp_subcategory_ids(product), collapse = ";"),
        observed_at = observed_at)
      row = lapply(row, \(x) if (is.null(x) || !length(x)) NA else x[[1]])
      rows[[length(rows) + 1L]] = as.data.frame(row, stringsAsFactors = F)
    }
  }
  fp_rows(rows)
}

fp_membership = \(products, sub_id) {
  fp_rows(lapply(products, \(product) {
    skus = vapply(c(list(product$masterVariant), product$variants),
      \(v) as.character(v$sku), "")
    data.frame(sub_id = sub_id, product_key = product$key, sku = skus,
      stringsAsFactors = F)
  }))
}

fp_parse_details = \(products, requested) {
  rows = list()
  for (product in products) {
    if (is.null(product$key) || is.null(product$masterVariant)) {
      fp_schema_fail("detail fields")
    }
    for (v in c(list(product$masterVariant), product$variants)) {
      abv = fp_attr(v, "AlcoholContent")
      rows[[length(rows) + 1L]] = data.frame(product_key = product$key,
        sku = as.character(v$sku), alcohol_content = if (is.null(abv)) NA_real_
          else as.numeric(abv), abv_in_detail = !is.null(abv),
        detail_description_text = fp_clean(gsub("<[^>]+>", " ",
          fp_de(product$description))),
        ingredients_text = fp_clean(as.character(fp_attr(v,
          "Ingredients"))[1]), stringsAsFactors = F)
    }
  }
  list(details = fp_rows(rows), returned_keys = vapply(products,
    \(p) as.character(p$key), ""), missing_keys = setdiff(requested,
    vapply(products, \(p) as.character(p$key), "")))
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 4) COLLECTION RUN AND COMPLETENESS CHECKS
# <-----------------------------------------------------------------------------

fp_collect = \(ctx, warehouse_id, purpose = c("technical_test",
  "inventory_proof", "production"), subcategory_ids = NULL, with_details = T,
  brand_checks = 0L, replicate_lists = 0L, probe_ids = integer(),
  evidence = fp_load_abv_evidence(), classifier = NULL) {
  purpose = match.arg(purpose)
  collector = ctx$collector
  runner = ctx$runner
  if (!isTRUE(runner$live_authorized)) stop("Live access is not authorized.",
    call. = F)
  verification = collector$warehouse_verification
  verified = identical(verification$status, "verified") &&
    verification$independent_sessions >= 2L
  if (purpose != "technical_test") {
    if (!verified) stop("Warehouse assignment is not verified.", call. = F)
    if (purpose == "production" && !isTRUE(runner$terms_cleared)) stop(
      "Terms release is missing.", call. = F)
    if (!is.null(subcategory_ids)) stop("This purpose needs every category.",
      call. = F)
    if (!identical(as.integer(warehouse_id),
        as.integer(collector$warehouse_id))) {
      stop("Warehouse id differs from the verified assignment.", call. = F)
    }
  }
  if (is.null(warehouse_id) || is.na(warehouse_id)) stop(
    "Warehouse id is required.", call. = F)
  status_label = if (verified) "verified_for_delivery_address" else
    "unmapped_technical_only"
  started = format(ctx$clock(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  tree = fp_http_json(ctx, paste0("category/warehouse/", warehouse_id),
    label = "category_tree")
  categories = fp_category_table(tree, collector)
  missing_roots = setdiff(collector$scope_root_external_ids,
    categories$root_id)
  todo = categories[categories$fetch, ]
  scope_limited = !is.null(subcategory_ids)
  if (scope_limited) todo = todo[todo$sub_id %in% subcategory_ids, ]
  variants = list()
  members = list()
  listing = list()
  for (i in seq_len(nrow(todo))) {
    sub = todo$sub_id[i]
    products = fp_http_json(ctx, paste0("products/prv/warehouse/",
      warehouse_id), list(subcategory_id = sub), label = paste0("sub_", sub))
    observed = format(ctx$clock(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    parsed = fp_parse_products(products, collector, warehouse_id, observed,
      status_label)
    variants[[i]] = parsed
    members[[i]] = if (length(products)) fp_membership(products, sub) else
      NULL
    listing[[i]] = data.frame(sub_id = sub, products = length(products),
      variants = nrow(parsed), stringsAsFactors = F)
  }
  all_variants = fp_rows(variants)
  conflicts = character()
  if (nrow(all_variants)) {
    for (sku in unique(all_variants$sku[duplicated(all_variants$sku)])) {
      rows = all_variants[all_variants$sku == sku, ]
      core = c("product_key", "price_cents", "pack_count", "unit_volume_l",
        "available")
      if (nrow(unique(rows[, core])) > 1) conflicts = c(conflicts, sku)
    }
  }
  unique_variants = all_variants[!duplicated(all_variants$sku), ]
  details = NULL
  missing_keys = character()
  if (with_details && nrow(unique_variants)) {
    keys = unique(unique_variants$product_key)
    parts = split(keys, ceiling(seq_along(keys) / collector$detail_batch_size))
    detail_rows = list()
    for (part in parts) {
      answer = fp_http_json(ctx, paste0("products/pdp/warehouse/",
        warehouse_id), list(ids = paste(part, collapse = ",")),
        label = paste0("detail_", part[1]))
      parsed = fp_parse_details(answer, part)
      detail_rows[[length(detail_rows) + 1L]] = parsed$details
      missing_keys = c(missing_keys, parsed$missing_keys)
    }
    details = fp_rows(detail_rows)
    if (!nrow(details)) details = data.frame(sku = character(),
      product_key = character(), alcohol_content = numeric(),
      abv_in_detail = logical(), detail_description_text = character(),
      ingredients_text = character(), stringsAsFactors = F)
    if (anyNA(details$sku) || anyDuplicated(details$sku)) fp_schema_fail(
      "missing identity or duplicate detail SKU")
    missing_skus = setdiff(unique_variants$sku, details$sku)
    if (length(missing_skus)) missing_keys = c(missing_keys,
      paste0("sku:", missing_skus))
    linked = match(unique_variants$sku, details$sku)
    if (any(details$product_key[linked] != unique_variants$product_key,
        na.rm = T)) {
      fp_schema_fail("detail SKU belongs to another product")
    }
    order_before = unique_variants$sku
    unique_variants = merge(unique_variants, details[, c("sku",
      "alcohol_content", "abv_in_detail", "detail_description_text",
      "ingredients_text")], by = "sku", all.x = T, sort = F)
    unique_variants = unique_variants[match(order_before,
      unique_variants$sku), ]
    unique_variants$abv_in_detail[is.na(unique_variants$abv_in_detail)] = F
  } else if (nrow(unique_variants)) {
    unique_variants$alcohol_content = NA_real_
    unique_variants$abv_in_detail = F
    unique_variants$detail_description_text = NA_character_
    unique_variants$ingredients_text = NA_character_
  }
  if (nrow(unique_variants)) {
    unique_variants = fp_resolve_variant_abv(unique_variants, evidence)
    unique_variants = fp_product_hints(unique_variants, collector)
    unique_variants = fp_resolve_packs(unique_variants, collector)
    if (!is.null(classifier)) {
      assigned = fp_assign_categories(unique_variants, unique_variants[,
        c("sku", "detail_description_text", "ingredients_text")], categories,
        classifier, collector)
      unique_variants = cbind(unique_variants, assigned[, -1])
    }
    texts = unique_variants[, c("sku", "description_text",
      "detail_description_text", "ingredients_text")]
    unique_variants$description_text = NULL
    unique_variants$detail_description_text = NULL
    unique_variants$ingredients_text = NULL
    unique_variants$abv_in_detail = NULL
  }
  membership = unique(fp_rows(members))
  if (!nrow(unique_variants)) texts = fp_rows(list())
  proof = list(blocking = character(), warnings = character())
  full_scope = !scope_limited
  reviewed = fp_reviewed_set_check(categories, collector)
  if (full_scope && purpose != "technical_test" && !reviewed$match) {
    proof$blocking = "category_tree_differs_from_reviewed_set"
  }
  replicated = fp_rows(list())
  if (replicate_lists > 0L && nrow(membership)) {
    replicated = fp_replicate_lists(ctx, warehouse_id, todo, membership,
      replicate_lists)
    if (nrow(replicated) && any(!replicated$same_sku_set)) proof$blocking =
      c(proof$blocking, "list_unstable_between_fetches")
  }
  probe = fp_rows(list())
  if (length(probe_ids)) {
    probe = fp_outside_probe(ctx, warehouse_id, probe_ids, collector)
    if (any(probe$alcohol_hits > 0)) proof$blocking = c(proof$blocking,
      "alcohol_found_outside_scope_roots")
    if (any(probe$detail_missing > 0)) proof$blocking = c(proof$blocking,
      "outside_probe_detail_missing")
  } else if (full_scope && purpose != "technical_test") {
    proof$blocking = c(proof$blocking, "outside_scope_probe_not_run")
  }
  overlap = fp_overlap(membership, categories)
  proof_table = fp_proof_table(todo, fp_rows(listing), membership)
  brand_missing = character()
  if (brand_checks > 0L && nrow(unique_variants)) {
    brand_missing = fp_brand_crosscheck(ctx, warehouse_id, unique_variants,
      if (nrow(todo)) todo$sub_id else integer(), brand_checks, collector)
  }
  checks = fp_collect_checks(categories, todo, listing, unique_variants,
    conflicts, missing_keys, missing_roots, brand_missing, with_details,
    collector)
  checks$blocking = unique(c(checks$blocking, proof$blocking,
    fp_variant_consistency(unique_variants)))
  checks$feature_gaps = if (nrow(unique_variants)) fp_feature_gaps(
    unique_variants) else character()
  clean = !length(checks$blocking) && !scope_limited
  status = if (clean && purpose == "production") "complete_candidate" else
    if (clean && purpose == "inventory_proof") "inventory_proof_passed" else
    if (!length(checks$blocking) && purpose == "technical_test")
      "technical_test_unmapped_warehouse" else "partial_not_a_result"
  list(status = status, purpose = purpose, warehouse_id = warehouse_id,
    warehouse_assignment = status_label, scope_limited = scope_limited,
    started_at = started, categories = categories, listing = fp_rows(listing),
    variants = unique_variants, texts = texts, membership = membership,
    calls = fp_rows(ctx$state$calls), checks = checks,
    quality = if (nrow(unique_variants)) fp_abv_quality(unique_variants) else
      fp_rows(list()),
    abv_review = fp_abv_review(unique_variants),
    category_summary = fp_category_summary(unique_variants),
    category_review = fp_category_review(unique_variants),
    inclusion_summary = fp_inclusion_summary(unique_variants),
    refinement = fp_refinement_table(unique_variants),
    harmonisation_audit = fp_harmonisation_audit(unique_variants),
    sensitivity_by_category = fp_sensitivity_table(unique_variants),
    boundary_review = fp_boundary_review(unique_variants),
    proof = proof_table, overlap = overlap$pairs,
    lists_per_sku = overlap$lists_per_sku, reviewed_set = reviewed,
    replicates = replicated, outside_probe = probe,
    volume_review = if (nrow(unique_variants)) fp_volume_review(
      unique_variants) else fp_rows(list()),
    crosstab = if (nrow(unique_variants)) fp_crosstab(unique_variants) else
      fp_rows(list()),
    collector_version = collector$version)
}

fp_collect_checks = \(categories, todo, listing, variants, conflicts,
  missing_keys, missing_roots, brand_missing, with_details, collector) {
  blocking = character()
  warnings = character()
  if (length(missing_roots)) blocking = c(blocking, "scope_root_missing")
  if (!nrow(todo)) blocking = c(blocking, "no_subcategory_fetched")
  if (!nrow(variants)) blocking = c(blocking, "empty_inventory")
  listing = if (is.data.frame(listing)) listing else fp_rows(listing)
  if (nrow(listing) && any(listing$products == 0)) warnings =
    c(warnings, paste0("empty_subcategory:", listing$sub_id[
      listing$products == 0]))
  if (nrow(variants)) {
    if (anyDuplicated(variants$sku)) blocking = c(blocking, "duplicate_sku")
    bad_price = is.na(variants$price_cents) | variants$price_cents <= 0
    if (any(bad_price)) blocking = c(blocking, "invalid_price")
    per_l = variants$price_eur / variants$total_volume_l
    shown = suppressWarnings(as.numeric(sub(",", ".", gsub(
      "[^0-9,]", "", variants$price_per_unit_text))))
    if (any(!is.na(shown) & abs(per_l - shown) > 0.011, na.rm = T)) {
      warnings = c(warnings, "displayed_base_price_conflict")
    }
    if (!with_details) blocking = c(blocking, "details_not_collected")
  }
  if (length(conflicts)) blocking = c(blocking, "sku_conflict_between_lists")
  if (length(missing_keys)) blocking = c(blocking, "detail_missing_for_key")
  if (length(brand_missing)) blocking = c(blocking,
    "brand_crosscheck_missing_sku")
  list(blocking = unique(blocking), warnings = unique(warnings),
    sku_conflicts = conflicts, missing_detail_keys = missing_keys,
    brand_missing_skus = brand_missing)
}

# Independent evidence: brand lists must not contain in-scope SKUs that the
# category walk missed.
fp_brand_crosscheck = \(ctx, warehouse_id, variants, fetched_subs, n,
  collector) {
  brands = sort(unique(variants$brand_id[!is.na(variants$brand_id)]))
  if (!length(brands)) return(character())
  pick = brands[unique(round(seq(1, length(brands), length.out = min(n,
    length(brands)))))]
  missing = character()
  for (brand in pick) {
    products = fp_http_json(ctx, paste0("products/prv/warehouse/",
      warehouse_id), list(brand_id = brand), label = paste0("brand_", brand))
    for (product in products) {
      in_scope = any(fp_subcategory_ids(product) %in% fetched_subs)
      skus = vapply(c(list(product$masterVariant), product$variants),
        \(v) as.character(v$sku), "")
      if (in_scope) missing = c(missing, setdiff(skus, variants$sku))
    }
  }
  unique(missing)
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 5) STORING A RUN: PARTIAL RUNS ARE NEVER LABELLED AS RESULTS
# <-----------------------------------------------------------------------------

fp_write_run = \(result, base_dir, runner, run_date = Sys.Date()) {
  fp_private_dir(base_dir, runner)
  suffix = switch(result$status, complete_candidate = "COMPLETE_CANDIDATE",
    inventory_proof_passed = "INVENTORY_PROOF_PASSED",
    technical_test_unmapped_warehouse = if (identical(
      result$warehouse_assignment, "unmapped_technical_only"))
      "TECHNICAL_TEST_UNMAPPED" else "TECHNICAL_TEST_NOT_A_RESULT",
    "PARTIAL_NOT_A_RESULT")
  dir = file.path(base_dir, sprintf("flaschenpost_%s_w%s_%s", run_date,
    result$warehouse_id, suffix))
  dir.create(dir, showWarnings = F, recursive = T)
  for (name in c("categories", "listing", "variants", "membership", "calls",
      "quality", "proof", "overlap", "replicates", "outside_probe",
      "volume_review", "crosstab", "texts", "abv_review",
      "category_summary", "category_review", "inclusion_summary",
      "boundary_review", "sensitivity_by_category", "refinement",
      "harmonisation_audit")) {
    write.csv(result[[name]], file.path(dir, paste0(name, ".csv")),
      row.names = F, na = "")
  }
  write_json(list(status = result$status, purpose = result$purpose,
    warehouse_id = result$warehouse_id,
    warehouse_assignment = result$warehouse_assignment,
    scope_limited = result$scope_limited, started_at = result$started_at,
    variants = nrow(result$variants),
    products = length(unique(result$variants$product_key)),
    subcategories = nrow(result$listing), calls = nrow(result$calls),
    inventory_blocking = result$checks$blocking,
    feature_gaps = result$checks$feature_gaps,
    warnings = result$checks$warnings,
    reviewed_category_set_matches = result$reviewed_set$match,
    new_category_ids = result$reviewed_set$new_ids,
    lost_category_ids = result$reviewed_set$lost_ids,
    analysis_variants = fp_variant_definition(),
    sensitivity_by_category = result$sensitivity_by_category,
    collector_version = result$collector_version),
    file.path(dir, "meta.json"), pretty = T, auto_unbox = T)
  dir
}
