# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING - FLASCHENPOST PREPARATION
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261026

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) ABV RESOLUTION: MISSING FEATURES ARE NOT AN INCOMPLETE INVENTORY
# <-----------------------------------------------------------------------------

fp_abv_pattern = paste0("[0-9]+(?:[.,][0-9]+)?\\s*",
  "(?:%\\s*vol\\.?|vol[.\\s-]*%)")

# One explicit ABV from free text; several different numbers are ambiguous.
fp_text_abv = \(text, pattern = fp_abv_pattern) {
  none = \(status) list(value = NA_real_, status = status)
  if (is.null(text) || length(text) != 1L || is.na(text)) return(none(
    "no_text"))
  text = fp_clean(gsub("<[^>]+>", " ", text))
  # Legal or comparative sentences describe other products, not this one.
  context = paste0("mindestens|mind\\.|per gesetz|gesetzlich|vorgeschrieben|",
    "muss |m\u00fcssen|streng genommen|statt |anstelle|ersetzt|vergleich")
  sentences = strsplit(text, "(?<=[.!?])\\s+", perl = T)[[1]]
  text = paste(sentences[!grepl(context, sentences, ignore.case = T,
    perl = T)], collapse = " ")
  hits = regmatches(text, gregexpr(pattern, text, perl = T,
    ignore.case = T))[[1]]
  if (!length(hits)) return(none("no_match"))
  values = unique(fp_number(sub("^([0-9]+(?:[.,][0-9]+)?).*$", "\\1", hits,
    perl = T)))
  if (length(values) > 1L) return(none("ambiguous_text"))
  if (grepl("<|weniger als|unter |bis zu|höchstens|max\\.", text,
      ignore.case = T, perl = T)) return(none("qualified_not_exact"))
  if (!is.finite(values) || values < 0 || values > 100) return(none(
    "invalid_value"))
  if (values == 0) return(none("zero_claim_not_exact"))
  list(value = values, status = "explicit_exact")
}

fp_name_abv = \(name) {
  text = fp_clean(name)
  other = "fett|zucker|frucht|saft|anteil|i\\.\\s?tr|trocken"
  hits = regmatches(text, gregexpr(paste0("[0-9]+(?:[.,][0-9]+)?\\s*%",
    "(?!\\s*(?:", other, "))"), text, perl = T,
    ignore.case = T))[[1]]
  if (!length(hits)) return(list(value = NA_real_, status = "no_match"))
  values = unique(fp_number(sub("\\s*%", "", hits)))
  if (length(values) != 1L || !is.finite(values) || values < 0 ||
      values > 100) return(list(value = NA_real_, status = "ambiguous_text"))
  if (values == 0) return(list(value = NA_real_,
    status = "zero_claim_not_exact"))
  list(value = values, status = "explicit_exact")
}

# Qualified statements give an upper bound, never an exact value.
fp_text_bound = \(text) {
  none = list(value = NA_real_, kind = "none")
  if (is.null(text) || length(text) != 1L || is.na(text)) return(none)
  text = fp_clean(gsub("&lt;", "<", gsub("<[^>]+>", " ", text)))
  number = "([0-9]+(?:[.,][0-9]+)?)"
  patterns = c(paste0("frei\\s*\\(\\s*<\\s*", number,
    "\\s*%\\s*\\)\\s*von\\s*alkohol"), paste0("(?:<|weniger als)\\s*",
    number, "\\s*%\\s*(?:vol\\.?|alkohol|von alkohol)"))
  for (pattern in patterns) {
    hit = regmatches(text, regexec(pattern, text, perl = T,
      ignore.case = T))[[1]]
    if (length(hit) == 2L) return(list(value = fp_number(hit[2]),
      kind = "strict_upper_bound"))
  }
  weak = c(paste0("(?:bis zu|h\u00f6chstens|maximal|max\\.?)\\s*", number,
    "\\s*%\\s*(?:vol\\.?|alkohol|von alkohol)"), paste0("von\\s*[0-9]+(?:",
    "[.,][0-9]+)?\\s*bis\\s*", number, "\\s*%\\s*v"))
  for (pattern in weak) {
    hit = regmatches(text, regexec(pattern, text, perl = T,
      ignore.case = T))[[1]]
    if (length(hit) == 2L) return(list(value = fp_number(hit[2]),
      kind = "weak_upper_bound"))
  }
  none
}

fp_abv_claim = \(text) {
  zero = paste0("(?<![0-9.,])0[,.]0\\s*%?\\s*(vol|alkohol)|",
    "(?<![0-9.,])0[,.]0\\s*%")
  if (grepl(zero, text, ignore.case = T, perl = T)) return(
    "zero_percent_claim")
  if (grepl("alkoholfrei|entalkoholisiert|ohne alkohol", text,
      ignore.case = T)) return("alcohol_free_claim")
  NA_character_
}

fp_text_components = \(text) {
  if (is.null(text) || length(text) != 1L || is.na(text)) return(character())
  hits = regmatches(text, gregexpr(paste0("[0-9]+ x [^()]{1,40}\\(",
    "([0-9]+(?:[.,][0-9]+)?)\\s*%\\)"), text, perl = T))[[1]]
  values = fp_number(sub("^.*\\(([0-9]+(?:[.,][0-9]+)?)\\s*%\\)$", "\\1",
    hits, perl = T))
  if (length(unique(values)) > 1L) as.character(values) else character()
}

fp_load_abv_evidence = \(path = file.path(preparation_root,
  collector$abv_evidence_file)) {
  columns = c("sku", "product_key", "product_name", "total_volume_l",
    "alcohol_content", "evidence_ref", "evidence_sha256")
  if (!file.exists(path)) return(setNames(data.frame(matrix(character(),
    0, length(columns))), columns))
  evidence = read.csv(path, colClasses = "character", na.strings = "")
  stopifnot(all(columns %in% names(evidence)), !anyDuplicated(evidence$sku))
  evidence$total_volume_l = as.numeric(evidence$total_volume_l)
  evidence$alcohol_content = as.numeric(evidence$alcohol_content)
  evidence
}

# Order: detail attribute, detail text, list text, pack text, name, evidence.
fp_resolve_variant_abv = \(variants, evidence) {
  n = nrow(variants)
  out = data.frame(alcohol_content = rep(NA_real_, n),
    abv_source = rep("unknown", n), abv_status = rep("unknown", n),
    abv_evidence_ref = rep(NA_character_, n), abv_text_conflict = rep(F, n),
    abv_upper_bound = rep(NA_real_, n), abv_bound_source = rep(NA_character_,
      n), abv_claim = rep(NA_character_, n), abv_components = rep(NA_character_,
      n), abv_bound_kind = rep(NA_character_, n),
    abv_boundary_status = rep(NA_character_, n), stringsAsFactors = F)
  for (i in seq_len(n)) {
    row = variants[i, ]
    attribute = if (isTRUE(row$abv_in_detail) &&
        !is.na(row$alcohol_content)) row$alcohol_content else NA_real_
    beverage = !"content_kind" %in% names(row) || row$content_kind == "volume"
    candidates = if (!beverage) list() else list(
      detail_description = fp_text_abv(row$detail_description_text),
      list_description = fp_text_abv(row$description_text),
      article_description = fp_text_abv(row$article_description),
      product_name = fp_name_abv(row$product_name))
    all_text = paste(row$detail_description_text, row$description_text,
      row$product_name)
    out$abv_claim[i] = fp_abv_claim(all_text)
    if (!is.na(attribute) && attribute >= 0 && attribute <= 100) {
      out[i, c("alcohol_content", "abv_source", "abv_status")] = list(
        attribute, "detail_attribute", "explicit_exact")
      found = Filter(\(x) !is.na(x$value), candidates)
      out$abv_text_conflict[i] = any(vapply(found, \(x) abs(x$value -
        attribute) > 1e-9, logical(1)))
      if (abs(attribute - 0.5) < 1e-9) {
        bounds = lapply(c(row$detail_description_text, row$description_text,
          row$article_description), fp_text_bound)
        qualified = Filter(\(b) b$kind != "none" &&
          abs(b$value - 0.5) < 1e-9, bounds)
        out$abv_boundary_status[i] = if (length(qualified))
          "qualified_text_conflict" else "documented_numeric_no_qualifier"
      }
      next
    }
    parts = fp_text_components(row$detail_description_text)
    if (length(parts)) {
      out$abv_components[i] = paste(parts, collapse = ";")
      out$abv_status[i] = "mixed_assortment"
    }
    for (field in c("detail_description_text", "description_text")) {
      bound = fp_text_bound(row[[field]])
      if (bound$kind != "none") {
        out$abv_upper_bound[i] = bound$value
        out$abv_bound_source[i] = field
        out$abv_bound_kind[i] = bound$kind
        break
      }
    }
    if (!is.na(attribute)) out$abv_status[i] = "invalid_in_source"
    else if (!length(parts)) {
      reasons = vapply(candidates, \(x) x$status, "")
      reasons = reasons[!reasons %in% c("no_text", "no_match")]
      if (length(reasons)) out$abv_status[i] = reasons[[1]]
    }
    hit = Filter(\(x) !is.na(x$value), candidates)
    if (length(hit)) {
      out[i, c("alcohol_content", "abv_source", "abv_status")] = list(
        hit[[1]]$value, names(hit)[1], hit[[1]]$status)
      next
    }
    j = match(row$sku, evidence$sku)
    if (is.na(j)) next
    same = identical(as.character(evidence$product_key[j]),
      as.character(row$product_key)) &&
      identical(evidence$product_name[j], row$product_name) &&
      isTRUE(abs(evidence$total_volume_l[j] - row$total_volume_l) < 1e-9)
    if (!same) {
      out$abv_status[i] = "evidence_identity_mismatch"
      next
    }
    out[i, c("alcohol_content", "abv_source", "abv_status",
      "abv_evidence_ref")] = list(evidence$alcohol_content[j],
      "reviewed_evidence", "explicit_exact", evidence$evidence_ref[j])
  }
  variants$alcohol_content = NULL
  variants = cbind(variants, out)
  exact = !is.na(variants$alcohol_content)
  conflict = variants$abv_boundary_status %in% "qualified_text_conflict"
  bound = variants$abv_upper_bound
  below = !exact & !is.na(bound) & bound < 0.5
  atmost = !exact & !is.na(bound) & bound <= 0.5
  variants$abv_filter_min_0_5 = ifelse(exact, ifelse(conflict,
    "unverified_boundary", ifelse(variants$alcohol_content >= 0.5, "yes",
    "no")), ifelse(below, "no", "unknown"))
  # Sensitivity filter: strictly above 0.5; ceilings up to 0.5 decide it.
  variants$abv_filter_gt_0_5 = ifelse(exact, ifelse(variants$alcohol_content >
    0.5, "yes", "no"), ifelse(atmost, "no", "unknown"))
  variants$abv_filter_basis = ifelse(conflict, "boundary_conflict",
    ifelse(exact, "exact", ifelse(below | atmost, "upper_bound", "unknown")))
  if ("content_kind" %in% names(variants)) {
    other = variants$content_kind != "volume"
    variants$abv_filter_min_0_5[other] = "not_applicable"
    variants$abv_filter_gt_0_5[other] = "not_applicable"
    variants$abv_filter_basis[other] = "not_applicable"
  }
  variants
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 2) CATEGORY HINTS (NOT A CLASSIFICATION) AND QUALITY STATISTICS
# <-----------------------------------------------------------------------------

fp_hint_table = \(collector) {
  rows = list()
  for (certainty in c("direct", "review")) {
    groups = collector$analysis_hints[[certainty]]
    for (label in names(groups)) {
      ids = as.integer(groups[[label]])
      if (length(ids)) rows[[length(rows) + 1L]] = data.frame(sub_id = ids,
        label = label, certainty = certainty, stringsAsFactors = F)
    }
  }
  fp_rows(rows)
}

fp_product_hints = \(variants, collector) {
  table = fp_hint_table(collector)
  state = character(nrow(variants))
  label = rep(NA_character_, nrow(variants))
  for (i in seq_len(nrow(variants))) {
    subs = suppressWarnings(as.integer(strsplit(variants$subcategory_ids[i],
      ";", fixed = T)[[1]]))
    hit = table[table$sub_id %in% subs, ]
    direct = unique(hit$label[hit$certainty == "direct"])
    review = unique(hit$label[hit$certainty == "review"])
    if (length(direct) == 1L) {
      state[i] = "direct"
      label[i] = direct
    } else if (length(direct) > 1L) {
      state[i] = "ambiguous"
    } else if (length(review)) {
      state[i] = "review"
      label[i] = paste(review, collapse = "|")
    } else {
      state[i] = "none"
    }
  }
  variants$analysis_hint = label
  variants$analysis_hint_state = state
  variants
}

fp_is_beverage = \(variants) {
  if ("content_kind" %in% names(variants)) variants$content_kind == "volume"
  else rep(T, nrow(variants))
}

fp_abv_quality = \(variants) {
  n = nrow(variants)
  beverage = fp_is_beverage(variants)
  known = !is.na(variants$alcohol_content)
  hint_ok = variants$analysis_hint_state == "direct"
  metrics = c(variants = n, beverages = sum(beverage),
    abv_known = sum(known & beverage),
    abv_unknown = sum(!known & beverage),
    nonvolume_items = sum(!beverage),
    nonvolume_items_with_abv = sum(known & !beverage),
    abv_share_known_beverages = if (any(beverage)) round(mean(
      known[beverage]), 4) else NA,
    filter_yes = sum(variants$abv_filter_min_0_5 == "yes"),
    filter_no = sum(variants$abv_filter_min_0_5 == "no"),
    filter_unknown = sum(variants$abv_filter_min_0_5 == "unknown"),
    filter_unverified_boundary = sum(variants$abv_filter_min_0_5 ==
      "unverified_boundary"),
    filter_gt_yes = sum(variants$abv_filter_gt_0_5 == "yes"),
    filter_gt_no = sum(variants$abv_filter_gt_0_5 == "no"),
    filter_gt_unknown = sum(variants$abv_filter_gt_0_5 == "unknown"),
    filter_not_applicable = sum(!beverage),
    abv_text_conflicts = sum(variants$abv_text_conflict),
    hint_direct = sum(hint_ok),
    hint_review = sum(variants$analysis_hint_state == "review"),
    hint_ambiguous = sum(variants$analysis_hint_state == "ambiguous"),
    hint_none = sum(variants$analysis_hint_state == "none"),
    not_classifiable = sum(beverage & (!known | !hint_ok)))
  sources = table(factor(variants$abv_source, levels = c("detail_attribute",
    "detail_description", "list_description", "article_description",
    "product_name", "reviewed_evidence", "unknown")))
  names(sources) = paste0("abv_source_", names(sources))
  data.frame(metric = c(names(metrics), names(sources)),
    value = c(unname(metrics), as.integer(sources)), stringsAsFactors = F)
}

fp_feature_gaps = \(variants) {
  gaps = character()
  count = \(flag) sum(flag, na.rm = T)
  beverage = fp_is_beverage(variants)
  unknown = count(is.na(variants$alcohol_content) & beverage)
  nonvolume = count(!beverage)
  if (nonvolume) gaps = c(gaps, paste0("non_volume_items:", nonvolume))
  if (unknown) gaps = c(gaps, paste0("abv_unknown:", unknown))
  unk = is.na(variants$alcohol_content) & beverage
  for (part in list(c("label_claim_only", count(unk &
      !is.na(variants$abv_claim))), c("mixed_assortment", count(unk &
      variants$abv_status %in% "mixed_assortment")), c("upper_bound_only",
      count(unk & !is.na(variants$abv_upper_bound))))) {
    if (as.numeric(part[2])) gaps = c(gaps, paste0("abv_unknown_", part[1],
      ":", part[2]))
  }
  conflicts = count(variants$abv_text_conflict)
  if (conflicts) gaps = c(gaps, paste0("abv_text_conflict:", conflicts))
  for (state in c("text_only", "resolved_text", "resolved_attribute",
      "unresolved_conflict", "unresolved_no_pack")) {
    n_state = count(variants$volume_status == state)
    if (n_state) gaps = c(gaps, paste0("volume_", state, ":", n_state))
  }
  boundary = count(variants$abv_filter_min_0_5 %in% "unverified_boundary")
  if (boundary) gaps = c(gaps, paste0("abv_boundary_unverified:", boundary))
  invalid = count(variants$abv_status == "invalid_in_source")
  if (invalid) gaps = c(gaps, paste0("abv_invalid_in_source:", invalid))
  mismatch = count(variants$abv_status == "evidence_identity_mismatch")
  if (mismatch) gaps = c(gaps, paste0("abv_evidence_mismatch:", mismatch))
  unhinted = count(variants$analysis_hint_state != "direct")
  if (unhinted) gaps = c(gaps, paste0("category_hint_not_direct:", unhinted))
  gaps
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 3) INVENTORY PROOF: REVIEWED CATEGORY SET, OVERLAPS, REPLICATES, PROBES
# <-----------------------------------------------------------------------------

fp_reviewed_set_check = \(categories, collector) {
  current = categories$sub_id[categories$in_scope & categories$visible &
    !categories$nonbeverage_excluded]
  reviewed = as.integer(collector$reviewed_subcategory_ids)
  list(new_ids = setdiff(current, reviewed),
    lost_ids = setdiff(reviewed, current),
    match = setequal(current, reviewed))
}

fp_overlap = \(membership, categories) {
  if (!nrow(membership)) return(list(pairs = fp_rows(list()),
    lists_per_sku = integer()))
  per_sku = table(membership$sku)
  subs = sort(unique(membership$sub_id))
  rows = list()
  for (a in seq_along(subs)) for (b in seq_along(subs)) {
    if (b <= a) next
    shared = length(intersect(membership$sku[membership$sub_id == subs[a]],
      membership$sku[membership$sub_id == subs[b]]))
    if (shared) rows[[length(rows) + 1L]] = data.frame(sub_a = subs[a],
      sub_b = subs[b], shared_skus = shared, stringsAsFactors = F)
  }
  name = \(id) categories$sub_name[match(id, categories$sub_id)]
  pairs = fp_rows(rows)
  if (nrow(pairs)) {
    pairs$name_a = name(pairs$sub_a)
    pairs$name_b = name(pairs$sub_b)
    pairs = pairs[order(-pairs$shared_skus), ]
  }
  list(pairs = pairs, lists_per_sku = as.integer(table(per_sku)),
    lists_per_sku_levels = names(table(per_sku)))
}

fp_proof_table = \(todo, listing, membership) {
  seen = character()
  rows = list()
  for (i in seq_len(nrow(todo))) {
    sub = todo$sub_id[i]
    skus = unique(membership$sku[membership$sub_id == sub])
    row = listing[listing$sub_id == sub, ]
    rows[[i]] = data.frame(sub_id = sub, sub_name = todo$sub_name[i],
      root_name = todo$root_name[i], fetched = nrow(row) == 1L,
      products = if (nrow(row)) row$products else NA_integer_,
      variants = if (nrow(row)) row$variants else NA_integer_,
      new_unique_skus = length(setdiff(skus, seen)),
      shared_with_earlier_lists = length(intersect(skus, seen)),
      stringsAsFactors = F)
    seen = union(seen, skus)
  }
  fp_rows(rows)
}

# Repeat a few lists: the SKU sets must be identical (price moves are fine).
fp_replicate_lists = \(ctx, warehouse_id, todo, membership, n) {
  if (n < 1L || !nrow(todo)) return(fp_rows(list()))
  pick = todo$sub_id[unique(round(seq(1, nrow(todo),
    length.out = min(n, nrow(todo)))))]
  fp_rows(lapply(pick, \(sub) {
    again = fp_http_json(ctx, paste0("products/prv/warehouse/",
      warehouse_id), list(subcategory_id = sub),
      label = paste0("replicate_", sub))
    skus = unlist(lapply(again, \(p) vapply(c(list(p$masterVariant),
      p$variants), \(v) as.character(v$sku), "")))
    first = membership$sku[membership$sub_id == sub]
    data.frame(sub_id = sub, same_sku_set = setequal(skus, first),
      first_skus = length(unique(first)), second_skus = length(unique(skus)),
      stringsAsFactors = F)
  }))
}

# Lists outside the three roots are checked for alcohol via detail ABV only.
fp_outside_probe = \(ctx, warehouse_id, ids, collector) {
  rows = list()
  for (sub in ids) {
    products = fp_http_json(ctx, paste0("products/prv/warehouse/",
      warehouse_id), list(subcategory_id = sub),
      label = paste0("outside_", sub))
    keys = unique(vapply(products, \(p) as.character(p$key), ""))
    hits = character()
    missing = character()
    for (part in split(keys, ceiling(seq_along(keys) /
        collector$detail_batch_size))) {
      answer = fp_http_json(ctx, paste0("products/pdp/warehouse/",
        warehouse_id), list(ids = paste(part, collapse = ",")),
        label = paste0("outside_detail_", sub, "_", part[1]))
      parsed = fp_parse_details(answer, part)
      missing = c(missing, parsed$missing_keys)
      with_abv = parsed$details[!is.na(parsed$details$alcohol_content) &
        parsed$details$alcohol_content >= 0.5, ]
      hits = c(hits, with_abv$sku)
    }
    rows[[length(rows) + 1L]] = data.frame(sub_id = sub,
      products = length(keys), alcohol_hits = length(unique(hits)),
      detail_missing = length(missing), stringsAsFactors = F)
  }
  fp_rows(rows)
}
# <=============================================================================
# <=============================================================================
# <=============================================================================

# 4) SALE VOLUME: ATTRIBUTE VERSUS PACK TEXT, DECIDED BY SIBLING EVIDENCE
# <-----------------------------------------------------------------------------

# The displayed price per litre is derived from the attribute volume and is no
# independent evidence. A conflict is resolved only when same-brand SKUs whose
# attribute and pack text agree support exactly one candidate and either one
# supporter has the identical price or at least three supporters exist. All
# other conflicts keep NA, both candidates and the evidence.
fp_shown_per_l = \(text) {
  hit = regmatches(text, regexpr("[0-9]+,[0-9]+\\s*\u20ac/L", text,
    perl = T))
  value = rep(NA_real_, length(text))
  found = !is.na(text) &
    grepl("[0-9]+,[0-9]+\\s*\u20ac/L", text, perl = T)
  value[found] = fp_number(sub("\\s*\u20ac/L$", "", hit))
  value
}

fp_resolve_packs = \(variants, collector) {
  n = nrow(variants)
  valid = \(x) is.finite(x) & x > 0
  attr_l = variants$pack_count * variants$unit_volume_l
  attr_l[!valid(variants$pack_count) | !valid(variants$unit_volume_l)] =
    NA_real_
  text_l = variants$text_volume_l
  text_l[!valid(text_l)] = NA_real_
  volume = variants$content_kind == "volume"
  agree = volume & !is.na(attr_l) & !is.na(text_l) &
    abs(attr_l - text_l) < 1e-6
  solo = volume & xor(!is.na(attr_l), !is.na(text_l))
  anchor = agree | (solo & !is.na(attr_l))
  shown = fp_shown_per_l(variants$price_per_unit_text)
  status = character(n)
  evidence = character(n)
  final = rep(NA_real_, n)
  pack = rep(NA_real_, n)
  unit = rep(NA_real_, n)
  same_price = \(a, b) abs(a - b) < 0.005
  for (i in seq_len(n)) {
    take = "none"
    if (!volume[i]) {
      status[i] = "non_volume_item"
    } else if (agree[i] || (solo[i] && !is.na(attr_l[i]))) {
      status[i] = if (agree[i]) "consistent" else "attribute_only"
      take = "attribute"
    } else if (solo[i]) {
      status[i] = "text_only"
      evidence[i] = sprintf("attribute_invalid(pack=%s,unit_l=%s);text_l=%.4g",
        variants$pack_count[i], variants$unit_volume_l[i], text_l[i])
      take = "text"
    } else if (is.na(attr_l[i]) && is.na(text_l[i])) {
      status[i] = "unresolved_no_pack"
    } else {
      equal_count = isTRUE(abs(variants$pack_count[i] -
        variants$text_pack_count[i]) < 1e-9)
      match_a = anchor & !is.na(variants$brand_id) &
        variants$brand_id == variants$brand_id[i] & seq_len(n) != i &
        abs(variants$unit_volume_l - variants$unit_volume_l[i]) < 1e-9 &
        (equal_count | abs(variants$pack_count - variants$pack_count[i]) <
          1e-9)
      match_t = anchor & !is.na(variants$brand_id) &
        variants$brand_id == variants$brand_id[i] & seq_len(n) != i &
        abs(variants$unit_volume_l - variants$text_unit_volume_l[i]) < 1e-9 &
        (equal_count | abs(variants$pack_count -
          variants$text_pack_count[i]) < 1e-9)
      sup_a = sum(match_a)
      sup_t = sum(match_t)
      id_a = any(same_price(variants$price_eur[match_a], variants$price_eur[i]))
      id_t = any(same_price(variants$price_eur[match_t], variants$price_eur[i]))
      evidence[i] = sprintf(paste0("attr_l=%.4g;text_l=%.4g;shown_eur_per_l=",
        "%s;implied_l_from_shown=%.4g;brand_siblings_attr=%d;",
        "brand_siblings_text=%d;identical_price_attr=%s;",
        "identical_price_text=%s"), attr_l[i], text_l[i], shown[i],
        variants$price_eur[i] / shown[i], sup_a, sup_t, id_a, id_t)
      status[i] = "unresolved_conflict"
      if (sup_t > 0 && sup_a == 0 && (id_t || sup_t >= 3)) {
        status[i] = "resolved_text"
        take = "text"
      } else if (sup_a > 0 && sup_t == 0 && (id_a || sup_a >= 3)) {
        status[i] = "resolved_attribute"
        take = "attribute"
      }
    }
    if (take == "attribute") {
      pack[i] = variants$pack_count[i]
      unit[i] = variants$unit_volume_l[i]
      final[i] = attr_l[i]
    } else if (take == "text") {
      pack[i] = variants$text_pack_count[i]
      unit[i] = variants$text_unit_volume_l[i]
      final[i] = text_l[i]
    }
  }
  variants$volume_attribute_l = attr_l
  variants$volume_text_l = text_l
  variants$volume_status = status
  variants$volume_evidence = evidence
  variants$pack_count = pack
  variants$unit_volume_l = unit
  variants$total_volume_l = final
  variants$text_pack_count = NULL
  variants$text_unit_volume_l = NULL
  variants$text_volume_l = NULL
  variants
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 5) REVIEW TABLES AND OFFLINE REPLAY OF A STORED RUN
# <-----------------------------------------------------------------------------

fp_volume_review = \(variants) {
  keep = variants$volume_status %in% c("resolved_text", "resolved_attribute",
    "unresolved_conflict", "text_only", "unresolved_no_pack")
  x = variants[keep, ]
  if (!nrow(x)) return(fp_rows(list()))
  shown = fp_shown_per_l(x$price_per_unit_text)
  implied = x$price_eur / shown
  basis = c(resolved_text = "brand siblings support the pack text",
    resolved_attribute = "brand siblings support the attribute",
    unresolved_conflict = "no sufficient independent evidence; volume NA",
    text_only = "attribute invalid; pack text used, no conflicting value",
    unresolved_no_pack = "no usable pack information; volume NA")
  data.frame(sku = x$sku, product_key = x$product_key,
    product_name = x$product_name, brand = x$brand, price_eur = x$price_eur,
    displayed_per_l = x$price_per_unit_text, implied_l_from_displayed = implied,
    displayed_matches_attribute = abs(implied - x$volume_attribute_l) <=
      0.01 * x$volume_attribute_l, container_type = x$container_type,
    deposit_total_eur = x$deposit_total_eur,
    article_description = x$article_description,
    volume_attribute_l = x$volume_attribute_l, volume_text_l = x$volume_text_l,
    decided_volume_l = x$total_volume_l, status = x$volume_status,
    decision_basis = basis[x$volume_status], evidence = x$volume_evidence,
    stringsAsFactors = F)
}

fp_crosstab = \(variants) {
  known = ifelse(is.na(variants$alcohol_content), "abv_missing", "abv_known")
  out = aggregate(list(skus = variants$sku), list(content_kind =
    variants$content_kind, abv = known, abv_filter_min_0_5 =
    variants$abv_filter_min_0_5, available = variants$available),
    FUN = length)
  out[order(out$content_kind, out$abv, out$abv_filter_min_0_5,
    out$available), ]
}

# Serves the stored raw answers of a finished run in call order; no network.
fp_replay_transport = \(run_dir) {
  calls = read.csv(Sys.glob(file.path(run_dir, "flaschenpost_*",
    "calls.csv"))[1], colClasses = "character", na.strings = "")
  files = list.files(file.path(run_dir, "raw"), full.names = T)
  if (length(files) != nrow(calls) || anyNA(calls$sha256) ||
      !identical(unname(vapply(files, fp_hash, "")), calls$sha256)) {
    stop("Stored replay response checksum mismatch.", call. = F)
  }
  queue = new.env()
  for (i in seq_len(nrow(calls))) {
    key = paste(calls$path[i], ifelse(is.na(calls$query[i]), "",
      calls$query[i]))
    body = rawToChar(readBin(files[i], "raw", file.size(files[i])))
    Encoding(body) = "UTF-8"
    queue[[key]] = c(queue[[key]], list(body))
  }
  \(url, query, collector) {
    path = sub(paste0(collector$api_base, "/"), "", url, fixed = T)
    key = paste(path, paste(names(query), unlist(query), sep = "=",
      collapse = "&"))
    pending = queue[[key]]
    if (!length(pending)) stop("Replay has no stored answer for ", key,
      call. = F)
    queue[[key]] = pending[-1]
    list(status = 200L, body = pending[[1]])
  }
}
