# <=============================================================================
# <=============================================================================
# <=============================================================================
# PROJECT TITLE:  ALCOHOL PRICE MONITORING - FLASCHENPOST PREPARATION
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261026
# <=============================================================================
# <=============================================================================
# <=============================================================================
# 1) PORTABLE FROZEN PROJECT RULES (READ ONLY)
# <-----------------------------------------------------------------------------
# Only the pinned dependency closure is evaluated; no external project files.
fp_classifier = \(collector, root = preparation_root) {
  path = collector$classifier_rules_manifest
  if (!identical(path, "rules/apm_20261027/provenance.json")) stop(
    "Unexpected classifier manifest path.")
  manifest = file.path(root, path)
  source = file.path(dirname(manifest), "definitions.R")
  if (!file.exists(manifest) || !file.exists(source) ||
      !identical(fp_hash(manifest), collector$classifier_rules_sha256)) stop(
    "Classifier provenance is missing or changed.")
  provenance = read_json(manifest, simplifyVector = T)
  if (!identical(fp_hash(source), provenance$definitions_sha256)) stop(
    "Frozen classifier definitions changed.")
  env = new.env(parent = baseenv())
  for (pkg in names(provenance$imports)) {
    for (name in provenance$imports[[pkg]]) {
      assign(name, getExportedValue(pkg, name), envir = env)
    }
  }
  sys.source(source, envir = env)
  list(env = env, classification_version = env$CLASSIFICATION_VERSION,
    price_analysis_version = env$PRICE_ANALYSIS_VERSION,
    snapshot_classification_version = env$SNAPSHOT_CLASSIFICATION_VERSION,
    snapshot_analysis_version = env$SNAPSHOT_ANALYSIS_VERSION,
    rules_version = provenance$version,
    sha256 = c(definitions = fp_hash(source), provenance = fp_hash(manifest)))
}
# <=============================================================================
# <=============================================================================
# <=============================================================================
# 2) CATEGORY ASSIGNMENT WITH VISIBLE RULES
# <-----------------------------------------------------------------------------
fp_category_labels = \(variants, categories, vocabulary = list()) {
  term = vapply(seq_len(nrow(categories)), \(i) {
    key = as.character(categories$sub_id[i])
    if (key %in% names(vocabulary)) vocabulary[[key]] else
      tolower(categories$sub_name[i])
  }, "")
  mapped = vapply(seq_len(nrow(categories)), \(i) as.character(
    categories$sub_id[i]) %in% names(vocabulary), logical(1))
  # Only mapped subcategories give category evidence; roots never do.
  label = setNames(ifelse(mapped, term, NA_character_), categories$sub_id)
  vapply(strsplit(variants$subcategory_ids, ";", fixed = T), \(ids) {
    found = label[ids]
    paste(found[!is.na(found)], collapse = " | ")
  }, "")
}
fp_assign_categories = \(variants, texts, categories, classifier,
  collector) {
  vocabulary = collector$category_vocabulary
  n = nrow(variants)
  abv = variants$alcohol_content
  if (is.null(variants$abv_filter_min_0_5)) variants$abv_filter_min_0_5 =
    ifelse(is.na(abv), "unknown", ifelse(abv >= 0.5, "yes", "no"))
  if (is.null(variants$abv_filter_gt_0_5)) variants$abv_filter_gt_0_5 =
    ifelse(is.na(abv), "unknown", ifelse(abv > 0.5, "yes", "no"))
  texts = texts[match(variants$sku, texts$sku), ]
  description = paste(ifelse(is.na(texts$detail_description_text), "",
    texts$detail_description_text), ifelse(is.na(texts$ingredients_text), "",
    paste("Zutaten:", texts$ingredients_text)))
  # FP-A01: a legal designation inside the name is used as designation.
  name_lower = tolower(variants$product_name)
  fortified = grepl("\\b(portwein|likörwein)\\b", name_lower, perl = T) &
    !grepl("whisk|\\brum\\b|\\bgin\\b|vodka|brandy", name_lower, perl = T)
  designation = ifelse(fortified, ifelse(grepl("portwein", name_lower),
    "likörwein (name: portwein)", "likörwein"), NA_character_)
  products = data.frame(product_name = variants$product_name,
    designation_raw = designation,
    retailer_category = fp_category_labels(variants, categories,
      vocabulary),
    description_raw = description, alcohol_content = variants$alcohol_content,
    stringsAsFactors = F)
  classified = classifier$env$classify_products(products)
  priced = classifier$env$assign_price_categories(classified)
  first_rule = priced$analysis_category_rule
  conalco = fp_conalco_equivalent(priced, designation, description,
    classifier)
  # FP-I01: the numeric filter (ABV >= 0.5) is never overridden by a label.
  # The project no-lo rule is lifted for those rows and the family decides.
  override = variants$content_kind == "volume" &
    variants$abv_filter_min_0_5 %in% "yes" & is.na(priced$analysis_category) &
    first_rule %in% "T00_nolo_separate"
  if (any(override)) {
    again = classified[override, ]
    again$no_lo_status = "alcoholic"
    again$alcohol_content = NA_real_
    second = classifier$env$assign_price_categories(again)
    priced$analysis_category[override] = second$analysis_category
    priced$analysis_category_rule[override] = paste0(
      second$analysis_category_rule, "+FP-I01_numeric_filter_over_label")
    priced$analysis_category_evidence[override] =
      second$analysis_category_evidence
  }
  nonvolume = variants$content_kind != "volume"
  ingredients = tolower(ifelse(is.na(texts$ingredients_text), "",
    texts$ingredients_text))
  ids = strsplit(variants$subcategory_ids, ";", fixed = T)
  mixer_only = vapply(ids, \(x) length(x) > 0 && all(suppressWarnings(
    as.integer(x)) %in% as.integer(collector$mixer_subcategory_ids)), T)
  no_alcohol_ingredient = !nonvolume & is.na(variants$alcohol_content) &
    mixer_only & nzchar(ingredients) & !grepl(paste0("alkohol|wein|bier|",
    "rum\\b|gin\\b|vodka|whisk|brannt|likör|spirituose|hefe|malz|trauben|",
    "most\\b"), ingredients, perl = T)
  excluded = nonvolume | no_alcohol_ingredient |
    priced$scope_status %in% "excluded_nonfamily_product"
  assigned = !excluded & !is.na(priced$analysis_category)
  state = ifelse(excluded, "excluded_nonbeverage", ifelse(assigned,
    "assigned", ifelse(priced$analysis_category_rule %in% "T00_nolo_separate",
    "nolo_separate", "review_unresolved")))
  rule = ifelse(nonvolume, "FP-S00_non_volume_unit", ifelse(
    no_alcohol_ingredient & !priced$scope_status %in%
    "excluded_nonfamily_product", "FP-X01_mixer_ingredients_without_alcohol",
    priced$analysis_category_rule))
  filter = variants$abv_filter_min_0_5
  inclusion = ifelse(excluded, "excluded_nonbeverage", ifelse(filter ==
    "yes", "included", ifelse(filter == "no", "excluded_below_threshold",
    "unclear")))
  category = ifelse(assigned, priced$analysis_category, NA_character_)
  ready = inclusion == "included" & !is.na(category)
  hint = variants$analysis_hint
  conflict = assigned & variants$analysis_hint_state == "direct" &
    !is.na(hint) & hint != priced$analysis_category
  evidence = substr(ifelse(is.na(priced$analysis_category_evidence), "",
    priced$analysis_category_evidence), 1, 160)
  assigned_table = data.frame(sku = variants$sku, price_category = category,
    category_state = state, inclusion_state = inclusion,
    analysis_ready = ready,
    analysis_ready_gt_0_5 = ready & variants$abv_filter_gt_0_5 == "yes",
    category_first_pass_rule = first_rule,
    category_rule = rule, category_evidence = ifelse(no_alcohol_ingredient,
      substr(ingredients, 1, 160), evidence),
    category_basis_abv = ifelse(assigned & is.na(variants$alcohol_content),
      "family_only_abv_unknown", ifelse(assigned, "family_and_abv", NA)),
    beverage_group = classified$beverage_group,
    scope_rule = classified$scope_rule,
    no_lo_status = classified$no_lo_status,
    alkopop_status = priced$alkopop_status,
    alkopop_rule = priced$alkopop_rule,
    tax_review_reason = priced$tax_review_reason,
    subcategory_hint_conflict = conflict,
    abv_claim_conflict = !is.na(variants$abv_claim) &
      !is.na(variants$alcohol_content) & variants$alcohol_content > 1.2,
    classification_version = classifier$classification_version,
    price_analysis_version = classifier$price_analysis_version,
    conalco_091_category = conalco$category, conalco_091_rule = conalco$rule,
    conalco_product_style = conalco$style, conalco_091_main_category =
      conalco$main,
    conalco_version = paste(classifier$snapshot_classification_version,
      classifier$snapshot_analysis_version, sep = " / "),
    stringsAsFactors = F)
  fp_refine_categories(assigned_table, variants, texts)
}
# <=============================================================================
# <=============================================================================
# <=============================================================================
# 3) REVIEW TABLES
# <-----------------------------------------------------------------------------
fp_abv_review = \(variants) {
  if (!nrow(variants) || !"abv_status" %in% names(variants)) return(
    fp_rows(list()))
  keep = is.na(variants$alcohol_content) & variants$content_kind == "volume"
  x = variants[keep, ]
  if (!nrow(x)) return(fp_rows(list()))
  data.frame(sku = x$sku, product_name = x$product_name,
    available = x$available, abv_status = x$abv_status,
    abv_claim = x$abv_claim, abv_upper_bound = x$abv_upper_bound,
    abv_bound_source = x$abv_bound_source, abv_components = x$abv_components,
    abv_filter_min_0_5 = x$abv_filter_min_0_5,
    abv_filter_basis = x$abv_filter_basis, stringsAsFactors = F)
}
fp_category_summary = \(variants) {
  if (!"category_state" %in% names(variants)) return(fp_rows(list()))
  known = ifelse(is.na(variants$alcohol_content), "abv_unknown", "abv_known")
  out = aggregate(list(skus = variants$sku), list(category_state =
    variants$category_state, price_category = ifelse(is.na(
    variants$price_category), "none", variants$price_category), abv = known),
    FUN = length)
  out[order(out$category_state, out$price_category, out$abv), ]
}
fp_category_review = \(variants) {
  if (!"category_state" %in% names(variants)) return(fp_rows(list()))
  x = variants[variants$category_state %in% c("review_unresolved") |
    variants$subcategory_hint_conflict %in% T, ]
  if (!nrow(x)) return(fp_rows(list()))
  data.frame(sku = x$sku, product_name = x$product_name,
    alcohol_content = x$alcohol_content, abv_claim = x$abv_claim,
    category_state = x$category_state, price_category = x$price_category,
    category_rule = x$category_rule, beverage_group = x$beverage_group,
    alkopop_rule = x$alkopop_rule, tax_review_reason = x$tax_review_reason,
    subcategory_hint = x$analysis_hint, hint_conflict =
    x$subcategory_hint_conflict, stringsAsFactors = F)
}
fp_inclusion_summary = \(variants) {
  if (!"inclusion_state" %in% names(variants)) return(fp_rows(list()))
  boundary = ifelse(is.na(variants$abv_boundary_status), "no",
    variants$abv_boundary_status)
  out = aggregate(list(skus = variants$sku), list(inclusion_state =
    variants$inclusion_state, abv_filter_min_0_5 =
    variants$abv_filter_min_0_5, price_category = ifelse(is.na(
    variants$price_category), "none", variants$price_category),
    boundary_case = boundary), FUN = length)
  out[order(out$inclusion_state, out$abv_filter_min_0_5,
    out$price_category), ]
}
fp_boundary_review = \(variants) {
  if (!"inclusion_state" %in% names(variants)) return(fp_rows(list()))
  low = !is.na(variants$alcohol_content) & variants$alcohol_content >= 0.5 &
    variants$alcohol_content <= 1.2
  keep = !is.na(variants$abv_boundary_status) | low
  x = variants[keep, ]
  if (!nrow(x)) return(fp_rows(list()))
  data.frame(sku = x$sku, product_name = x$product_name,
    alcohol_content = x$alcohol_content, abv_source = x$abv_source,
    label_claim = x$abv_claim, boundary_status = x$abv_boundary_status,
    abv_filter_min_0_5 = x$abv_filter_min_0_5,
    abv_filter_gt_0_5 = x$abv_filter_gt_0_5,
    inclusion_state = x$inclusion_state, price_category = x$price_category,
    first_pass_rule = x$category_first_pass_rule,
    final_rule = x$category_rule, stringsAsFactors = F)
}
# <=============================================================================
# <=============================================================================
# <=============================================================================
# 4) PRIMARY ANALYSIS AND SENSITIVITY: SAME DATA, ONLY THE FILTER DIFFERS
# <-----------------------------------------------------------------------------
# sensitivity_abv_ge_0_5 and primary_abv_gt_0_5 use the same rows, categories
# and quality rules; the difference is exactly the documented 0.5 % values.
fp_sensitivity_table = \(variants) {
  if (!"analysis_ready" %in% names(variants)) return(fp_rows(list()))
  levels = c(classifier_levels(variants), "Total")
  inclusive = variants$analysis_ready %in% c(T, "TRUE")
  strict = variants$analysis_ready_gt_0_5 %in% c(T, "TRUE")
  category = ifelse(is.na(variants$price_category), "none",
    variants$price_category)
  exact = variants$abv_boundary_status %in% "documented_numeric_no_qualifier"
  rows = lapply(levels, \(level) {
    use = if (level == "Total") rep(T, nrow(variants)) else category == level
    data.frame(price_category = level,
      sensitivity_abv_ge_0_5 = sum(use & inclusive),
      primary_abv_gt_0_5 = sum(use & strict),
      difference = sum(use & inclusive) - sum(use & strict),
      documented_exact_0_5 = sum(use & inclusive & exact),
      stringsAsFactors = F)
  })
  out = fp_rows(rows)
  out[out$price_category == "Total" | out$sensitivity_abv_ge_0_5 > 0 |
    out$primary_abv_gt_0_5 > 0, ]
}
classifier_levels = \(variants) c("Bier", "Wein", "Schaumwein", "Spirituosen",
  "Zwischenerzeugnisse ≤15 % vol", "Zwischenerzeugnisse >15 % vol",
  "Alkopops")
# Sensitivity rows must be a subset of the primary rows, and the difference
# must be the documented exact 0.5 % rows (nothing else may drop out).
fp_variant_consistency = \(variants) {
  if (!"analysis_ready" %in% names(variants)) return(character())
  primary = variants$analysis_ready %in% c(T, "TRUE")
  sens = variants$analysis_ready_gt_0_5 %in% c(T, "TRUE")
  exact = variants$abv_boundary_status %in% "documented_numeric_no_qualifier"
  problems = character()
  if (any(sens & !primary)) problems = "sensitivity_not_subset_of_primary"
  if (!setequal(which(primary & !sens), which(primary & exact))) problems =
    c(problems, "difference_is_not_the_documented_0_5_rows")
  # Separate product types and priced-with-accessory bundles are not ready.
  not_applied = variants$project_status %in% c("separate_product_type",
    "proposed_not_applied") | variants$refinement_decision %in%
    "assigned_bundle_not_ready"
  if (any(primary & not_applied)) problems = c(problems,
    "not_applied_decision_counted_as_ready")
  problems
}
fp_variant_definition = \() list(
  primary = list(label = "primary_abv_gt_0_5", rule = "ABV > 0.5 % vol",
    column = "analysis_ready_gt_0_5"),
  sensitivity = list(label = "sensitivity_abv_ge_0_5",
    rule = "ABV >= 0.5 % vol", column = "analysis_ready"),
  shared = "same raw data, quality rules, categories and formulas")
# <=============================================================================
# <=============================================================================
# <=============================================================================
# 5) REFINEMENT OF INCLUDED SKUS WITHOUT A CATEGORY (EVIDENCE RULES)
# <-----------------------------------------------------------------------------
# Only SKUs that are included (ABV >= 0.5) and still without a category are
# examined. A decision needs product text evidence; ABV is a necessary
# condition at most (spirits >= 15 %, whisky >= 40 %, alkopop < 10 %), never
# the evidence itself. Everything else stays unresolved and is labelled.
fp_snippet = \(pattern, text, width = 70) {
  hit = regexpr(pattern, text, perl = T)
  if (hit < 0) return("")
  start = max(1, hit - 20)
  substr(text, start, min(nchar(text), hit + attr(hit, "match.length") +
    width - 20))
}
fp_refine_one = \(name, desc, ing, abv, group, rule) {
  both = paste(desc, ing)
  res = \(decision, category, family, id, evidence, certainty) list(
    decision = decision, category = category, family = family, rule = id,
    evidence = evidence, certainty = certainty)
  if (rule == "S01_bundle_review" && !is.na(abv) && abv >= 15 &&
      group == "spirits") return(res("assigned_bundle_not_ready",
    "Spirituosen", "Spirituose mit Zubehör", "FP-R01_spirit_bundle",
    name, "high"))
  if (group == "beer" && grepl("radler|biermisch", paste(name, both),
      perl = T) && grepl("\\b(bier|pils|exportbier|pilsener)\\b", both,
      perl = T) && !grepl("wein|\\brum\\b|\\bgin\\b|vodka|brannt", ing,
      perl = T)) return(res("assigned", "Bier", "Biermischgetr\u00e4nk",
    "FP-R02_beer_mix_documented_beer_base", fp_snippet(
    "\\b(bier|pils|exportbier|pilsener)\\b", both), "high"))
  if (grepl("whisk(e)?y", name, perl = T) && !is.na(abv) && abv >= 40) {
    return(res("assigned", "Spirituosen", "Whisky",
      "FP-R03_whisky_name_and_min_40_percent", name, "high"))
  }
  if (!is.na(abv) && abv >= 15 && grepl(paste0("orangenlik\u00f6r|",
      "ist ein [^.]{0,30}lik\u00f6r"), desc, perl = T)) return(res(
    "assigned", "Spirituosen", "Lik\u00f6r", "FP-R04_liqueur_designation",
    fp_snippet("orangenlik\u00f6r|lik\u00f6r", desc), "high"))
  if (!is.na(abv) && abv >= 15 && grepl("rosolio", paste(name, desc),
      perl = T)) return(res("assigned", "Spirituosen", "Lik\u00f6r (Rosolio)",
    "FP-R04_liqueur_designation", fp_snippet("rosolio", paste(name,
    desc)), "medium"))
  if (group %in% c("cider", "other_wine")) return(res(
    "outside_seven_categories", NA, if (group == "cider")
    "Cider / apfelweinhaltiges Getr\u00e4nk" else
    "Gl\u00fchwein (aromatisiertes weinhaltiges Getr\u00e4nk)",
    "FP-R05_other_fermented_beverage", paste(group, name), "high"))
  # Serving suggestions (cocktail or longdrink as a use) are no evidence.
  explicit_ready = paste0("trinkfertig|servierfertig|fertig gemixt|fertig ",
    "gemischt|mixgetr\u00e4nk|vorgefertigt|ready.to.(drink|serve)|startklar|",
    "(diesen?|dieser) (longdrink|cocktail|mix)")
  ready = grepl(explicit_ready, both, perl = T) ||
    grepl("cocktail|longdrink|mixgetr\u00e4nk", ing, perl = T) ||
    (grepl("spritz", name) && grepl("prosecco|wein|soda", ing, perl = T))
  if (!is.na(abv) && abv >= 10 && ready) return(res(
    "outside_seven_categories", NA,
    "trinkfertiges Mischgetr\u00e4nk ab 10 % vol",
    "FP-R06_ready_mix_not_below_10_percent", fp_snippet(paste0(
    "trinkfertig|servierfertig|fertig gemixt|mixgetr\u00e4nk|longdrink|",
    "cocktail|vorgefertigt|ready|spritz"), paste(name, both)), "medium"))
  wine_pattern = paste0("\\b(wein|fruchtwein|perlwein|weisswein|wei\u00dfwein|",
    "rotwein|prosecco|apfelwein|obstwein)\\b")
  wine_documented = grepl(wine_pattern,
    ing, perl = T) && grepl("wasser|limonade|saft|zucker|aroma", ing,
    perl = T) && !grepl(paste0("lik\u00f6r|(?<!ent)alkohol|\\brum\\b|",
    "\\bgin\\b|vodka|brannt|spirituose"), ing, perl = T)
  designation = grepl("weinhaltig|fruchtweinhaltig|aromatisiert", both,
    perl = T) || grepl("mischung aus [^.]{0,30}(prosecco|wein)", desc,
    perl = T)
  term = grepl("fruchtsecco|bowle|sangria", name, perl = T)
  if (!is.na(abv) && abv < 10 && (wine_documented || designation || term)) {
    return(res("outside_seven_categories", NA,
      "aromatisiertes weinhaltiges Getr\u00e4nk",
      "FP-R07_wine_based_diluted_or_flavoured", fp_snippet(paste0(
      "weinhaltig|aromatisiert|\\b(fruchtwein|perlwein|wein|prosecco)\\b|",
      "fruchtsecco|bowle|sangria"), paste(name, both)),
      if (wine_documented || designation) "high" else "medium"))
  }
  family = if (group == "semi_sparkling_wine") "Perlwein (Frizzante)" else
    if (group == "fortified_wine_aperitif" || grepl("wermut|lillet",
      name)) "Wein-Aperitif / aromatisierter Wein ohne belegten Alkoholzusatz"
    else if (grepl("aperitiv|aperitif|aperol|spritz", paste(name, desc),
      perl = T)) "Aperitif unbekannter Alkoholbasis" else "ungekl\u00e4rt"
  res("unresolved", NA, family, "FP-R99_no_sufficient_evidence", "",
    "low")
}

fp_refine_categories = \(assigned, variants, texts) {
  texts = texts[match(variants$sku, texts$sku), ]
  clean = \(x) tolower(gsub("&quot;|&#39;|<[^>]+>", " ", ifelse(is.na(x), "",
    x)))
  desc = clean(texts$detail_description_text)
  ing = clean(texts$ingredients_text)
  name = tolower(variants$product_name)
  target = assigned$inclusion_state == "included" &
    is.na(assigned$price_category) & variants$content_kind == "volume"
  columns = c("refinement_decision", "refinement_family", "refinement_rule",
    "refinement_evidence", "refinement_certainty", "tax_class", "tax_basis",
    "project_status", "price_category_proposed", "stage1_decision",
    "product_type", "closure_evidence", "decision_origin")
  for (column in columns) assigned[[column]] = NA_character_
  assigned$differs_from_conalco = NA
  assigned$status_before = ifelse(target, sub("[+].*", "",
    assigned$category_rule), NA_character_)
  for (i in which(target)) {
    out = fp_refine_one(name[i], desc[i], ing[i], variants$alcohol_content[i],
      assigned$beverage_group[i], sub("[+].*", "", assigned$category_rule[i]))
    assigned$refinement_decision[i] = out$decision
    assigned$refinement_family[i] = out$family
    assigned$refinement_rule[i] = out$rule
    assigned$refinement_evidence[i] = substr(gsub("[[:space:]]+", " ",
      out$evidence), 1, 140)
    assigned$refinement_certainty[i] = out$certainty
    assigned$stage1_decision[i] = out$decision
    tax = list(tax_class = tax_default(out$rule), tax_basis = NA_character_,
      status = if (out$decision %in% c("assigned",
      "assigned_bundle_not_ready")) "applied" else "none",
      product_type = tax_type(out$rule), closure_evidence = NA_character_)
    if (out$decision %in% c("outside_seven_categories", "unresolved")) {
      tax = fp_tax_one(name[i], desc[i], ing[i], variants$alcohol_content[i],
        variants$container_type[i], out$family)
      if (tax$status == "applied") {
        out$decision = "assigned"
        out$category = tax$category
        out$family = tax$tax_class
        out$rule = tax$rule
        out$evidence = tax$evidence
        out$certainty = tax$certainty
      } else if (tax$status == "separate_product_type") {
        out$decision = "separate_product_type"
        out$rule = tax$rule
        out$certainty = tax$certainty
        out$evidence = tax$evidence
      } else if (!identical(tax$rule, "FP-T99_no_sufficient_evidence")) {
        out$family = tax$tax_class
        out$rule = tax$rule
        out$decision = "unresolved"
        out$category = NA
      }
    }
    assigned$tax_class[i] = tax$tax_class
    assigned$tax_basis[i] = tax$tax_basis
    assigned$project_status[i] = tax$status
    assigned$product_type[i] = tax$product_type
    assigned$closure_evidence[i] = substr(tax$closure_evidence, 1, 200)
    assigned$decision_origin[i] = if (assigned$stage1_decision[i] %in%
      c("assigned", "assigned_bundle_not_ready")) "earlier_run" else
      "tax_review"
    assigned$refinement_decision[i] = out$decision
    assigned$refinement_family[i] = out$family
    assigned$refinement_rule[i] = out$rule
    assigned$refinement_evidence[i] = substr(gsub("[[:space:]]+", " ",
      out$evidence), 1, 140)
    assigned$refinement_certainty[i] = out$certainty
    differs = !is.na(out$category) && !identical(out$category,
      assigned$conalco_091_category[i])
    assigned$differs_from_conalco[i] = differs
    if (!is.na(out$category)) {
      assigned$price_category[i] = out$category
      assigned$category_state[i] = "assigned"
      assigned$category_rule[i] = out$rule
    } else if (out$decision == "outside_seven_categories") {
      assigned$category_state[i] = "outside_seven_categories"
      assigned$category_rule[i] = out$rule
    } else {
      assigned$category_rule[i] = out$rule
    }
  }
  not_ready = assigned$refinement_decision %in% "assigned_bundle_not_ready"
  assigned$analysis_ready = assigned$inclusion_state == "included" &
    !is.na(assigned$price_category) & !not_ready
  assigned$analysis_ready_gt_0_5 = assigned$analysis_ready &
    variants$abv_filter_gt_0_5 == "yes"
  assigned
}
# What the current Conalco logic (snapshot_analysis.R) decides for the same
# rows; evaluated read-only before any Flaschenpost-specific rule.
fp_conalco_equivalent = \(priced, designation, description, classifier) {
  x = priced
  x$designation_raw = designation
  x$ingredients_raw = NA_character_
  x$archived_description_raw = description
  x$product_description_evidence = description
  x$in_rtd_rts_category = F
  out = tryCatch({
    x = classifier$env$snapshot_product_style(x)
    classifier$env$snapshot_intermediate_evidence(x)
  }, error = \(e) NULL)
  if (is.null(out)) return(list(category = rep(NA_character_, nrow(x)),
    rule = rep("snapshot_logic_failed", nrow(x)), style = rep(NA_character_,
    nrow(x)), main = rep(NA_character_, nrow(x))))
  list(category = out$analysis_category, rule = out$analysis_category_rule,
    style = out$product_style, main = out$main_category)
}
# <=============================================================================
# <=============================================================================
# <=============================================================================

# 6) TAX CLASSIFICATION (SchaumwZwStG, VwV 28.05.2025) AND PROJECT CATEGORY
# <-----------------------------------------------------------------------------

# The tax classification and the project category are decided separately.
# Sources: SchaumwZwStG 1 Abs. 2, 29, 30, 32 Abs. 1; AlkopopStG 1 Abs. 2;
# Regulation (EU) 251/2014; VwV Schaumwein, Zwischenerzeugnisse und Wein of
# 28.05.2025 (Rn. 5-7, 11-14). Evidence must come from the archived product
# data; pressure, closure and composition are never assumed.

fp_tax_one = \(name, desc, ing, abv, container, family) {
  both = paste(desc, ing)
  # Closure, pressure and designation hints; never a substitute for a bar value.
  hint_pattern = paste0("drehverschluss|twist.?off|schraubverschluss|",
    "kronkork|kornkork|schaumweinstopfen|sektkorken|agraffe|[0-9] bar|",
    "\u00fcberdruck|perlwein|frizzante|schaumwein")
  hints = regmatches(both, gregexpr(paste0(".{0,25}(", hint_pattern,
    ").{0,25}"), both, perl = T))[[1]]
  closure = paste(c(paste("Container:", container), unique(gsub("[[:space:]]+",
    " ", hints))), collapse = " | ")
  res = \(tax, basis, category, status, certainty, rule, evidence,
      type = NA_character_) list(tax_class = tax, tax_basis = basis,
    category = category, status = status, certainty = certainty, rule = rule,
    evidence = evidence, product_type = type, closure_evidence = closure)
  open = res("nicht bestimmbar", "Zusammensetzung nicht belegt", NA,
    "none", "low", "FP-T99_no_sufficient_evidence", "")
  if (is.na(abv)) return(open)
  if (abv <= 1.2) return(res(
    "nicht steuerbar nach SchaumwZwStG (h\u00f6chstens 1,2 % vol)",
    "\u00a7\u00a7 1, 29, 32 SchaumwZwStG (Alkoholgrenze > 1,2 % vol)", NA,
    "none", "medium", "FP-T10_not_taxable_up_to_1_2", "",
    "alcohol_free_wine_based_drink"))
  carbonated = grepl(paste0("kohlens|kohlendioxid|kohlenstoffdioxid|",
    "prickel|perlend|sprudel"), both, perl = T) ||
    grepl("secco|sekt|spritz|sprizz|prosecco|frizzante", name, perl = T)
  carton = grepl("karton|tetra|bag", tolower(container), perl = T)
  wine_pattern_tax = paste0("\\b(wein|weisswein|wei\u00dfwein|rotwein|",
    "perlwein|fruchtwein|prosecco)\\b")
  wine_base = grepl(wine_pattern_tax, both, perl = T) ||
    grepl("fruchtsecco|sangria|bowle|gl\u00fchwein", name, perl = T)
  w_basis = paste0("VwV Rn. 13; \u00a7 32 Abs. 1 SchaumwZwStG; VO (EU) ",
    "251/2014")
  weinhaltig = abv >= 4.5 && abv <= 14.5
  if (grepl("gl\u00fchwein", name) && weinhaltig && !carbonated) return(res(
    "Wein (aromatisiertes weinhaltiges Getr\u00e4nk)", w_basis, "Wein",
    "applied", "high", "FP-T01_gluehwein_still", fp_snippet(
    "gl\u00fchwein", paste(name, both)), "wine_based_drink"))
  if (grepl("sangria", name) && weinhaltig && wine_base &&
      (!carbonated || carton || !grepl("kohlens|prickel|perlend", both))) {
    return(res("Wein (aromatisiertes weinhaltiges Getr\u00e4nk)", w_basis,
      "Wein", "applied", "medium", "FP-T02_sangria_still",
      fp_snippet("sangria", paste(name, both)), "wine_based_drink"))
  }
  aromatised = grepl(paste0("mazerat|extrakt|chinin|kr\u00e4uter|aromat|",
    "gew\u00fcrz|botanical"), desc, perl = T)
  spirit = grepl(paste0("\\b(vodka|wodka|rum|gin|whisk(e)?y|lik\u00f6r|",
    "liqueur|branntwein)\\b"), both, perl = T) || grepl("alkohol", ing,
    perl = T)
  vermouth_name = grepl("wermut|vermouth", name, perl = T)
  if ((vermouth_name || (grepl("\\bweine?\\b", desc, perl = T) &&
      grepl("aperitif", desc, perl = T) && aromatised)) &&
      abv >= 14.5 && abv < 22 && !spirit) {
    return(res("Zwischenerzeugnis (aromatisierter Wein)",
      "VwV Rn. 11/12; \u00a7 29 Abs. 2 SchaumwZwStG; VO (EU) 251/2014",
      if (abv <= 15) "Zwischenerzeugnisse \u226415 % vol" else
      "Zwischenerzeugnisse >15 % vol", "applied", if (vermouth_name)
      "high" else "medium", "FP-T03_aromatised_wine_14_5_to_22",
      fp_snippet("wermut|vermouth|wein", paste(name, desc)),
      "aromatised_wine"))
  }
  # Only the product's own designation counts, not a component in the recipe.
  if (grepl("perlwein", paste(name, desc), perl = T) && wine_base &&
      weinhaltig) return(res("Wein (Perlwein, unter 3 bar)",
    "VwV Rn. 7 und 13", "Wein", "applied", "high",
    "FP-T04_perlwein_designation", fp_snippet("perlwein", paste(name, desc)),
    "perlwein"))
  if (grepl("frizzante", paste(name, desc), perl = T) && abv > 1.2 &&
      abv <= 15) return(res("Wein (Perlwein/Frizzante, unter 3 bar)",
    "VwV Rn. 7; \u00a7 32 Abs. 1 SchaumwZwStG", "Wein", "applied",
    "medium", "FP-T05_frizzante_designation", fp_snippet("frizzante",
    paste(name, desc)), "perlwein"))
  ready_mix = family == "trinkfertiges Mischgetr\u00e4nk ab 10 % vol"
  if (ready_mix && spirit && abv >= 10) return(res(
    "Alkohol (AlkStG; Spirituosenmischung, kein Alkopop)",
    "\u00a7 1 Abs. 2 AlkopopStG (unter 10 % vol); VwV Rn. 9, 14", NA,
    "separate_product_type", "medium", "FP-T07_spirit_based_rtd",
    fp_snippet(paste0("\\b(vodka|wodka|rum|gin|whisk(e)?y|lik\u00f6r|",
    "alkohol)\\b"), both), "spirit_based_rtd"))
  if (wine_base && weinhaltig && carbonated) return(res(
    "Wein oder Schaumwein (Druck/Aufmachung nicht belegt)",
    paste(w_basis, "und Rn. 6"), NA, "none", "low",
    "FP-T08_pressure_or_closure_missing", "", "wine_based_drink"))
  fruit_wine = grepl("apfelwein|fruchtwein|obstwein|cider|cidre",
    paste(name, both), perl = T)
  if (fruit_wine && abv > 1.2 && abv <= 10) return(res(
    paste("Wein oder Schaumwein (Perlwein unter 3 bar oder Schaumwein;",
      "Druck nicht belegt)"),
    paste("VwV Rn. 7; \u00a7 32 Abs. 1 Nr. 2 und \u00a7 1 Abs. 2 Nr. 2",
      "SchaumwZwStG"), NA, "none", "low",
    "FP-T09_fruit_wine_pressure_missing", "", "cider_or_fruit_wine"))
  if (ready_mix && !spirit && grepl("\\b(vodka|wodka|rum|gin|whisk(e)?y)\\b",
      name, perl = T)) return(res(
    "Alkohol (AlkStG) wahrscheinlich; Spirituosenanteil nur im Namen belegt",
    "VwV Rn. 9, 14", NA, "none", "low", "FP-T12_spirit_in_name_only", "",
    "ready_mix_unspecified"))
  if (ready_mix && !spirit) return(res(
    "Zwischenerzeugnis, Wein oder Alkohol (Zusammensetzung nicht belegt)",
    "VwV Rn. 5, 9, 14", NA, "none", "low",
    "FP-T11_ready_mix_composition_missing", "", "ready_mix_unspecified"))
  open
}

tax_type = \(rule) {
  switch(rule, "FP-R01_spirit_bundle" = "spirit_with_accessory",
    "FP-R02_beer_mix_documented_beer_base" = "beer_mixture",
    "FP-R03_whisky_name_and_min_40_percent" = "spirit",
    "FP-R04_liqueur_designation" = "liqueur", NA_character_)
}

tax_default = \(rule) {
  if (rule %in% c("FP-R03_whisky_name_and_min_40_percent",
      "FP-R04_liqueur_designation", "FP-R01_spirit_bundle")) {
    return("Alkohol (AlkStG): Spirituose")
  }
  if (rule == "FP-R02_beer_mix_documented_beer_base") return(
    "Bier (BierStG): Biermischgetr\u00e4nk")
  NA_character_
}

fp_refinement_table = \(variants) {
  if (!"refinement_decision" %in% names(variants)) return(fp_rows(list()))
  x = variants[!is.na(variants$refinement_decision), ]
  if (!nrow(x)) return(fp_rows(list()))
  data.frame(sku = x$sku, product_name = x$product_name,
    alcohol_content = x$alcohol_content, previous_status = x$status_before,
    decision = x$refinement_decision, proposed_category = ifelse(
    is.na(x$price_category), x$price_category_proposed, x$price_category),
    project_status = x$project_status, tax_class = x$tax_class,
    tax_basis = x$tax_basis, family = x$refinement_family,
    rule = x$refinement_rule, evidence = x$refinement_evidence,
    certainty = x$refinement_certainty, conalco_091_category =
    x$conalco_091_category, conalco_091_rule = x$conalco_091_rule,
    differs_from_conalco = x$differs_from_conalco,
    stage1_decision = x$stage1_decision, decision_origin = x$decision_origin,
    product_type = x$product_type, closure_evidence = x$closure_evidence,
    analysis_ready = x$analysis_ready, stringsAsFactors = F)
}
# <=============================================================================
# <=============================================================================
# <=============================================================================

# 7) HARMONISATION AUDIT AGAINST THE CURRENT CONALCO LOGIC
# <-----------------------------------------------------------------------------

fp_harmonisation_steps = \(reason) switch(reason,
  numeric_filter_over_nolo_label = paste("Conalco: no-lo label or ABV up to",
    "1.2 separate; decide whether the 0.5 percent filter is applied there"),
  vwv_aromatised_wine_drink_is_wine = paste("Conalco keeps other_wine apart",
    "(T05); adopt VwV Rn. 13 (Wine) or keep both views"),
  vwv_aromatised_wine_is_intermediate = paste("Conalco T08 needs documented",
    "added alcohol; VwV Rn. 11 treats aromatised wine as intermediate"),
  spirit_with_accessory_bundle = "consistent: Conalco S01 bundle review",
  spirit_based_rtd_separate_as_in_conalco = paste("consistent: both keep the",
    "product outside the seven categories"),
  mixer_ingredients_without_alcohol = paste("Conalco assigns by name pattern",
    "('bitter'); ingredient-based exclusion could be adopted"),
  beer_mixture_documented_beer_base = paste("Conalco needs a documented",
    "fermented-only composition; align the evidence form"),
  name_or_designation_spirit = paste("Conalco uses the legal designation;",
    "Flaschenpost has none, name and text are used"),
  "no step recorded")

fp_harmonisation_audit = \(variants) {
  if (!"conalco_091_category" %in% names(variants)) return(fp_rows(list()))
  rule = sub("[+].*", "", variants$category_rule)
  lookup = c("FP-T01_gluehwein_still" = "vwv_aromatised_wine_drink_is_wine",
    "FP-T02_sangria_still" = "vwv_aromatised_wine_drink_is_wine",
    "FP-T04_perlwein_designation" = "vwv_aromatised_wine_drink_is_wine",
    "FP-T05_frizzante_designation" = "vwv_aromatised_wine_drink_is_wine",
    "FP-T03_aromatised_wine_14_5_to_22" =
      "vwv_aromatised_wine_is_intermediate",
    "FP-R02_beer_mix_documented_beer_base" =
      "beer_mixture_documented_beer_base",
    "FP-R03_whisky_name_and_min_40_percent" = "name_or_designation_spirit",
    "FP-R04_liqueur_designation" = "name_or_designation_spirit",
    "FP-R01_spirit_bundle" = "spirit_with_accessory_bundle",
    "FP-T07_spirit_based_rtd" = "spirit_based_rtd_separate_as_in_conalco",
    "FP-X01_mixer_ingredients_without_alcohol" =
      "mixer_ingredients_without_alcohol")
  reason = unname(lookup[rule])
  reason[grepl("FP-I01", variants$category_rule)] =
    "numeric_filter_over_nolo_label"
  mine = ifelse(is.na(variants$price_category), "none",
    variants$price_category)
  theirs = ifelse(is.na(variants$conalco_091_category), "none",
    variants$conalco_091_category)
  keep = !is.na(reason) & (mine != theirs | !is.na(variants$product_type) &
    variants$product_type %in% "spirit_based_rtd")
  other = mine != theirs & is.na(reason)
  reason[other] = "other_difference"
  keep = keep | other
  if (!any(keep)) return(fp_rows(list()))
  table = aggregate(list(skus = variants$sku[keep]), list(reason = reason[keep],
    product_type = ifelse(is.na(variants$product_type[keep]), "n/a",
    variants$product_type[keep]), flaschenpost_category = mine[keep],
    conalco_091_category = theirs[keep], conalco_091_rule = sub("[+].*", "",
    variants$conalco_091_rule[keep])), FUN = length)
  table$harmonisation_step = vapply(table$reason, fp_harmonisation_steps, "")
  table[order(table$reason, -table$skus), ]
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 7) RUNTIME PORTABILITY PREFLIGHT; NO NETWORK OR PACKAGE INSTALLATION
# <-----------------------------------------------------------------------------

fp_portability_check = \() {
  stopifnot(getRversion() >= "4.1.0", l10n_info()[["UTF-8"]],
    identical(tolower("ÄÖÜ"), "äöü"))
  rules = fp_classifier(collector)
  stopifnot(identical(rules$classification_version, "beverage-0.7.1"),
    identical(rules$price_analysis_version, "price-analysis-0.4.1"))
  scratch = tempfile("fp_portability_")
  dir.create(scratch)
  on.exit(unlink(scratch, recursive = T), add = T)
  first = file.path(scratch, "space and unicode_ä.rds")
  second = file.path(scratch, "renamed.rds")
  probe = data.frame(sku = "001", price = 12.49, available = T)
  saveRDS(probe, first, version = 3)
  stopifnot(file.rename(first, second), identical(readRDS(second), probe))
  connection = gzfile(file.path(scratch, "panel.csv.gz"), "wt")
  write.csv(probe, connection, row.names = F)
  close(connection)
  connection = gzfile(file.path(scratch, "panel.csv.gz"), "rt")
  restored = read.csv(connection, colClasses = c(sku = "character"))
  close(connection)
  stopifnot(identical(restored, probe),
    identical(fp_panel_date(c("2026-10-26T23:30:00Z",
      "2026-07-01T22:30:00Z")), c("2026-10-27", "2026-07-02")))
  list(status = "passed", os = unname(Sys.info()["sysname"]),
    machine = unname(Sys.info()["machine"]), r_version = R.version.string,
    linux_executed = identical(unname(Sys.info()["sysname"]), "Linux"),
    utf8 = T, berlin_dates = T, gzip_roundtrip = T, same_volume_rename = T,
    rules_version = rules$rules_version,
    packages = as.list(vapply(names(config$dependency_versions),
      \(pkg) as.character(packageVersion(pkg)), "")), network_calls = 0L)
}
