# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING - FLASCHENPOST PREPARATION
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261023

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) LOCAL EVIDENCE AND EXACT VALUES
# <-----------------------------------------------------------------------------

fp_parser_version = "flaschenpost-preparation-0.1.0"

fp_rows = \(rows) {
  if (length(rows)) do.call(rbind, rows) else data.frame()
}

fp_clean = \(value) trimws(gsub("[[:space:]\u00a0\u202f]+", " ",
  value, perl = T))

fp_hash = \(path) digest(file = path, algo = "sha256")

fp_number = \(value) {
  suppressWarnings(as.numeric(sub(",", ".", value, fixed = T)))
}

fp_groups = \(value, pattern) {
  if (is.na(value)) return(character())
  regmatches(value, regexec(pattern, value, perl = T,
    ignore.case = T))[[1]]
}

fp_pack = \(value) {
  value = fp_clean(value)
  m = fp_groups(value, paste0("((?:[0-9]+\\s*[x×]\\s*)+)",
    "([0-9]+(?:[.,][0-9]+)?)\\s*(ml|cl|l)\\b"))
  if (length(m)) {
    factors = regmatches(m[2], gregexpr("[0-9]+", m[2]))[[1]]
    count = prod(as.numeric(factors))
    unit = fp_number(m[3]) * switch(tolower(m[4]), ml = 0.001,
      cl = 0.01, l = 1)
    return(list(pack_count = count, unit_volume_l = unit,
      volume_l = count * unit, pack_basis = "explicit_pack"))
  }
  list(pack_count = NA_real_, unit_volume_l = NA_real_,
    volume_l = NA_real_, pack_basis = "unresolved_pack")
}

fp_abv = \(value) {
  m = fp_groups(value, paste0("([0-9]+(?:[.,][0-9]+)?)\\s*",
    "(?:%\\s*vol\\.?|vol[.\\s-]*%)"))
  if (!length(m)) return(list(alcohol_content = NA_real_,
    alcohol_content_raw = value, abv_status = "missing_exact_abv"))
  qualified = grepl("<|weniger als|unter|bis zu|höchstens|max\\.",
    value, ignore.case = T, perl = T)
  number = fp_number(m[2])
  status = if (qualified) "qualified_not_exact" else "explicit_exact"
  if (!is.finite(number) || number < 0 || number > 100) {
    status = "invalid_abv"
  }
  if (status != "explicit_exact") number = NA_real_
  list(alcohol_content = number, alcohol_content_raw = value,
    abv_status = status)
}

fp_region = \(dom) {
  grepl('- button "10997 Berlin', dom, fixed = T)
}

fp_inventory = \(raw_dir) {
  paths = sort(list.files(raw_dir, "\\.json$", full.names = T))
  rows = lapply(paths, \(path) {
    meta = read_json(path, simplifyVector = T)
    stem = sub("\\.json$", "", path)
    html_path = paste0(stem, ".html")
    text_path = paste0(stem, ".txt")
    dom_path = paste0(stem, ".dom.txt")
    body = paste(readLines(html_path, warn = F), collapse = "\n")
    dom = paste(readLines(dom_path, warn = F), collapse = "\n")
    plain = paste(readLines(text_path, warn = F), collapse = "\n")
    truncated = grepl("[Truncated]", body, fixed = T)
    kind = if (grepl("/p/", meta$url, fixed = T)) "detail" else
      if (grepl("/(bier|weine|spirituosen)/", meta$url)) "category" else
        "preflight"
    data.frame(capture_id = basename(stem), observed_at = meta$at,
      source_url = meta$url, capture_kind = kind,
      region_confirmed = fp_region(dom), html_chars = nchar(body),
      html_truncated = truncated,
      dom_truncated = grepl("[Truncated]", dom, fixed = T),
      text_truncated = grepl("[Truncated]", plain, fixed = T),
      html_closed = grepl("</html>\\s*$", body),
      empty_or_loading = !nzchar(trimws(plain)),
      html_file = html_path, html_sha256 = fp_hash(html_path),
      raw_file = text_path, raw_sha256 = fp_hash(text_path),
      dom_file = dom_path, dom_sha256 = fp_hash(dom_path),
      status = "offline_preparation_not_productive",
      stringsAsFactors = F)
  })
  fp_rows(rows)
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 2) OWN DETAIL OBSERVATIONS
# <-----------------------------------------------------------------------------

fp_detail = \(capture) {
  doc = read_html(capture$html_file)
  own = xml_find_first(doc, "//*[@data-cnstrc-product-detail]")
  id = xml_attr(own, "data-cnstrc-item-variation-id")
  if (is.na(id)) return(NULL)
  heading = xml_find_first(own, ".//h1")
  name = fp_clean(xml_text(heading))
  header = xml_parent(heading)
  volume_raw = fp_clean(xml_text(xml_find_first(header,
    "./p[1]/span[1]")))
  unit_text = fp_clean(xml_text(xml_find_first(header,
    "./p[contains(., '€/L')]")))
  base = fp_groups(unit_text, "([0-9]+[.,][0-9]+)\\s*€/L")
  deposit = fp_groups(unit_text,
    "zzgl\\.\\s*([0-9]+[.,][0-9]+)\\s*€\\s*Pfand")
  body = paste(readLines(capture$raw_file, warn = F), collapse = "\n")
  own_text = strsplit(body, "Ähnliche Produkte|Unser Unternehmen",
    perl = T)[[1]][1]
  raw_abv = fp_groups(own_text, "Alkoholgehalt\\s*\\n([^\\n]+)")
  abv = fp_abv(if (length(raw_abv)) raw_abv[2] else NA_character_)
  description = fp_groups(own_text, "Beschreibung\\s*\\n([\\s\\S]+)$")
  before_name = strsplit(own_text, name, fixed = T)[[1]][1]
  money = regmatches(before_name, gregexpr(
    "[0-9]+[.,][0-9]{2}\\s*€", before_name, perl = T))[[1]]
  reference = if (length(money) >= 2) fp_number(sub("\\s*€", "",
    money[2])) else NA_real_
  low = fp_groups(own_text, paste0("Niedrigster Preis der letzten ",
    "30 Tage:\\s*([0-9]+[.,][0-9]+)"))
  multi = fp_groups(before_name, "-([0-9]+)% ab ([0-9]+) Stück")
  button = xml_find_first(own,
    ".//button[@data-cnstrc-btn='add_to_cart']")
  button_present = !is.na(xml_name(button))
  orderable = button_present && is.na(xml_attr(button, "disabled"))
  breadcrumb = xml_find_first(own,
    ".//nav[@aria-label='Brotkrumennavigation']")
  breadcrumb_labels = fp_clean(xml_text(xml_find_all(breadcrumb,
    "./a|./span")))
  row = c(list(retailer = "flaschenpost", retailer_product_id = id,
    retailer_parent_product_id = xml_attr(own, "data-cnstrc-item-id"),
    variant_id = id, product_name = name, product_url = capture$source_url,
    ean_gtin = NA_character_, volume_raw = volume_raw), fp_pack(volume_raw),
    list(price_current = fp_number(xml_attr(own, "data-cnstrc-item-price")),
      price_reference = reference,
      price_reference_type = if (is.na(reference)) NA_character_ else
        "displayed_strikethrough",
      price_lowest_30_days = if (length(low)) fp_number(low[2]) else
        NA_real_,
      multi_buy_raw = if (length(multi)) multi[1] else NA_character_,
      multi_buy_min_quantity = if (length(multi)) as.numeric(multi[3]) else
        NA_real_,
      multi_buy_discount_pct = if (length(multi)) as.numeric(multi[2]) else
        NA_real_,
      multi_buy_price = NA_real_,
      deposit = if (length(deposit)) fp_number(deposit[2]) else NA_real_,
      deposit_raw = if (length(deposit)) deposit[1] else NA_character_,
      deposit_status = if (length(deposit)) "explicit_separate" else
        "not_shown_not_zero",
      price_per_l_displayed = if (length(base)) fp_number(base[2]) else
        NA_real_, currency = "EUR"), abv,
    list(abv_raw_file = capture$raw_file,
      abv_raw_sha256 = capture$raw_sha256,
      abv_observed_at = capture$observed_at,
      alcohol_content_source = if (abv$abv_status == "explicit_exact")
        "public_detail_text" else NA_character_,
      product_description = if (length(description)) description[2] else
        NA_character_, designation_raw = NA_character_,
      retailer_category = paste(breadcrumb_labels, collapse = " > "),
      availability = if (orderable) "orderable_ui" else "unknown",
      availability_raw = if (orderable) "In den Warenkorb" else
        NA_character_,
      postal_code = if (capture$region_confirmed) "10997" else
        NA_character_, region_confirmed = capture$region_confirmed,
      fulfillment_mode = "delivery", warehouse_id = NA_character_,
      market_id = NA_character_,
      observed_at = capture$observed_at, raw_file = capture$raw_file,
      raw_sha256 = capture$raw_sha256,
      html_file = capture$html_file, html_sha256 = capture$html_sha256,
      capture_id = capture$capture_id,
      parser_version = fp_parser_version,
      data_source = "public_rendered_dom_text",
      html_truncated = capture$html_truncated,
      raw_truncated = capture$text_truncated,
      panel_retained = T, analysis_category = NA_character_,
      classification_version = NA_character_,
      price_analysis_eligible = F,
      status = "offline_preparation_not_productive"))
  row = as.data.frame(row, stringsAsFactors = F)
  row$variant_key = paste(row$retailer, row$postal_code,
    row$fulfillment_mode, row$variant_id, sep = "|")
  row$price_per_l = row$price_current / row$volume_l
  row$statistics_eligible_abv_only = ifelse(is.na(row$alcohol_content),
    NA, row$alcohol_content >= 0.5)
  row$parse_status = if (is.na(row$alcohol_content)) "partial" else
    "own_fields_parsed"
  row$validation_flags = paste(c("gtin_missing", "designation_missing",
    "classification_pending", "capture_not_atomic",
    if (row$html_truncated) "html_truncated",
    if (is.na(row$deposit)) "deposit_not_shown_not_zero",
    if (is.na(row$alcohol_content)) "missing_exact_abv"), collapse = ";")
  row
}

fp_details = \(captures) {
  selected = captures[captures$capture_kind == "detail" &
    captures$region_confirmed, , drop = F]
  rows = lapply(seq_len(nrow(selected)), \(i) fp_detail(selected[i, ]))
  rows = Filter(Negate(is.null), rows)
  fp_rows(rows)
}

fp_merge_details = \(observations) {
  groups = split(observations, observations$variant_key)
  fp_rows(lapply(groups, \(group) {
    group = group[order(group$observed_at), , drop = F]
    latest = group[nrow(group), , drop = F]
    compatible = group$product_name == latest$product_name &
      group$volume_raw == latest$volume_raw &
      group$retailer_parent_product_id == latest$retailer_parent_product_id
    exact = group[compatible & !is.na(group$alcohol_content), , drop = F]
    invalid = group$abv_status %in% c("qualified_not_exact", "invalid_abv")
    conflict = length(unique(exact$alcohol_content)) > 1
    if (conflict || any(invalid)) {
      latest$alcohol_content = NA_real_
      latest$abv_status = "conflicting_or_qualified_evidence"
    } else if (nrow(exact)) {
      verified = exact[nrow(exact), , drop = F]
      fields = c("alcohol_content", "alcohol_content_raw", "abv_status",
        "abv_raw_file", "abv_raw_sha256", "abv_observed_at",
        "alcohol_content_source")
      for (field in fields) latest[[field]] = verified[[field]]
    }
    latest$statistics_eligible_abv_only = ifelse(
      is.na(latest$alcohol_content), NA, latest$alcohol_content >= 0.5)
    latest$parse_status = if (is.na(latest$alcohol_content)) "partial" else
      "own_fields_parsed"
    flags = strsplit(latest$validation_flags, ";", fixed = T)[[1]]
    flags = setdiff(flags, "missing_exact_abv")
    if (is.na(latest$alcohol_content)) flags = c(flags, "missing_exact_abv")
    latest$validation_flags = paste(unique(flags), collapse = ";")
    latest
  }))
}
