# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING - FLASCHENPOST PREPARATION
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261023

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) OPEN CATEGORY FRONTIER AND PUBLIC CONTROLS
# <-----------------------------------------------------------------------------

fp_category_path = \(url) {
  path = sub("^https://www\\.flaschenpost\\.de", "", url)
  path = sub("#.*$", "", path)
  path = sub("/$", "", path)
  if (!grepl("^/(bier|weine|spirituosen)(/|$)", path)) {
    return(NA_character_)
  }
  path
}

fp_category_links = \(captures) {
  links = list()
  for (i in seq_len(nrow(captures))) {
    capture = captures[i, ]
    lines = readLines(capture$dom_file, warn = F)
    urls = grep("- /url: ", lines, fixed = T)
    roots = list()
    for (j in urls) {
      url = trimws(sub(".*- /url: ", "", lines[j]))
      path = fp_category_path(url)
      if (is.na(path)) next
      link_line = if (j > 1) lines[j - 1] else ""
      depth = nchar(link_line) - nchar(sub("^\\s*", "", link_line))
      family = paste0("/", strsplit(sub("^/", "", path), "/")[[1]][1])
      label = fp_groups(link_line, '- link "(.*)":$')
      label = if (length(label)) label[2] else NA_character_
      parent = if (path == family) NA_character_ else family
      basis = "path_prefix_inferred"
      if (identical(path, family)) {
        roots[[family]] = depth
        basis = "root_link_observed"
      } else if (!is.null(roots[[family]]) && roots[[family]] < depth) {
        basis = "nested_navigation_observed"
      }
      links[[length(links) + 1L]] = data.frame(
        category_path = path, parent_path = parent, label = label,
        edge_basis = basis, region_confirmed = capture$region_confirmed,
        capture_id = capture$capture_id, raw_file = capture$dom_file,
        raw_sha256 = capture$dom_sha256, stringsAsFactors = F)
    }
  }
  unique(fp_rows(links))
}

fp_controls = \(captures) {
  rows = list()
  for (i in seq_len(nrow(captures))) {
    capture = captures[i, ]
    doc = read_html(capture$html_file)
    nav = xml_find_first(doc, "//nav[@aria-label='Kategorie-Navigation']")
    buttons = xml_find_all(nav, ".//button")
    for (button in buttons) {
      label = fp_clean(xml_text(button))
      if (!nzchar(label)) next
      region = xml_find_first(button, "ancestor::*[@role='region'][1]")
      parent_button = xml_find_first(region,
        "preceding-sibling::button[1]")
      parent_label = fp_clean(xml_text(parent_button))
      rows[[length(rows) + 1L]] = data.frame(
        category_url = capture$source_url, label = label,
        parent_label = parent_label,
        expanded = xml_attr(button, "aria-expanded"),
        target_path = NA_character_, target_verified = F,
        region_confirmed = capture$region_confirmed,
        capture_id = capture$capture_id, raw_file = capture$html_file,
        raw_sha256 = capture$html_sha256, stringsAsFactors = F)
    }
  }
  unique(fp_rows(rows))
}

fp_category_registry = \(links, captures) {
  paths = sort(unique(c(unlist(config$discovery_roots, use.names = F),
    links$category_path)))
  fp_rows(lapply(paths, \(path) {
    seen = captures$source_url == paste0("https://www.flaschenpost.de", path)
    regional = seen & captures$region_confirmed
    labels = links$label[links$category_path == path & !is.na(links$label)]
    data.frame(category_path = path,
      label = if (length(labels)) labels[1] else NA_character_,
      link_observed = path %in% links$category_path,
      regional_link_observed = path %in%
        links$category_path[links$region_confirmed],
      visited_any = any(seen), visited_regional = any(regional),
      terminal_verified = F, scope_review = "pending_beverage_scope_review",
      discovery_status = if (any(regional)) "visited_end_unproven" else
        "frontier_unvisited", stringsAsFactors = F)
  }))
}

fp_next_categories = \(registry) {
  registry$category_path[registry$scope_review != "excluded_nonbeverage" &
    (!registry$visited_regional | !registry$terminal_verified)]
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 2) VARIANTS ARE IDENTIFIED BY ID, NOT URL
# <-----------------------------------------------------------------------------

fp_cards = \(captures) {
  rows = list()
  for (i in seq_len(nrow(captures))) {
    capture = captures[i, ]
    doc = read_html(capture$html_file)
    cards = xml_find_all(doc,
      paste0("//*[@data-cnstrc-item-variation-id and ",
        "@data-cnstrc-item-id and not(@data-cnstrc-product-detail)]"))
    for (card in cards) {
      label = xml_attr(card, "data-cnstrc-item-name")
      pack = fp_pack(label)
      href = xml_attr(card, "href")
      price = fp_number(xml_attr(card, "data-cnstrc-item-price"))
      preceding = fp_clean(xml_text(xml_find_first(card,
        "preceding::h2[1]")))
      option = xml_name(card) == "article" && !is.na(preceding) &&
        preceding == "Weitere Varianten"
      if (option && is.na(href)) href = sub("https://www.flaschenpost.de",
        "", capture$source_url, fixed = T)
      card_text = fp_clean(xml_text(card))
      deposit = fp_groups(card_text,
        "zzgl\\.\\s*([0-9]+[.,][0-9]+)\\s*€\\s*Pfand")
      base = fp_groups(card_text, "([0-9]+[.,][0-9]+)\\s*€/L")
      row = c(list(variant_id = xml_attr(card,
        "data-cnstrc-item-variation-id"),
        parent_id = xml_attr(card, "data-cnstrc-item-id"),
        product_label = label, product_path = href,
        category_url = capture$source_url,
        source_role = if (option) "detail_variant_option" else
          if (capture$capture_kind == "detail")
          "detail_recommendation" else "category_card_role_unverified",
        price_current = price,
        deposit = if (length(deposit)) fp_number(deposit[2]) else NA_real_,
        deposit_raw = if (length(deposit)) deposit[1] else NA_character_,
        price_per_l_displayed = if (length(base)) fp_number(base[2]) else
          NA_real_), pack,
        list(region_confirmed = capture$region_confirmed,
          observed_at = capture$observed_at, capture_id = capture$capture_id,
          html_truncated = capture$html_truncated,
          raw_file = capture$html_file, raw_sha256 = capture$html_sha256))
      rows[[length(rows) + 1L]] = as.data.frame(row,
        stringsAsFactors = F)
    }
  }
  unique(fp_rows(rows))
}

fp_snapshot_candidates = \(captures, cards) {
  rows = list()
  for (i in seq_len(nrow(captures))) {
    capture = captures[i, ]
    if (capture$capture_kind != "category") next
    lines = readLines(capture$dom_file, warn = F)
    for (j in grep("- /url: /p/", lines, fixed = T)) {
      caption = if (j > 1) lines[j - 1] else ""
      if (!grepl("€", caption, fixed = T)) next
      href = trimws(sub(".*- /url: ", "", lines[j]))
      pack = fp_pack(caption)
      same = cards$product_path == href &
        cards$region_confirmed == capture$region_confirmed &
        !is.na(cards$volume_l) & !is.na(pack$volume_l) &
        cards$pack_count == pack$pack_count &
        abs(cards$unit_volume_l - pack$unit_volume_l) < 1e-9
      same[is.na(same)] = F
      matches = unique(cards$variant_id[same])
      rows[[length(rows) + 1L]] = as.data.frame(c(list(
        product_path = href, caption = trimws(caption),
        category_url = capture$source_url,
        variant_id = if (length(matches) == 1) matches else NA_character_,
        identity_status = if (length(matches) == 1) "unique_id_pack_match"
          else "unresolved_identity", region_confirmed =
            capture$region_confirmed), pack,
        list(capture_id = capture$capture_id, raw_file = capture$dom_file,
          raw_sha256 = capture$dom_sha256)), stringsAsFactors = F)
    }
  }
  unique(fp_rows(rows))
}

fp_variant_registry = \(cards, details) {
  cards = cards[cards$region_confirmed, , drop = F]
  card_rows = data.frame(variant_id = cards$variant_id,
    parent_id = cards$parent_id, product_path = cards$product_path,
    product_label = cards$product_label, pack_count = cards$pack_count,
    unit_volume_l = cards$unit_volume_l, volume_l = cards$volume_l,
    evidence_role = cards$source_role, raw_file = cards$raw_file,
    raw_sha256 = cards$raw_sha256, stringsAsFactors = F)
  own_rows = data.frame(variant_id = details$variant_id,
    parent_id = details$retailer_parent_product_id,
    product_path = sub("https://www.flaschenpost.de", "",
      details$product_url, fixed = T), product_label = details$product_name,
    pack_count = details$pack_count, unit_volume_l = details$unit_volume_l,
    volume_l = details$volume_l, evidence_role = "own_detail",
    raw_file = details$raw_file, raw_sha256 = details$raw_sha256,
    stringsAsFactors = F)
  all = unique(rbind(card_rows, own_rows))
  fp_rows(lapply(split(all, all$variant_id), \(group) {
    score = 4 * (group$evidence_role == "own_detail") +
      2 * !is.na(group$volume_l) + !is.na(group$product_path)
    row = group[which.max(score), , drop = F]
    row$observed_product_paths = paste(sort(unique(na.omit(
      group$product_path))), collapse = "|")
    row$own_detail_verified = any(group$evidence_role == "own_detail")
    row$identity_conflict = length(unique(na.omit(group$parent_id))) > 1 |
      length(unique(na.omit(group$volume_l))) > 1 |
      length(unique(na.omit(group$pack_count))) > 1 |
      length(unique(na.omit(group$unit_volume_l))) > 1
    row$evidence_rows = nrow(group)
    row
  }))
}

fp_shared_urls = \(registry) {
  paths = fp_rows(lapply(seq_len(nrow(registry)), \(i) {
    values = strsplit(registry$observed_product_paths[i], "|",
      fixed = T)[[1]]
    if (!length(values)) return(NULL)
    rows = registry[rep(i, length(values)), , drop = F]
    rows$product_path = values
    rows
  }))
  groups = split(paths, paths$product_path)
  fp_rows(lapply(Filter(\(x) nrow(x) > 1, groups), \(group) {
    data.frame(product_path = group$product_path[1],
      n_variants = nrow(group),
      variant_ids = paste(sort(group$variant_id), collapse = "|"),
      parent_ids = paste(sort(unique(group$parent_id)), collapse = "|"),
      volumes_l = paste(sort(unique(group$volume_l)), collapse = "|"),
      stringsAsFactors = F)
  }))
}

fp_overlaps = \(candidates) {
  candidates = candidates[candidates$region_confirmed, , drop = F]
  groups = split(candidates[!is.na(candidates$variant_id), , drop = F],
    candidates$variant_id[!is.na(candidates$variant_id)])
  fp_rows(lapply(Filter(\(x) length(unique(x$category_url)) > 1, groups),
    \(group) data.frame(variant_id = group$variant_id[1],
      n_categories = length(unique(group$category_url)),
      category_urls = paste(sort(unique(group$category_url)),
        collapse = "|"), interpretation =
          "visible_on_category_pages_not_canonical_membership",
      raw_files = paste(sort(unique(group$raw_file)), collapse = "|"),
      stringsAsFactors = F)))
}
