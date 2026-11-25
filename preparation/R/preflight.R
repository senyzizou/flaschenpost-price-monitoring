# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING - FLASCHENPOST PREPARATION
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261027

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) THREE-REQUEST REACHABILITY TEST; NO INVENTORY OR PANEL UPDATE
# <-----------------------------------------------------------------------------

fp_runner_http_probe = \(ctx) {
  if (!isTRUE(ctx$runner$live_authorized)) stop(
    "Runner HTTP probe needs a separate explicit release.")
  if (!identical(as.integer(ctx$collector$warehouse_id), 36L) ||
      !identical(ctx$collector$warehouse_verification$status, "verified") ||
      length(ctx$state$calls)) stop(
    "Probe requires a fresh warehouse-36 context.")
  ctx$collector$max_retries = 0L
  ctx$budget$max_navigations = 3L
  ctx$budget$max_seconds = 180
  ctx$budget$runner$min_interval_seconds = max(10,
    ctx$budget$runner$min_interval_seconds)
  tree = fp_http_json(ctx, "category/warehouse/36")
  categories = fp_category_table(tree, ctx$collector)
  if (!397L %in% categories$sub_id[categories$fetch]) stop(
    "Reviewed probe subcategory is missing.")
  products = fp_http_json(ctx, "products/prv/warehouse/36",
    list(subcategory_id = 397L))
  if (!length(products)) stop("Probe list is empty.")
  for (product in products) fp_check_product(product, ctx$collector)
  key = as.character(products[[1]]$key)
  details = fp_http_json(ctx, "products/pdp/warehouse/36", list(ids = key))
  parsed = fp_parse_details(details, key)
  if (length(parsed$missing_keys) || anyDuplicated(parsed$details$sku) ||
      !all(parsed$details$product_key == key)) stop(
    "Probe detail identity gap.")
  list(status = "bounded_http_probe_passed", warehouse_id = 36L,
    calls = fp_rows(ctx$state$calls), n_requests = 3L,
    inventory_complete = F, panel_updated = F)
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 2) SYNTHETIC DOM FIXTURES; NO PRIVATE ARCHIVE IS READ OR COPIED
# <-----------------------------------------------------------------------------

fp_synthetic_dom = \(destination) {
  if (file.exists(destination)) stop("Synthetic fixture target exists.")
  raw = file.path(destination, "raw")
  dir.create(raw, recursive = T)
  ids = c("5909", "685", "686", "677", "678", "741", "744")
  abvs = c(5, 4.8, 4.8, 0.35, 0.35, 4.8, 5)
  names = paste("Synthetic beverage", seq_along(ids))
  paths = paste0("/p/synthetic/beverage-", seq_along(ids))
  paths[4:5] = "/p/jever/jever-fun"
  card = \(i, tag = "a", link = T) sprintf(paste0(
    "<%s data-cnstrc-item-variation-id='%s' data-cnstrc-item-id='p%s' ",
    "data-cnstrc-item-name='%s 20 x 0,5 L' data-cnstrc-item-price='12.49' ",
    "%s>20 x 0,5 L 1,249 €/L zzgl. 3,10 € Pfand</%s>"),
    tag, ids[i], ids[i], names[i], if (link) paste0("href='", paths[i],
      "'") else "", tag)
  nav = paste0('<nav aria-label="Kategorie-Navigation">',
    '<button aria-expanded="true">Alkoholfrei</button><div role="region">',
    '<button aria-expanded="true">Synthetische Untergruppe</button>',
    '</div></nav>')
  dom = c('- button "10997 Berlin"', '- link "Bier":',
    '  - /url: /bier', '  - link "Pils":', '    - /url: /bier/pils',
    '- link "Synthetic unknown 6 x 1 L 5,00 €":',
    '  - /url: /p/synthetic/unknown')
  write_capture = \(id, path, html, text) {
    stem = file.path(raw, id)
    writeLines(html, paste0(stem, ".html"))
    writeLines(text, paste0(stem, ".txt"))
    writeLines(dom, paste0(stem, ".dom.txt"))
    write_json(list(at = "2026-10-23T08:00:00Z", url = paste0(
      "https://www.flaschenpost.de", path), synthetic = T),
      paste0(stem, ".json"), auto_unbox = T)
  }
  for (i in seq_along(ids)) {
    html = paste0("<html><head><meta charset='utf-8'/></head><body>", nav,
      "<main data-cnstrc-product-detail='true' ",
      "data-cnstrc-item-variation-id='", ids[i],
      "' data-cnstrc-item-id='p", ids[i],
      "' data-cnstrc-item-price='12.49'><header><h1>", names[i],
      "</h1><p><span>20 x 0,5 L</span></p>",
      "<p>1,249 €/L zzgl. 3,10 € Pfand</p></header>",
      "<button data-cnstrc-btn='add_to_cart'>In den Warenkorb</button>",
      "</main><h2>Weitere Varianten</h2>", card(5, "article", F),
      "</body></html>")
    # Only attach the Jever variant to a page with that same product URL.
    if (!i %in% 4:5) html = sub(paste0("<h2>Weitere Varianten</h2>",
      card(5, "article", F)), "", html, fixed = T)
    write_capture(paste0("detail_", ids[i]), paths[i], html,
      paste("12,49 €", names[i], "Alkoholgehalt", paste0(abvs[i], " % vol"),
        "Beschreibung", "Synthetic fixture only", sep = "\n"))
  }
  write_capture("category", "/bier/pils", paste0(
    "<html><head><meta charset='utf-8'/></head><body>", nav,
    paste(vapply(seq_along(ids), card, ""), collapse = ""),
    "[Truncated]</body></html>"), "Synthetic listing only")
  write.csv(data.frame(retailer_product_id = ids, price_current = 12.49,
    alcohol_content = abvs, pack_count = 20, unit_volume_l = 0.5,
    volume_l = 10, deposit = 3.1),
    file.path(destination, "product_sample.csv"), row.names = F)
  files = list.files(destination, recursive = T, full.names = T)
  relative = substring(files, nchar(destination) + 2L)
  write_json(as.list(setNames(vapply(files, fp_hash, ""), relative)),
    file.path(destination, "checksums.json"), pretty = T, auto_unbox = T)
  invisible(destination)
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 3) CLOSED EXPORT LIST AND PRIVACY CHECK FOR THE OFFLINE RUNNER CORPUS
# <-----------------------------------------------------------------------------

fp_preflight_files = \() {
  runtime = list.files(file.path(preparation_root, "R"), "[.]R$")
  runtime = file.path("R", runtime)
  scripts = file.path("scripts", c("bootstrap.R", "ubuntu_preflight.R",
    "install_preflight_packages.R", "runner_http_probe.R",
    "build_preflight_bundle.R", "assess_readiness.R", "collect_flaschenpost.R",
    "prepare_daily.R", "replay_archives.R", "reprocess_run.R"))
  tests = file.path("tests", c("run_tests.R", "mock_driver.R", "test_panel.R",
    "test_readiness.R", "test_transport.R"))
  fixtures = file.path("tests", "fixtures", c("category_tree.json",
    "detail_3598_514_508.json", "pils_w1_head.json"))
  rules = file.path("rules", "apm_20261027",
    c("definitions.R", "provenance.json"))
  configs = file.path("config", c("offline_design.json", "runner_design.json",
    "collector_design.json", "flaschenpost_abv_evidence.csv",
    "ubuntu_packages.csv"))
  proof = file.path("output", "inventory_proofs")
  run = file.path(proof, "run_20261026_144918")
  raw = list.files(file.path(preparation_root, run, "raw"), "[.]json$")
  saved = "flaschenpost_2026-10-26_w36_INVENTORY_PROOF_PASSED"
  run_meta = file.path(run, saved, c("calls.csv", "meta.json"))
  comparison = file.path(proof, "reprocessed_volume_rules",
    "flaschenpost_2026-10-26_w36_INVENTORY_PROOF_PASSED",
    c(paste0(c("variants", "categories", "listing", "membership"), ".csv"),
      "meta.json"))
  c(runtime, scripts, tests, fixtures, rules, configs,
    file.path(run, "raw", raw), run_meta, comparison)
}

fp_preflight_privacy = \(files, text = NULL) {
  # Public product descriptions may contain business contact information.
  # Private browser files, request credentials and personal fields are banned.
  blocked = paste0("(?i)(/", "Users/|Bearer [A-Za-z0-9._-]{16,}|",
    "Authorization: (Basic|Bearer)|fp-account-[a-z-]+[=:]|",
    '"(street|house_?number|delivery_?address|customer_?id|',
    'session_?id|access_?token|refresh_?token|password)"\\s*:)')
  if (is.null(text)) text = vapply(files, \(path) paste(readLines(path,
    warn = F, encoding = "UTF-8"), collapse = "\n"), "")
  hits = grepl(blocked, text, perl = T)
  if (any(hits)) stop("Preflight privacy scan failed; nothing exportable.")
  invisible(list(status = "passed", files = length(files),
    private_browser_files_included = 0L))
}

fp_prepare_preflight = \(destination) {
  fp_private_dir(destination, runner)
  if (file.exists(destination)) stop("Preflight destination must be new.")
  paths = fp_preflight_files()
  source = file.path(preparation_root, paths)
  if (!all(file.exists(source))) stop("Preflight release files missing.")
  # Scan before copying, never recursively export the working directory.
  scan = fp_preflight_privacy(source[!grepl("[.]json$", source)])
  dir.create(destination, recursive = T)
  projections = list()
  excluded = c("street", "housenumber", "house_number", "city", "zipcode",
    "postalcode", "postal_code", "deliveryaddress", "delivery_address",
    "customerid", "customer_id", "sessionid", "session_id", "access_token",
    "refresh_token", "password", "email", "phone", "latitude", "longitude")
  strip = \(node) {
    if (!is.list(node)) return(node)
    if (!is.null(names(node))) node = node[!tolower(names(node)) %in% excluded]
    lapply(node, strip)
  }
  for (i in seq_along(paths)) {
    target = file.path(destination, paths[i])
    dir.create(dirname(target), recursive = T, showWarnings = F)
    if (grepl("[.]json$", source[i]) &&
        grepl("^(tests/fixtures/|output/inventory_proofs/.*raw/)", paths[i])) {
      original = read_json(source[i], simplifyVector = F)
      projected = strip(original)
      if (!identical(original, projected)) {
        text = toJSON(projected, auto_unbox = T, null = "null", digits = NA)
        fp_preflight_privacy(character(), text = text)
        writeLines(text, target)
        projections[[paths[i]]] = list(source_sha256 = fp_hash(source[i]),
          export_sha256 = fp_hash(target),
          change = "unused address/contact metadata fields removed")
        next
      }
    }
    fp_preflight_privacy(source[i])
    if (!file.copy(source[i], target, overwrite = F)) stop("Copy failed.")
  }
  run = file.path(destination, "output", "inventory_proofs",
    "run_20261026_144918")
  calls_file = Sys.glob(file.path(run, "flaschenpost_*", "calls.csv"))
  calls = read.csv(calls_file, colClasses = "character", na.strings = "")
  raw_files = list.files(file.path(run, "raw"), full.names = T)
  stopifnot(length(raw_files) == nrow(calls))
  calls$sha256 = vapply(raw_files, fp_hash, "")
  calls$chars = vapply(raw_files, \(path) nchar(rawToChar(readBin(path,
    "raw", file.size(path)))), integer(1))
  write.csv(calls, calls_file, row.names = F, na = "")
  fp_synthetic_dom(file.path(destination, "tests", "fixtures", "portable_dom"))
  dir.create(file.path(destination, "output", "continuation_20261027"),
    recursive = T, showWarnings = F)
  files = list.files(destination, full.names = T, recursive = T)
  scan = fp_preflight_privacy(files)
  records = lapply(files, \(file) list(path = substring(file,
    nchar(destination) + 2L), sha256 = fp_hash(file)))
  write_json(list(status = "offline_preflight_bundle", privacy = scan,
    files = records, warehouse_id = 36L,
    source = "HTTP evidence + synthetic DOM", projections = projections,
    live_network_enabled = F),
    file.path(destination, "preflight_manifest.json"),
    pretty = T, auto_unbox = T)
  invisible(destination)
}
