# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING - FLASCHENPOST PREPARATION
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261026

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) DELIVERY ADDRESS: MEMORY ONLY
# <-----------------------------------------------------------------------------

# The address exists only inside a closure. It is never written, printed,
# logged or put into an error message; every outgoing text passes fp_scrub().

fp_escape = \(text) {
  text = gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", text)
  gsub("[[:space:]]+", "[[:space:] ]+", text)
}

fp_address_terms = \(parts) {
  street = fp_clean(parts$street)
  number = fp_clean(parts$number)
  streets = unique(c(street, sub("stra(ss|ß)e", "str.", street,
    ignore.case = T), sub("str\\.", "strasse", street, ignore.case = T)))
  terms = c(paste(streets, number), paste0(streets, number), streets,
    paste(number, streets))
  terms = unique(terms[nchar(terms) >= 3])
  terms[order(-nchar(terms))]
}

fp_secret = \(parts) {
  force(parts)
  terms = fp_address_terms(parts)
  \(what = c("terms", "parts")) {
    switch(match.arg(what), terms = terms, parts = parts)
  }
}

fp_scrub = \(text, secret) {
  for (term in secret("terms")) {
    text = gsub(fp_escape(term), "[ADDRESS]", text, ignore.case = T)
  }
  text
}

# Hits are counted after merging overlapping variants into one replacement.
fp_scrub_hits = \(text, secret) {
  count = \(x) sum(lengths(regmatches(x, gregexpr("[ADDRESS]", x,
    fixed = T))))
  count(fp_scrub(text, secret)) - count(text)
}

# Hidden terminal prompt; refuses to run without a real local terminal.
fp_tty_ask = \(prompt, hidden = T) {
  if (!file.exists("/dev/tty")) stop("No local terminal for secure input.",
    call. = F)
  tty = file("/dev/tty", "r")
  if (hidden) system("stty -echo < /dev/tty")
  on.exit({
    if (hidden) system("stty echo < /dev/tty")
    close(tty)
  })
  cat(prompt, file = stderr())
  value = readLines(tty, n = 1L)
  cat("\n", file = stderr())
  fp_clean(value)
}

fp_ask_address = \(ask = fp_tty_ask) {
  parts = list(street = ask("Street (hidden): "),
    number = ask("House number (hidden): "),
    postal_code = ask("Postal code (hidden): ", hidden = F),
    city = ask("City (hidden): "))
  if (!all(nzchar(unlist(parts))) ||
      !identical(parts$postal_code, config$postal_code)) {
    stop("Incomplete address or postal code is not ", config$postal_code,
      ".", call. = F)
  }
  fp_secret(parts)
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 2) COMPLETE CAPTURES: NO CHARACTER LIMIT, TRUNCATION IS AN ERROR
# <-----------------------------------------------------------------------------

fp_capture_check = \(page, runner) {
  reasons = character()
  for (field in c("html", "text", "dom")) {
    value = page[[field]]
    if (is.null(value) || !is.character(value) || length(value) != 1L) {
      reasons = c(reasons, paste0(field, "_missing"))
      next
    }
    if (!nzchar(trimws(value))) reasons = c(reasons, paste0(field, "_empty"))
    if (grepl("[Truncated]", value, fixed = T)) reasons =
      c(reasons, paste0(field, "_truncation_marker"))
  }
  html = page$html
  if (is.character(html) && length(html) == 1L) {
    cap = abs(nchar(html) - runner$transport_cap_chars)
    if (cap <= runner$transport_cap_tolerance) reasons =
      c(reasons, "html_at_transport_cap")
    if (!grepl("</html>\\s*$", html)) reasons = c(reasons, "html_not_closed")
    if (!is.null(page$reported_chars) && page$reported_chars != nchar(html)) {
      reasons = c(reasons, "html_length_mismatch")
    }
  }
  unique(reasons)
}

fp_private_dir = \(dir, runner) {
  path = normalizePath(dir, mustWork = F)
  parts = strsplit(path, "/", fixed = T)[[1]]
  forbidden = vapply(runner$private_path_forbidden_parts,
    \(part) any(grepl(part, parts, fixed = T)), logical(1))
  if (!startsWith(path, "/") || any(forbidden)) {
    stop("Output directory is not a private local directory.", call. = F)
  }
  invisible(path)
}

fp_write_exact = \(text, path) {
  writeBin(charToRaw(enc2utf8(text)), path)
  fp_hash(path)
}

fp_capture_write = \(page, dir, id, secret, runner, observed_at = Sys.time()) {
  fp_private_dir(dir, runner)
  dir.create(dir, showWarnings = F, recursive = T)
  hits = 0
  for (field in c("html", "text", "dom")) {
    if (is.character(page[[field]]) && length(page[[field]]) == 1L) {
      hits = hits + fp_scrub_hits(page[[field]], secret)
    }
  }
  reasons = fp_capture_check(page, runner)
  for (field in c("html", "text", "dom")) {
    if (is.character(page[[field]]) && length(page[[field]]) == 1L) {
      page[[field]] = fp_scrub(page[[field]], secret)
    }
  }
  stem = file.path(dir, id)
  hashes = c(html = "", text = "", dom = "")
  files = c(html = ".html", text = ".txt", dom = ".dom.txt")
  for (field in names(files)) {
    if (is.character(page[[field]]) && length(page[[field]]) == 1L) {
      hashes[[field]] = fp_write_exact(page[[field]],
        paste0(stem, files[[field]]))
    }
  }
  url = sub("\\?.*$", "", page$url)
  write_json(list(at = format(observed_at, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    url = url, chars = nchar(page$html), valid = !length(reasons),
    reasons = reasons, scrubbed_hits = hits, html_sha256 = hashes[["html"]],
    text_sha256 = hashes[["text"]], dom_sha256 = hashes[["dom"]]),
    paste0(stem, ".json"), pretty = T, auto_unbox = T)
  data.frame(capture_id = id, source_url = url, valid = !length(reasons),
    reasons = paste(reasons, collapse = ";"), scrubbed_hits = hits,
    region_confirmed = is.character(page$dom) && fp_region(page$dom),
    stringsAsFactors = F)
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 3) ACCESS RULES, PACING AND BLOCK HANDLING
# <-----------------------------------------------------------------------------

fp_url_allowed = \(url, runner) {
  parts = regmatches(url, regexec("^https://([^/?#]+)([^?#]*)(.*)$", url))[[1]]
  if (length(parts) != 4L || parts[2] != runner$host) return(F)
  blocked = vapply(runner$blocked_path_prefixes,
    \(prefix) startsWith(parts[3], prefix), logical(1))
  !any(blocked) && !grepl("suche", parts[4], ignore.case = T)
}

fp_block_signal = \(text, status, runner) {
  head = tolower(substr(text, 1, 3000))
  status %in% c(403L, 429L, 503L) ||
    any(vapply(runner$block_patterns, \(p) grepl(p, head, fixed = T),
      logical(1)))
}

fp_budget = \(runner, max_navigations, max_seconds,
  clock = Sys.time, sleep = Sys.sleep) {
  state = new.env(parent = emptyenv())
  state$count = 0L
  state$start = NULL
  state$last = NULL
  list(state = state, runner = runner, max_navigations = max_navigations,
    max_seconds = max_seconds, clock = clock, sleep = sleep)
}

# Every navigation passes here: hard budget, fixed minimum gap, no parallelism.
fp_budget_step = \(budget) {
  state = budget$state
  now = budget$clock()
  if (is.null(state$start)) state$start = now
  if (state$count >= budget$max_navigations) stop("Navigation budget used.",
    call. = F)
  if (as.numeric(difftime(now, state$start, units = "secs")) >
      budget$max_seconds) stop("Time budget used.", call. = F)
  if (!is.null(state$last)) {
    gap = budget$runner$min_interval_seconds -
      as.numeric(difftime(now, state$last, units = "secs"))
    if (gap > 0) budget$sleep(gap)
  }
  state$last = budget$clock()
  state$count = state$count + 1L
  invisible(state$count)
}

fp_log = \(file, event, secret, ..., clock = Sys.time) {
  fields = list(...)
  fields = lapply(fields, \(x) fp_scrub(sub("\\?.*$", "", as.character(x)),
    secret))
  line = toJSON(c(list(at = format(clock(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    event = event), fields), auto_unbox = T)
  cat(line, "\n", sep = "", file = file, append = T)
  invisible(line)
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 4) LIST STATE: PAGINATION, LAZY LOADING, SUBCATEGORIES, VARIANTS
# <-----------------------------------------------------------------------------

fp_list_probe = \(html) {
  doc = read_html(html)
  cards = xml_find_all(doc, paste0("//*[@data-cnstrc-item-variation-id and ",
    "@data-cnstrc-item-id and not(@data-cnstrc-product-detail)]"))
  ids = unique(xml_attr(cards, "data-cnstrc-item-variation-id"))
  links = unique(xml_attr(xml_find_all(doc, "//a[contains(@href,'/p/')]"),
    "href"))
  buttons = fp_clean(xml_text(xml_find_all(doc, "//button")))
  more = grepl("(mehr|weitere).*(laden|anzeigen|produkte)", buttons,
    ignore.case = T)
  page_links = unique(xml_attr(xml_find_all(doc, paste0(
    "//a[contains(@href,'page=') or contains(@href,'seite=') or ",
    "@rel='next']")), "href"))
  busy = xml_find_all(doc, paste0("//*[@aria-busy='true' or ",
    "@role='progressbar' or contains(@class,'skeleton') or ",
    "contains(@class,'spinner')]"))
  total = fp_groups(fp_clean(xml_text(doc)),
    "([0-9][0-9.]*)\\s+(Produkte|Artikel)\\b")
  nav = xml_find_first(doc, "//nav[@aria-label='Kategorie-Navigation']")
  controls = xml_find_all(nav, ".//button[@aria-expanded]")
  states = xml_attr(controls, "aria-expanded")
  options = xml_find_all(doc, paste0("//article[@data-cnstrc-item-",
    "variation-id and preceding::h2[1][normalize-space()=",
    "'Weitere Varianten']]"))
  list(variant_ids = ids, n_variants = length(ids), n_links = length(links),
    load_more = sum(more), load_more_label = if (any(more)) buttons[more][1]
      else NA_character_, page_links = page_links, loading = length(busy),
    declared_total = if (length(total)) as.integer(gsub("\\.", "", total[2]))
      else NA_integer_, collapsed_controls = sum(states == "false"),
    expanded_controls = sum(states == "true"),
    variant_options = length(options))
}

# The end is proven only by repeated unchanged state with nothing pending.
fp_scroll_until_stable = \(driver, runner, pause = Sys.sleep) {
  stable = 0L
  history = integer()
  probe = fp_list_probe(driver$html())
  for (round in seq_len(runner$scroll_max_rounds)) {
    if (probe$load_more > 0) driver$click_text(probe$load_more_label) else
      driver$scroll()
    pause(runner$scroll_wait_seconds)
    after = fp_list_probe(driver$html())
    history = c(history, after$n_variants)
    unchanged = after$n_variants == probe$n_variants &&
      after$load_more == 0 && after$loading == 0
    stable = if (unchanged) stable + 1L else 0L
    probe = after
    if (stable >= runner$scroll_stable_rounds) break
  }
  reason = "terminal_state_proven"
  if (stable < runner$scroll_stable_rounds) reason = "max_rounds_without_end"
  else if (length(probe$page_links)) reason = "pagination_present"
  else if (!is.na(probe$declared_total) &&
      probe$declared_total != probe$n_variants) reason =
    "declared_total_mismatch"
  else if (probe$n_variants == 0L) reason = "no_products_seen"
  list(terminal = identical(reason, "terminal_state_proven"), reason = reason,
    rounds = length(history), history = history, probe = probe)
}

fp_page_urls = \(probe, runner) {
  urls = as.character(probe$page_links)
  if (!length(urls)) return(character())
  urls = ifelse(startsWith(urls, "/"), paste0("https://", runner$host,
    urls), urls)
  unique(urls[startsWith(urls, "https://")])
}

# Unknown category controls are clicked in context; the effect is recorded.
fp_resolve_control = \(driver, label, parent_label, base_url,
  pause = Sys.sleep) {
  before = fp_list_probe(driver$html())
  clicked = driver$click_text(label)
  pause(1)
  after = fp_list_probe(driver$html())
  url = driver$url()
  navigated = !identical(url, base_url)
  data.frame(label = label, parent_label = parent_label, clicked = clicked,
    target_path = fp_category_path(url), navigated = navigated,
    filter_state = clicked && !navigated &&
      !setequal(before$variant_ids, after$variant_ids),
    new_variants = length(setdiff(after$variant_ids, before$variant_ids)),
    target_verified = clicked && navigated &&
      !is.na(fp_category_path(url)),
    stringsAsFactors = F)
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 5) REGIONAL STABILITY AND PRIVACY EVIDENCE
# <-----------------------------------------------------------------------------

# runs: one data frame per independent session with category_path,
# variant_id and region_confirmed.
fp_region_stability = \(runs, runner) {
  reasons = character()
  if (length(runs) < runner$region_min_sessions) reasons =
    "too_few_sessions"
  if (any(!vapply(runs, \(run) nrow(run) > 0 && all(run$region_confirmed),
      logical(1)))) reasons = c(reasons, "region_not_confirmed")
  paths = Reduce(intersect, lapply(runs, \(run) unique(run$category_path)))
  all_paths = unique(unlist(lapply(runs, \(run) run$category_path)))
  if (length(setdiff(all_paths, paths))) reasons =
    c(reasons, "category_missing_in_session")
  rows = list()
  for (path in paths) for (i in seq_len(length(runs) - 1L)) {
    a = unique(runs[[i]]$variant_id[runs[[i]]$category_path == path])
    b = unique(runs[[i + 1L]]$variant_id[runs[[i + 1L]]$category_path == path])
    rows[[length(rows) + 1L]] = data.frame(category_path = path,
      sessions = paste(i, i + 1L, sep = "-"),
      jaccard = length(intersect(a, b)) / max(1, length(union(a, b))),
      stringsAsFactors = F)
  }
  jaccard = fp_rows(rows)
  if (nrow(jaccard) && any(jaccard$jaccard < runner$region_min_jaccard)) {
    reasons = c(reasons, "selection_unstable")
  }
  list(pass = !length(reasons), reasons = reasons, jaccard = jaccard)
}

fp_scan_outputs = \(dir, secret) {
  files = list.files(dir, recursive = T, full.names = T, all.files = T)
  hits = vapply(files, \(path) {
    fp_scrub_hits(paste(readLines(path, warn = F, encoding = "UTF-8"),
      collapse = "\n"), secret)
  }, numeric(1))
  data.frame(file = basename(files), hits = unname(hits),
    stringsAsFactors = F)
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 6) PRECONDITIONS FOR A FULL COLLECTION
# <-----------------------------------------------------------------------------

fp_full_run_gate = \(runner, assessment, end_states, stability, captures,
  privacy) {
  reasons = character()
  if (!isTRUE(runner$terms_cleared)) reasons = "terms_consent_not_cleared"
  if (!isTRUE(assessment$inventory_complete)) reasons =
    c(reasons, assessment$inventory_reasons)
  if (!nrow(end_states) || any(!end_states$terminal)) reasons =
    c(reasons, "list_end_not_proven")
  if (!isTRUE(stability$pass)) reasons = c(reasons, "region_unstable")
  if (!nrow(captures) || any(!captures$valid)) reasons =
    c(reasons, "invalid_or_truncated_capture")
  if (any(privacy$hits > 0)) reasons = c(reasons, "address_in_outputs")
  list(pass = !length(reasons), reasons = unique(reasons))
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 7) SESSION RUNNER (DRIVER INJECTED; LIVE ONLY AFTER SEPARATE RELEASE)
# <-----------------------------------------------------------------------------

# driver: list(goto, html, text, dom, url, scroll, click_text,
# apply_address, close). The default runner config forbids live use.
fp_run_session = \(driver, secret, paths, out_dir, scope, runner,
  budget, gate = NULL, pause = Sys.sleep) {
  stopifnot(scope %in% c("small_test", "full"))
  if (!isTRUE(runner$live_authorized)) stop("Live access is not authorized.",
    call. = F)
  if (scope == "full" && !isTRUE(gate$pass)) stop("Full-run gate not passed.",
    call. = F)
  fp_private_dir(out_dir, runner)
  dir.create(out_dir, showWarnings = F, recursive = T)
  log_file = file.path(out_dir, "runner_log.jsonl")
  on.exit(driver$close(), add = T)
  captures = list()
  ends = list()
  save_page = \(id, url) {
    page = list(url = url, html = driver$html(), text = driver$text(),
      dom = driver$dom())
    fp_capture_write(page, out_dir, id, secret, runner)
  }
  visit = \(url) {
    if (!fp_url_allowed(url, runner)) stop("URL not allowed.", call. = F)
    fp_budget_step(budget)
    status = driver$goto(url)
    fp_log(log_file, "navigate", secret, url = url, status = status)
    if (fp_block_signal(driver$text(), status, runner)) {
      stop("Access block signal; stopping without retry.", call. = F)
    }
  }
  tryCatch({
    home = paste0("https://", runner$host, "/")
    visit(home)
    driver$apply_address(secret)
    pause(runner$scroll_wait_seconds)
    captures[[1]] = save_page("000_home_region", home)
    if (!captures[[1]]$region_confirmed) stop("Region not confirmed.",
      call. = F)
    queue = paste0(home, sub("^/", "", paths))
    seen = character()
    while (length(queue)) {
      url = queue[1]
      queue = queue[-1]
      if (url %in% seen) next
      seen = c(seen, url)
      visit(url)
      end = fp_scroll_until_stable(driver, runner, pause)
      id = sprintf("%03d_%s", length(captures), gsub("[^a-z0-9]+", "_",
        tolower(sub("^https://[^/]+/?", "", url))))
      captures[[length(captures) + 1L]] = save_page(id, url)
      ends[[length(ends) + 1L]] = data.frame(url = url,
        category_path = fp_category_path(url), terminal = end$terminal,
        reason = end$reason, variants = end$probe$n_variants,
        stringsAsFactors = F)
      queue = c(queue, setdiff(fp_page_urls(end$probe, runner), seen))
    }
  }, error = \(e) stop(fp_scrub(conditionMessage(e), secret), call. = F))
  list(captures = fp_rows(captures), end_states = fp_rows(ends),
    scan = fp_scan_outputs(out_dir, secret))
}

# Optional real browser adapter. Not installed here, not validated, and the
# address form still has to be calibrated in the first controlled live test.
fp_chromote_driver = \() {
  if (!requireNamespace("chromote", quietly = T)) stop(
    "chromote is not installed; no installation attempted.", call. = F)
  session = getExportedValue("chromote", "ChromoteSession")$new()
  run_js = \(code) session$Runtime$evaluate(code,
    returnByValue = T)$result$value
  list(validated = F,
    goto = \(url) {
      session$Page$navigate(url)
      session$Page$loadEventFired()
      200L
    },
    html = \() run_js("document.documentElement.outerHTML"),
    text = \() run_js("document.body.innerText"),
    dom = \() run_js(paste0("[...document.querySelectorAll('button,a')]",
      ".map(e=>e.tagName=='A'?'- link \"'+e.innerText.trim()+'\":\\n  ",
      "- /url: '+e.getAttribute('href'):'- button \"'+e.innerText.trim()",
      "+'\"').join('\\n')")),
    url = \() run_js("location.href"),
    scroll = \() run_js("window.scrollTo(0, document.body.scrollHeight)"),
    click_text = \(label) isTRUE(run_js(paste0("(()=>{const b=[...document.",
      "querySelectorAll('button,a')].find(e=>e.innerText.trim()==",
      jsonlite::toJSON(label, auto_unbox = T), ");if(b){b.click();",
      "return true}return false})()"))),
    apply_address = \(secret) stop(
      "Address form selectors are not calibrated yet.", call. = F),
    close = \() session$close())
}
