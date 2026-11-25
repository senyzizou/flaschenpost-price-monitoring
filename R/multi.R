# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261030

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) CONFIGURATION AND OBSERVATION KEYS
# <-----------------------------------------------------------------------------

rp_mw_load = \(root = ".") {
  file = file.path(root, "config", "multi_warehouse.json")
  mw = jsonlite::fromJSON(file, simplifyVector = F)
  ids = vapply(mw$warehouses, \(w) as.integer(w$warehouse_id), integer(1))
  if (anyNA(ids) || anyDuplicated(ids)) {
    stop("Warehouse ids must be unique integers.", call. = F)
  }
  mw
}

rp_mw_ids = \(mw) {
  vapply(mw$warehouses, \(w) as.integer(w$warehouse_id), integer(1))
}

rp_mw_obs_key = \(retailer, warehouse_id, sku, date) {
  paste(retailer, paste0("w", warehouse_id), sku, date, sep = "|")
}

rp_mw_check_keys = \(df) {
  need = c("retailer", "warehouse_id", "sku", "run_date")
  if (!all(need %in% names(df))) stop("Key columns missing.", call. = F)
  key = rp_mw_obs_key(df$retailer, df$warehouse_id, df$sku, df$run_date)
  list(ok = !anyDuplicated(key) && !anyNA(key),
    duplicates = unique(key[duplicated(key)]))
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 2) ELIGIBILITY, RELEASE AND RUN PLAN
# <-----------------------------------------------------------------------------

rp_mw_release_ok = \(mw, date) {
  rel = mw$release
  isTRUE(!is.null(rel$run_id) && nzchar(rel$run_id) &&
    identical(rel$valid_for_date, date) &&
    length(rel$released_warehouses) >= 1L)
}

rp_mw_eligibility = \(mw, date, store_wids = integer()) {
  min_s = mw$min_independent_sessions_for_production
  rel_ok = rp_mw_release_ok(mw, date)
  rows = lapply(mw$warehouses, \(w) {
    id = as.integer(w$warehouse_id)
    sessions_ok = w$independent_sessions >= min_s
    set = w$category_set
    category_ok = set %in% c("reviewed_w36", "baseline_reviewed",
      "baseline_pending")
    review_open = identical(set, "baseline_pending") &&
      id %in% store_wids
    released = rel_ok && id %in% unlist(mw$release$released_warehouses)
    why = c(if (!isTRUE(w$observation_unit)) "not_observation_unit",
      if (!sessions_ok) "needs_second_independent_session",
      if (!category_ok) "category_set_not_reviewed",
      if (review_open) "baseline_review_pending",
      if (!released) "not_released")
    data.frame(warehouse_id = id, sessions_ok = sessions_ok,
      category_ok = category_ok, released = released,
      eligible = length(why) == 0L,
      reasons = paste(why, collapse = ";"))
  })
  do.call(rbind, rows)
}

rp_mw_plan = \(mw, date, committed = integer(), only = NULL,
  store_wids = integer()) {
  el = rp_mw_eligibility(mw, date, store_wids)
  todo = el$eligible & !el$warehouse_id %in% committed
  if (!is.null(only)) todo = todo & el$warehouse_id %in% only
  run = el$warehouse_id[todo]
  cap = mw$release$max_warehouses_per_run
  if (length(run) > cap) run = run[seq_len(cap)]
  list(run = run, eligibility = el)
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 3) RUN MANIFEST (COMMITTED ENTRIES ARE IMMUTABLE)
# <-----------------------------------------------------------------------------

rp_mw_states = c("planned", "committed", "already_committed", "failed")

rp_mw_manifest_new = \(date, ids) {
  list(date = date, entries = setNames(lapply(ids, \(i) list(
    warehouse_id = as.integer(i), status = "planned", attempts = 0L,
    reason = NA_character_)), paste0("w", ids)))
}

rp_mw_manifest_set = \(m, id, status, reason = NA_character_,
  max_attempts = 2L) {
  if (!status %in% rp_mw_states) stop("Unknown state.", call. = F)
  key = paste0("w", id)
  old = m$entries[[key]]
  if (is.null(old)) old = list(warehouse_id = as.integer(id),
    status = "planned", attempts = 0L, reason = NA_character_)
  if (old$status %in% c("committed", "already_committed")) {
    stop("Committed warehouse entries are immutable.", call. = F)
  }
  if (status %in% c("committed", "failed") && old$attempts >= max_attempts) {
    stop("Attempt limit reached for this warehouse and day.", call. = F)
  }
  old$status = status
  old$reason = reason
  if (status %in% c("committed", "failed")) old$attempts = old$attempts + 1L
  m$entries[[key]] = old
  m
}

rp_mw_manifest_file = \(dir, date) file.path(dir, paste0("manifest_", date,
  ".json"))

rp_mw_manifest_read = \(dir, date) {
  file = rp_mw_manifest_file(dir, date)
  if (!file.exists(file)) return(NULL)
  jsonlite::fromJSON(file, simplifyVector = F)
}

rp_mw_manifest_write = \(m, dir, mw) {
  rp_mw_path_ok(dir, mw)
  dir.create(dir, recursive = T, showWarnings = F)
  prev = rp_mw_manifest_read(dir, m$date)
  if (!is.null(prev)) {
    for (k in names(prev$entries)) {
      if (prev$entries[[k]]$status %in% c("committed", "already_committed") &&
          !identical(m$entries[[k]]$status, prev$entries[[k]]$status)) {
        stop("Manifest would alter a committed warehouse.", call. = F)
      }
    }
  }
  file = rp_mw_manifest_file(dir, m$date)
  tmp = tempfile(".manifest_", tmpdir = dir)
  jsonlite::write_json(m, tmp, auto_unbox = T, pretty = T, na = "null")
  if (!file.rename(tmp, file)) stop("Cannot write manifest.", call. = F)
  invisible(file)
}

rp_mw_day_status = \(m, required) {
  st = vapply(required, \(i) {
    e = m$entries[[paste0("w", i)]]
    if (is.null(e)) "missing" else e$status
  }, character(1))
  done = st %in% c("committed", "already_committed")
  list(complete = all(done),
    open = required[!done], states = setNames(st, required))
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 4) ISOLATED DAILY ORCHESTRATION (COLLECTOR IS INJECTED)
# <-----------------------------------------------------------------------------

rp_mw_run_day = \(mw, date, collect_fn, manifest_dir, only = NULL,
  store_wids = integer()) {
  required = as.integer(unlist(mw$release$released_warehouses))
  if (!rp_mw_release_ok(mw, date)) {
    return(list(status = "blocked", reason = "no_valid_release",
      manifest = NULL, open = required))
  }
  m = rp_mw_manifest_read(manifest_dir, date)
  if (is.null(m)) m = rp_mw_manifest_new(date, required)
  done = vapply(m$entries, \(e) e$status %in%
    c("committed", "already_committed"), logical(1))
  committed = vapply(m$entries[done], \(e) as.integer(e$warehouse_id),
    integer(1))
  plan = rp_mw_plan(mw, date, committed, only, store_wids)
  max_att = mw$release$max_attempts_per_warehouse_per_day
  for (id in plan$run) {
    prior = m$entries[[paste0("w", id)]]
    if (!is.null(prior) && prior$attempts >= max_att) next
    res = tryCatch(collect_fn(id, date), error = \(e) e)
    ok = !inherits(res, "error") && res$status %in%
      c("committed", "already_committed")
    if (ok) {
      m = rp_mw_manifest_set(m, id, res$status, max_attempts = max_att)
    } else {
      why = if (inherits(res, "error")) conditionMessage(res) else
        "unexpected_result"
      m = rp_mw_manifest_set(m, id, "failed", substr(why, 1, 200),
        max_attempts = max_att)
    }
  }
  rp_mw_manifest_write(m, manifest_dir, mw)
  st = rp_mw_day_status(m, required)
  list(status = if (st$complete) "complete" else "incomplete",
    manifest = m, open = st$open, plan = plan)
}

rp_mw_require_complete = \(run) {
  if (!identical(run$status, "complete")) {
    stop(paste("Incomplete warehouse collection; open:",
      paste(run$open, collapse = ",")), call. = F)
  }
  invisible(T)
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 5) ADAPTER TO THE EXISTING SINGLE-WAREHOUSE COLLECTOR (NO LIVE CALL HERE)
# <-----------------------------------------------------------------------------

rp_mw_store_path = \(mw, store_root, id) {
  file.path(store_root, sub("{warehouse_id}", id, mw$panel_store_pattern,
    fixed = T))
}

rp_mw_fp_collect_fn = \(fp, mw, store_root, raw_root, log_root, transport,
  clock = Sys.time) {
  \(id, date) {
    w = Filter(\(x) x$warehouse_id == id, mw$warehouses)
    if (!length(w)) stop("Warehouse is not configured.", call. = F)
    w = w[[1]]
    if (!rp_mw_release_ok(mw, date) ||
        !id %in% unlist(mw$release$released_warehouses)) stop(
      "Warehouse is not released for this date.", call. = F)
    if (w$independent_sessions < mw$min_independent_sessions_for_production) {
      stop("Second independent warehouse session is missing.", call. = F)
    }
    collector = modifyList(fp$collector, list(warehouse_id = as.integer(id),
      warehouse_verification = list(status = "verified",
        independent_sessions = as.integer(w$independent_sessions))))
    policy = modifyList(fp$runner, list(live_authorized = T,
      terms_cleared = T))
    budget = fp$fp_budget(policy, mw$release$max_navigations_per_warehouse,
      mw$release$max_seconds_per_warehouse)
    run_dir = file.path(raw_root, paste0("w", id), date)
    ctx = fp$fp_ctx(collector, policy, budget, transport = transport,
      raw_dir = file.path(run_dir, "raw"),
      log_file = file.path(log_root, paste0("w", id), date, "run.jsonl"))
    res = fp$fp_collect_day(ctx, rp_mw_store_path(mw, store_root, id), clock)
    if (!identical(res$date, date)) stop("Collected on another day.",
      call. = F)
    res
  }
}

rp_mw_init_store = \(fp, mw, store_root, id) {
  fp$fp_panel_init(rp_mw_store_path(mw, store_root, id), id, "production")
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 6) COMBINED READ-ONLY VIEWS
# <-----------------------------------------------------------------------------

rp_mw_read_stores = \(fp, mw, store_root, ids) {
  out = lapply(ids, \(id) {
    h = fp$fp_panel_read(rp_mw_store_path(mw, store_root, id))
    if (h$identity$warehouse_id != id) stop("Store identity differs.",
      call. = F)
    h$panel
  })
  rp_mw_stack(out)
}

rp_mw_stack = \(panels) {
  x = do.call(rbind, panels)
  chk = rp_mw_check_keys(x)
  if (!chk$ok) stop("Duplicate observation keys.", call. = F)
  x$obs_key = rp_mw_obs_key(x$retailer, x$warehouse_id, x$sku, x$run_date)
  x
}

rp_mw_products = \(stack) {
  id = stack[!duplicated(stack$sku), c("sku", "product_key", "product_name")]
  by = tapply(stack$product_key, stack$sku, \(k) length(unique(k[!is.na(k)])))
  list(identities = id, conflicts = names(by)[by > 1L])
}

rp_mw_assortment = \(stack, date) {
  x = stack[stack$run_date == date & stack$listing_state == "listed", ]
  tab = table(x$sku, x$warehouse_id) > 0
  data.frame(sku = rownames(tab), as.data.frame.matrix(tab) * 1L,
    n_warehouses = rowSums(tab), check.names = F, row.names = NULL)
}

rp_mw_assortment_changes = \(stack, id) {
  x = stack[stack$warehouse_id == id & stack$listing_state == "listed", ]
  days = sort(unique(x$run_date))
  if (length(days) < 2L) return(data.frame(date = character(),
    added = integer(), removed = integer()))
  rows = lapply(seq_along(days)[-1], \(i) {
    a = x$sku[x$run_date == days[i - 1L]]
    b = x$sku[x$run_date == days[i]]
    data.frame(date = days[i], added = length(setdiff(b, a)),
      removed = length(setdiff(a, b)))
  })
  do.call(rbind, rows)
}

rp_mw_price_spread = \(stack, date) {
  x = stack[stack$run_date == date & stack$listing_state == "listed" &
    stack$available %in% T, ]
  rows = lapply(split(x, x$sku), \(g) {
    if (nrow(g) < 2L) return(NULL)
    data.frame(sku = g$sku[1], n_warehouses = nrow(g),
      min_cents = min(g$price_cents), max_cents = max(g$price_cents),
      spread_cents = max(g$price_cents) - min(g$price_cents),
      n_distinct = length(unique(g$price_cents)))
  })
  rows = rows[!vapply(rows, is.null, logical(1))]
  if (!length(rows)) return(data.frame())
  do.call(rbind, rows)
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 7) PRIVACY AND PATH GUARDS
# <-----------------------------------------------------------------------------

rp_mw_path_ok = \(path, mw) {
  parts = strsplit(normalizePath(path, mustWork = F), "/", fixed = T)[[1]]
  bad = unlist(mw$privacy$forbidden_path_parts)
  if (!startsWith(path, "/") && !startsWith(path, "~") ||
      any(vapply(bad, \(b) any(grepl(b, parts, fixed = T)), logical(1)))) {
    stop("Path is not a private local directory.", call. = F)
  }
  invisible(T)
}

rp_mw_privacy_check = \(df, mw) {
  bad_cols = intersect(names(df), unlist(mw$privacy$forbidden_columns))
  chr = df[vapply(df, is.character, logical(1))]
  txt = tolower(unlist(chr, use.names = F))
  street = "(stra(ss|ß)e|str\\.|weg|allee|platz|gasse)\\s*[0-9]+"
  list(ok = !length(bad_cols) && !any(grepl(street, txt)),
    forbidden_columns = bad_cols)
}
