# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261030

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) PATH AND PRIVACY GUARDS
# <-----------------------------------------------------------------------------

# Resolves symlinks of the nearest existing ancestor, then appends the rest.
fp_lr_resolve = \(path) {
  rest = character()
  p = path
  while (!file.exists(p) && !identical(dirname(p), p)) {
    rest = c(basename(p), rest)
    p = dirname(p)
  }
  do.call(file.path, as.list(c(normalizePath(p, mustWork = F), rest)))
}

fp_lr_inside = \(path, root) {
  p = fp_lr_resolve(path)
  r = normalizePath(root, mustWork = T)
  startsWith(p, paste0(r, "/")) || identical(p, r)
}

fp_lr_forbidden_part = \(path, parts) {
  segs = strsplit(fp_lr_resolve(path), "/",
    fixed = T)[[1]]
  segs = tolower(segs)
  any(vapply(tolower(parts), \(p) any(grepl(p, segs,
    fixed = T)), logical(1)))
}

fp_lr_header = \(file) {
  con = if (grepl("\\.gz$", file)) gzfile(file, "rt") else {
    file(file, "rt")
  }
  on.exit(close(con))
  line = readLines(con, n = 1L, warn = F)
  if (!length(line)) return(character())
  gsub('"', "", strsplit(line, ",", fixed = T)[[1]], fixed = T)
}

# Column-header scan of every exported table; raw API JSON is not a table.
fp_lr_privacy_scan = \(dir, forbidden_columns) {
  files = list.files(dir, pattern = "\\.csv(\\.gz)?$",
    recursive = T, full.names = T)
  hits = lapply(files, \(f) {
    cols = tolower(fp_lr_header(f))
    bad = intersect(cols, tolower(forbidden_columns))
    if (length(bad)) data.frame(file = f, column = bad,
      stringsAsFactors = F)
  })
  hits = hits[!vapply(hits, is.null, logical(1))]
  if (length(hits)) do.call(rbind, hits) else {
    data.frame(file = character(), column = character())
  }
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 2) PREFLIGHT (OFFLINE, NO HTTP)
# <-----------------------------------------------------------------------------

fp_lr_manifest_attempts = \(manifest_dir, date, wid) {
  m = rp_mw_manifest_read(manifest_dir, date)
  e = if (is.null(m)) NULL else m$entries[[paste0("w", wid)]]
  if (is.null(e)) 0L else as.integer(e$attempts)
}

fp_lr_preflight = \(root, mw, cfg, wid, date, store, out_dir,
  manifest_dir, runner_interval = 10L,
  verification_log = rp_vf_path(root)) {
  wid = as.integer(wid)
  ids = rp_mw_ids(mw)
  w = if (wid %in% ids) mw$warehouses[[which(ids == wid)]] else NULL
  history = if (dir.exists(store)) fp_vs_history(store) else list()
  store_wids = unique(vapply(history, \(m) {
    as.integer(m$warehouse_id)
  }, 1L))
  done = any(vapply(history, \(m) {
    as.integer(m$warehouse_id) == wid && identical(m$date, date)
  }, logical(1)))
  el = if (is.null(w)) NULL else {
    rp_mw_eligibility(mw, date, store_wids)
  }
  row = if (is.null(el)) NULL else el[el$warehouse_id == wid, ]
  caps = mw$release
  need_log = isTRUE(w$verification_log_required)
  supported = if (need_log) {
    rp_vf_supported(rp_vf_read(verification_log), mw, root, wid)
  } else NA_integer_
  rows = list(
    fp_cv_check("warehouse_configured", !is.null(w), wid),
    fp_cv_check("observation_unit", isTRUE(w$observation_unit)),
    fp_cv_check("two_independent_sessions",
      !is.null(row) && row$sessions_ok,
      paste0(w$independent_sessions, " of ",
        mw$min_independent_sessions_for_production)),
    fp_cv_check("session_count_backed_by_evidence",
      !need_log || (!is.null(w) &&
        w$independent_sessions <= supported),
      if (need_log) paste0("supported ", supported) else "not required"),
    fp_cv_check("category_baseline_state",
      !is.null(row) && row$category_ok &&
        !grepl("baseline_review_pending", row$reasons),
      paste(w$category_set, if (wid %in% store_wids) "(has commit)" else
        "(no commit yet)")),
    fp_cv_check("release_valid_for_date_and_warehouse",
      !is.null(row) && row$released,
      paste(caps$run_id, caps$valid_for_date)),
    fp_cv_check("regional_flags",
      isTRUE(cfg$live_authorized) && isTRUE(cfg$terms_cleared)),
    fp_cv_check("pacing_at_least_config_interval",
      runner_interval >= cfg$min_request_interval_seconds,
      runner_interval),
    fp_cv_check("attempts_left",
      fp_lr_manifest_attempts(manifest_dir, date, wid) <
        caps$max_attempts_per_warehouse_per_day,
      fp_lr_manifest_attempts(manifest_dir, date, wid)),
    fp_cv_check("not_already_committed", !done, date),
    fp_cv_check("store_exists_with_chain",
      dir.exists(store) && !is.null(tryCatch(
        fp_vs_history(store), error = \(e) NULL
      ))),
    fp_cv_check("output_inside_regional_project",
      fp_lr_inside(out_dir, root), out_dir),
    fp_cv_check("output_path_not_forbidden",
      !fp_lr_forbidden_part(out_dir,
        mw$privacy$forbidden_path_parts), out_dir)
  )
  checks = do.call(rbind, rows)
  list(
    ok = all(checks$ok), checks = checks,
    mode = if (wid %in% store_wids) "daily" else "baseline"
  )
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 3) WAREHOUSE-SPECIFIC COLLECTOR CONFIGURATION
# <-----------------------------------------------------------------------------

# The preparation project is only read. Verification comes from the regional
# configuration; the reviewed category set from this warehouse's own store.
fp_lr_collector_config = \(base, mw, cfg, wid, store, date) {
  wid = as.integer(wid)
  ids = rp_mw_ids(mw)
  w = mw$warehouses[[which(ids == wid)]]
  verification = if (identical(wid, as.integer(base$warehouse_id))) {
    base$warehouse_verification
  } else {
    entry = cfg$warehouses[[paste0("w", wid)]]
    list(
      status = if (is.null(entry)) "unverified" else {
        entry$verification_status
      },
      independent_sessions = as.integer(w$independent_sessions),
      method = if (is.null(entry)) NA_character_ else {
        entry$verification_method
      }
    )
  }
  prior = fp_cv_prior_from_store(store, wid, before = date)
  reviewed = if (is.null(prior)) integer() else {
    fp_cv_reviewed_ids(prior$categories)
  }
  modifyList(base, list(
    warehouse_id = wid,
    warehouse_verification = verification,
    reviewed_subcategory_ids = reviewed
  ))
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 4) ONE WAREHOUSE: COLLECT, CHECK, COMMIT
# <-----------------------------------------------------------------------------

fp_lr_assess_and_commit = \(root, store, mw, stored, date, wid,
  blocking, reassessment = NULL) {
  if (length(blocking) &&
      !identical(as.character(blocking), fp_cv_category_blocker)) {
    return(list(status = "blocked_collector", blocking = blocking,
      run_dir = stored))
  }
  hits = fp_lr_privacy_scan(stored, mw$privacy$forbidden_columns)
  if (nrow(hits)) {
    return(list(status = "blocked_privacy", hits = hits,
      run_dir = stored))
  }
  ref_id = as.integer(mw$reference_warehouse_id)
  prior = fp_cv_prior_from_store(store, wid, before = date)
  reference = if (identical(as.integer(wid), ref_id)) NULL else {
    fp_cv_prior_from_store(store, ref_id)
  }
  res = fp_pl_from_assessed_run(
    store, stored, date = date, warehouse_id = wid,
    prior = prior, reference = reference,
    require_classification = T, reassessment = reassessment
  )
  res$run_dir = stored
  res
}

fp_lr_run_warehouse = \(root, fp, mw, cfg, wid, date, store,
  out_base, collect_fn = NULL, sleep = Sys.sleep, transport = NULL,
  clock = Sys.time, run_id = NULL, reassessment = NULL) {
  wid = as.integer(wid)
  collector = fp_lr_collector_config(
    fp$collector, mw, cfg, wid, store, date
  )
  runner = modifyList(fp$runner, list(
    live_authorized = isTRUE(cfg$live_authorized),
    terms_cleared = isTRUE(cfg$terms_cleared)
  ))
  caps = mw$release
  if (is.null(run_id)) {
    run_id = format(Sys.time(), paste0("run_w", wid, "_%Y%m%d_%H%M%S"))
  }
  run_dir = file.path(out_base, paste0("w", wid), run_id)
  raw_dir = file.path(run_dir, "raw")
  fp$fp_private_dir(raw_dir, runner)
  budget = fp$fp_budget(
    runner, as.integer(caps$max_navigations_per_warehouse),
    as.numeric(caps$max_seconds_per_warehouse), sleep = sleep
  )
  ctx = fp$fp_ctx(
    collector, runner, budget, raw_dir = raw_dir,
    log_file = file.path(run_dir, "collector_log.jsonl"),
    clock = clock, sleep = sleep
  )
  if (!is.null(transport)) ctx$transport = transport
  dir.create(run_dir, showWarnings = F, recursive = T)
  classifier = fp$fp_classifier(collector)
  result = if (is.null(collect_fn)) {
    fp$fp_collect(
      ctx, wid, "inventory_proof", subcategory_ids = NULL,
      brand_checks = 8L, replicate_lists = 3L,
      probe_ids = as.integer(collector$probe_outside_scope_ids),
      classifier = classifier
    )
  } else collect_fn(ctx, wid, classifier)
  stored = fp$fp_write_run(
    result, run_dir, runner, run_date = as.Date(date)
  )
  assessed = fp_lr_assess_and_commit(
    root, store, mw, stored, date, wid, result$checks$blocking, reassessment
  )
  assessed$calls = result$calls
  assessed$raw_dir = raw_dir
  assessed
}

# Offline re-processing of a retained run (no HTTP).
fp_lr_replay_run = \(root, mw, store, stored, date, wid) {
  meta = jsonlite::read_json(
    file.path(stored, "meta.json"), simplifyVector = T
  )
  fp_lr_assess_and_commit(
    root, store, mw, stored, date, as.integer(wid),
    as.character(meta$inventory_blocking)
  )
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 5) ORCHESTRATION WITH PREFLIGHT AND MANIFEST
# <-----------------------------------------------------------------------------

fp_lr_run_day = \(root, fp, mw, cfg, wid, date, store, out_base,
  manifest_dir, collect_fn = NULL, dry_run = F) {
  pre = fp_lr_preflight(
    root, mw, cfg, wid, date, store, out_base, manifest_dir,
    runner_interval = as.integer(fp$runner$min_interval_seconds)
  )
  if (!pre$ok) {
    return(list(status = "preflight_failed", preflight = pre))
  }
  if (dry_run) return(list(status = "dry_run", preflight = pre))
  history = fp_vs_history(store)
  store_wids = unique(vapply(history, \(m) {
    as.integer(m$warehouse_id)
  }, 1L))
  fn = \(id, day) fp_lr_run_warehouse(
    root, fp, mw, cfg, id, day, store, out_base, collect_fn
  )
  day = rp_mw_run_day(
    mw, date, fn, manifest_dir, only = as.integer(wid),
    store_wids = store_wids
  )
  list(status = day$status, day = day, preflight = pre)
}
