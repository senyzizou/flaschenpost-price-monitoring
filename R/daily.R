# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261102

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) AUTOMATION CONFIGURATION AND BOUNDED RELEASE
# <-----------------------------------------------------------------------------

fp_dy_config = \(root, get = Sys.getenv) {
  auto = jsonlite::fromJSON(file.path(root, "config", "automation.json"),
    simplifyVector = F)
  if (identical(get("FP_LIVE_ENABLED"), "true")) {
    auto$enabled = T
    auto$authorization = list(valid_from = get("FP_VALID_FROM"),
      valid_until = get("FP_VALID_UNTIL"))
  }
  auto
}

fp_dy_today = \(auto, now = Sys.time()) {
  format(now, "%Y-%m-%d", tz = auto$timezone)
}

fp_dy_authorized = \(auto, date) {
  if (!isTRUE(auto$enabled)) return(F)
  dates = unlist(auto$authorization[c("valid_from", "valid_until")])
  if (length(dates) != 2L ||
      any(!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", dates))) return(F)
  days = as.Date(dates, format = "%Y-%m-%d")
  if (anyNA(days) || any(format(days, "%Y-%m-%d") != dates)) return(F)
  span = as.integer(days[2] - days[1]) + 1L
  span >= 1L && span <= auto$limits$max_authorization_days &&
    date >= dates[1] && date <= dates[2]
}

# A release exists only for one warehouse, today, and an existing baseline.
fp_dy_release = \(mw, auto, wid, date, today, store_wids) {
  ids = rp_mw_ids(mw)
  w = if (as.integer(wid) %in% ids) mw$warehouses[[which(ids == wid)]] else NULL
  az = fp_dy_authorized(auto, date)
  why = c(
    if (!az) "automation_not_authorized",
    if (!identical(date, today)) "date_is_not_today",
    if (!as.integer(wid) %in% as.integer(unlist(auto$warehouse_order))) {
      "warehouse_not_in_automation"
    },
    if (is.null(w)) "warehouse_not_configured",
    if (!is.null(w) &&
        !w$category_set %in% unlist(auto$allowed_category_sets)) {
      "baseline_not_reviewed"
    },
    if (!as.integer(wid) %in% as.integer(store_wids)) {
      "no_baseline_commit_automation_never_creates_baselines"
    }
  )
  rel = list(
    run_id = paste0("auto_", date, "_w", wid),
    released_warehouses = list(as.integer(wid)),
    valid_for_date = date,
    max_warehouses_per_run = 1L,
    max_attempts_per_warehouse_per_day =
      auto$limits$max_attempts_per_warehouse_per_day,
    max_navigations_per_warehouse = mw$release$max_navigations_per_warehouse,
    max_seconds_per_warehouse = mw$release$max_seconds_per_warehouse
  )
  mw_auto = mw
  mw_auto$release = rel
  list(ok = !length(why), reasons = why, mw = mw_auto, run_id = rel$run_id)
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 2) RECOVER OR COLLECT UNDER THE SAME REMOTE WRITER LOCK
# <-----------------------------------------------------------------------------

fp_dy_attempt = \(root, fp, release, cfg, store, work, transport,
  attempt, date, wid, started, site = NULL, clock = Sys.time,
  capture_run = fp_lr_run_warehouse) {
  is_live = !is.null(site)
  capture = fp_rc_read_checkpoint(transport, attempt)
  if (!is.null(capture)) {
    return(fp_rc_publish(transport, attempt, capture, store, date, wid))
  }
  rejected = paste0(attempt, "/rejected.rds")
  if (!is.null(fp_rc_get(transport, rejected))) return(NULL)
  if (!is.null(fp_rc_get(transport, paste0(attempt, "/access_block.rds")))) {
    return(NULL)
  }
  run_id = paste0("run_w", wid, "_", date, "_", basename(attempt))
  if (!identical(started$revision, Sys.getenv("GITHUB_SHA"))) {
    stop("Replay requires the workflow revision that started the capture.")
  }
  ctx = fp_rc_capture(transport, attempt, started$time, site, clock)
  wait = if (is_live) Sys.sleep else \(seconds) invisible(NULL)
  out = tempfile("capture_", tmpdir = work)
  dir.create(out, mode = "0700")
  on.exit(unlink(out, recursive = T), add = T)
  result = tryCatch(capture_run(root, fp, release, cfg, wid,
    date, store, out, transport = ctx$request,
    clock = ctx$clock, sleep = wait, run_id = run_id),
    fp_capture_incomplete = \(e) list(status = "interrupted_collection"))
  if (!result$status %in% c("committed", "already_committed")) {
    fp_rc_put(transport, rejected, result$status)
    return(NULL)
  }
  id = fp_vs_find_commit(store, date, wid)
  bundle = fp_rc_checkpoint(transport, attempt, store, id, result$calls,
    run_id, result$raw_dir)
  fp_rc_publish(transport, attempt, bundle, store, date, wid)
}

fp_dy_run_day = \(root, fp, mw, cfg, auto, store, work, transport,
  now = Sys.time(), site = fp$fp_http_transport,
  capture_run = fp_lr_run_warehouse, clock = Sys.time) {
  today = fp_dy_today(auto, now)
  if (!fp_dy_authorized(auto, today)) {
    return(list(status = "blocked", date = today))
  }
  berlin_clock = \() {
    current = clock()
    if (fp_dy_today(auto, current) != today) {
      stop("Berlin date changed during live collection.")
    }
    current
  }
  cfg$live_authorized = T
  cfg$terms_cleared = T
  fp_rm_with_lock(transport, \() {
    if (file.exists(store)) stop("Daily run requires a fresh local store.")
    fp_rm_restore(transport, store)
    history = fp_vs_history(store)
    store_wids = unique(vapply(history, \(m) as.integer(m$warehouse_id), 1L))
    runs = paste0(fp_rm_base, "/runs")
    dates = fp_rm_children(transport, runs)
    dates = dates[grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", dates)]
    last = max(vapply(history, \(m) m$date, ""))
    dates = sort(unique(c(dates[dates >= last & dates <= today], last,
      today)))
    rows = list()
    for (date in dates) {
      for (wid in as.integer(unlist(auto$warehouse_order))) {
        dir = sprintf("%s/%s/w%d", runs, date, wid)
        attempts = sort(fp_rm_children(transport, dir))
        attempts = attempts[grepl("^a[0-9]{2}$", attempts)]
        release = fp_dy_release(mw, auto, wid, today, today, store_wids)
        if (!release$ok) stop(paste(release$reasons, collapse = ", "))
        release$mw$release$valid_for_date = date
        id = NULL
        for (name in attempts) {
          attempt = paste0(dir, "/", name)
          started = fp_rc_get(transport, paste0(attempt, "/started.rds"))
          if (is.null(started)) next
          id = fp_dy_attempt(root, fp, release$mw, cfg, store, work,
            transport, attempt, date, wid, started, capture_run = capture_run)
          if (!is.null(id)) break
        }
        previous = fp_vs_find_commit(store, date, wid)
        if (is.null(id) && !is.null(previous)) {
          receipt = jsonlite::read_json(file.path(store, "commits",
            previous, "origin_receipt.json"))
          run_prefix = paste0("run_w", wid, "_", date, "_a")
          if (date == today || startsWith(receipt$run_id, run_prefix)) {
            stop("Store commit has no verified raw archive.")
          }
        }
        blocked = any(vapply(attempts, \(name) {
          path = paste0(dir, "/", name, "/access_block.rds")
          !is.null(fp_rc_get(transport, path))
        }, logical(1)))
        if (is.null(id) && date == today && !blocked &&
            length(attempts) < auto$limits$max_attempts_per_warehouse_per_day) {
          pre = fp_lr_preflight(root, release$mw, cfg, wid, date, store,
            work, work, as.integer(fp$runner$min_interval_seconds))
          if (!pre$ok) {
            stop(paste(pre$checks$check[!pre$checks$ok], collapse = ", "))
          }
          attempt = sprintf("%s/a%02d", dir, length(attempts) + 1L)
          started = list(time = berlin_clock(),
            revision = Sys.getenv("GITHUB_SHA"))
          fp_rm_mkcols(transport, paste0(attempt, "/responses/placeholder"))
          fp_rc_put(transport, paste0(attempt, "/started.rds"), started)
          id = fp_dy_attempt(root, fp, release$mw, cfg, store, work,
            transport, attempt, date, wid, started, site, berlin_clock,
            capture_run)
        }
        status = if (!is.null(id)) "verified" else if (blocked) {
          "access_blocked"
        } else "incomplete"
        rows[[length(rows) + 1L]] = data.frame(date = date,
          warehouse_id = wid, status = status,
          commit = if (is.null(id)) NA_character_ else id)
      }
    }
    days = do.call(rbind, rows)
    complete = all(days$status[days$date == today] == "verified")
    list(status = if (complete) "complete" else "incomplete",
      date = today, warehouses = days)
  })
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 4) DAILY RESULT SUMMARY (SMALL, SUITABLE FOR GITHUB)
# <-----------------------------------------------------------------------------

fp_dy_inclusion = \(panel) {
  abv = suppressWarnings(as.numeric(panel$alcohol_content))
  main = panel$analysis_ready_gt_0_5 %in% c(T, "TRUE") &
    !is.na(abv) & abv > 0.5
  sensitivity = panel$analysis_ready %in% c(T, "TRUE") &
    !is.na(abv) & abv >= 0.5
  list(main = main, sensitivity = sensitivity)
}

fp_dy_summary = \(store, date, root = ".") {
  h = fp_vs_history(store)
  ids = names(h)[vapply(h, \(m) identical(m$date, date), T)]
  rows = lapply(ids, \(cid) {
    m = h[[cid]]
    p = fp_vs_restore(store, date, date, as.integer(m$warehouse_id))
    p = fp_apply_annotations(p, root)
    use = fp_dy_inclusion(p)
    data.frame(
      date = date, warehouse_id = m$warehouse_id, commit = cid,
      skus = m$n_products, new_versions = m$new_versions,
      changed_products = m$changed_products,
      available = sum(p$available == "TRUE"),
      not_available = sum(p$available == "FALSE"),
      classified = sum(!is.na(p$category_state)),
      main_analysis = sum(use$main),
      sensitivity_analysis = sum(use$sensitivity),
      manifest_sha256 = fp_vs_hash(file.path(store, "commits", cid,
        "manifest.json")),
      stringsAsFactors = F
    )
  })
  if (length(rows)) do.call(rbind, rows) else NULL
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 5) WEBDAV ACCEPTANCE PROBE
# <-----------------------------------------------------------------------------

fp_rm_capability_probe = \(transport) {
  # Unique prefix leaves immutable test objects in the validation area.
  tag = substr(digest::digest(list(Sys.time(), runif(1)), "sha256"), 1, 16)
  dir = paste0(fp_rm_base, "/capability_probe/", tag)
  path = paste0(dir, "/first.txt")
  fp_rm_mkcols(transport, paste0(dirname(dir), "/placeholder"))
  files = tempfile("fp_probe_", tmpdir = tempdir())
  dir.create(files)
  on.exit(unlink(files, recursive = T), add = T)
  original = file.path(files, "original")
  different = file.path(files, "different")
  scratch = file.path(files, "scratch")
  writeLines("original", original)
  writeLines("different", different)
  made = transport("MKCOL", dir) == 201L
  exists = transport("MKCOL", dir) == 405L
  created = transport("PUT_NEW", path, input = original) == 201L
  rejected = transport("PUT_NEW", path, input = different) == 412L
  readback = fp_rm_get(transport, path, scratch) &&
    identical(fp_vs_hash(original), fp_vs_hash(scratch))
  listing = "first.txt" %in% fp_rm_children(transport, dir)
  lock_path = paste0(dir, "/writer_lock")
  acquired = transport("MKCOL_LOCK", lock_path) == 201L
  exclusive = acquired && transport("MKCOL_LOCK", lock_path) == 405L
  released = acquired &&
    transport("DELETE_LOCK", lock_path) %in% c(200L, 204L)
  checks = c(made = made, exists = exists, created = created,
    rejected = rejected, readback = readback, listing = listing,
    exclusive = exclusive, released = released)
  list(all_ok = all(checks), checks = checks)
}
