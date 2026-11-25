# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  FLASCHENPOST PRICE MONITORING
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261125

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) DURABLE CAPTURE AND OFFLINE REPLAY
# <-----------------------------------------------------------------------------

fp_rc_get = \(transport, path) {
  file = tempfile("fp_record_")
  on.exit(unlink(file), add = T)
  if (fp_rm_get(transport, path, file)) readRDS(file) else NULL
}

fp_rc_put = \(transport, path, record) {
  file = tempfile("fp_record_")
  on.exit(unlink(file), add = T)
  saveRDS(record, file, version = 3)
  fp_rm_put_immutable(transport, file, path, check_existing = F)
}

fp_rc_raw_hash = \(body) {
  digest::digest(charToRaw(enc2utf8(body)), "sha256", serialize = F)
}

fp_rc_capture = \(transport, attempt, started, http = NULL, clock = Sys.time) {
  state = new.env(parent = emptyenv())
  state$seq = 0L
  state$time = started
  request = \(url, query, collector) {
    seq = state$seq + 1L
    path = sprintf("%s/responses/%04d.rds", attempt, seq)
    signature = digest::digest(list(url, query), "sha256")
    if (is.null(http)) {
      entry = fp_rc_get(transport, path)
      if (is.null(entry)) {
        stop(errorCondition("Capture ends before collection completed.",
          class = "fp_capture_incomplete"))
      }
      stopifnot(identical(entry$request, signature),
        identical(entry$sha256, fp_rc_raw_hash(entry$body)))
    } else {
      clock()
      answer = http(url, query, collector)
      if (isTRUE(answer$status %in% c(403L, 429L))) {
        fp_rc_put(transport, paste0(attempt, "/access_block.rds"),
          as.integer(answer$status))
        stop(errorCondition("HTTP access blocked.", code = answer$status,
          class = "fp_access_blocked"))
      }
      if (!isTRUE(answer$status == 200L)) return(answer)
      entry = list(request = signature, time = clock(),
        body = answer$body, sha256 = fp_rc_raw_hash(answer$body))
      fp_rc_put(transport, path, entry)
    }
    state$seq = seq
    state$time = entry$time
    list(status = 200L, body = entry$body)
  }
  list(request = request, clock = \() state$time)
}

# The collector has already validated the catalog and created a local commit.
# Preserve those exact bytes before publishing anything to the visible store.
fp_rc_checkpoint = \(transport, attempt, store, id, calls, run_id, raw_dir) {
  dir = fp_rm_unit_dir(store, "commit", id)
  marker = fp_rm_marker(store, "commit", id)
  files = marker$files
  stopifnot(nrow(calls) > 0L, identical(calls$seq, seq_len(nrow(calls))))
  bodies = lapply(file.path(dir, files$file), \(p) {
    readBin(p, "raw", n = file.size(p))
  })
  raw = calls[, c("seq", "sha256", "at")]
  raw$file = sort(list.files(raw_dir))
  stopifnot(length(raw$file) == nrow(raw),
    identical(unname(vapply(file.path(raw_dir, raw$file), fp_vs_hash, "")),
      raw$sha256))
  capture = list(id = id, run_id = run_id, marker = marker,
    bodies = bodies, raw = raw)
  file = tempfile("fp_capture_")
  on.exit(unlink(file), add = T)
  saveRDS(capture, file, version = 3)
  path = paste0(attempt, "/capture_", fp_vs_hash(file), ".rds")
  fp_rm_put_immutable(transport, file, path)
  capture
}

fp_rc_read_checkpoint = \(transport, attempt) {
  names = fp_rm_children(transport, attempt)
  names = names[grepl("^capture_[0-9a-f]{64}[.]rds$", names)]
  if (!length(names)) return(NULL)
  file = tempfile("fp_capture_")
  on.exit(unlink(file), add = T)
  capture = NULL
  for (name in names) {
    path = paste0(attempt, "/", name)
    sha = substr(name, 9L, 72L)
    if (!fp_rm_get(transport, path, file)) {
      stop("A listed capture checkpoint is missing.")
    }
    if (!identical(fp_vs_hash(file), sha)) {
      message("Incomplete checkpoint detected. Checking the raw journal.")
      next
    }
    if (!is.null(capture)) stop("Multiple verified capture checkpoints.")
    capture = readRDS(file)
  }
  capture
}

fp_rc_verify = \(transport, attempt, capture) {
  raw = capture$raw
  stopifnot(nrow(raw) > 0L, identical(raw$seq, seq_len(nrow(raw))),
    all(grepl("^[0-9]{4}_[A-Za-z0-9_]+[.]json$", raw$file)))
  dir = paste0(attempt, "/responses")
  names = fp_rm_children(transport, dir)
  stopifnot(setequal(names, sprintf("%04d.rds", raw$seq)))
  for (i in raw$seq) {
    entry = fp_rc_get(transport, sprintf("%s/%04d.rds", dir, i))
    stopifnot(!is.null(entry), identical(entry$sha256, raw$sha256[i]),
      identical(fp_rc_raw_hash(entry$body), raw$sha256[i]),
      identical(format(entry$time, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        raw$at[i]))
  }
  invisible(T)
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 2) VERIFY THE CANDIDATE, THEN PUBLISH THE STORE COMMIT
# <-----------------------------------------------------------------------------

fp_rc_publish = \(transport, attempt, capture, store, date, wid) {
  fp_rc_verify(transport, attempt, capture)
  history = fp_vs_history(store)
  id = fp_vs_find_commit(store, date, wid)
  is_new = is.null(id)
  if (is_new) id = fp_vs_seq_dir(length(history) + 1L)
  marker = capture$marker
  files = marker$files
  stopifnot(identical(capture$id, id), identical(marker$id, id),
    identical(marker$kind, "commit"), !anyDuplicated(files$file),
    all(grepl("^[a-z][a-z0-9_.]*$", files$file)),
    length(capture$bodies) == nrow(files),
    identical(fp_vs_hash(file.path(store, "schema.json")),
      marker$schema_sha256))
  target = fp_rm_unit_dir(store, "commit", id)
  staged = tempfile("candidate_", tmpdir = store)
  dir.create(staged)
  on.exit(unlink(staged, recursive = T), add = T)
  for (i in seq_len(nrow(files))) {
    f = file.path(staged, files$file[i])
    writeBin(capture$bodies[[i]], f)
    stopifnot(identical(fp_vs_hash(f), files$sha256[i]),
      file.size(f) == files$bytes[i])
  }
  parent = if (is_new) length(history) else match(id, names(history)) - 1L
  previous = if (parent == 0L) "GENESIS" else {
    fp_vs_hash(file.path(store, "commits", names(history)[parent],
      "manifest.json"))
  }
  manifest = fp_vs_validate_commit(staged, marker$schema_sha256, previous)
  receipt = jsonlite::read_json(file.path(staged, "origin_receipt.json"))
  stopifnot(identical(manifest$date, date),
    identical(as.integer(manifest$warehouse_id), as.integer(wid)),
    identical(receipt$run_id, capture$run_id),
    setequal(files$file, c("manifest.json", manifest$files$file)),
    identical(fp_vs_hash(file.path(staged, "manifest.json")),
      marker$manifest_sha256))
  if (is_new) stopifnot(file.rename(staged, target))
  stopifnot(identical(fp_rm_marker(store, "commit", id), marker))
  fp_rm_publish_unit(transport, store, "commit", id)
  verified = tempfile("verified_", tmpdir = store)
  on.exit(unlink(verified, recursive = T), add = T)
  received = fp_rm_fetch_unit(transport, "commit", id, verified)
  stopifnot(identical(received$manifest_sha256, marker$manifest_sha256))
  invisible(id)
}
