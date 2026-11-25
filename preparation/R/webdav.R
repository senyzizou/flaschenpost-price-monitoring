# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING - FLASCHENPOST PREPARATION
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261027

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) TRANSPORT-INJECTED, SEPARATE FLASCHENPOST STATE INTERFACE
# <-----------------------------------------------------------------------------

# No live default transport or credential handling is enabled in this stage.
fp_dav_base = "APM/flaschenpost/validation/w36/state"

fp_dav_path = \(path) {
  if (length(path) != 1L || is.na(path) ||
      !startsWith(path, paste0(fp_dav_base, "/")) ||
      grepl("[\\\\%?#]|(^|/)[.]{1,2}(/|$)", path) ||
      grepl("//|/$", path)) stop("Invalid Flaschenpost WebDAV path.")
  path
}

# transport(method, path, input, output) must support conditional PUT:
# PUT_NEW creates only when absent (HTTP If-None-Match: *), never overwrites.
fp_dav_get = \(transport, path, output) {
  status = transport("GET", fp_dav_path(path), output = output)
  if (!status %in% c(200L, 404L)) stop("WebDAV read failed.")
  status == 200L
}

fp_dav_immutable = \(transport, local, remote) {
  remote = fp_dav_path(remote)
  expected = fp_hash(local)
  scratch = tempfile("fp_dav_read_")
  on.exit(unlink(scratch), add = T)
  if (fp_dav_get(transport, remote, scratch)) {
    if (!identical(fp_hash(scratch), expected)) stop(
      "Remote object conflicts.")
    return(invisible(expected))
  }
  status = transport("PUT_NEW", remote, input = local)
  if (!status %in% c(201L, 204L, 412L)) stop("WebDAV create failed.")
  if (!fp_dav_get(transport, remote, scratch) ||
      !identical(fp_hash(scratch), expected)) stop(
    "Remote verification failed.")
  invisible(expected)
}

fp_dav_directories = \(transport, paths) {
  directories = character()
  for (path in paths) {
    fp_dav_path(path)
    parts = strsplit(dirname(path), "/", fixed = T)[[1]]
    for (n in seq.int(2L, length(parts))) directories = c(directories,
      paste(parts[seq_len(n)], collapse = "/"))
  }
  for (path in unique(directories)) {
    status = transport("MKCOL", path)
    if (!status %in% c(201L, 405L)) stop("WebDAV directory creation failed.")
  }
}

fp_dav_dates = \(transport) {
  listing = tempfile("fp_dav_listing_")
  on.exit(unlink(listing), add = T)
  status = transport("PROPFIND", paste0(fp_dav_base, "/commits"),
    output = listing)
  if (status == 404L) return(character())
  if (status != 207L) stop("Cannot list committed Flaschenpost history.")
  doc = read_xml(listing, options = "NONET")
  hrefs = xml_text(xml_find_all(doc, '//*[local-name()="href"]'))
  names = basename(vapply(hrefs, URLdecode, ""))
  names = names[grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}[.]json$", names)]
  sort(unique(sub("[.]json$", "", names)))
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 2) VERIFIED CHECKPOINT PUBLICATION; COMMIT MARKER ALWAYS LAST
# <-----------------------------------------------------------------------------

fp_dav_publish = \(store, directory, transport, initialize = F) {
  verified = attr(transport, "capabilities_verified")
  if (is.function(verified) && !verified()) stop(
    "WebDAV publication needs verified server capabilities.")
  history = fp_panel_read(store)
  if (history$identity$warehouse_id != 36L) stop("WebDAV warehouse mismatch.")
  if (!length(history$days)) stop("Cannot publish empty history.")
  fp_dav_directories(transport, paste0(fp_dav_base, "/placeholder"))
  lock = paste0(fp_dav_base, "/writer_lock")
  # The lock itself must be created exactly once, never treated as existing.
  # Its parent is created above; no stale lock is automatically removed.
  status = transport("MKCOL_LOCK", lock)
  if (status != 201L) stop("Remote writer lock exists or cannot be acquired.")
  on.exit({
    if (!transport("DELETE_LOCK", lock) %in% c(200L, 204L)) stop(
      "Remote writer lock could not be released; inspect before retry.")
  }, add = T)
  dates = fp_dav_dates(transport)
  if (!length(dates) && !isTRUE(initialize)) stop(
    "Remote history missing; explicit initialization required.")
  if (length(dates)) {
    scratch = tempfile("fp_remote_history_")
    on.exit(unlink(scratch, recursive = T), add = T)
    restored = fp_dav_restore(tail(dates, 1L), scratch, transport,
      mode = history$identity$mode)
    if (!all(restored$days %in% history$days)) stop(
      "Local candidate omits committed remote history.")
    for (date in restored$days) {
      for (file in c("day.rds", "receipt.json")) {
        if (!identical(fp_hash(file.path(scratch, date, file)),
            fp_hash(file.path(store, date, file)))) stop(
          "Local candidate conflicts with remote history.")
      }
    }
  }
  checkpoint = file.path(directory, "checkpoint")
  manifest_sha = fp_panel_checkpoint(store, checkpoint)
  manifest = read_json(file.path(checkpoint, "checkpoint.json"))
  object = paste0(fp_dav_base, "/objects/", manifest_sha)
  date = tail(history$days, 1L)
  remote_paths = c(vapply(manifest$files, \(entry) paste0(object, "/",
    entry$path), ""), paste0(object, "/checkpoint.json"),
    paste0(fp_dav_base, "/commits/", date, ".json"))
  fp_dav_directories(transport, remote_paths)
  for (entry in manifest$files) fp_dav_immutable(transport,
    file.path(checkpoint, entry$path), paste0(object, "/", entry$path))
  fp_dav_immutable(transport, file.path(checkpoint, "checkpoint.json"),
    paste0(object, "/checkpoint.json"))
  # Checkpoint also carries a derived panel and quality report, never raw HTML.
  if (!identical(fp_panel_read(store)$head_sha256, history$head_sha256)) stop(
    "Local history changed before publication.")
  commit = list(version = fp_panel_version, date = date,
    warehouse_id = 36L, mode = history$identity$mode,
    checkpoint_sha256 = manifest_sha, head_sha256 = history$head_sha256)
  commit_file = file.path(directory, "commit.json")
  write_json(commit, commit_file, pretty = T, auto_unbox = T)
  fp_dav_immutable(transport, commit_file,
    paste0(fp_dav_base, "/commits/", date, ".json"))
  invisible(commit)
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 3) RESTORE INTO A NEW TARGET ONLY; NEVER FALL BACK TO CHECKOUT DATA
# <-----------------------------------------------------------------------------

fp_dav_restore = \(date, target, transport, mode = "production") {
  if (length(date) != 1L || is.na(as.Date(date)) ||
      !identical(format(as.Date(date), "%Y-%m-%d"), date)) stop(
    "Invalid restore date.")
  if (file.exists(target)) stop("Restore target must not exist.")
  scratch = tempfile("fp_dav_restore_")
  dir.create(scratch)
  on.exit(unlink(scratch, recursive = T), add = T)
  commit_file = file.path(scratch, "commit.json")
  if (!fp_dav_get(transport, paste0(fp_dav_base, "/commits/", date,
      ".json"), commit_file)) stop("No committed Flaschenpost history.")
  commit = read_json(commit_file, simplifyVector = T)
  hash = commit$checkpoint_sha256
  if (!identical(commit$date, date) || !identical(commit$mode, mode) ||
      commit$warehouse_id != 36L || !identical(commit$version,
        fp_panel_version) || length(hash) != 1L ||
      !grepl("^[0-9a-f]{64}$", hash)) stop("Invalid remote commit.")
  checkpoint = file.path(scratch, "checkpoint")
  dir.create(checkpoint)
  object = paste0(fp_dav_base, "/objects/", hash)
  manifest_file = file.path(checkpoint, "checkpoint.json")
  if (!fp_dav_get(transport, paste0(object, "/checkpoint.json"),
      manifest_file) || !identical(fp_hash(manifest_file), hash)) stop(
    "Missing or damaged remote checkpoint.")
  manifest = read_json(manifest_file)
  for (entry in manifest$files) {
    path = entry$path
    allowed = fp_checkpoint_member(path)
    if (!isTRUE(allowed)) stop("Unsafe checkpoint member.")
    output = file.path(checkpoint, path)
    dir.create(dirname(output), recursive = T, showWarnings = F)
    if (!fp_dav_get(transport, paste0(object, "/", path), output)) stop(
      "Remote checkpoint member missing.")
  }
  checked = fp_panel_read(checkpoint)
  if (!identical(checked$head_sha256, commit$head_sha256) ||
      !identical(tail(checked$days, 1L), date) ||
      !identical(checked$identity$mode, mode)) stop("Remote head differs.")
  fp_panel_restore(checkpoint, target, hash)
}
