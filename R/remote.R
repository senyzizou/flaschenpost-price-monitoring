# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261102

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) REMOTE LAYOUT AND TRANSPORT CONTRACT
# <-----------------------------------------------------------------------------

# The remote archive (UKEbox over WebDAV) mirrors the local versioned store:
#   <base>/schema.json                       immutable
#   <base>/commits/<id>/<file>               immutable, one directory per commit
#   <base>/supplements/<id>/<file>           immutable
#   <base>/markers/commits/<id>.json         written LAST; complete = marker
#   <base>/markers/supplements/<id>.json     written LAST
#   <base>/raw/<date>/w<id>_<run>.tar.gz     original JSON responses
#   <base>/raw/<date>/w<id>_<run>.json       raw manifest, written LAST
#   <base>/runs/<date>/...json               unique, append-only run records
# Files without a marker are an unfinished upload: they are ignored by every
# restore and are completed (verified byte for byte) by the next publish.
# Existing remote objects are never overwritten.
#
# transport(method, path, input = NULL, output = NULL) returns an HTTP status.
# Methods: GET, PUT_NEW (create only), MKCOL, PROPFIND, MKCOL_LOCK,
# DELETE_LOCK. No transport is created or used offline.

fp_rm_base = "APM/flaschenpost/versioned"

fp_rm_path = \(path) {
  if (length(path) != 1L || is.na(path) ||
      !startsWith(path, paste0(fp_rm_base, "/")) ||
      grepl("[\\\\%?#]|(^|/)[.]{1,2}(/|$)", path) ||
      grepl("//|/$", path)) {
    stop("Invalid Flaschenpost remote path: ", path, call. = F)
  }
  path
}

fp_rm_get = \(transport, path, output) {
  status = transport("GET", fp_rm_path(path), output = output)
  if (!status %in% c(200L, 404L)) {
    stop("Remote read failed (HTTP ", status, "): ", path, call. = F)
  }
  status == 200L
}

# Creates the object if absent, verifies it byte for byte, never overwrites.
fp_rm_put_immutable = \(transport, local, remote, check_existing = T) {
  expected = fp_vs_hash(local)
  scratch = tempfile("fp_rm_read_")
  on.exit(unlink(scratch), add = T)
  if (check_existing && fp_rm_get(transport, remote, scratch)) {
    if (!identical(fp_vs_hash(scratch), expected)) {
      stop("Remote object conflicts with local file: ", remote, call. = F)
    }
    return(invisible("already_present"))
  }
  status = transport("PUT_NEW", fp_rm_path(remote), input = local)
  if (!status %in% c(201L, 204L, 412L)) {
    stop("Remote create failed (HTTP ", status, "): ", remote, call. = F)
  }
  if (!fp_rm_get(transport, remote, scratch) ||
      !identical(fp_vs_hash(scratch), expected)) {
    stop("Remote verification failed: ", remote, call. = F)
  }
  invisible("created")
}

fp_rm_mkcols = \(transport, remote_paths) {
  dirs = character()
  for (p in remote_paths) {
    fp_rm_path(p)
    parts = strsplit(dirname(p), "/", fixed = T)[[1]]
    base_n = length(strsplit(fp_rm_base, "/", fixed = T)[[1]])
    for (n in seq.int(base_n, length(parts))) {
      dirs = c(dirs, paste(parts[seq_len(n)], collapse = "/"))
    }
  }
  for (d in unique(dirs)) {
    status = transport("MKCOL", d)
    if (!status %in% c(201L, 405L)) {
      stop("Remote directory creation failed: ", d, call. = F)
    }
  }
}

# Names of the children of one remote directory (no XML dependency).
fp_rm_children = \(transport, dir_path) {
  out = tempfile("fp_rm_list_")
  on.exit(unlink(out), add = T)
  status = transport("PROPFIND", fp_rm_path(dir_path), output = out)
  if (status == 404L) return(character())
  if (status != 207L) {
    stop("Remote listing failed (HTTP ", status, "): ", dir_path, call. = F)
  }
  txt = paste(readLines(out, warn = F), collapse = "")
  m = regmatches(txt, gregexpr("<[A-Za-z]*:?href>[^<]+</", txt))[[1]]
  hrefs = sub("^<[A-Za-z]*:?href>", "", sub("</$", "", m))
  nm = basename(vapply(hrefs, utils::URLdecode, ""))
  nm[nzchar(nm) & nm != basename(dir_path)]
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 2) UNITS: COMMITS AND SUPPLEMENTS WITH A MARKER WRITTEN LAST
# <-----------------------------------------------------------------------------

fp_rm_unit_dir = \(store, kind, id) {
  file.path(store, if (kind == "commit") "commits" else "supplements", id)
}

fp_rm_unit_remote = \(kind, id) {
  paste0(fp_rm_base, "/", if (kind == "commit") "commits" else "supplements",
    "/", id)
}

fp_rm_marker_remote = \(kind, id) {
  paste0(fp_rm_base, "/markers/", kind, "s/", id, ".json")
}

# Deterministic marker (no timestamps) so that a repeated upload is identical.
fp_rm_marker = \(store, kind, id) {
  dir = fp_rm_unit_dir(store, kind, id)
  files = sort(list.files(dir))
  list(
    version = "fp-remote-1", kind = kind, id = id,
    manifest_sha256 = fp_vs_hash(file.path(dir, "manifest.json")),
    schema_sha256 = fp_vs_hash(file.path(store, "schema.json")),
    files = data.frame(
      file = files, bytes = file.size(file.path(dir, files)),
      sha256 = vapply(file.path(dir, files), fp_vs_hash, ""),
      stringsAsFactors = F, row.names = NULL
    )
  )
}

fp_rm_read_marker = \(transport, kind, id) {
  tmp = tempfile("fp_rm_marker_")
  on.exit(unlink(tmp), add = T)
  if (!fp_rm_get(transport, fp_rm_marker_remote(kind, id), tmp)) return(NULL)
  jsonlite::read_json(tmp, simplifyVector = T)
}

fp_rm_publish_unit = \(transport, store, kind, id) {
  dir = fp_rm_unit_dir(store, kind, id)
  if (!dir.exists(dir)) stop("No local ", kind, ": ", id, call. = F)
  base = fp_rm_unit_remote(kind, id)
  marker = fp_rm_marker(store, kind, id)
  remote_files = paste0(base, "/", marker$files$file)
  fp_rm_mkcols(transport, c(remote_files, fp_rm_marker_remote(kind, id)))
  created = 0L
  for (i in seq_along(remote_files)) {
    r = fp_rm_put_immutable(transport, file.path(dir, marker$files$file[i]),
      remote_files[i])
    if (identical(r, "created")) created = created + 1L
  }
  tmp = tempfile("fp_rm_marker_")
  on.exit(unlink(tmp), add = T)
  jsonlite::write_json(marker, tmp, auto_unbox = T, pretty = T)
  fp_rm_put_immutable(transport, tmp, fp_rm_marker_remote(kind, id))
  invisible(created)
}

fp_rm_remote_units = \(transport) {
  one = \(kind) {
    ids = sub("[.]json$", "", fp_rm_children(transport,
      paste0(fp_rm_base, "/markers/", kind, "s")))
    ids = sort(ids[grepl(if (kind == "commit") "^c[0-9]{4}$" else {
      "^s[0-9]{4}$"
    }, ids)])
    if (length(ids)) data.frame(kind = kind, id = ids, stringsAsFactors = F)
  }
  out = rbind(one("commit"), one("supplement"))
  if (is.null(out)) {
    data.frame(kind = character(), id = character(), stringsAsFactors = F)
  } else out
}

fp_rm_local_units = \(store) {
  ids_c = names(fp_vs_history(store))
  ids_s = names(fp_an_supplements(store))
  data.frame(
    kind = c(rep("commit", length(ids_c)), rep("supplement", length(ids_s))),
    id = c(ids_c, ids_s), stringsAsFactors = F
  )
}

# Local versus remote: ids only on one side, ids with different manifests.
fp_rm_compare = \(transport, store) {
  local = fp_rm_local_units(store)
  remote = fp_rm_remote_units(transport)
  key = \(u) paste(u$kind, u$id)
  both = intersect(key(local), key(remote))
  bad = character()
  for (k in both) {
    parts = strsplit(k, " ", fixed = T)[[1]]
    mk = fp_rm_read_marker(transport, parts[1], parts[2])
    here = fp_vs_hash(file.path(fp_rm_unit_dir(store, parts[1], parts[2]),
      "manifest.json"))
    if (is.null(mk) || !identical(mk$manifest_sha256, here)) {
      bad = c(bad, k)
    }
  }
  list(
    local_only = local[!key(local) %in% key(remote), , drop = F],
    remote_only = remote[!key(remote) %in% key(local), , drop = F],
    conflicts = bad
  )
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 3) INCREMENTAL, LOCKED, RETRYABLE PUBLICATION
# <-----------------------------------------------------------------------------

fp_rm_lock_path = paste0(fp_rm_base, "/writer_lock")
fp_rm_lock_info_path = paste0(
  fp_rm_base, "/writer_lock/lock_info.json")

fp_rm_write_lock_info = \(transport) {
  info = list(
    acquired_utc = format(
      Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    runner_id = Sys.getenv(
      "GITHUB_RUN_ID",
      unset = paste0("local_", Sys.getpid())),
    hostname = Sys.info()[["nodename"]]
  )
  tmp = tempfile("fp_lock_info_")
  on.exit(unlink(tmp), add = T)
  jsonlite::write_json(info, tmp,
    auto_unbox = T, pretty = T)
  transport("PUT_NEW", fp_rm_lock_info_path,
    input = tmp)
  invisible(info)
}

fp_rm_read_lock_info = \(transport) {
  tmp = tempfile("fp_lock_read_")
  on.exit(unlink(tmp), add = T)
  if (!fp_rm_get(transport, fp_rm_lock_info_path,
    tmp)) return(NULL)
  tryCatch(jsonlite::read_json(tmp), error = \(e)
    list(acquired_utc = NA_character_))
}

fp_rm_lock_probe = \(transport) {
  status = transport("PROPFIND", fp_rm_lock_path)
  exists = status == 207L
  info = if (exists) {
    fp_rm_read_lock_info(transport)
  }
  age_hours = if (!is.null(info) &&
    !is.na(info$acquired_utc)) {
    as.numeric(difftime(
      Sys.time(),
      as.POSIXct(info$acquired_utc,
        format = "%Y-%m-%dT%H:%M:%SZ",
        tz = "UTC"),
      units = "hours"))
  } else NA_real_
  list(
    locked = exists,
    info = info,
    age_hours = age_hours,
    stale = exists && !is.na(age_hours) &&
      age_hours > 3
  )
}

fp_rm_break_lock = \(transport, confirm = F) {
  probe = fp_rm_lock_probe(transport)
  if (!probe$locked) {
    return(list(status = "no_lock"))
  }
  if (!probe$stale && !confirm) {
    return(list(
      status = "active_lock_not_broken",
      info = probe$info,
      age_hours = probe$age_hours,
      reason = paste0(
        "Lock is ", round(probe$age_hours, 1),
        "h old (< 3h threshold). Pass ",
        "confirm = TRUE to force.")))
  }
  transport("DELETE_LOCK", fp_rm_lock_path)
  list(
    status = "broken",
    was_stale = probe$stale,
    info = probe$info,
    age_hours = probe$age_hours)
}

fp_rm_with_lock = \(transport, work) {
  fp_rm_mkcols(transport,
    paste0(fp_rm_base, "/schema.json"))
  lock = fp_rm_lock_path
  if (transport("MKCOL_LOCK", lock) != 201L) {
    probe = fp_rm_lock_probe(transport)
    stop("Remote writer lock exists",
      if (!is.null(probe$info)) paste0(
        " (acquired ", probe$info$acquired_utc,
        " by ", probe$info$runner_id, ")"),
      ". Use fp_rm_break_lock() if stale.",
      call. = F)
  }
  on.exit(transport("DELETE_LOCK", lock), add = T)
  fp_rm_write_lock_info(transport)
  work()
}

fp_rm_publish_store = \(transport, store, locked = F) {
  if (!locked) {
    return(fp_rm_with_lock(transport, \() {
      fp_rm_publish_store(transport, store, locked = T)
    }))
  }
  cmp = fp_rm_compare(transport, store)
  if (length(cmp$conflicts)) {
    stop("Local store conflicts with remote: ",
      paste(cmp$conflicts, collapse = ", "),
      call. = F)
  }
  if (nrow(cmp$remote_only)) {
    stop("Local store is behind the remote: ",
      paste(cmp$remote_only$id, collapse = ", "),
      call. = F)
  }
  fp_rm_put_immutable(transport,
    file.path(store, "schema.json"),
    paste0(fp_rm_base, "/schema.json"))
  todo = cmp$local_only
  published = character()
  for (i in seq_len(nrow(todo))) {
    fp_rm_publish_unit(transport, store,
      todo$kind[i], todo$id[i])
    published = c(published, todo$id[i])
  }
  list(
    status = if (length(published)) {
      "published"
    } else "nothing_to_publish",
    published = published)
}

fp_rm_try_publish = \(transport, store) {
  tryCatch(
    fp_rm_publish_store(transport, store),
    error = \(e) {
      list(status = "failed", published = character(),
        error = conditionMessage(e))
    })
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 4) RESTORE AND CATCH-UP FROM THE REMOTE ARCHIVE
# <-----------------------------------------------------------------------------

fp_rm_fetch_unit = \(transport, kind, id, dest_dir) {
  mk = fp_rm_read_marker(transport, kind, id)
  if (is.null(mk)) stop("Remote ", kind, " has no marker: ", id, call. = F)
  stopifnot(identical(mk$id, id), identical(mk$kind, kind),
    nrow(mk$files) > 0L, !anyDuplicated(mk$files$file),
    all(grepl("^[a-z][a-z0-9_.]*$", mk$files$file)))
  dir.create(dest_dir, recursive = T)
  base = fp_rm_unit_remote(kind, id)
  for (i in seq_len(nrow(mk$files))) {
    f = file.path(dest_dir, mk$files$file[i])
    if (!fp_rm_get(transport, paste0(base, "/", mk$files$file[i]), f)) {
      stop("Missing remote file: ", base, "/", mk$files$file[i], call. = F)
    }
    if (!identical(fp_vs_hash(f), mk$files$sha256[i]) ||
        file.size(f) != mk$files$bytes[i]) {
      stop("Corrupt remote file: ", base, "/", mk$files$file[i], call. = F)
    }
  }
  if (!identical(fp_vs_hash(file.path(dest_dir, "manifest.json")),
    mk$manifest_sha256)) {
    stop("Remote manifest differs from its marker: ", id, call. = F)
  }
  invisible(mk)
}

fp_rm_restore = \(transport, target) {
  if (file.exists(target)) stop("Restore target must not exist.", call. = F)
  units = fp_rm_remote_units(transport)
  if (!any(units$kind == "commit")) {
    stop("Remote archive has no committed history.", call. = F)
  }
  work = paste0(target, ".restoring")
  if (dir.exists(work)) unlink(work, recursive = T)
  on.exit(unlink(work, recursive = T), add = T)
  dir.create(file.path(work, "commits"), recursive = T)
  dir.create(file.path(work, "transactions"))
  if (!fp_rm_get(transport, paste0(fp_rm_base, "/schema.json"),
    file.path(work, "schema.json"))) {
    stop("Remote schema is missing.", call. = F)
  }
  for (i in seq_len(nrow(units))) {
    k = units$kind[i]
    id = units$id[i]
    if (k == "supplement") dir.create(file.path(work, "supplements"),
      showWarnings = F)
    mk = fp_rm_fetch_unit(transport, k, id, fp_rm_unit_dir(work, k, id))
    if (!identical(mk$schema_sha256, fp_vs_hash(file.path(work,
      "schema.json")))) {
      stop("Schema differs from marker: ", id, call. = F)
    }
  }
  h = fp_vs_history(work)
  s = fp_an_supplements(work)
  if (!file.rename(work, target)) stop("Cannot finish restore.", call. = F)
  invisible(list(commits = names(h), supplements = names(s)))
}

# Brings an older local store up to the remote state; never rewrites history.
fp_rm_catch_up = \(transport, store) {
  cmp = fp_rm_compare(transport, store)
  if (length(cmp$conflicts)) {
    stop("Local store conflicts with remote history: ",
      paste(cmp$conflicts, collapse = ", "), call. = F)
  }
  fetched = character()
  for (i in seq_len(nrow(cmp$remote_only))) {
    k = cmp$remote_only$kind[i]
    id = cmp$remote_only$id[i]
    local_ids = if (k == "commit") names(fp_vs_history(store)) else {
      names(fp_an_supplements(store))
    }
    if (length(local_ids) && id < max(local_ids)) {
      stop("Remote ", k, " ", id, " would rewrite local history.", call. = F)
    }
    tmp = tempfile("fp_rm_unit_")
    on.exit(unlink(tmp, recursive = T), add = T)
    fp_rm_fetch_unit(transport, k, id, tmp)
    if (k == "supplement") dir.create(file.path(store, "supplements"),
      showWarnings = F)
    dest = fp_rm_unit_dir(store, k, id)
    if (!file.rename(tmp, dest)) stop("Cannot place ", id, call. = F)
    fp_vs_history(store)
    fp_an_supplements(store)
    fetched = c(fetched, id)
  }
  list(fetched = fetched, local_only = cmp$local_only$id)
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 5) RAW JSON ARCHIVE AND RUN RECORDS
# <-----------------------------------------------------------------------------

fp_rm_archive_members = \(archive, expected) {
  members = utils::untar(archive, list = T, tar = "internal")
  stopifnot(!anyDuplicated(members), setequal(members, expected$file),
    all(grepl("^[0-9]{4}_[A-Za-z0-9_]+[.]json$", members)))
  dir = tempfile("fp_members_")
  dir.create(dir, mode = "0700")
  on.exit(unlink(dir, recursive = T), add = T)
  utils::untar(archive, files = expected$file, exdir = dir, tar = "internal")
  paths = file.path(dir, expected$file)
  stopifnot(all(file.exists(paths)), !any(nzchar(Sys.readlink(paths))),
    identical(unname(vapply(paths, fp_vs_hash, "")), expected$sha256))
  invisible(T)
}

# Existing pilot archive format, with readback even on a repeated transfer.
fp_rm_archive_raw = \(transport, raw_dir, date, wid, run_id) {
  stopifnot(grepl("^[A-Za-z0-9_-]+$", run_id))
  stem = paste0(fp_rm_base, "/raw/", date, "/w", wid, "_", run_id)
  fp_rm_mkcols(transport, paste0(stem, ".json"))
  dir = tempfile("fp_raw_")
  dir.create(dir, mode = "0700")
  on.exit(unlink(dir, recursive = T), add = T)
  marker = file.path(dir, "manifest.json")
  archive = file.path(dir, "raw.tar.gz")
  files = sort(list.files(raw_dir))
  stopifnot(length(files) > 0L,
    all(grepl("^[0-9]{4}_[A-Za-z0-9_]+[.]json$", files)))
  expected = data.frame(file = files, sha256 = unname(vapply(
    file.path(raw_dir, files), fp_vs_hash, "")), stringsAsFactors = F)
  marked = fp_rm_get(transport, paste0(stem, ".json"), marker)
  present = fp_rm_get(transport, paste0(stem, ".tar.gz"), archive)
  if (marked && !present) stop("Raw archive is missing behind its marker.")
  if (!present) {
    old = setwd(raw_dir)
    tryCatch(utils::tar(archive, files = files, compression = "gzip",
      tar = "internal"), finally = setwd(old))
    fp_rm_put_immutable(transport, archive, paste0(stem, ".tar.gz"))
  }
  fp_rm_archive_members(archive, expected)
  manifest = list(version = "fp-remote-raw-1", date = date,
    warehouse_id = as.integer(wid), run_id = run_id,
    tar_sha256 = fp_vs_hash(archive), tar_bytes = file.size(archive),
    files = expected)
  if (marked) {
    remote = jsonlite::read_json(marker, simplifyVector = T)
    stopifnot(identical(remote$files, expected),
      identical(remote$tar_sha256, manifest$tar_sha256),
      remote$tar_bytes == manifest$tar_bytes,
      identical(remote$warehouse_id, as.integer(wid)),
      identical(remote$date, date), identical(remote$run_id, run_id))
  } else {
    jsonlite::write_json(manifest, marker, auto_unbox = T, pretty = T)
    fp_rm_put_immutable(transport, marker, paste0(stem, ".json"))
  }
  invisible(list(status = "verified", files = nrow(expected)))
}

fp_rm_put_record = \(transport, date, wid, record) {
  nonce = paste0(format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"), "_",
    substr(digest::digest(runif(1)), 1, 6))
  remote = paste0(fp_rm_base, "/runs/", date, "/w", wid, "_", nonce, ".json")
  fp_rm_mkcols(transport, remote)
  tmp = tempfile("fp_rm_rec_")
  on.exit(unlink(tmp), add = T)
  jsonlite::write_json(record, tmp, auto_unbox = T, pretty = T, null = "null")
  fp_rm_put_immutable(transport, tmp, remote)
  invisible(remote)
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 6) HTTP TRANSPORT (BUILT BUT NOT USED BY THE OFFLINE STAGE)
# <-----------------------------------------------------------------------------

# Credentials come from FP_UKEBOX_* variables only; nothing is stored here.
# `perform(req, output)` can be replaced to test request building offline.
fp_rm_http = \(get = Sys.getenv, perform = NULL) {
  names = c("FP_UKEBOX_WEBDAV_URL", "FP_UKEBOX_USERNAME",
    "FP_UKEBOX_APP_PASSWORD")
  values = vapply(names, get, "")
  if (any(!nzchar(trimws(values)))) {
    stop("Missing Flaschenpost WebDAV environment variables.", call. = F)
  }
  base = sub("/+$", "", unname(values[1]))
  if (!identical(base, "https://box.uke.de/remote.php/webdav")) {
    stop("Unexpected Flaschenpost WebDAV endpoint.", call. = F)
  }
  if (is.null(perform)) {
    perform = \(req, output) {
      httr2::resp_status(httr2::req_perform(req, path = output))
    }
  }
  \(method, path, input = NULL, output = NULL) {
    allowed = c("GET", "PUT_NEW", "MKCOL", "PROPFIND", "MKCOL_LOCK",
      "DELETE_LOCK")
    if (!method %in% allowed) stop("Unsupported WebDAV method.", call. = F)
    if (!(method == "MKCOL" &&
        path %in% c("APM", "APM/flaschenpost", fp_rm_base))) {
      fp_rm_path(path)
    }
    actual = switch(method, PUT_NEW = "PUT", MKCOL_LOCK = "MKCOL",
      DELETE_LOCK = "DELETE", method)
    req = httr2::request(paste0(base, "/", path))
    req = httr2::req_auth_basic(req, unname(values[2]), unname(values[3]))
    req = httr2::req_method(req, actual)
    req = httr2::req_timeout(req, 120)
    req = httr2::req_options(req, followlocation = F, maxredirs = 0L,
      connecttimeout = 30, ssl_verifypeer = T, ssl_verifyhost = 2L)
    req = httr2::req_error(req, is_error = \(resp) F)
    if (method == "PROPFIND") req = httr2::req_headers(req, Depth = "1")
    if (method == "PUT_NEW") {
      req = httr2::req_headers(req, `If-None-Match` = "*")
    }
    if (!is.null(input)) req = httr2::req_body_file(req, input)
    status = tryCatch(suppressWarnings(perform(req, output)),
      error = \(e) NA_integer_)
    if (length(status) != 1L || is.na(status)) {
      stop("Flaschenpost WebDAV transport failure.", call. = F)
    }
    if (!status %in% c(200L, 201L, 204L, 207L, 404L, 405L, 409L, 412L)) {
      stop("Flaschenpost WebDAV stopped at HTTP ", status, ".", call. = F)
    }
    as.integer(status)
  }
}
